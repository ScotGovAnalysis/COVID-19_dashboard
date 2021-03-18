# Read files ------------------------------------------------------------------
# These files are used multiple times - so we read them in once here. Other
# files are only used once, so they are read in as and when needed.
datasets[["sg_template"]] <- paths[["sg_template"]] %>%
  excel_sheets() %>%
  set_names() %>%
  map(read_excel, path = paths[["sg_template"]])

# 1 Direct health -------------------------------------------------------------
# R number --------------------------------------------------------------------
datasets[["1.1_R"]] <- datasets[["sg_template"]][["1.1_R"]] %>%
  spread(key = Variable, value = Value) %>%
  rename(low = `R lower bound`,
         high = `R upper bound`) %>%
  mutate(Date = as.Date(Date),
         middle = (high + low) / 2,
         text = paste("<b>R estimated between", low, "and", high, "</b>\n",
                      "on", format(Date, "%d %B %Y")),
         text_short = paste("<b>", low, "to", high, "</b>\n",
                            format(Date, "%d %B %Y"))) %>%
  rename(date = Date)


# Infectious people -----------------------------------------------------------
datasets[["1.2_infectious"]] <- datasets[["sg_template"]][["1.2_infectious"]] %>%
  spread(key = Variable, value = Value) %>%
  rename_at(.vars = vars(starts_with("Infectious_people_")),
            ~ stringr::str_remove(., "Infectious_people_")) %>% 
  rename(lowerbound = "Infections lower bound",
         upperbound = "Infections upper bound") %>%
  mutate(
    Date = as.Date(Date),
    midpoint = (lowerbound + upperbound) / 2,
    text = paste0(
      "<b>Between ",
      lowerbound,
      # format(lowerbound, big.mark = ","),
      " and ",
      upperbound,
      " new daily infections per 100k</b>\non ",
      format(Date, "%d %B %Y")
    ),
    text_short = paste0("<b>", format(lowerbound, big.mark = ","), " to ",
                        format(upperbound, big.mark = ","), "</b>\n",
                        format(Date, "%d %B %Y"))
  ) %>%
  rename(date = Date)


# Cases -----------------------------------------------------------------------
datasets[["1.3_cases"]] <-
  datasets[["sg_template"]][["1.3_cases"]] %>%
  mutate(
    date = as.Date(date),
    count_text = case_when(
      !is.na(count) ~ paste0(
        "<b>",
        format(count, big.mark = ","),
        " cases</b>\n",
        "(",
        format(date, "%d %B %Y"),
        ")"
      )
    ),
    count_7day_avg_text = case_when(
      !is.na(count_7day_avg) ~ paste0(
        "<b>",
        format(round(count_7day_avg, digits = 1), big.mark = ","),
        " average cases per day</b>\n",
        "(week ending ",
        format(date, "%d %B %Y"),
        ")"
      )
    ),
    count_7day_avg_text_short = case_when(
      !is.na(count_7day_avg) ~ paste0(
        "<b>",
        format(round(count_7day_avg, digits = 1), big.mark = ","),
        "</b>\n",
        "7 day average", "\n",
        "w/e ",
        format(date, "%d %B %Y")
      )
    )
  )

# Weekly deaths ---------------------------------------------------------------
datasets[["1.4_deaths"]] <-
  datasets[["sg_template"]][["1.4_deaths"]] %>%
  rename(week_num = `Week number`,
         week_beginning = `Week beginning`,
         count = `Deaths involving COVID-19`) %>%
  mutate(
    text = paste0(
      "<b>",
      count,
      " deaths registered with COVID-19 on death certificate</b>\n",
      "(week beginning ",
      format(week_beginning, "%d %B %Y"),
      ")"
    ),
    text_short = paste0(
      "<b>",
      count,
      "</b>\n",
      "w/b ",
      format(week_beginning, "%d %B %Y")
    )
  )

# New admissions to hospital with Covid-19 ------------------------------------
datasets[["1.5_admissions"]] <-
  datasets[["sg_template"]][["1.5_admissions"]] %>%
  rename(count = `Hospital admissions`,
         count_7day_avg = `7-Day Moving Average`) %>%
  mutate(
    text_count = paste0(
      "<b>",
      count,
      " new hospital admissions</b>\n",
      "(",
      format(Date, "%d %B %Y"),
      ")"
    ),
    text_7day_avg = paste0(
      "<b>",
      round(count_7day_avg, 1),
      " average new hospital admissions per day</b>\n",
      "(",
      format(Date, "%d %B %Y"),
      ")"
    ),
    text_7day_avg_short = paste0(
      "<b>",
      round(count_7day_avg, 1),
      "</b>\n",
      "7 day average", "\n",
      "w/e ",
      format(Date, "%d %B %Y")
    )
  )

# 2 Indirect health -----------------------------------------------------------
# A&E attendances -------------------------------------------------------------
datasets[["2.1_A&E"]] <-
  datasets[["sg_template"]][["2.1_A&E"]] %>%
  mutate(week_ending_date = as.Date(week_ending_date),
         year = lubridate::year(week_ending_date),
         week_ending_date_2020 = `year<-`(week_ending_date, 2020),
         week_id = format(week_ending_date, "%V"),
         year_chart = case_when(
           year < 2020 ~ "previous five years",
           year == 2020 ~ "2020",
           year == 2021 ~ "2021"
         ),
         # week_ending_date_2020 = case_when(
         #   year == 2021 ~ `year<-`(week_ending_date, 2021),
         #   year < 2021  ~ `year<-`(week_ending_date, 2020)),    
         text = paste0(
           "<b>",
           attendance %>% format(big.mark = ","),
           " A&E attendances</b>\n",
           "(week ending ",
           format(week_ending_date, "%d %B %Y"),
           ")"
         ),
         text_short = paste0(
           "<b>",
           attendance %>% format(big.mark = ","),
           "</b>\n",
           "w/e ",
           format(week_ending_date, "%d %B %Y")
         )) 

# to calculate five year average
five_year <- datasets[["2.1_A&E"]] %>% filter(year < 2020)%>% 
  group_by(week_id) %>%
  summarise(five_year_attendance=mean(attendance)) 
  # don't have full five years

five_year$five_year_attendance[as.numeric(five_year$week_id)<8] <- NA
datasets[["2.1_A&E"]] <- left_join(datasets[["2.1_A&E"]],five_year,by="week_id")


# Excess deaths ---------------------------------------------------------------
datasets[["2.2_excess"]] <-
  datasets[["sg_template"]][["2.2_excess"]] %>%
  mutate(date = as.Date(date)) %>%
  gather(key = "measure", value = "count", -week_number, -date) %>%
  mutate(linetype = case_when(
    measure == "average_previous_5_years" ~ "solid",
    TRUE ~ "dot")) %>%
  filter(date > as.Date("2020-03-11")) %>%
  mutate(text = case_when(
    measure == "total_deaths" ~ paste0(
      "<b>",
      count %>% as.integer() %>% format(big.mark = ","),
      " deaths</b>\n",
      "(week beginning ",
      format(date, "%d %B %Y"),
      ")"
    ),
    measure == "average_previous_5_years" ~ paste0(
      "<b>",
      count %>% format(big.mark = ","),
      " average deaths this week in 2015-19</b>\n",
      "(week beginning ",
      format(date, "%d %B %Y"),
      ")"
    ),
    measure == "COVID-19_deaths" ~ paste0(
      "<b>",
      count %>% as.integer() %>% format(big.mark = ","),
      " COVID-19 deaths</b>\n",
      "(week beginning ",
      format(date, "%d %B %Y"),
      ")"
    )
  ))

datasets[["2.2_excess_spark"]] <- datasets[["2.2_excess"]] %>%
  filter(measure != "COVID-19_deaths") %>%
  select(-c(linetype, text)) %>%
  spread(key = measure, value = count) %>%
  mutate(excess_deaths = total_deaths - average_previous_5_years,
         text = paste0(
           "<b>",
           round(excess_deaths, digits = 1),
           " excess deaths</b>\n",
           "(week beginning ",
           format(date, "%d %B %Y"),
           ")"
         ),
         text_short = paste0(
           "<b>",
           round(excess_deaths, digits = 1),
           "</b>\n",
           "7 day average", "\n",
           "w/b ",
           format(date, "%d %B %Y")
         )) %>%
  rename(all_2020 = total_deaths,
         avg_2015_19 = average_previous_5_years) %>%
  arrange(date)

# Emergency and planned admissions --------------------------------------------
datasets[["2.3_admissions"]] <-
  datasets[["sg_template"]][["2.3_admissions"]] %>%
  mutate(Week_ending = as.Date(Week_ending),
         text_variation = paste0(
           "<b>",
           scales::percent(-variation_rate, accuracy = 0.1),
           " fewer</b>\n",
           "than 2018/19\n",
           "w/e ",
           format(Week_ending, "%d %B %Y")
         ),
         text_variation_short = paste0(
           "<b>",
           scales::percent(-variation_rate, accuracy = 0.1),
           " fewer admissions</b>\n",
           "than the same weeks in 2018 and 2019\n",
           "(week ending ",
           format(Week_ending, "%d %B %Y"),
           ")"
         ),
         text_2020 = paste0(
           "<b>",
           format(Count, big.mark = ","),
           " ",
           stringr::str_to_lower(Admission_type),
           " admissions</b>\n",
           "(week ending ",
           format(Week_ending, "%d %B %Y"),
           ")"
         ),
         text_2018_19 = paste0(
           "<b>",
           format(Average_2018_2019, big.mark = ","),
           " ",
           stringr::str_to_lower(Admission_type),
           " admissions</b>\n",
           "(average for the same weeks in 2018 and 2019"
         ))


# Avoiding ----------------------------------------------------------------
datasets[["2.4_avoiding"]] <- datasets[["sg_template"]][["2.4_avoiding"]] %>%
  arrange(date_start) %>%
  mutate(date = forcats::as_factor(Date),
         date_start = as.Date(date_start),
         sentiment = factor(sentiment, levels = c("strongly disagree",
                                                  "tend to disagree",
                                                  "neither agree or disagree",
                                                  "tend to agree",
                                                  "strongly agree")),
         rate = percent / 100,
         text_2020 = paste0(
           "<b>",
           round(percent, digits = 0),
           "% of people ",
           sentiment,
           "</b>\n",
           "(",
           date,
           ")"
         )) %>%
  arrange(date_start, sentiment)



# 3 Society -------------------------------------------------------------------
## Children at school ---------------------------------------------------------
datasets[["3.1_schools"]] <-
  datasets[["sg_template"]][["3.1_schools"]] %>%
  mutate(date = as.Date(Date)) %>%
  full_join(tibble(date = seq(
    from = as_date(min(.$date)),
    to = as_date(max(.$date)),
    by = 1
  )), by = "date") %>% #add breaks for weekends
  arrange(date) %>%
  gather(key = "Measure",
         value = "count",
         Primary,
         Secondary_AM,
         Secondary_PM,
         Special
         ) %>%
   mutate(
     CYP_label = case_when(
       grepl("Primary", Measure, ignore.case = TRUE) ~
         "% of openings where primary school pupils were in attendance",
       grepl("Secondary_AM", Measure, ignore.case = TRUE) ~
         "% of secondary pupils physically attending in the morning",
       grepl("Secondary_PM", Measure, ignore.case = TRUE) ~
         "% of secondary pupils physically attending in the afternoon",
       grepl("Special", Measure, ignore.case = TRUE) ~
         "% of pupils at special schools physically attending"
     ),
       text = paste0(
       "<b>",
        format(round(count*100,1), big.mark = ","),
        CYP_label,
        "</b>\n",
        "(",
        format(date, "%A %d %B %Y"),
        ")"
      ),
      text_short = paste0(
        "<b>",
        format(round(count*100,1), big.mark = ","),
        "% attendance \n",
        "at special schools",
        "</b>\n",
        format(date, "%a %d %B %Y")
      )
   ) %>%
   select(Measure, date, count, text, text_short)

#up to 15 March chart: 
#datasets[["3.1_schools"]] <-
#  datasets[["sg_template"]][["3.1_schools"]] %>%
#  mutate(date = as.Date(Date),
#         attendance = All_attending) %>%
#  full_join(tibble(date = seq(
#    from = as_date(min(.$date)),
#    to = as_date(max(.$date)),
#    by = 1
#  )), by = "date") %>% #add breaks for weekends
#  arrange(date) %>%
#  filter(!is.na(attendance)) %>%
#  mutate(text = paste0(
#    "<b>",
#    format(round(attendance*100,1), big.mark = ","),
#    "% of children physically attended school</b>\n",
#    "</b>\n",
#    "(",
#    format(date, "%A %d %B %Y"),
#    ")"
#  ),
#  text_short = paste0(
#    "<b>",
#    format(round(attendance*100,1), big.mark = ","),
#    "%",
#    "</b>\n",
#    "physical attendance",
#    "\n",
#    format(date, "%d %B"))) %>%
#  select(date, attendance,text, text_short)

#   gather("Measure",
#          "count",
#          All_attending,
#          Non_covid_absence,
#          Covid_absence) %>%
#   mutate(
#     CYP_label = case_when(
#       grepl("All", Measure, ignore.case = TRUE) ~
#         "Percentage attendance",
#       grepl("Non_covid_absence", Measure, ignore.case = TRUE) ~
#         "Percentage of opening where pupils were not in school for non COVID \n related reasons (authorised and unauthorised, including exclusions)",
#       grepl("Covid_absence", Measure, ignore.case = TRUE) ~
#         "Percentage of openings where pupils were not in school because of \n COVID-19 related reasons"
#     ),
#     text = paste0(
#       "<b>",
#       format(round(count*100,0), big.mark = ","),
#       "% ",
#       CYP_label,
#       "</b>\n",
#       "(",
#       format(date, "%A %d %B %Y"),
#       ")"
#     ),
#     text_short = paste0(
#       "<b>",
#       format(round(count*100,0), big.mark = ","),
#       "% Covid-19\n related\n absence",
#       "</b>\n",
#       format(date, "%d %B")
#     )
#   ) %>%
#   select(Measure, date, count, text, text_short)





# BM start commenting

# 3 Society -------------------------------------------------------------------
## Children at school ---------------------------------------------------------
# datasets[["3.1_schools"]] <-
#   datasets[["sg_template"]][["3.1_schools"]] %>%
#   mutate(date = as.Date(Date)) %>%
#   full_join(tibble(date = seq(
#     from = min(.$date),
#     to = max(.$date),
#     by = 1
#   )), by = "date") %>% #add breaks for weekends
#   arrange(date) %>%
#   gather("Measure",
#          "count",
#          All_attending,
#          Non_covid_absence,
#          Covid_absence) %>%
#   mutate(
#     CYP_label = case_when(
#       grepl("All", Measure, ignore.case = TRUE) ~
#         "Percentage attendance",
#       grepl("Non_covid_absence", Measure, ignore.case = TRUE) ~
#         "Percentage of opening where pupils were not in school for non COVID \n related reasons (authorised and unauthorised, including exclusions)",
#       grepl("Covid_absence", Measure, ignore.case = TRUE) ~
#         "Percentage of openings where pupils were not in school because of \n COVID-19 related reasons"
#     ),
#     text = paste0(
#       "<b>",
#       format(round(count*100,0), big.mark = ","),
#       "% ",
#       CYP_label,
#       "</b>\n",
#       "(",
#       format(date, "%A %d %B %Y"),
#       ")"
#     ),
#     text_short = paste0(
#       "<b>",
#       format(round(count*100,0), big.mark = ","),
#       "% Covid-19\n related\n absence",
#       "</b>\n",
#       format(date, "%d %B")
#     )
#   ) %>%
#   select(Measure, date, count, text, text_short)

# BM end of uncommenting



# datasets[["3.1_schools"]] <-
#   datasets[["sg_template"]][["3.1_schools"]] %>%
#   mutate(date = as.Date(Date)) %>%
#   full_join(tibble(date = seq(
#     from = min(.$date),
#     to = max(.$date),
#     by = 1
#   )), by = "date") %>% #add breaks for weekends
#   arrange(date) %>%
#   gather("Measure",
#          "count",
#          All_CYP_attending,
#          Key_worker_CYP,
#          Vulnerable_CYP) %>%
#   mutate(
#     CYP_label = case_when(
#       grepl("All", Measure, ignore.case = TRUE) ~
#         "total children &\nyoung people attending",
#       grepl("Key", Measure, ignore.case = TRUE) ~
#         "key worker children &\nyoung people attending",
#       grepl("Vulnerable", Measure, ignore.case = TRUE) ~
#         "vulnerable children &\nyoung people attending"
#     )#,
#     # text = paste0(
#     #   "<b>",
#     #   format(count, big.mark = ","),
#     #   " ",
#     #   CYP_label,
#     #   "</b>\n",
#     #   "(",
#     #   format(date, "%A %d %B %Y"),
#     #   ")"
#     # ),
#     # text_short = paste0(
#     #   "<b>",
#     #   format(count, big.mark = ","),
#     #   " attending",
#     #   "</b>\n",
#     #   format(date, "%a %d %B %Y")
#     # )
#   ) #%>%
#   #select(Measure, date, count, text, text_short)

## Crisis applications --------------------------------------------------------
datasets[["3.2_crisis"]] <-
  datasets[["sg_template"]][["3.2_crisis"]] %>%
  mutate(month_ending_date = as.Date(month_ending_date),
         text = paste0(
           "<b>",
           format(crisis_applications, big.mark = ","),
           " crisis applications</b>\n",
           "(month ending ",
           format(month_ending_date, "%d %B %Y"),
           ")"
         ),
         text_short = paste0(
           "<b>",
           format(crisis_applications, big.mark = ","),
           "</b> \n",
           format(month_ending_date, "%d %B %Y")
         )
         )

#datasets[["3.2_crisis_spark"]] <-
#  datasets[["3.2_crisis"]] %>%
#  mutate(
#    year = lubridate::year(month_ending_date),
#    month = lubridate::month(month_ending_date, label = TRUE)
#  ) %>%
#  select(-c(text, month_ending_date)) %>%
#  spread(key = year, value = crisis_applications) %>%
#  drop_na() %>%
#  mutate(
#    variation = `2021` - mean(c(`2018`, `2019`)),
#    month_ending_date = datasets[["3.2_crisis"]] %>%
#      filter(lubridate::year(month_ending_date) == 2021) %>%
#      pull(month_ending_date),
#      text = paste0(
#      "<b>",
#      format(crisis_applications, big.mark = ","),
#      " more crisis applications</b> (",
#      format(month_ending_date, "%d %B %Y"),
#      ")",
#      "\nthan average of 2018/19."
#    ),
#    text_short = paste0(
#      "<b>",
#      format(crisis_applications, big.mark = ","),
#      " more</b> \n",
#      "than 2018/19 \n",
#      "(",
#      format(month_ending_date, "%B"),
#      ")"
#    )
#  )
  
## Crime ----------------------------------------------------------------------
datasets[["3.3_crime"]] <- datasets[["sg_template"]][["3.3_crime"]] %>%
  mutate(crime_group = factor(crime_group,
                              levels = c("Total crimes",
                                         "Crimes of dishonesty",
                                         "Other crimes",
                                         "Fire-raising, vandalism etc.",
                                         "Sexual crimes",
                                         "Non-sexual crimes of violence",
                                         "Total offences",
                                         "Miscellaneous offences",
                                         "Motor vehicle offences")),
         
#         month = factor(month, levels = month.name),
         date = as.Date(paste(year, month, "01"), format = "%Y %B %d") %>%
           lubridate::ceiling_date(unit = "month") - lubridate::days(1),
         text = paste0(
           "<b>",
           format(recorded, big.mark = ","),
           " ",
           stringr::str_to_lower(crime_group),
           " recorded</b>\n",
           "(",
           month,
           " ",
           year,
           ")"
         ),
         total = case_when(
           grepl("total", crime_group, ignore.case = TRUE) ~ TRUE,
           TRUE ~ FALSE
         )) %>%
  arrange(year, date)

datasets[["3.3_crime_spark"]] <- datasets[["3.3_crime"]] %>%
  filter(total == TRUE) %>%
  mutate(year_present = case_when(
    month=="Jan" ~ year - 1,
    month!="Jan" ~ year
  )) %>%
  select(crime_group, recorded, year_present, month) %>%
  spread(key = year_present, value = recorded) %>%
  mutate(variation = `2020` - `2019`,
         variation_rate = variation / `2019`,
         text_variation_short = case_when(
           month=="Jan" ~ paste0(
             "<b>",
             scales::percent(-variation_rate, accuracy = 0.1),
             " fewer</b>\n",
             "crimes than\n",
             month,
            " 2020"
         ),
           month!="Jan"~ paste0(
             "<b>",
             scales::percent(-variation_rate, accuracy = 0.1),
             " fewer</b>\n",
             "crimes than\n",
             month,
             " 2019"
           ))) %>%
  select(-c(`2019`, `2020`)) %>%
  mutate(date = as.Date(paste("2020", month, "01"), format = "%Y %B %d") %>%
           lubridate::ceiling_date(unit = "month") -
           lubridate::days(1)) %>%
  arrange(crime_group, date)




# Loneliness ------------------------------------------------------------------
datasets[["3.4_loneliness"]] <- datasets[["sg_template"]][["3.4_loneliness"]] %>%
  mutate(date = forcats::as_factor(Date),
         date_start = as.Date(date_start),
         percent = as.numeric(`%`),
         text_2020 = paste0(
           "<b>",
           format(percent, big.mark = ","),
           "% of people felt lonely in the past week</b>\n",
           "(",
           date,
           ")"
         ),
         text_2020_short = paste0(
           "<b>",
           format(percent, big.mark = ","),
           "%</b>\n",
           date
         ))

# Trust in Government----------------------------------------------------------
datasets[["3.5_trust"]] <- datasets[["sg_template"]][["3.5_trust"]] %>%
  mutate(date = forcats::as_factor(Date),
         date_start = as.Date(date_start),
         percent = as.numeric(`%`),
         text_2020 = paste0(
           "<b>",
           format(percent, big.mark = ","),
           "% of people trusted the</br>Scottish Government</b>\n",
           "(",
           date,
           ")"
         ),
         text_2020_short = paste0(
           "<b>",
           format(percent, big.mark = ","),
           "%</b>\n",
           date
         ))

# Threat to Jobs --------------------------------------------------------------
datasets[["3.6_job"]] <- datasets[["sg_template"]][["3.6_job"]] %>%
  mutate(date = forcats::as_factor(Date),
         date_start = as.Date(date_start),
         percent = as.numeric(`%`),
         text_2020 = paste0(
           "<b>",
           format(percent, big.mark = ","),
           "% of people perceived a</br>threat to their job/business</b>\n",
           "(",
           date,
           ")"
         ),
         text_2020_short = paste0(
           "<b>",
           format(percent, big.mark = ","),
           "%</b>\n",
           date
         ))

# Transport -------------------------------------------------------------------
datasets[["3.7_transport"]] <-
  datasets[["sg_template"]][["3.7_transport"]] %>%
  drop_na() %>%
  mutate(
    Date = forcats::as_factor(Date),
    percent = scales::percent(`%`, accuracy = 1),
    text = paste0(
      "<b>",
      percent,
      " of people say they will avoid public transport more than usual</b>\n",
      "(asked during ",
      Date,
      ")"
    ),
    text_short = paste0(
      "<b>",
      percent,
      "</b>\n",
      Date
    )
  )

# 4 Economy -------------------------------------------------------------------
# Turnover --------------------------------------------------------------------
datasets[["4.1_turnover"]] <- datasets[["sg_template"]][["4.1_turnover"]] %>%
  mutate(date = ymd(paste0(year, "-", month, "-", "01")),
         month_long = month(date, label = TRUE, abbr = FALSE),
         month_short = month(date, label = TRUE),
         text = paste0(
    "<b>",
    round(turnover, digits = 1),
    "</b>\n",
    # "of firms in ",
    # industry,
    # " reported increasing turnover</b>\n",
    # "in real terms compared to 12 months ago\n",
    # "(",
    month_long,
    " 2020"
  ),
  text_short = paste0(
    "<b>",
    round(turnover, digits = 1),
    "</b>\n",
    month_short,
    " '", substr(year, start = 3, stop = 4)
  ))

# datasets[["4.1_turnover"]] <- datasets[["sg_template"]][["4.1_turnover"]] %>%
#   rename(industry = `Monthly Business Turnover Index.`) %>%
#   gather(key = "month", value = turnover, -industry) %>%
#   mutate(month = forcats::as_factor(month),
#          date = as.Date(paste("2020", month, "01"), format = "%Y %B %d")) %>%
#   mutate(text = paste0(
#     "<b>",
#     round(turnover, digits = 1),
#     "% of firms in ",
#     industry,
#     " reported increasing turnover</b>\n",
#     "in real terms compared to 12 months ago\n",
#     "(",
#     month,
#     " 2020)"
#   ),
#   text_short = paste0(
#     "<b>",
#     round(turnover, digits = 1),
#     "%</b>\n",
#     month
#   ),
#   month_short = substr(month, 1, 3) %>% forcats::as_factor()) %>%
#   group_by(industry)

# GDP ----------------------------------------------------------------
datasets[["4.2_GDP"]] <- datasets[["sg_template"]][["4.2_GDP"]] %>%
  mutate(
    date = lubridate::as_date(paste0(year, month, "01")),
    text = paste0(
      "<b>Scottish GDP (2017=100) ",
      round(`GDP (2017=100)`, 1),
      "</b>\n",
      "(",
      month,
      " ",
      year,
      ")"
    ),
    text_short = paste0(
      "<b>",
      round(`GDP (2017=100)`, 1),
      "</b>\n",
      "2017 = 100\n",
      month,
      " ",
      year
    )
  )

# Unemployment ----------------------------------------------------------------
datasets[["4.3_unemployment"]] <-
  datasets[["sg_template"]][["4.3_unemployment"]] %>%
  mutate(
    quarter = forcats::as_factor(quarter),
    year_quarter = paste(year, quarter) %>% forcats::as_factor(),
    date = as.Date(paste(year,  substr(quarter, 7, 9), "01"),
                   format = "%Y %b %d"),
    text = paste0(
      "<b>",
      scales::percent(rate),
      " of people unemployed</b>\n",
      "(quarter ",
      quarter,
      " ",
      year,
      ")"
    ),
    text_short = paste0(
      "<b>",
      scales::percent(rate),
      "</b>\n",
      quarter,
      " ",
      year
    )
  )

datasets[["4_unemployment_spark"]] <- datasets[["4.3_unemployment"]] %>%
  filter(date >= as.Date("2020-01-01"))

# Claimant counts -------------------------------------------------------------
datasets[["4.4_claimants"]] <- datasets[["sg_template"]][["4.4_claimants"]] %>%
  rename(count = `Number of claimant counts`,
         change = `% change in claimant counts`) %>%
  mutate(count = count * 1000,
         date = lubridate::as_date(paste0(year, month, "01")),
         text = paste0(
           "<b>",
           format(count, big.mark = ","),
           " claimant counts</b>\n",
           "(",
           stringr::str_to_sentence(month),
           " ",
           year,
           ")"
         ),
         text_short = paste0(
           "<b>",
           format(count, big.mark = ","),
           "</b>\n",
           stringr::str_to_sentence(month),
           " ",
           year
         ))

