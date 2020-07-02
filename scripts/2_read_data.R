# Read files ------------------------------------------------------------------
# These files are used multiple times - so we read them in once here. Other
# files are only used once, so they are read in as and when needed.
datasets[["sg_template"]] <- paths[["sg_template"]] %>%
  excel_sheets() %>%
  set_names() %>%
  map(read_excel, path = paths[["sg_template"]])

# datasets[["sg_template"]][["TEXT"]] <-
#   datasets[["sg_template"]][["TEXT"]] %>%
#   group_by(worksheet_name) %>%
#   mutate(
#     spark_text = case_when(
#       worksheet_name %in% c("H3_job", "H3_trust", "H2_emergency") ~ paste0(
#         "<b>",
#         TITLE_max_35_characters,
#         ":</b>\n",
#         HEADLINE_max_60_characters %>%
#           stringr::str_wrap(width = 32)
#       ),
#       TRUE ~ paste0(
#         "<b>",
#         TITLE_max_35_characters,
#         ":</b> ",
#         HEADLINE_max_60_characters
#       ) %>% stringr::str_wrap(width = 32)
#     )
#   )

datasets[["sg_template"]][["TEXT"]] <-
  datasets[["sg_template"]][["TEXT"]] %>%
  mutate(
    spark_text = paste0("<H3 style = 'margin:0; font-size:18px;'>",
                        TITLE_max_35_characters,
                        "</H3>",
                        HEADLINE_max_60_characters)
    )

datasets[["nrs"]] <- paths[["nrs"]] %>%
  excel_sheets() %>%
  stringr::str_subset(pattern = "Table|data") %>%
  set_names() %>%
  map(read_excel, path = paths[["nrs"]])

datasets[["sitrep"]] <- read_excel(path = paths[["sitrep"]],
                                   sheet = "Data")

# 1 Direct health -------------------------------------------------------------
# R number --------------------------------------------------------------------
# Dummy data used to mock-up a chart for the R number
datasets[["1r"]] <- data.frame(date = as.Date(c("2020-02-19",
                                        "2020-03-11",
                                        "2020-03-16",
                                        "2020-03-18",
                                        "2020-03-23",
                                        "2020-05-20",
                                        "2020-05-27")),
                               high = c(6.8, 6.5, 3.1, 3, 1, 0.9, 0.9),
                               low = c(4.1, 4, 1.9, 1.8, 0.7, 0.7, 0.7)) %>%
  mutate(middle = (high + low) / 2,
         text = paste("<b>R estimated between", low, "and", high, "</b>\n",
                      "on", format(date, "%d %B %Y")))




datasets[["1r_recent"]] <- datasets[["sg_template"]][["H1_R"]] %>%
  spread(key = Variable, value = Value) %>%
  rename(low = `R lower bound`,
         high = `R upper bound`) %>%
  mutate(Date = as.Date(Date),
         middle = (high + low) / 2,
         text = paste("<b>R estimated between", low, "and", high, "</b>\n",
                      "on", format(Date, "%d %B %Y"))) %>%
  rename(date = Date)


# Infectious people -----------------------------------------------------------
datasets[["1_infect"]] <-
  datasets[["sg_template"]][["H1_infectious"]] %>%
  spread(key = Variable, value = Value) %>%
  rename_at(.vars = vars(starts_with("Infectious_people_")),
            ~ stringr::str_remove(., "Infectious_people_")) %>%
  mutate(
    Date = as.Date(Date),
    text = paste0(
      "<b>Between ",
      format(lowerbound, big.mark = ","),
      " and ",
      format(upperbound, big.mark = ","),
      " infectious people</b>\non ",
      format(Date, "%d %B %Y")
    )
  ) %>%
  rename(date = Date)


# Cases -----------------------------------------------------------------------
datasets[["1_cases"]] <- datasets[["sitrep"]] %>%
  filter(Data == "Overnight change in total confirmed cases TOTAL") %>%
  select(-c(Slide, Data)) %>%
  gather(key = "date", value = "cases") %>%
  drop_na(cases) %>%
  mutate(
    date = as.Date(as.numeric(date), origin = "1899-12-30"),
    cases = as.numeric(cases),
    cases_7day_avg = data.table::frollmean(cases, 7),
    cases_text = case_when(
      !is.na(cases) ~ paste0(
        "<b>",
        format(cases, big.mark = ","),
        " cases</b>\n",
        "(",
        format(date, "%d %B %Y"),
        ")"
      )
    ),
    cases_7day_avg_text = case_when(
      !is.na(cases_7day_avg) ~ paste0(
        "<b>",
        format(round(cases_7day_avg, digits = 1), big.mark = ","),
        " average cases per day</b>\n",
        "(week ending ",
        format(date, "%d %B %Y"),
        ")"
      )
    )
  )

datasets[["1_cases"]] <-
  datasets[["sg_template"]][["H1_cases"]] %>%
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
    )
  )

datasets[["1b_weeknum_lookup"]] <-
  datasets[["nrs"]][["Figure 5 data"]][2:3, ] %>%
  select(-1) %>%
  `names<-`(.[1, ]) %>%
  filter(`Week 1` != "Week 1") %>%
  gather(key = "week", value = "week_ending_date") %>%
  mutate(week_ending_date = dmy(
    paste(week_ending_date,
          if_else(week == "Week 1", "2019", "2020"))),
    week = as.numeric(substr(
      week, start = 6, stop = length(week)
    )))

# Weekly deaths ---------------------------------------------------------------
datasets[["H1_deaths"]] <-
  datasets[["sg_template"]][["H1_deaths"]] %>%
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
    )
  )

# New admissions to hospital with Covid-19 ------------------------------------
datasets[["H1_admissions"]] <-
  datasets[["sg_template"]][["H1_admissions"]] %>%
  rename(count = `Hospital Admissions`,
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
    )
  )

# 2 Indirect health -----------------------------------------------------------
# A&E attendances -------------------------------------------------------------
datasets[["2a"]] <-
  datasets[["sg_template"]][["H2_A&E"]] %>%
  mutate(week_ending_date = as.Date(week_ending_date),
         week_ending_date_2020 = `year<-`(week_ending_date, 2020),
         year = lubridate::year(week_ending_date),
         text = paste0(
           "<b>",
           attendance %>% format(big.mark = ","),
           " A&E attendances</b>\n",
           "(week ending ",
           format(week_ending_date, "%d %B %Y"),
           ")"
         ))

datasets[["2a_recent"]] <- datasets[["2a"]] %>%
  filter(week_ending_date > as.Date("2020-01-01"))

# Excess deaths ---------------------------------------------------------------
datasets[["2_excess"]] <-
  datasets[["sg_template"]][["H2_excess"]] %>%
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
      "(week ending ",
      format(date, "%d %B %Y"),
      ")"
    ),
    measure == "average_previous_5_years" ~ paste0(
      "<b>",
      count %>% format(big.mark = ","),
      " average deaths this week in 2015-19</b>\n",
      "(week ending ",
      format(date, "%d %B %Y"),
      ")"
    ),
    measure == "COVID-19_deaths" ~ paste0(
      "<b>",
      count %>% as.integer() %>% format(big.mark = ","),
      " COVID-19 deaths</b>\n",
      "(week ending ",
      format(date, "%d %B %Y"),
      ")"
    )
  ))

datasets[["2_excess_spark"]] <- datasets[["2_excess"]] %>%
  filter(measure != "COVID-19_deaths") %>%
  select(-c(linetype, text)) %>%
  spread(key = measure, value = count) %>%
  mutate(excess_deaths = total_deaths - average_previous_5_years,
         text = paste0(
           "<b>",
           round(excess_deaths, digits = 1),
           " excess deaths</b>\n",
           "(week ending ",
           format(date, "%d %B %Y"),
           ")"
         )) %>%
  rename(all_2020 = total_deaths,
         avg_2015_19 = average_previous_5_years)

# Emergency and planned admissions --------------------------------------------
datasets[["H2_admissions"]] <-
  datasets[["sg_template"]][["H2_admissions"]] %>%
  mutate(Week_ending = as.Date(Week_ending),
         text_variation = paste0(
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

# People avoiding GPs or hospitals --------------------------------------------
# datasets[["2_GP"]] <- datasets[["sg_template"]][["H2_avoiding"]] %>%
#   mutate(date = as.Date(Date),
#          percent = as.numeric(`%`),
#          text_2020 = paste0(
#            "<b>",
#            format(percent, big.mark = ","),
#            "% of people avoiding GPs & Hospitals</b>\n",
#            "(",
#            format(date, "%d %B %Y"),
#            ")"
#          )) %>%
#   select(Measure, date, percent, text_2020)

datasets[["2_GP"]] <- datasets[["sg_template"]][["H2_avoiding"]] %>%
  mutate(date = forcats::as_factor(Date),
         date_start = as.Date(date_start),
         sentiment = forcats::as_factor(sentiment) %>% forcats::fct_rev(),
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
  arrange(sentiment)

# 3 Society -------------------------------------------------------------------
## Children at school ---------------------------------------------------------
datasets[["3_school"]] <-
  datasets[["sg_template"]][["H3_schools"]] %>%
  mutate(date = as.Date(Date)) %>%
  full_join(tibble(date = seq(
    from = min(.$date),
    to = max(.$date),
    by = 1
  )), by = "date") %>% #add breaks for weekends
  arrange(date) %>%
  gather("Measure",
         "count",
         All_CYP_attending,
         Key_worker_CYP,
         Vulnerable_CYP) %>%
  mutate(
    CYP_label = case_when(
      grepl("All", Measure, ignore.case = TRUE) ~
        "total children &\nyoung people attending",
      grepl("Key", Measure, ignore.case = TRUE) ~
        "key worker children &\nyoung people attending",
      grepl("Vulnerable", Measure, ignore.case = TRUE) ~
        "vulnerable children &\nyoung people attending"
    ),
    text = paste0(
      "<b>",
      format(count, big.mark = ","),
      " ",
      CYP_label,
      "</b>\n",
      "(",
      format(date, "%A %d %B %Y"),
      ")"
    )
  ) %>%
  select(Measure, date, count, text)

## Crisis applications --------------------------------------------------------
datasets[["3_crisis_applications"]] <-
  datasets[["sg_template"]][["H3_crisis"]] %>%
  mutate(month_ending_date = as.Date(month_ending_date),
         text = paste0(
           "<b>",
           format(crisis_applications, big.mark = ","),
           " crisis applications</b>\n",
           "(month ending ",
           format(month_ending_date, "%d %B %Y"),
           ")"
         ))

datasets[["3_crisis_applications_spark"]] <-
  datasets[["3_crisis_applications"]] %>%
  mutate(
    year = lubridate::year(month_ending_date),
    month = lubridate::month(month_ending_date, label = TRUE)
  ) %>%
  select(-c(text, month_ending_date)) %>%
  spread(key = year, value = crisis_applications) %>%
  drop_na() %>%
  mutate(
    variation = `2020` - mean(c(`2018`, `2019`)),
    month_ending_date = datasets[["3_crisis_applications"]] %>%
      filter(lubridate::year(month_ending_date) == 2020) %>%
      pull(month_ending_date),
    text = paste0(
      "<b>",
      format(round(variation), big.mark = ","),
      " more crisis applications</b> (",
      format(month_ending_date, "%d %B %Y"),
      ")",
      "\nthan average of previous 2 years."
    )
  )
  
## Crime ----------------------------------------------------------------------
datasets[["3_crime"]] <- datasets[["sg_template"]][["H3_crime"]] %>%
  filter(month == "May") %>%
  arrange(year, recorded) %>%
  mutate(crime_group = forcats::as_factor(crime_group),
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
         ))

datasets[["3_crime_spark"]] <- datasets[["3_crime"]] %>%
  filter(total == TRUE) %>%
  select(crime_group, recorded, year, month) %>%
  spread(key = year, value = recorded) %>%
  mutate(variation = `2020` - `2019`) %>%
  select(-c(`2019`, `2020`)) %>%
  mutate(date = as.Date(paste("2020", month, "01"), format = "%Y %B %d") %>%
           lubridate::ceiling_date(unit = "month") -
           lubridate::days(1))

# Loneliness ------------------------------------------------------------------
# datasets[["3_loneliness"]] <- datasets[["sg_template"]][["H3_loneliness"]] %>%
#   mutate(date = as.Date(Date),
#          percent = as.numeric(`%`),
#          text_2020 = paste0(
#            "<b>",
#            format(percent, big.mark = ","),
#            "% of people felt lonely in the past week</b>\n",
#            "(",
#            format(date, "%d %B %Y"),
#            ")"
#          )) %>%
#   select(Measure, date, percent, text_2020)

datasets[["3_loneliness"]] <- datasets[["sg_template"]][["H3_loneliness"]] %>%
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
         ))

# Trust in Government----------------------------------------------------------
# datasets[["3_trust"]] <- datasets[["sg_template"]][["H3_trust"]] %>%
#   mutate(date = as.Date(Date),
#          percent = as.numeric(`%`),
#          text_2020 = paste0(
#            "<b>",
#            format(percent, big.mark = ","),
#            "% of people trusted the</br>Scottish Government</b>\n",
#            "(",
#            format(date, "%d %B %Y"),
#            ")"
#          )) %>%
#   select(Measure, date, percent, text_2020)

datasets[["3_trust"]] <- datasets[["sg_template"]][["H3_trust"]] %>%
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
         ))

# Threat to Jobs --------------------------------------------------------------
# datasets[["3_job"]] <- datasets[["sg_template"]][["H3_job"]] %>%
#   mutate(date = as.Date(Date),
#          percent = as.numeric(`%`),
#          text_2020 = paste0(
#            "<b>",
#            format(percent, big.mark = ","),
#            "% of people perceived a</br>threat to their job/business</b>\n",
#            "(",
#            format(date, "%d %B %Y"),
#            ")"
#          )) %>%
#   select(Measure, date, percent, text_2020)

datasets[["3_job"]] <- datasets[["sg_template"]][["H3_job"]] %>%
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
         ))

# Transport -------------------------------------------------------------------
datasets[["H3_transport"]] <-
  datasets[["sg_template"]][["H3_transport"]] %>%
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
    )
  )

# 4 Economy -------------------------------------------------------------------
datasets[["4a"]] <- datasets[["sitrep"]] %>%
  filter(Data == "Number of Universal Credit Claims") %>%
  select(-c(Slide, Data)) %>%
  gather(key = "date", value = "claims") %>%
  mutate(date = as.Date(as.numeric(date), origin = "1899-12-30"),
         claims = as.numeric(claims)) %>%
  drop_na() %>%
  mutate(claims_7day_avg = data.table::frollmean(claims, 7),
         claims_text = paste0(
           "<b>",
           format(claims, big.mark = ","),
           " claims on</b> ",
           format(date, "%A %d %B %Y")
         ),
         claims_7day_avg_text = paste0(
           "<b>",
           claims_7day_avg %>% round() %>% format(big.mark = ","),
           " average claims per day</b>\n",
           "(week ending ",
           format(date, "%d %B %Y"),
           ")"
         ))

# Turnover --------------------------------------------------------------------
datasets[["4_turnover"]] <- datasets[["sg_template"]][["H4_turnover"]] %>%
  rename(industry = `Monthly Business Turnover Index.`) %>%
  gather(key = "month", value = turnover, -industry) %>%
  mutate(month = forcats::as_factor(month),
         date = as.Date(paste("2020", month, "01"), format = "%Y %B %d")) %>%
  mutate(text = paste0(
    "<b>",
    round(turnover, digits = 1),
    "% of firms in ",
    industry,
    " reported increasing turnover</b>\n",
    "in real terms compared to 12 months ago\n",
    "(",
    month,
    " 2020)"
  ),
  month_short = substr(month, 1, 3) %>% forcats::as_factor()) %>%
  group_by(industry)

# Unemployment ----------------------------------------------------------------
datasets[["4_unemployment"]] <-
  datasets[["sg_template"]][["H4_unemployment"]] %>%
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
    )
  )

datasets[["4_unemployment_spark"]] <- datasets[["4_unemployment"]] %>%
  filter(date >= as.Date("2020-01-01"))

# Claimant counts -------------------------------------------------------------
datasets[["4_claimants"]] <- datasets[["sg_template"]][["H4_claimants"]] %>%
  rename(count = `Number of claimant counts (LHS)`,
         change = `% change in claimant counts (RHS)`) %>%
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
         ))

# GDP ----------------------------------------------------------------
datasets[["4_GDP"]] <- datasets[["sg_template"]][["H4_GDP"]] %>%
  mutate(
    date = lubridate::as_date(paste0(year, month, "01")),
    text = paste0(
      "<b>Scottish GDP (2016=100) ",
      round(gdp, 1),
      "</b>\n",
      "(",
      month,
      " ",
      year,
      ")"
    )
  )
