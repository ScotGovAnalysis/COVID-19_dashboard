# Read files ------------------------------------------------------------------
# These files are used multiple times - so we read them in once here. Other
# files are only used once, so they are read in as and when needed.
datasets[["sg_template"]] <- paths[["sg_template"]] %>%
  excel_sheets() %>%
  set_names() %>%
  map(read_excel, path = paths[["sg_template"]])

datasets[["nrs"]] <- paths[["nrs"]] %>%
  excel_sheets() %>%
  stringr::str_subset(pattern = "Table|data") %>%
  set_names() %>%
  map(read_excel, path = paths[["nrs"]])

datasets[["sitrep"]] <- read_excel(path = paths[["sitrep"]],
                                   sheet = "Data")

# 1 Direct health -------------------------------------------------------------
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


datasets[["1r_recent"]] <- datasets[["sg_template"]][["R"]] %>%
  spread(key = Variable, value = Value) %>%
  rename(low = `R lower bound`,
         high = `R upper bound`) %>%
  mutate(Date = as.Date(Date),
         middle = (high + low) / 2,
         text = paste("<b>R estimated between", low, "and", high, "</b>\n",
                      "on", format(Date, "%d %B %Y"))) %>%
  rename(date = Date)
# Dummy data used to mock-up a chart for the R number

datasets[["1_infect"]] <- datasets[["sg_template"]][["Infectious_people"]] %>%
  spread(key = Variable, value = Value) %>%
  rename_at(.vars = vars(starts_with("Infectious_people_")),
            ~stringr::str_remove(., "Infectious_people_")) %>%
  mutate(Date = as.Date(Date),
         text = paste0("<b>Between ", format(lowbound, big.mark = ","), " and ",
                       format(upperbound, big.mark = ","),
                       " infectious people</b>\non ",
                       format(Date, "%d %B %Y"))) %>%
  rename(date = Date)

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


datasets[["1a"]] <- datasets[["nrs"]][["Figure 1 data"]] %>%
  select(1:2) %>%
  `names<-`(.[2,]) %>%
  drop_na() %>%
  filter(Date != "Date") %>%
  mutate(Date = as.Date(as.numeric(Date), origin = "1899-12-30"),
         Count = as.numeric(Count)) %>%
  rename(count_cumulative = Count) %>%
  mutate(
    count = c(0, diff(count_cumulative)),
    count_7day_avg = data.table::frollmean(count, 7),
    count_7day_avg_text = case_when(
      !is.na(count_7day_avg) ~ paste0(
        "<b>", round(count_7day_avg, digits = 1),
        " average deaths per day\n", "</b>",
        "(week ending ", format(Date, "%d %B %Y"), ")"
      )
    ),
    count_text = paste0("<b>", count, " deaths registered ",
                        "with COVID-19 on death certificate</b>\n",
                        format(Date, "%a %d %B %Y"))
  )

datasets[["1b_weeknum_lookup"]] <-
  datasets[["nrs"]][["Figure 5 data"]][2:3,] %>%
  select(-1) %>%
  `names<-`(.[1,]) %>%
  filter(`Week 1` != "Week 1") %>%
  gather(key = "week", value = "week_ending_date") %>%
  mutate(week_ending_date = dmy(
    paste(week_ending_date,
          if_else(week == "Week 1", "2019", "2020"))),
    week = as.numeric(substr(
      week, start = 6, stop = length(week)
    )))

datasets[["1b"]] <- datasets[["nrs"]][["Figure 7 data"]][2:8,] %>%
  filter(
    `Figure 7: Deaths involving COVID-19 by location of death, weeks 14 to 23, 2020` %in%
      c("Week number",
        "Care Home",
        "Home / Non-institution",
        "Hospital",
        "Other institution")
  ) %>%
  `names<-`(.[1,]) %>%
  filter(`Week number` != "Week number") %>%
  rename(setting = `Week number`) %>%
  gather(key = "week", value = "deaths", -setting) %>%
  mutate(linetype = case_when(
    setting %in% c("Hospital", "Other institution") ~ "solid",
    TRUE ~ "dash"),
         week = as.numeric(week)) %>%
  left_join(datasets[["1b_weeknum_lookup"]], by = "week") %>%
  mutate(text = paste0("<b>", deaths, " deaths in ", setting, "</b>\n",
                      "(week ending ",
                      format(week_ending_date, "%d %B %Y"), ")"))



datasets[["1c"]] <- read_excel(path = paths[["sg"]],
                               sheet = "Table 2 - Hospital Care",
                               range = anchored("A4", dim = c(NA, 7))) %>%
  `names<-`(
    c(
      "date",
      "icu_hdu_conf",
      "icu_hdu_sus",
      "icu_hdu",
      "hosp_conf",
      "hosp_sus",
      "hosp"
    )
  ) %>%
  select(date, icu_hdu, hosp_conf) %>%
  gather(key = "location", value = "covid_patients", -date) %>%
  mutate(
    location_label = if_else(
      location == "hosp_conf",
      "People in hospital (including ICU) with confirmed COVID-19",
      "People in ICU with confirmed <b>or</b> suspected COVID-19"
    ),
    text = paste0("<b>", format(covid_patients, big.mark = ","), " ",
                  location_label, "</b>\n",
                  format(date, "%d %B %Y"))
  )

datasets[["1c_icu_hdu"]] <- datasets[["1c"]] %>%
  filter(location == "icu_hdu") %>%
  mutate(text = paste0(
    "<b>",
    covid_patients,
      " people in ICU/HDU on ",
    format(date, "%d %B %Y"),
    "\n</b>",
    "(confirmed and suspected COVID-19)"
  ))

datasets[["1c_hosp_conf"]] <- datasets[["1c"]] %>%
  filter(location == "hosp_conf") %>%
  mutate(text = paste0(
    "<b>",
    covid_patients %>% format(big.mark = ","),
    " people in hospital on ",
    format(date, "%d %B %Y"),
    "\n</b>",
    "(confirmed COVID-19 only)"
  ))

# 2 Indirect health -----------------------------------------------------------
datasets[["2a"]] <- read.csv(paths[["phs"]]) %>%
  select(week_ending_date, attendance) %>%
  mutate(week_ending_date = as.Date(week_ending_date, format = "%Y-%m-%d")) %>%
  # filter(week_ending_date > as.Date('2018-12-31')) %>%
  mutate(text = paste0(
    "<b>",
    attendance %>% format(big.mark = ","),
    " A&E attendances</b>\n",
    "(week ending ",
    format(week_ending_date, "%d %B %Y"),
    ")"
  ))

datasets[["2a_recent"]] <- datasets[["2a"]] %>%
  filter(week_ending_date > as.Date("2020-03-01"))

datasets[["2_excess"]] <- datasets[["nrs"]][["Figure 5 data"]] %>%
  filter(
    `Figure 5: Deaths by week of registration, Scotland, 2020` %in%
      c("Week number",
        "Total deaths 2020",
        "Average for previous 5 years",
        "COVID-19 deaths 2020")
  ) %>%
  `names<-`(.[1,]) %>%
  filter(`Week number` != "Week number") %>%
  rename(measure = `Week number`) %>%
  gather(key = "week", value = "count", -measure) %>%
  mutate(week = stringr::str_remove(week, pattern = "Week "),
         week = as.integer(week),
         count = as.numeric(count)) %>%
  left_join(datasets[["1b_weeknum_lookup"]], by = "week") %>%
  rename(date = week_ending_date) %>%
  mutate(linetype = case_when(
    measure == "Average for previous 5 years" ~ "solid",
    TRUE ~ "dot")) %>%
  filter(date > as.Date("2020-03-11")) %>%
  mutate(text = case_when(
    measure == "Total deaths 2020" ~ paste0(
      "<b>",
      count %>% as.integer() %>% format(big.mark = ","),
      " deaths</b>\n",
      "(week ending ",
      format(date, "%d %B %Y"),
      ")"
    ),
    measure == "Average for previous 5 years" ~ paste0(
      "<b>",
      count %>% format(big.mark = ","),
      " average deaths this week in 2015-19</b>\n",
      "(week ending ",
      format(date, "%d %B %Y"),
      ")"
    ),
    measure == "COVID-19 deaths 2020" ~ paste0(
      "<b>",
      count %>% as.integer() %>% format(big.mark = ","),
      " COVID-19 deaths</b>\n",
      "(week ending ",
      format(date, "%d %B %Y"),
      ")"
    )
  ))

datasets[["2_excess_spark"]] <- datasets[["2_excess"]] %>%
  filter(measure != "COVID-19 deaths 2020") %>%
  select(-c(linetype, text)) %>%
  spread(key = measure, value = count) %>%
  mutate(excess_deaths = `Total deaths 2020` - `Average for previous 5 years`,
         text = paste0(
           "<b>",
           excess_deaths,
           " excess deaths</b>\n",
           "(week ending ",
           format(date, "%d %B %Y"),
           ")"
         )) %>%
  rename(all_2020 = `Total deaths 2020`,
         avg_2015_19 = `Average for previous 5 years`)


datasets[["2_admissions"]] <- read.csv(paths[["phs_admissions"]]) %>%
  select(-c(Area_name, Area_type, Category, Specialty)) %>%
  rename(variation = Variation....) %>%
  mutate(Week_ending = as.Date(Week_ending, format = "%d-%b-%y"),
         week = lubridate::week(Week_ending),
         text_variation = paste0(
           "<b>",
           -variation,
           "% fewer admissions</b>\n",
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
           "(average for week ",
           week,
           " in 2018 and 2019)"
         ))

datasets[["2_GP"]] <- datasets[["sg_template"]][["Avoiding_GPs&Hospitals"]] %>%
  mutate(date = as.Date(Date),
         percent = as.numeric(`%`),
         text_2020 = paste0(
           "<b>",
           format(percent, big.mark = ","),
           "% of people avoiding GPs & Hospitals</b>\n",
           "(",
           format(date, "%d %B %Y"),
           ")"
         )) %>%
  select(Measure,date,percent,text_2020)

# 3 Society -------------------------------------------------------------------
## Vulnerable children at school ----------------------------------------------
datasets[["3a"]] <- datasets[["sitrep"]] %>%
  filter(Data == "Children vulnerable attending") %>%
  select(-c(Slide, Data)) %>%
  gather(key = "date", value = "children") %>%
  mutate(date = as.Date(as.numeric(date), origin = "1899-12-30"),
         children = as.numeric(children),
         text = paste0(
           "<b>",
            format(children, big.mark = ","),
           " vulnerable children at school</b>\n",
           "(",
           format(date, "%d %B %Y"),
           ")"
         ))

## Crisis applications --------------------------------------------------------


datasets[["3_crisis_applications"]] <-
  datasets[["sg_template"]][["data_crisis_applications"]] %>%
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
  ) %>%
  filter(month > "Feb")
  
## Crime ----------------------------------------------------------------------
datasets[["3_crime"]] <- datasets[["sg_template"]][["data_recorded_crime"]] %>%
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
datasets[["3_loneliness"]] <- datasets[["sg_template"]][["Loneliness"]] %>%
  mutate(date = as.Date(Date),
         percent = as.numeric(`%`),
         text_2020 = paste0(
           "<b>",
           format(percent, big.mark = ","),
           "% of people felt lonely in the past week</b>\n",
           "(",
           format(date, "%d %B %Y"),
           ")"
         )) %>%
  select(Measure,date,percent,text_2020)

# Trust in Government----------------------------------------------------------
datasets[["3_trust"]] <- datasets[["sg_template"]][["Trust_in_Government_(SG)"]] %>%
  mutate(date = as.Date(Date),
         percent = as.numeric(`%`),
         text_2020 = paste0(
           "<b>",
           format(percent, big.mark = ","),
           "% of people trust the</br>Scottish Government</b>\n",
           "(",
           format(date, "%d %B %Y"),
           ")"
         )) %>%
  select(Measure,date,percent,text_2020)

# Threat to Jobs --------------------------------------------------------------
datasets[["3_job"]] <- datasets[["sg_template"]][["Threat_To_Job"]] %>%
  mutate(date = as.Date(Date),
         percent = as.numeric(`%`),
         text_2020 = paste0(
           "<b>",
           format(percent, big.mark = ","),
           "% of people who perceive a</br>threat to their job/business</b>\n",
           "(",
           format(date, "%d %B %Y"),
           ")"
         )) %>%
  select(Measure,date,percent,text_2020)

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

