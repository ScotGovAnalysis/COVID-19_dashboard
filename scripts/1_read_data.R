# Read Harms 1 ------------------------------------------------------------
datasets[["1a"]] <- read_excel(
  path = paths[["nrs"]],
  sheet = "Figure 1 data",
  range = anchored("A3", dim = c(NA, 2)),
  col_types = c("date", "numeric")
) %>%
  drop_na() %>%
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

datasets[["1b_weeknum_lookup"]] <- read_excel(path = paths[["nrs"]],
                                              sheet = "Figure 5 data",
                                              range = cell_rows(3:4)) %>%
  select(-`Week number`) %>%
  gather(key = "week", value = "week_ending_date") %>%
  mutate(week_ending_date = dmy(paste(week_ending_date, "2020")),
         week = as.numeric(substr(
           week, start = 6, stop = length(week)
         )))

datasets[["1b"]] <- read_excel(path = paths[["nrs"]],
                      sheet = "Figure 7 data",
                      range = cell_rows(3:9)) %>% 
  drop_na() %>%
  rename(setting = `Week number`) %>% 
  gather(key = "week", value = "deaths", -setting) %>% 
  mutate(linetype = case_when(setting %in% c("Hospital", "Other institution") ~ "solid",
                              TRUE ~ "dash"),
         week = as.numeric(week)) %>% 
  left_join(datasets[["1b_weeknum_lookup"]], by = "week") %>% 
  mutate(text = paste0("<b>", deaths, " deaths in ", setting, "</b>\n",
                      "(week ending ", format(week_ending_date, "%d %B %Y"), ")"))



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

# Read Harms 2 ------------------------------------------------------------
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
  filter(week_ending_date > as.Date('2020-03-01'))
  

# Read Harms 3 ------------------------------------------------------------
datasets[["3a"]] <- read_excel(path = paths[["sitrep"]],
                      sheet = "Data",
                      range = cell_rows(1:159)) %>% 
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

# Read Harms 4 ------------------------------------------------------------
datasets[["4a"]] <- read_excel(path = paths[["sitrep"]],
                               sheet = "Data",
                               range = cell_rows(1:256)) %>% 
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

# Read annotations --------------------------------------------------------
annotations <- read_excel(path = paths[["text"]],
                          sheet = "annotations") %>% 
  mutate(x = as.Date(x)) %>% 
  bind_rows(
    datasets[["1a"]] %>% 
      filter(Date == max(Date)) %>%
      select(count_7day_avg, Date) %>% 
      mutate(Date = as.Date(Date),
             text = "7 day average",
             plot = "1a",
             dataset = "1a",
             showarrow = FALSE,
             xanchor = "left",
             xshift = 5,
             align = "left") %>% 
      rename(y = count_7day_avg,
             x = Date),
    datasets[["1b"]] %>% 
      filter(week == max(week)) %>%
      select(setting, deaths, week_ending_date) %>% 
      mutate(plot = "1b",
             dataset = "1b",
             showarrow = FALSE,
             xanchor = "left",
             align = "left",
             xshift = 5) %>% 
      rename(y = deaths,
             text = setting,
             x = week_ending_date),
    datasets[["1c"]] %>% 
      filter(date == as.Date("2020-05-09")) %>%
      select(location_label, covid_patients, date) %>% 
      mutate(date = as.Date(date),
             location_label = stringr::str_wrap(location_label, width = 30),
             plot = "1c",
             dataset = "1c",
             showarrow = FALSE,
             xanchor = "left",
             yanchor = "bottom",
             yshift = 5,
             align = "left") %>% 
      rename(y = covid_patients,
             text = location_label,
             x = date),
    datasets[["2a"]] %>% 
      filter(week_ending_date == max(week_ending_date)) %>%
      select(week_ending_date, attendance) %>% 
      mutate(plot = "2a",
             dataset = "2a",
             text = "2020",
             showarrow = FALSE,
             xanchor = "left",
             xshift = 8,
             align = "left") %>% 
      rename(y = attendance,
             x = week_ending_date),
    datasets[["4a"]] %>% 
      filter(date == max(date)) %>%
      select(claims_7day_avg, date) %>% 
      mutate(date = as.Date(date),
             text = "7 day average",
             plot = "4a",
             dataset = "4a",
             showarrow = FALSE,
             xanchor = "left",
             xshift = 5,
             align = "left") %>% 
      rename(y = claims_7day_avg,
             x = date)
  )



# Read web text -----------------------------------------------------------
narrative <- read_excel(path = paths[["text"]],
                          sheet = "narrative")

