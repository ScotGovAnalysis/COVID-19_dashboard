# Read Harms 1 ------------------------------------------------------------
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

datasets[["1r_recent"]] <- datasets[["1r"]] %>% 
  filter(date > as.Date('2020-03-22'))
# Dummy data used to mock-up a chart for the R number

datasets[["1_infect"]] <- data.frame(date = as.Date(c("2020-05-08",
                                                      "2020-05-15",
                                                      "2020-05-22",
                                                      "2020-05-29",
                                                      "2020-06-05",
                                                      "2020-06-12")),
                               Mid = c(20500,
                                       17000,
                                       14000,
                                       11500,
                                       9500,
                                       8000),
                               Lower = c(15000,
                                         11500,
                                         9000,
                                         7000,
                                         5500,
                                         4000),
                               Upper = c(27000,
                                         23000,
                                         20000,
                                         17000,
                                         14500,
                                         12500)) %>% 
  mutate(text = paste0("Between ", format(Lower, big.mark = ","), " and ",
                       format(Upper, big.mark = ","),
                       "\ninfectious people on ", format(date, "%d %B %Y")))

datasets[["1_cases"]] <- read_excel(path = paths[["sitrep"]],
                                    sheet = "Data",
                                    range = cell_rows(1:52)) %>%
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

datasets[["2_excess"]] <- read_excel(path = paths[["sitrep"]],
                                     sheet = "Data") %>% 
  filter(Data %in% c("All deaths 2020",
                     "Average deaths 2015-19",
                     "COVID deaths 2020")) %>% 
  select(-c(Slide)) %>% 
  gather(key = "date", value = "count", -Data) %>% 
  drop_na() %>% 
  mutate(date = as.Date(as.numeric(date), origin = "1899-12-30"),
         week = lubridate::week(date),
         count = as.numeric(count),
         linetype = case_when(Data == "Average deaths 2015-19" ~ "solid",
                              TRUE ~ "dot")) %>% 
  filter(date > as.Date("2020-03-11")) %>% 
  rename(measure = Data)

datasets[["2_excess_spark"]] <- datasets[["2_excess"]] %>% 
  filter(measure != "COVID deaths 2020") %>% 
  select(-linetype) %>% 
  spread(key = measure, value = count) %>% 
  mutate(excess_deaths = `All deaths 2020` - `Average deaths 2015-19`,
         week = lubridate::week(date)) %>% 
  rename(all_2020 = `All deaths 2020`,
         avg_2015_19 = `Average deaths 2015-19`)

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
  mutate(x = as.Date(x),
         text = case_when(id == 8 ~ stringr::str_wrap(text, width = 45),
                          id == 22 ~ stringr::str_wrap(text, width = 20),
                          stringr::str_ends(plot, "sparklines") & dataset != "3a" ~ stringr::str_wrap(text, width = 35), # str_wrap starts a new line inside a html tag for sparkline 3a. I've not figure out how to fix this properly so have hardcoded it to ignore this one.
                          TRUE ~ text)) %>% 
  bind_rows(
    datasets[["1_infect"]] %>% 
      filter(date == max(date)) %>%
      select(date, Mid, Lower, Upper) %>% 
      gather(key = "estimate", value = "value", -date) %>% 
      mutate(text = paste0("<b>", estimate, "</b>\nestimate"),
             plot = "1_infect",
             dataset = "1_infect",
             showarrow = FALSE,
             xanchor = "left",
             xshift = 10,
             align = "left") %>% 
      rename(y = value,
             x = date),
    datasets[["1_cases"]] %>% 
      filter(date == max(date)) %>%
      select(cases_7day_avg, date) %>% 
      mutate(text = "7 day average",
             plot = "1_cases",
             dataset = "1_cases",
             showarrow = FALSE,
             xanchor = "left",
             xshift = 5,
             align = "left") %>% 
      rename(y = cases_7day_avg,
             x = date),
    datasets[["1a"]] %>% 
      filter(Date == max(Date)) %>%
      select(count_7day_avg, Date) %>% 
      mutate(Date = as.Date(Date),
             text = "7 day average",
             font = list(list(color = col_palette["sg_blue"])),
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
             font = case_when(setting %in% c("Home / Non-institution", "Hospital") ~ list(list(color = col_palette["sg_blue"])),
                              TRUE ~ list(list(color = col_palette["sg_grey"]))),
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
             font = case_when(grepl("hospital",location_label) ~ list(list(color = col_palette["sg_grey"])),
                              TRUE ~ list(list(color = col_palette["sg_blue"]))),
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
             font = list(list(color = col_palette["sg_blue"])),
             showarrow = FALSE,
             xanchor = "left",
             xshift = 8,
             align = "left") %>% 
      rename(y = attendance,
             x = week_ending_date),
    datasets[["2_excess"]] %>% 
      filter(date == max(date)) %>%
      select(date, measure, count) %>% 
      mutate(plot = "2_excess",
             dataset = "2_excess",
             showarrow = FALSE,
             font = c(list(list(color = col_palette["sg_grey"])),
                      list(list(color = col_palette["sg_grey"])),
                      list(list(color = col_palette["sg_blue"]))),
             xanchor = "left",
             xshift = 8,
             align = "left") %>% 
      rename(y = count,
             x = date,
             text = measure),
    datasets[["4a"]] %>% 
      filter(date == max(date)) %>%
      select(claims_7day_avg, date) %>% 
      mutate(date = as.Date(date),
             text = "7 day average",
             plot = "4a",
             font = list(list(color = col_palette["sg_blue"])),
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

