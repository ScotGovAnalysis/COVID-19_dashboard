# Define function to read text --------------------------------------------
get_text <- function(worksheet,
                     column,
                     source = datasets[["sg_template"]][["text - used indicators"]]) {
  source %>%
    filter(worksheet_name == worksheet) %>%
    pull(column)
}

# Read annotations --------------------------------------------------------
annotations <- read_excel(path = paths[["text"]],
                          sheet = "annotations") %>%
  mutate(x = as.Date(x),
         text = case_when(id == 8 ~ stringr::str_wrap(text, width = 45),
                          id == 24 ~ stringr::str_wrap(text, width = 25),
                          stringr::str_ends(plot, "sparklines") &
                            dataset != "3a" ~
                            stringr::str_wrap(text, width = 35),
                          # str_wrap starts a new line inside a html tag for
                          # sparkline 3a. I've not figure out how to fix this
                          # properly so have hardcoded it to ignore this one.
                          TRUE ~ text)) %>%
  bind_rows(
    datasets[["1_infect"]] %>%
      filter(date == max(date)) %>%
      select(date, lowbound, midpoint, upperbound) %>%
      gather(key = "estimate", value = "value", -date) %>%
      mutate(text = paste0("<b>", stringr::str_to_title(estimate), "</b>\n"),
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
             font = case_when(setting %in%
                                c("Home / Non-institution", "Hospital") ~
                                list(list(color = col_palette["sg_blue"])),
                              TRUE ~
                                list(list(color = col_palette["sg_grey"]))),
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
             font = case_when(grepl("hospital", location_label) ~
                                list(list(color = col_palette["sg_grey"])),
                              TRUE ~
                                list(list(color = col_palette["sg_blue"]))),
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
      select(date, measure, count, week) %>%
      mutate(plot = "2_excess",
             dataset = "2_excess",
             showarrow = FALSE,
             font = c(list(list(color = col_palette["sg_grey"])),
                      list(list(color = col_palette["sg_grey"])),
                      list(list(color = col_palette["sg_blue"]))),
             xanchor = "left",
             xshift = 8,
             align = "left",
             date = date + lubridate::weeks(1)) %>%
      rename(y = count,
             x = date,
             text = measure),
    datasets[["2_admissions"]] %>%
      filter(Week_ending == max(Week_ending)) %>%
      select(Week_ending, Count, Average_2018_2019, Admission_type) %>%
      gather(key = measure, value = count, -Week_ending, -Admission_type) %>%
      mutate(plot = "2_admissions",
             dataset = "2_admissions",
             showarrow = FALSE,
             font = c(list(list(color = col_palette["sg_blue"])),
                      list(list(color = col_palette["sg_blue"])),
                      list(list(color = col_palette["sg_grey"])),
                      list(list(color = col_palette["sg_grey"]))),
             xanchor = "left",
             xshift = 8,
             align = "left",
             text = case_when(measure == "Count" ~
                                paste0("<b>", Admission_type, "</b>\n",
                                       "admissions 2020"),
                              TRUE ~
                                paste0("<b>", Admission_type, "</b>",
                                       "\nadmissions\n(average 2018-19)"))) %>%
      rename(y = count,
             x = Week_ending),
    datasets[["3_crime"]] %>%
      filter(year == 2020) %>%
      select(year, recorded, crime_group) %>%
      mutate(plot = "3_crime",
             dataset = "3_crime",
             showarrow = FALSE,
             xanchor = "left",
             xshift = 8,
             align = "left",
             x = as.Date("2020-04-01")) %>%
      rename(y = recorded,
             text = crime_group),
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