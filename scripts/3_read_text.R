# Define functions to read text --------------------------------------------
create_html <- function(md) {
  htmltools::HTML(
    markdown::markdownToHTML(
      text = md, fragment.only = TRUE
      )
    )
}

text_before_chart <- function(worksheet,
                              source = datasets[["sg_template"]][["TEXT"]]) {
  text <- source %>%
    filter(worksheet_name == worksheet)

  paste(
    sep = "\r\n\r\n",
    paste("###", pull(text, "TITLE_max_35_characters")),
    pull(text, "HEADLINE_max_60_characters"),
    pull(text, "SUMMARY_max_30_words")
  ) %>%
    create_html()
}

text_after_chart <- function(worksheet,
                              source = datasets[["sg_template"]][["TEXT"]]) {
  text <- source %>%
    filter(worksheet_name == worksheet)

  paste(
    sep = "\r\n\r\n",
    pull(text, "DISCUSSION_max_100_words"),
    paste("**Source:**", pull(text, "source")),
    paste("**Methodology:**",
          pull(text, "methodology"),
          "<br><br><br><br><br><br>")
  ) %>%
    create_html()
}

spark_labels <- datasets[["sg_template"]][["TEXT"]] %>%
  select(position, worksheet_name, spark_text) %>%
  filter(!(worksheet_name %in% c("H3_crime"))) %>%
  arrange(position)

# Read annotations --------------------------------------------------------
annotations <- datasets[["sg_template"]][["ANNOTATIONS"]] %>%
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
      filter(date == min(date)) %>%
      select(date, lowerbound, midpoint, upperbound) %>%
      gather(key = "estimate", value = "value", -date) %>%
      mutate(text = paste0("<b>", stringr::str_to_title(estimate), "</b>\n"),
             plot = "1_infect",
             dataset = "1_infect",
             showarrow = FALSE,
             xanchor = "left",
             xshift = -90,
             align = "left") %>%
      rename(y = value,
             x = date),
    datasets[["1_cases"]] %>%
      filter(date == max(date)) %>%
      select(count_7day_avg, date) %>%
      mutate(text = "7 day average",
             font = list(list(color = col_palette["sg_blue"])),
             plot = "1_cases",
             dataset = "1_cases",
             showarrow = FALSE,
             xanchor = "left",
             xshift = 5,
             align = "left") %>%
      rename(y = count_7day_avg,
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
      arrange(measure) %>%
      mutate(plot = "2_excess",
             dataset = "2_excess",
             showarrow = FALSE,
             font = c(list(list(color = col_palette["sg_grey"])),
                      list(list(color = col_palette["sg_blue"])),
                      list(list(color = col_palette["sg_blue"]))),
             xanchor = "left",
             yanchor = c("bottom", NA, "top"),
             xshift = 8,
             align = "left",
             measure = stringr::str_replace_all(measure, "_", " ") %>%
               stringr::str_to_sentence()) %>%
      rename(y = count,
             x = date,
             text = measure),
    datasets[["H2_admissions"]] %>%
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
    datasets[["3_school"]] %>%
      filter(date == max(date)) %>%
      select(count, date, Measure) %>%
      mutate(
        text = case_when(
          grepl("All", Measure, ignore.case = TRUE) ~ "All CYP attending",
          grepl("Key", Measure, ignore.case = TRUE) ~ "Key worker CYP",
          grepl("Vulnerable", Measure, ignore.case = TRUE) ~ "Vulnerable CYP"
        ),
        font = c(list(list(color = col_palette["sg_grey"])),
                 list(list(color = col_palette["sg_blue"])),
                 list(list(color = col_palette["sg_blue"]))),
        plot = "3_school",
        dataset = "3_school",
        showarrow = FALSE,
        xanchor = "left",
        xshift = 5,
        align = "left"
      ) %>%
      rename(y = count,
             x = date)
  )

# Read web text -----------------------------------------------------------
narrative <- datasets[["sg_template"]][["NARRATIVE"]]