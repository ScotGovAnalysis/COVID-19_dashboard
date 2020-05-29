plots[["1_sparklines"]] <-
  subplot(
    nrows = 3,
    shareX = TRUE,
    datasets[["1a"]] %>%
      plot_ly(
        x = ~ Date,
        y = ~ count_7day_avg,
        height = spark_height * 3,
        text = ~ count_7day_avg_text
      ) %>%
      add_style_spark() %>%
      layout(
        annotations = filter(annotations, plot == "1_sparklines", dataset == "1a")
      ),
    datasets[["1c_icu_hdu"]] %>%
      plot_ly(x = ~ date,
              y = ~ covid_patients,
              text = ~ text) %>%
      add_style_spark() %>%
      layout(
        annotations = filter(annotations, plot == "1_sparklines", dataset == "1c_icu_hdu")
      ),
    datasets[["1c_hosp_conf"]] %>%
      plot_ly(x = ~ date,
              y = ~ covid_patients,
              text = ~ text) %>%
      add_style_spark() %>%
      layout(
        annotations = filter(annotations, plot == "1_sparklines", dataset == "1c_hosp_conf")
      )
  ) %>%
  layout(showlegend = FALSE)

plots[["2_sparklines"]] <- list(
  datasets[["2a_recent"]] %>%
    plot_ly(x = ~ week_ending_date,
            y = ~ attendance,
            height = spark_height,
            text = ~ text) %>%
    add_style_spark() %>%
    layout(
      annotations = filter(annotations, plot == "2_sparklines", dataset == "2a_recent")
    )) %>%
  subplot(nrows = 1,
          shareX = TRUE) %>%
  layout(showlegend = FALSE)

plots[["3_sparklines"]] <- list(
  datasets[["3a"]] %>%
    plot_ly(x = ~ date, y = ~ children, height = spark_height,
            text = ~ text) %>%
    add_style_spark() %>%
    layout(
      annotations = filter(annotations, plot == "3_sparklines", dataset == "3a")
    )) %>% 
  subplot(nrows = 1,
          shareX = TRUE) %>% 
  layout(showlegend = FALSE)

plots[["4_sparklines"]] <- list(
  datasets[["4a"]] %>%
    plot_ly(x = ~ date, y = ~ claims_7day_avg, height = spark_height,
            text = ~ claims_7day_avg_text) %>%
    add_style_spark() %>%
    layout(
      annotations = filter(annotations, plot == "4_sparklines", dataset == "4a")
    )) %>% 
  subplot(nrows = 1,
          shareX = TRUE) %>% 
  layout(showlegend = FALSE)


