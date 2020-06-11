plots[["1_sparklines"]] <-
  subplot(
    nrows = 6,
    shareX = TRUE,
    datasets[["1r_recent"]] %>%
      plot_ly(
        x = ~ date,
        y = ~ middle,
        height = spark_height * 6,
        hoverinfo = "text"
      ) %>%
      add_trace(
        type = "scatter",
        mode = "markers",
        marker = list(opacity = 0,
                      color = "black"),
        error_y = ~list(array = high - middle,
                        arrayminus = middle - low,
                        color = "black",
                        thickness = 2,
                        width = 3),
        text = ~ text
      ) %>%
      config(displayModeBar = FALSE,
             showAxisDragHandles = FALSE) %>%
      layout(
        paper_bgcolor = "rgba(0, 0, 0, 0)",
        plot_bgcolor = "rgba(0, 0, 0, 0)",
        yaxis = list(
          fixedrange = TRUE,
          title = "",
          showgrid = FALSE,
          zeroline = FALSE,
          showticklabels = FALSE
        ),
        xaxis = list(
          fixedrange = TRUE,
          title = "",
          showgrid = FALSE,
          showticklabels = FALSE,
          range = c(dates[["start_sparklines"]], as.character(Sys.Date()))
        ),
        margin = list(l = 0,
                      r = 0,
                      t = 0,
                      b = 0),
        shapes = list(
          list(
            type = "line",
            layer = "below",
            x0 = "2020-03-23",
            x1 = "2020-05-27",
            y0 = 1,
            y1 = 1,
            line = list(color = col_palette["sg_grey"], dash = "dot")
          )
        ),
        annotations = filter(annotations, plot == "1_sparklines", dataset == "1r")
      ) %>%
      htmlwidgets::onRender(
        "function(el, x) {
    Plotly.d3.selectAll('.cursor-pointer').style('cursor', 'crosshair')}"
      ),
    datasets[["1_infect"]] %>%
      plot_ly(x = ~ date,
              y = ~ Mid,
              text = ~ text) %>%
      add_style_spark() %>%
      layout(
        annotations = filter(annotations, plot == "1_sparklines", dataset == "1_infect")
      ),
    datasets[["1_cases"]] %>%
      plot_ly(
        x = ~ date,
        y = ~ cases_7day_avg,
        text = ~ cases_7day_avg_text
      ) %>%
      add_style_spark() %>%
      layout(
        annotations = filter(annotations, plot == "1_sparklines", dataset == "1_cases")
      ),
    datasets[["1a"]] %>%
      plot_ly(
        x = ~ Date,
        y = ~ count_7day_avg,
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
  layout(showlegend = FALSE) %>% 
  htmlwidgets::onRender(
    "function(el, x) {
    Plotly.d3.selectAll('.cursor-pointer').style('cursor', 'crosshair')}"
  )

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
  layout(showlegend = FALSE) %>% 
  htmlwidgets::onRender(
    "function(el, x) {
    Plotly.d3.selectAll('.cursor-pointer').style('cursor', 'crosshair')}"
  )

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
  layout(showlegend = FALSE) %>% 
  htmlwidgets::onRender(
    "function(el, x) {
    Plotly.d3.selectAll('.cursor-pointer').style('cursor', 'crosshair')}"
  )

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
  layout(showlegend = FALSE) %>% 
  htmlwidgets::onRender(
    "function(el, x) {
    Plotly.d3.selectAll('.cursor-pointer').style('cursor', 'crosshair')}"
  )


