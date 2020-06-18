# 1 Direct health -------------------------------------------------------------
## Define sparklines ----------------------------------------------------------
plots[["1r_recent_spark"]] <- datasets[["1r_recent"]] %>%
  plot_ly(
    x = ~ date,
    y = ~ middle,
    height = spark_height * 3,
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
        x0 = dates[["start_sparklines"]],
        x1 = as.character(Sys.Date()),
        y0 = 1,
        y1 = 1,
        line = list(color = col_palette["sg_grey"], dash = "dot")
      )
    ),
    annotations = filter(annotations,
                         plot == "1_sparklines",
                         dataset == "1r")
  ) %>%
  htmlwidgets::onRender(
    "function(el, x) {
    Plotly.d3.selectAll('.cursor-pointer').style('cursor', 'crosshair')}"
  )

plots[["1_infect_spark"]] <-  datasets[["1_infect"]] %>%
  plot_ly(x = ~ date,
          y = ~ midpoint,
          text = ~ text) %>%
  add_style_spark() %>%
  layout(annotations = filter(annotations,
                              plot == "1_sparklines",
                              dataset == "1_infect"))

plots[["1_cases_spark"]] <- datasets[["1_cases"]] %>%
  plot_ly(
    x = ~ date,
    y = ~ cases_7day_avg,
    text = ~ cases_7day_avg_text
  ) %>%
  add_style_spark() %>%
  layout(annotations = filter(annotations,
                              plot == "1_sparklines",
                              dataset == "1_cases"))

# plots[["1a_spark"]] <- datasets[["1a"]] %>%
#   plot_ly(
#     x = ~ Date,
#     y = ~ count_7day_avg,
#     text = ~ count_7day_avg_text
#   ) %>%
#   add_style_spark() %>%
#   layout(annotations = filter(annotations,
#                               plot == "1_sparklines",
#                               dataset == "1a"))
# 
# plots[["1c_icu_hdu_spark"]] <- datasets[["1c_icu_hdu"]] %>%
#   plot_ly(x = ~ date,
#           y = ~ covid_patients,
#           text = ~ text) %>%
#   add_style_spark() %>%
#   layout(annotations = filter(annotations,
#                               plot == "1_sparklines",
#                               dataset == "1c_icu_hdu"))

# plots[["1c_hosp_conf_spark"]] <- datasets[["1c_hosp_conf"]] %>%
#   plot_ly(x = ~ date,
#           y = ~ covid_patients,
#           text = ~ text) %>%
#   add_style_spark() %>%
#   layout(annotations = filter(annotations,
#                               plot == "1_sparklines",
#                               dataset == "1c_hosp_conf"))

## Create subplots ------------------------------------------------------------
plots[["1_sparklines"]] <-
  plots[c(
    "1r_recent_spark",
    "1_infect_spark",
    "1_cases_spark"
    # "1a_spark",
    # "1c_icu_hdu_spark",
    # "1c_hosp_conf_spark"
  )] %>%
  subplot(nrows = 3,
          shareX = TRUE) %>%
  layout(showlegend = FALSE) %>%
  htmlwidgets::onRender(
    "function(el, x) {
    Plotly.d3.selectAll('.cursor-pointer').style('cursor', 'crosshair')}"
  )

# 2 Indirect health -----------------------------------------------------------
## Define sparklines ----------------------------------------------------------
plots[["2a_spark"]] <- datasets[["2a_recent"]] %>%
  plot_ly(x = ~ week_ending_date,
          y = ~ attendance,
          height = spark_height * 5,
          text = ~ text) %>%
  add_style_spark() %>%
  layout(
    annotations = filter(annotations,
                         plot == "2_sparklines",
                         dataset == "2a_recent")
  )

plots[["2_admissions_emergency_spark"]] <-  datasets[["2_admissions"]] %>%
  filter(Admission_type == "Emergency") %>%
  plot_ly(x = ~ Week_ending,
          y = ~ variation,
          text = ~ text_variation) %>%
  add_style_spark() %>%
  layout(
    annotations = filter(annotations,
                         plot == "2_sparklines",
                         dataset == "2_admissions_emergency")
  )

plots[["2_admissions_planned_spark"]] <- datasets[["2_admissions"]] %>%
  filter(Admission_type == "Planned") %>%
  plot_ly(x = ~ Week_ending,
          y = ~ variation,
          text = ~ text_variation) %>%
  add_style_spark() %>%
  layout(
    annotations = filter(annotations,
                         plot == "2_sparklines",
                         dataset == "2_admissions_planned")
  )

plots[["2_excess_spark"]] <- datasets[["2_excess_spark"]] %>%
  plot_ly(x = ~ date,
          y = ~ excess_deaths,
          text = ~ text) %>%
  add_style_spark() %>%
  layout(
    annotations = filter(annotations,
                         plot == "2_sparklines",
                         dataset == "2_excess_spark")
  )

plots[["2_GP_spark"]] <- datasets[["2_GP"]] %>%
  plot_ly(x = ~ date,
          y = ~ percent,
          text = ~ text_2020) %>%
  add_style_spark() %>%
  layout(
    annotations = filter(annotations,
                         plot == "2_sparklines",
                         dataset == "2_GP")
  )

## Create subplots ------------------------------------------------------------
plots[["2_sparklines"]] <-
  plots[c("2a_spark", "2_admissions_emergency_spark",
          "2_admissions_planned_spark", "2_excess_spark",
          "2_GP_spark")] %>%
  subplot(nrows = 5,
          shareX = TRUE) %>%
  layout(showlegend = FALSE) %>%
  htmlwidgets::onRender(
    "function(el, x) {
    Plotly.d3.selectAll('.cursor-pointer').style('cursor', 'crosshair')}"
  )

# 3 Society -------------------------------------------------------------------
## Define sparklines ----------------------------------------------------------
plots[["3a_spark"]] <- datasets[["3a"]] %>%
  plot_ly(x = ~ date, y = ~ children, height = 5 * spark_height,
          text = ~ text) %>%
  add_style_spark() %>%
  layout(
    annotations = filter(annotations,
                         plot == "3_sparklines",
                         dataset == "3a")
  )

plots[["3_crisis_applications_spark"]] <-
  datasets[["3_crisis_applications_spark"]] %>%
  plot_ly(x = ~ month_ending_date,
          y = ~ variation,
          text = ~ text) %>%
  add_style_spark() %>%
  layout(
    annotations = filter(annotations,
                         plot == "3_sparklines",
                         dataset == "3_crisis_applications")
  )

plots[["3_loneliness_spark"]] <- datasets[["3_loneliness"]] %>%
  plot_ly(x = ~ date,
          y = ~ percent,
          text = ~ text_2020) %>%
  add_style_spark() %>%
  layout(
    annotations = filter(annotations,
                         plot == "3_sparklines",
                         dataset == "3_loneliness")
  )

plots[["3_trust_spark"]] <- datasets[["3_trust"]] %>%
  plot_ly(x = ~ date,
          y = ~ percent,
          text = ~ text_2020) %>%
  add_style_spark() %>%
  layout(
    annotations = filter(annotations,
                         plot == "3_sparklines",
                         dataset == "3_trust")
  )

plots[["3_job_spark"]] <- datasets[["3_job"]] %>%
  plot_ly(x = ~ date,
          y = ~ percent,
          text = ~ text_2020) %>%
  add_style_spark() %>%
  layout(
    annotations = filter(annotations,
                         plot == "3_sparklines",
                         dataset == "3_job")
  )

## Create subplots ------------------------------------------------------------
plots[["3_sparklines"]] <-
  plots[c("3a_spark", "3_crisis_applications_spark",
          "3_loneliness_spark","3_trust_spark",
          "3_job_spark")] %>%
  subplot(nrows = length(.),
          shareX = TRUE) %>%
  layout(showlegend = FALSE) %>%
  htmlwidgets::onRender(
    "function(el, x) {
    Plotly.d3.selectAll('.cursor-pointer').style('cursor', 'crosshair')}"
  )

# 4 Economy -------------------------------------------------------------------
## Define sparklines ----------------------------------------------------------
plots[["4a_spark"]] <- datasets[["4a"]] %>%
  plot_ly(x = ~ date, y = ~ claims_7day_avg, height = 1 * spark_height,
          text = ~ claims_7day_avg_text) %>%
  add_style_spark() %>%
  layout(
    annotations = filter(annotations,
                         plot == "4_sparklines",
                         dataset == "4a")
  )

## Create subplots ------------------------------------------------------------
plots[["4_sparklines"]] <- list(
  plots[[c("4a_spark")]]) %>%
  subplot(nrows = length(.),
          shareX = TRUE) %>%
  layout(showlegend = FALSE) %>%
  htmlwidgets::onRender(
    "function(el, x) {
    Plotly.d3.selectAll('.cursor-pointer').style('cursor', 'crosshair')}"
  )
