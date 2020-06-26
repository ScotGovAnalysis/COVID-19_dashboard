# 1 Direct health -------------------------------------------------------------
## Define sparklines ----------------------------------------------------------
plots[["1r_recent_spark"]] <- datasets[["1r_recent"]] %>%
  plot_ly(
    x = ~ date,
    y = ~ middle,
    height = spark_height * 5,
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
    )
  ) %>%
  htmlwidgets::onRender(
    "function(el, x) {
    Plotly.d3.selectAll('.cursor-pointer').style('cursor', 'crosshair')}"
  )

plots[["1_infect_spark"]] <-  datasets[["1_infect"]] %>%
  plot_ly(x = ~ date,
          y = ~ midpoint,
          text = ~ text) %>%
  add_style_spark()

plots[["1_cases_spark"]] <- datasets[["1_cases"]] %>%
  plot_ly(
    x = ~ date,
    y = ~ count_7day_avg,
    text = ~ count_7day_avg_text
  ) %>%
  add_style_spark()

plots[["1_deaths_spark"]] <- datasets[["H1_deaths"]] %>%
  plot_ly(
    x = ~ week_beginning,
    y = ~ count,
    text = ~ text
  ) %>%
  add_style_spark()

plots[["1_admissions_spark"]] <- datasets[["H1_admissions"]] %>%
  plot_ly(
    x = ~ Date,
    y = ~ count_7day_avg,
    text = ~ text_7day_avg
  ) %>%
  add_style_spark()

## Create subplots ------------------------------------------------------------
plots[["1_sparklines"]] <-
  plots[c(
    "1r_recent_spark",
    "1_infect_spark",
    "1_cases_spark",
    "1_deaths_spark",
    "1_admissions_spark"
  )] %>%
  subplot(nrows = length(.),
          shareX = TRUE) %>%
  layout(showlegend = FALSE,
         annotations = filter(spark_labels,
                              harm_group == "H1") %>%
           mutate(x = 0,
                  xref = "paper",
                  yref = "paper",
                  y = seq(from = 9 / 10, by = -1 / 5, length.out = 5),
                  yanchor = "middle")) %>%
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
  add_style_spark()

plots[["2_admissions_emergency_spark"]] <- datasets[["H2_admissions"]] %>%
  filter(Admission_type == "Emergency") %>%
  plot_ly(x = ~ Week_ending,
          y = ~ variation,
          text = ~ text_variation) %>%
  add_style_spark()

plots[["2_admissions_planned_spark"]] <- datasets[["H2_admissions"]] %>%
  filter(Admission_type == "Planned") %>%
  plot_ly(x = ~ Week_ending,
          y = ~ variation,
          text = ~ text_variation) %>%
  add_style_spark()

plots[["2_excess_spark"]] <- datasets[["2_excess_spark"]] %>%
  plot_ly(x = ~ date,
          y = ~ excess_deaths,
          text = ~ text) %>%
  add_style_spark()

plots[["2_GP_spark"]] <- datasets[["2_GP"]] %>%
  filter(sentiment %in% c("tend to agree",
                          "strongly agree")) %>%
  group_by(date_start) %>%
  summarise(percent = sum(percent)) %>%
  mutate(text = paste0(
    "<b>",
    round(percent, digits = 1),
    "% of people say they would avoid GPs or hospital</b>\n",
    " for immediate non-COVID-19 health concerns\n",
    "(",
    format(date_start, "%d %B %Y"),
    ")"
  )) %>%
  plot_ly(x = ~ date_start,
          y = ~ percent,
          text = ~ text) %>%
  add_style_spark()

## Create subplots ------------------------------------------------------------
plots[["2_sparklines"]] <-
  plots[c("2a_spark", "2_admissions_emergency_spark",
          "2_admissions_planned_spark", "2_excess_spark",
          "2_GP_spark")] %>%
  subplot(nrows = length(.),
          shareX = TRUE) %>%
  layout(showlegend = FALSE,
         annotations = filter(spark_labels,
                              harm_group == "H2" & harm_id != "admissions") %>%
           mutate(x = 0,
                  xref = "paper",
                  yref = "paper",
                  y = seq(from = 9 / 10, by = -1 / 5, length.out = 5),
                  yanchor = "middle")) %>%
  htmlwidgets::onRender(
    "function(el, x) {
    Plotly.d3.selectAll('.cursor-pointer').style('cursor', 'crosshair')}"
  )

# 3 Society -------------------------------------------------------------------
## Define sparklines ----------------------------------------------------------
plots[["3_school_spark"]] <- datasets[["3_school"]] %>%
  filter(grepl("All", Measure, ignore.case = TRUE)) %>%
  plot_ly(x = ~ date, y = ~ count, height = 6 * spark_height,
          text = ~ text) %>%
  add_style_spark()

plots[["3_crisis_applications_spark"]] <-
  datasets[["3_crisis_applications_spark"]] %>%
  plot_ly(x = ~ month_ending_date,
          y = ~ variation,
          text = ~ text) %>%
  add_style_spark()

plots[["3_loneliness_spark"]] <- datasets[["3_loneliness"]] %>%
  plot_ly(x = ~ date_start,
          y = ~ percent,
          text = ~ text_2020) %>%
  add_style_spark()

plots[["3_trust_spark"]] <- datasets[["3_trust"]] %>%
  plot_ly(x = ~ date_start,
          y = ~ percent,
          text = ~ text_2020) %>%
  add_style_spark()

plots[["3_job_spark"]] <- datasets[["3_job"]] %>%
  plot_ly(x = ~ date_start,
          y = ~ percent,
          text = ~ text_2020) %>%
  add_style_spark()

plots[["3_transport_spark"]] <- datasets[["H3_transport"]] %>%
  plot_ly(x = ~ Date_start,
          y = ~ `%`,
          text = ~ text) %>%
  add_style_spark() %>%
  layout(
    yaxis = list(range = c(0, max(datasets[["H3_transport"]][["%"]]) * 1.05))
  )

## Create subplots ------------------------------------------------------------
plots[["3_sparklines"]] <-
  plots[c("3_school_spark", "3_crisis_applications_spark",
          "3_loneliness_spark", "3_trust_spark",
          "3_job_spark", "3_transport_spark")] %>%
  subplot(nrows = length(.),
          shareX = TRUE) %>%
  layout(showlegend = FALSE,
         annotations = filter(spark_labels,
                              harm_group == "H3") %>%
           mutate(x = 0,
                  xref = "paper",
                  yref = "paper",
                  y = seq(from = 11 / 12, by = -1 / 6, length.out = 6),
                  yanchor = "middle")) %>%
  htmlwidgets::onRender(
    "function(el, x) {
    Plotly.d3.selectAll('.cursor-pointer').style('cursor', 'crosshair')}"
  )

# 4 Economy -------------------------------------------------------------------
## Define sparklines ----------------------------------------------------------
plots[["4_turnover_spark"]] <- datasets[["4_turnover"]] %>%
  filter(grepl(x = industry, pattern = "MBS")) %>%
  plot_ly(
    x = ~ date,
    y = ~ turnover,
    text = ~ text,
    height = 4 * spark_height,
    color = "black"
  ) %>%
  add_style_spark(range = c(dates[["start_sparklines_economy"]],
                            as.character(Sys.Date())))

plots[["4_GDP_spark"]] <- datasets[["4_GDP"]] %>%
  plot_ly(
    x = ~ date,
    y = ~ gdp,
    text = ~ text
  ) %>%
  add_style_spark(range = c(dates[["start_sparklines_economy"]],
                            as.character(Sys.Date())))

plots[["4_claimants_spark"]] <- datasets[["4_claimants"]] %>%
  plot_ly(x = ~ date,
          y = ~ count,
          text = ~ text) %>%
  add_style_spark(range = c(dates[["start_sparklines_economy"]],
                            as.character(Sys.Date())))

plots[["4_unemployment_spark"]] <-
  datasets[["4_unemployment"]] %>%
  plot_ly(x = ~ date,
          y = ~ rate,
          text = ~ text) %>%
  add_style_spark(range = c(dates[["start_sparklines_economy"]],
                            as.character(Sys.Date())))

## Create subplots ------------------------------------------------------------
plots[["4_sparklines"]] <- plots[c(
  "4_turnover_spark",
  "4_GDP_spark",
  "4_claimants_spark",
  "4_unemployment_spark"
  )] %>%
  subplot(nrows = length(.),
          shareX = TRUE) %>%
  layout(showlegend = FALSE,
         annotations = filter(spark_labels,
                              harm_group == "H4") %>%
           mutate(x = 0,
                  xref = "paper",
                  yref = "paper",
                  y = c(7 / 8, 5 / 8, 0.32, 0.1),
                  yanchor = "middle")) %>%
  htmlwidgets::onRender(
    "function(el, x) {
    Plotly.d3.selectAll('.cursor-pointer').style('cursor', 'crosshair')}"
  )
