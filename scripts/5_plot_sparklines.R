# 1 Direct health -------------------------------------------------------------
## Define sparklines ----------------------------------------------------------
plots[["1.1_R_spark"]] <- datasets[["1.1_R"]] %>%
  plot_ly(
    x = ~ date,
    y = ~ middle,
    height = spark_height,
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
    text = ~ text_short
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

#Changed data set so the below runs
plots[["1.2_infectious_spark"]] <- datasets[["1.2_infectious"]] %>%
  plot_ly(
    x = ~ date,
    y = ~ midpoint,
    height = spark_height,
    hoverinfo = "text"
  ) %>%
  add_trace(
    type = "scatter",
    mode = "markers",
    marker = list(opacity = 0,
                  color = "black"),
    error_y = ~list(array = upperbound - midpoint,
                    arrayminus = midpoint - lowerbound,
                    color = "black",
                    thickness = 2,
                    width = 3),
    text = ~ text_short
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

  #   plot_ly(x = ~ date,
  #         y = ~ midpoint,
  #         height = spark_height#,
  #         #text = ~ text_short
  #         ) %>%
  # add_style_spark()

plots[["1.3_cases_spark"]] <- datasets[["1.3_cases"]] %>%
  plot_ly(
    x = ~ date,
    y = ~ count_7day_avg,
    height = spark_height,
    text = ~ count_7day_avg_text_short
  ) %>%
  add_style_spark()

plots[["1.4_deaths_spark"]] <- datasets[["1.4_deaths"]] %>%
  plot_ly(
    x = ~ week_beginning,
    y = ~ count,
    height = spark_height,
    text = ~ text_short
  ) %>%
  add_style_spark()

plots[["1.5_admissions_spark"]] <- datasets[["1.5_admissions"]] %>%
  plot_ly(
    x = ~ Date,
    y = ~ count_7day_avg,
    height = spark_height,
    text = ~ text_7day_avg_short
  ) %>%
  add_style_spark()

# 2 Indirect health -----------------------------------------------------------
## Define sparklines ----------------------------------------------------------
plots[["2.1_A&E_spark"]] <- datasets[["2.1_A&E"]] %>%
  filter(week_ending_date > as.Date("2020-01-01")) %>%
  plot_ly(x = ~ week_ending_date,
          y = ~ attendance,
          height = spark_height,
          text = ~ text_short) %>%
  add_style_spark()

plots[["2.2_excess_spark"]] <- datasets[["2.2_excess_spark"]] %>%
  plot_ly(x = ~ date,
          y = ~ excess_deaths,
          height = spark_height,
          text = ~ text_short) %>%
  add_style_spark()

plots[["2.3.1_admissions_spark"]] <- datasets[["2.3_admissions"]] %>%
  filter(Admission_type == "Emergency") %>%
  plot_ly(x = ~ Week_ending,
          y = ~ variation,
          height = spark_height,
          text = ~ text_variation) %>%
  add_style_spark()

plots[["2.3.2_admissions_spark"]] <- datasets[["2.3_admissions"]] %>%
  filter(Admission_type == "Planned") %>%
  plot_ly(x = ~ Week_ending,
          y = ~ variation,
          height = spark_height,
          text = ~ text_variation) %>%
  add_style_spark()

plots[["2.4_avoiding_spark"]] <- datasets[["2.4_avoiding"]] %>%
  filter(sentiment %in% c("tend to agree",
                          "strongly agree")) %>%
  group_by(date_start) %>%
  summarise(percent = sum(percent)) %>%
  mutate(text = paste0(
    "<b>",
    round(percent, digits = 1),
    "%</b>\n",
    format(date_start, "%d %B %Y")
  )) %>%
  plot_ly(x = ~ date_start,
          y = ~ percent,
          height = spark_height,
          text = ~ text) %>%
  add_style_spark()

# 3 Society -------------------------------------------------------------------
## Define sparklines ----------------------------------------------------------

# For some reason it's not possible to set connectgaps = TRUE in
# add_style_spark(). So for now, this just copies that function and adds the
# option manually
# plots[["3.1_schools_spark"]] <- datasets[["3.1_schools"]] %>%
#   #filter(grepl("All", Measure, ignore.case = TRUE)) %>%
#   plotly_empty(
#     #x = ~ date, 
#     #y = ~ count, 
#     height = spark_height#,
#           #text = ~ text_short
#     ) %>%
#   add_trace(
#     type = "scatter",
#     mode = "lines",
#     line = list(color = "black"),
#     fill = "tozeroy",
#     fillcolor = col_palette["sg_light_blue"],
#     hoverinfo = "text",
#     connectgaps = TRUE
#   ) %>%
#   config(displayModeBar = FALSE,
#          showAxisDragHandles = FALSE) %>%
#   layout(
#     paper_bgcolor = "rgba(0, 0, 0, 0)",
#     plot_bgcolor = "rgba(0, 0, 0, 0)",
#     yaxis = list(
#       fixedrange = TRUE,
#       title = "",
#       showgrid = FALSE,
#       zeroline = FALSE,
#       rangemode = "tozero",
#       showticklabels = FALSE
#     ),
#     xaxis = list(
#       fixedrange = TRUE,
#       title = "",
#       showgrid = FALSE,
#       showticklabels = FALSE,
#       range = c(dates[["start_sparklines"]],
#                 as.character(Sys.Date()))
#     ),
#     margin = list(l = 0, #210,
#                   r = 0,
#                   t = 0,
#                   b = 0)
#   ) %>%
#   htmlwidgets::onRender(
#     "function(el, x) {
#     Plotly.d3.selectAll('.cursor-pointer').style('cursor', 'crosshair')}"
#   )

#Get only the pubils absent due to covid-19
# plots[["3.1_schools_spark"]] <-datasets[["3.1_schools"]] %>% filter(Measure=='Covid_absence') %>%
#   #filter(grepl("All", Measure, ignore.case = TRUE)) %>%
#   plot_ly(
#     x = ~ date, 
#     y = ~ count, 
#     height = spark_height,
#     text = ~ text_short
#   ) %>%
#   add_trace(
#     type = "scatter",
#     mode = "lines",
#     line = list(color = "black"),
#     fill = "tozeroy",
#     fillcolor = col_palette["sg_light_blue"],
#     hoverinfo = "text",
#     connectgaps = TRUE
#   ) %>%
#   config(displayModeBar = FALSE,
#          showAxisDragHandles = FALSE) %>%
#   layout(
#     paper_bgcolor = "rgba(0, 0, 0, 0)",
#     plot_bgcolor = "rgba(0, 0, 0, 0)",
#     yaxis = list(
#       fixedrange = TRUE,
#       title = "",
#       showgrid = FALSE,
#       zeroline = FALSE,
#       rangemode = "tozero",
#       showticklabels = FALSE
#     ),
#     xaxis = list(
#       fixedrange = TRUE,
#       title = "",
#       showgrid = FALSE,
#       showticklabels = FALSE,
#       range = c(dates[["start_sparklines"]],
#                 as.character(Sys.Date()))
#     ),
#     margin = list(l = 0, #210,
#                   r = 0,
#                   t = 0,
#                   b = 0)
#   ) %>%
#   htmlwidgets::onRender(
#     "function(el, x) {
#     Plotly.d3.selectAll('.cursor-pointer').style('cursor', 'crosshair')}"
#   )


plots[["3.1_schools_spark"]] <-datasets[["3.1_schools"]] %>%
  plot_ly(x = ~ date,
          y = ~ attendance,
          height = spark_height,
          text = ~ text_short) %>%
  add_style_spark()


#%>%
#  add_style_spark(range = c(dates[["start_sparklines_economy"]],
#                            as.character(Sys.Date())))



plots[["3.2_crisis_spark"]] <-
  datasets[["3.2_crisis_spark"]] %>%
  plot_ly(x = ~ month_ending_date,
          y = ~ variation,
          height = spark_height,
          text = ~ text_short) %>%
  add_style_spark()

plots[["3.3_crime_spark"]] <- datasets[["3.3_crime_spark"]] %>%
  filter(crime_group == "Total crimes") %>%
  mutate(date_present=case_when(
    month=="Jan" ~ dmy("31-01-21"),
    month!="Jan" ~ date
    ),
  ) %>%
  arrange(date_present) %>%
  plot_ly(x = ~ date_present, y = ~ variation_rate, height = spark_height,
          text = ~ text_variation_short) %>%
  add_style_spark()

plots[["3.4_loneliness_spark"]] <- datasets[["3.4_loneliness"]] %>%
  plot_ly(x = ~ date_start,
          y = ~ percent,
          name = ~ source,
          height = spark_height,
          text = ~ text_2020_short) %>%
  add_style_spark()

plots[["3.5_trust_spark"]] <- datasets[["3.5_trust"]] %>%
  plot_ly(x = ~ date_start,
          y = ~ percent,
          name = ~ source,
          height = spark_height,
          text = ~ text_2020_short) %>%
  add_style_spark()

plots[["3.6_job_spark"]] <- datasets[["3.6_job"]] %>%
  plot_ly(x = ~ date_start,
          y = ~ percent,
          name = ~ source,
          height = spark_height,
          text = ~ text_2020_short) %>%
  add_style_spark()

plots[["3.7_transport_spark"]] <- datasets[["3.7_transport"]] %>%
  plot_ly(x = ~ Date_start,
          y = ~ `%`,
          height = spark_height,
          text = ~ text_short) %>%
  add_style_spark() %>%
  layout(
    yaxis = list(range = c(0, max(datasets[["3.7_transport"]][["%"]]) * 1.05))
  )

# 4 Economy -------------------------------------------------------------------
## Define sparklines ----------------------------------------------------------
plots[["4.1_turnover_spark"]] <- datasets[["4.1_turnover"]] %>%
  filter(industry == "All Industries") %>%
  plot_ly(
    x = ~ date,
    y = ~ turnover,
    text = ~ text_short,
    height = spark_height
  ) %>%
  add_style_spark(range = c(dates[["start_sparklines_economy"]],
                            as.character(Sys.Date())))

plots[["4.2_GDP_spark"]] <- datasets[["4.2_GDP"]] %>%
  plot_ly(
    x = ~ date,
    y = ~ `GDP (2017=100)`,
    height = spark_height,
    text = ~ text_short
  ) %>%
  add_style_spark(range = c(dates[["start_sparklines_economy"]],
                            as.character(Sys.Date())))

plots[["4.3_unemployment_spark"]] <-
  datasets[["4.3_unemployment"]] %>%
  plot_ly(x = ~ date,
          y = ~ rate,
          height = spark_height,
          text = ~ text_short) %>%
  add_style_spark(range = c(dates[["start_sparklines_economy"]],
                            as.character(Sys.Date())))

plots[["4.4_claimants_spark"]] <- datasets[["4.4_claimants"]] %>%
  plot_ly(x = ~ date,
          y = ~ count,
          height = spark_height,
          text = ~ text_short) %>%
  add_style_spark(range = c(dates[["start_sparklines_economy"]],
                            as.character(Sys.Date())))