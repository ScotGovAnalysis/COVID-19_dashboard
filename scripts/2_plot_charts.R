# Harms 1 ---------------------------------------------------------------------
plots[["1r"]] <- plot_ly(
  data = datasets[["1r"]],
  x = ~ date,
  hoverinfo = "text",
  y = ~ middle
) %>%
  add_trace(
    type = "scatter",
    mode = "markers",
    marker = list(opacity = 0),
    error_y = ~list(array = high - middle,
                    arrayminus = middle - low,
                    color = col_palette["sg_blue"],
                    thickness = 5,
                    width = 6),
    text = ~ text
  ) %>% 
  add_style_chart() %>%
  layout(
    xaxis = list(showspikes = TRUE,
                 spikemode = "across"),
    shapes = shapes[["1r"]],
    annotations = filter(annotations, plot == "1r", dataset == "1r")
  )

plots[["1_infect"]] <- plot_ly(
  data = datasets[["1_infect"]],
  x = ~ date,
  hoverinfo = "text",
  y = ~ Mid
) %>%
  add_trace(
    type = "scatter",
    mode = "markers",
    marker = list(color = col_palette["sg_blue"],
                  size = 10),
    error_y = ~list(array = Upper - Mid,
                    arrayminus = Mid - Lower,
                    color = col_palette["sg_grey"]),
    text = ~ text
  ) %>% 
  add_style_chart()

plots[["1_infect"]] <- plot_ly(
  data = datasets[["1_infect"]],
  x = ~ date,
  hoverinfo = "text",
  y = ~ Mid
) %>%
  add_ribbons(
    ymin = ~ Lower,
    ymax = ~ Upper,
    line = list(color = "transparent"),
    fillcolor = col_palette["sg_light_blue"]
  ) %>%
  add_trace(
    type = "scatter",
    mode = "lines+markers",
    marker = list(color = col_palette["sg_blue"]),
    line = list(color = col_palette["sg_blue"]),
    text = ~ text
  ) %>%
  add_style_chart() %>%
  layout(
    showlegend = FALSE,
    annotations = filter(annotations, plot == "1_infect", dataset == "1_infect"),
    shapes = shapes[["1_infect"]],
    xaxis = list(range = c(
      "2020-03-23", datasets[["1_infect"]][["date"]] %>%
        max(. + lubridate::days(10)) %>%
        as.character()
    ))
  )

plots[["1_cases"]] <- plot_ly(
  data = datasets[["1_cases"]],
  x = ~ date,
  y = ~ cases,
  hoverinfo = "text"
) %>%
  add_trace(
    name = "Cases",
    y = ~ cases,
    type = "bar",
    marker = list(color = col_palette["sg_grey"]),
    text = ~ cases_text
  ) %>%
  add_trace(
    name = "7 day average",
    y = ~ cases_7day_avg,
    type = "scatter",
    mode = "markers+lines",
    marker = list(size = 7, color = col_palette["sg_blue"]),
    line = list(color = col_palette["sg_blue"]),
    text = ~ cases_7day_avg_text
  ) %>%
  add_style_chart() %>%
  layout(
    showlegend = FALSE,
    shapes = list(
      list(
        type = "line",
        layer = "below",
        x0 = dates[["lockdown"]],
        x1 = dates[["lockdown"]],
        y0 = 0,
        y1 = 450,
        line = list(color = col_palette["sg_grey"], dash = "dot")
      )
    ),
    annotations = filter(annotations, plot == "1_cases", dataset == "1_cases"),
    legend = list(orientation = 'h',
                  x = 0, y = 100)
  )

plots[["1a"]] <- plot_ly(
  data = datasets[["1a"]],
  x = ~ Date,
  y = ~ count,
  hoverinfo = "text"
) %>%
  add_trace(
    name = "Deaths",
    y = ~ count,
    type = "bar",
    marker = list(color = col_palette["sg_grey"]),
    text = ~ count_text
  ) %>%
  add_trace(
    name = "7 day average",
    y = ~ count_7day_avg,
    type = "scatter",
    mode = "markers+lines",
    marker = list(size = 7, color = col_palette["sg_blue"]),
    line = list(color = col_palette["sg_blue"]),
    text = ~ count_7day_avg_text
  ) %>%
  add_style_chart() %>%
  layout(
    showlegend = FALSE,
    shapes = list(
      list(
        type = "line",
        layer = "below",
        x0 = dates[["lockdown"]],
        x1 = dates[["lockdown"]],
        y0 = 0,
        y1 = 160,
        line = list(color = col_palette["sg_grey"], dash = "dot")
      )
    ),
    annotations = filter(annotations, plot == "1a", dataset == "1a") %>%  pmap(list), #convert to list
    legend = list(orientation = 'h',
                  x = 0, y = 100)
  )

plots[["1b"]] <- plot_ly(
  data = datasets[["1b"]],
  x = ~ week_ending_date,
  y = ~ deaths,
  marker = list(size = 7),
  name = ~ setting,
  linetype = ~ linetype,
  hoverinfo = "text",
  text = ~ text
) %>%
  add_trace(
    type = "scatter",
    mode = "markers+lines"
  ) %>%
  config(displayModeBar = FALSE,
         showAxisDragHandles = FALSE) %>%
  layout(
    showlegend = FALSE,
    xaxis = list(
      linecolor = rgb(255, 255, 255, maxColorValue = 255),
      width = 0,
      fill = NA,
      fixedrange = TRUE,
      bty = "n",
      showline = FALSE,
      title = "",
      showgrid = FALSE,
      zeroline = FALSE
    ),
    yaxis = list(
      fixedrange = TRUE,
      showline = FALSE,
      title = "",
      showgrid = FALSE,
      tick0 = 0,
      zeroline = FALSE
    ),
    colorway = c(col_palette[c("sg_grey", "sg_blue", "sg_blue", "sg_grey")]),
    paper_bgcolor = "rgba(0, 0, 0, 0)",
    plot_bgcolor = "rgba(0, 0, 0, 0)",
    margin = list(l = 0,
                  r = 0),
    legend = list(orientation = 'h',
                  x = 0, y = 100),
    annotations = filter(annotations, plot == "1b", dataset == "1b") %>% pmap(list) #transpose and convert to list
  ) %>% 
  htmlwidgets::onRender(
    "function(el, x) {
      Plotly.d3.select('.cursor-pointer').style('cursor', 'crosshair')}"
  )

plots[["1c"]] <- plot_ly(
  data = datasets[["1c"]],
  x = ~ date,
  y = ~ covid_patients,
  marker = list(size = 7),
  name = ~ location_label,
  hoverinfo = ~ "text"
) %>%
  add_trace(type = "scatter",
            mode = "markers+lines",
            text = ~ text) %>%
  add_style_chart() %>% 
  layout(showlegend = FALSE,
         colorway = c(col_palette),
         legend = list(orientation = 'v',
                       x = 0, y = 100),
         annotations = filter(annotations, plot == "1c", dataset == "1c") %>% pmap(list), #transpose and convert to list
         shapes = shapes[["1c"]])
  

# Harms 2 ---------------------------------------------------------------------
plots[["2a"]] <- plot_ly(
  x = ~ lubridate::week(week_ending_date),
  y = ~ attendance,
  hoverinfo = ~ "text"
) %>%
  add_trace(data = datasets[["2a"]] %>% 
              group_by(year = year(week_ending_date)) %>% 
              filter(year != 2020),
            type = "scatter",
            mode = "lines",
            text = ~ text) %>%
  add_trace(data = datasets[["2a"]] %>% 
              filter(year(week_ending_date) == 2020),
            type = "scatter",
            mode = "markers+lines",
            marker = list(size = 7),
            text = ~ text) %>% 
  add_style_chart() %>% 
  layout(
    xaxis = list(title = "Week number"),
    showlegend = FALSE,
    colorway = c(col_palette),
    shapes = shapes[["2a"]],
    annotations = filter(annotations, plot == "2a", dataset == "2a") %>% 
      mutate(x = week(x)) %>% # Use week numbers instead of dates
      pmap(list) #transpose and convert to list
  )

# Harms 3 ---------------------------------------------------------------------
plots[["3a"]] <- plot_ly(
  data = datasets[["3a"]],
  x = ~ date,
  y = ~ children,
  marker = list(size = 7),
  hoverinfo = ~ "text"
) %>%
  add_style_chart() %>%
  add_trace(type = "scatter",
            mode = "markers+lines",
            text = ~ text) %>% 
  layout(
    shapes = shapes[["3a"]],
    annotations = filter(annotations, plot == "3a", dataset == "3a")
  )

# Harms 4 ---------------------------------------------------------------------
plots[["4a"]] <- plot_ly(
  data = datasets[["4a"]],
  x = ~ date,
  marker = list(size = 7),
  hoverinfo = ~ "text"
) %>%
  add_style_chart() %>%
  add_trace(name = "Universal Credit Claims",
            type = "bar",
            y = ~ claims,
            text = ~ claims_text
            ) %>% 
  add_trace(name = "7 day average",
            type = "scatter",
            mode = "markers+lines",
            y = ~ claims_7day_avg,
            text = ~ claims_7day_avg_text
            ) %>% 
  layout(
    showlegend = FALSE,
    colorway = c(col_palette),
    legend = list(orientation = 'h',
                  x = 0, y = 100),
    shapes = shapes[["4a"]],
    annotations = filter(annotations, plot == "4a", dataset == "4a") %>% pmap(list) #transpose and convert to list
  )
