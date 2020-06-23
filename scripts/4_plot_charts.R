# 1 Direct health -------------------------------------------------------------
## R value --------------------------------------------------------------------
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
    title = "Note: this chart shows dummy data",
    xaxis = list(showspikes = TRUE,
                 spikemode = "across"),
    shapes = shapes[["1r"]],
    annotations = filter(annotations, plot == "1r", dataset == "1r") %>%
      pmap(list)
  )

## Number of infectious people ------------------------------------------------
plots[["1_infect"]] <- plot_ly(
  data = datasets[["1_infect"]],
  x = ~ date,
  hoverinfo = "text",
  y = ~ midpoint
) %>%
  add_ribbons(
    ymin = ~ lowerbound,
    ymax = ~ upperbound,
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
    annotations = filter(annotations,
                         plot == "1_infect",
                         dataset == "1_infect"),
    shapes = shapes[["1_infect"]]
  )

## Number of cases ------------------------------------------------------------
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
    annotations = filter(annotations, plot == "1_cases", dataset == "1_cases") %>%
      pmap(list),
    legend = list(orientation = "h",
                  x = 0, y = 100)
  )

## Hospital admissions --------------------------------------------------------
plots[["H1_admissions"]] <- plot_ly(
  data = datasets[["H1_admissions"]],
  x = ~ Date,
  hoverinfo = "text"
) %>%
  add_trace(
    y = ~ count,
    type = "bar",
    marker = list(color = col_palette["sg_grey"]),
    text = ~ text_count
  ) %>%
  add_trace(
    y = ~ count_7day_avg,
    type = "scatter",
    mode = "markers+lines",
    marker = list(size = 7, color = col_palette["sg_blue"]),
    line = list(color = col_palette["sg_blue"]),
    text = ~ text_7day_avg
  ) %>%
  add_style_chart() %>%
  layout(
    showlegend = FALSE
  )

# Weekly deaths ---------------------------------------------------------------
plots[["H1_deaths"]] <- plot_ly(
  data = datasets[["H1_deaths"]],
  x = ~ week_beginning,
  y = ~ count,
  marker = list(size = 7),
  hoverinfo = ~ "text"
) %>%
  add_trace(
    type = "scatter",
    text = ~ text,
    mode = "markers+lines",
    line = list(color = col_palette["sg_blue"]),
    marker = list(color = col_palette["sg_blue"])
  ) %>%
  add_style_chart() %>%
  layout(
    showlegend = FALSE
  )

# 2 Indirect health -----------------------------------------------------------
## A&E attendance -------------------------------------------------------------
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

## Emergency and planned admissions -------------------------------------------
plots[["2_admissions"]] <- plot_ly(
  data = datasets[["2_admissions"]],
  x = ~ week,
  marker = list(size = 7),
  name = ~ Admission_type,
  hoverinfo = ~ "text"
) %>%
  add_trace(
    type = "scatter",
    y = ~ Average_2018_2019,
    mode = "markers+lines",
    line = list(color = col_palette["sg_grey"]),
    marker = list(color = col_palette["sg_grey"]),
    text = ~text_2018_19
  ) %>%
  add_trace(
    type = "scatter",
    y = ~ Count,
    text = ~text_2020,
    mode = "markers+lines",
    line = list(color = col_palette["sg_blue"]),
    marker = list(color = col_palette["sg_blue"])
  ) %>%
  add_style_chart() %>%
  layout(showlegend = FALSE,
         xaxis = list(title = "Week number"),
         annotations = filter(annotations,
                              plot == "2_admissions",
                              dataset == "2_admissions") %>%
           mutate(x = week(x)) %>% # Use week numbers instead of dates
           pmap(list))

## Excess deaths --------------------------------------------------------------
plots[["2_excess"]] <- plot_ly(
  data = datasets[["2_excess"]],
  x = ~ week,
  y = ~ count,
  hoverinfo = ~ "text"
) %>%
  add_ribbons(
    data = datasets[["2_excess_spark"]],
    ymin = ~ avg_2015_19,
    ymax = ~ all_2020,
    line = list(color = "transparent"),
    fillcolor = col_palette["sg_light_blue"],
    fill = "tonext"
  ) %>%
  add_trace(
    data = datasets[["2_excess"]],
    text = ~text,
    type = "scatter",
    mode = "markers+lines",
    marker = list(size = 7),
    name = ~ measure,
    linetype = ~ linetype
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
      title = "Week number",
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
    shapes = shapes[["2_excess"]],
    annotations = filter(annotations,
                         plot == "2_excess",
                         dataset == "2_excess") %>%
      mutate(x = lubridate::week(x)) %>%
      pmap(list),
    colorway = c(col_palette[c("sg_blue", "sg_grey", "sg_blue")]),
    paper_bgcolor = "rgba(0, 0, 0, 0)",
    plot_bgcolor = "rgba(0, 0, 0, 0)",
    margin = list(l = 0,
                  r = 0)
  ) %>%
  htmlwidgets::onRender(
    "function(el, x) {
    Plotly.d3.select('.cursor-pointer').style('cursor', 'crosshair')}"
  )

# Avoiding GPs and hospitals --------------------------------------------------
plots[["2_GP"]] <- plot_ly(
  data = datasets[["2_GP"]],
  x = ~ week(date),
  marker = list(size = 7),
  name = ~ Measure,
  hoverinfo = ~ "text"
) %>%
  add_trace(
    type = "scatter",
    y = ~ percent,
    text = ~text_2020,
    mode = "markers+lines",
    line = list(color = col_palette["sg_blue"]),
    marker = list(color = col_palette["sg_blue"])
  ) %>%
  add_style_chart() %>%
  layout(showlegend = FALSE,
         xaxis = list(title = "Week number"),
         shapes = shapes[["2_GP"]],
         annotations = filter(annotations,
                              plot == "2_GP",
                              dataset == "2_GP") %>%
           mutate(x = week(x)) %>% # Use week numbers instead of dates
           pmap(list))

# 3 Society -------------------------------------------------------------------
## Children at school ----------------------------------------------
plots[["3_school"]] <- plot_ly(
  data = datasets[["3_school"]],
  x = ~ date,
  y = ~ count,
  name = ~ Measure,
  marker = list(size = 7),
  hoverinfo = ~ "text"#,
  #color = col_palette["sg_blue"]
) %>%
  add_style_chart() %>%
  add_trace(type = "scatter",
            mode = "markers+lines",
            text = ~ text) %>%
  layout(
    showlegend = FALSE,
    colorway = c(col_palette["sg_grey"], col_palette["sg_blue"], col_palette["sg_blue"]),
    shapes = shapes[["3_school"]],
    annotations = filter(annotations, plot == "3_school", dataset == "3_school") %>%
      pmap(list)
  )


## Crisis applications --------------------------------------------------------
plots[["3_crisis_applications"]] <- plot_ly(
  x = ~ lubridate::month(month_ending_date, label = TRUE),
  y = ~ crisis_applications,
  hoverinfo = ~ "text"
) %>%
  add_trace(data = datasets[["3_crisis_applications"]] %>%
              group_by(year = year(month_ending_date)) %>%
              filter(year != 2020),
            type = "scatter",
            mode = "lines",
            text = ~ text) %>%
  add_trace(data = datasets[["3_crisis_applications"]] %>%
              filter(year(month_ending_date) == 2020),
            type = "scatter",
            mode = "markers+lines",
            marker = list(size = 7),
            text = ~ text) %>%
  add_style_chart() %>%
  layout(
    showlegend = FALSE,
    colorway = c(col_palette),
    shapes = shapes[["3_crisis_applications"]],
    annotations = filter(annotations,
                         plot == "3_crisis_applications",
                         dataset == "3_crisis_applications") %>%
      mutate(x = month(x)) %>% # Use week numbers instead of dates
      pmap(list) #transpose and convert to list
  )

## Crime ----------------------------------------------------------------------
plots[["3_crime"]] <- plot_ly(
  data = datasets[["3_crime"]],
  x = ~ recorded,
  y = ~ crime_group,
  name = ~ year,
  text = ~ text,
  hoverinfo = ~ "text"
) %>%
  add_trace(type = "bar") %>%
  add_style_chart() %>%
  layout(colorway = c(col_palette["sg_grey"], col_palette["sg_blue"]))

# Loneliness ------------------------------------------------------------------
plots[["3_loneliness"]] <- plot_ly(
  data = datasets[["3_loneliness"]],
  x = ~ date,
  marker = list(size = 7),
  name = ~ Measure,
  hoverinfo = ~ "text"
) %>%
  add_trace(
    type = "scatter",
    y = ~ percent,
    text = ~text_2020,
    mode = "markers+lines",
    line = list(color = col_palette["sg_blue"]),
    marker = list(color = col_palette["sg_blue"])
  ) %>%
  add_style_chart() %>%
  layout(showlegend = FALSE,
         shapes = shapes[["3_loneliness"]],
         annotations = filter(annotations,
                              plot == "3_loneliness",
                              dataset == "3_loneliness") %>%
           pmap(list))

# Trust in government ---------------------------------------------------------
plots[["3_trust"]] <- plot_ly(
  data = datasets[["3_trust"]],
  x = ~ date,
  marker = list(size = 7),
  name = ~ Measure,
  hoverinfo = ~ "text"
) %>%
  add_trace(
    type = "scatter",
    y = ~ percent,
    text = ~text_2020,
    mode = "markers+lines",
    line = list(color = col_palette["sg_blue"]),
    marker = list(color = col_palette["sg_blue"])
  ) %>%
  add_style_chart() %>%
  layout(showlegend = FALSE,
         shapes = shapes[["3_trust"]],
         annotations = filter(annotations,
                              plot == "3_trust",
                              dataset == "3_trust") %>%
           pmap(list))

# Threats to Jobs -------------------------------------------------------------
plots[["3_job"]] <- plot_ly(
  data = datasets[["3_job"]],
  x = ~ date,
  marker = list(size = 7),
  name = ~ Measure,
  hoverinfo = ~ "text"
) %>%
  add_trace(
    type = "scatter",
    y = ~ percent,
    text = ~text_2020,
    mode = "markers+lines",
    line = list(color = col_palette["sg_blue"]),
    marker = list(color = col_palette["sg_blue"])
  ) %>%
  add_style_chart() %>%
  layout(showlegend = FALSE,
         shapes = shapes[["3_job"]],
         annotations = filter(annotations,
                              plot == "3_job",
                              dataset == "3_job") %>%
           pmap(list))

# Transport -------------------------------------------------------------------
plots[["H3_transport"]] <- plot_ly(
  data = datasets[["H3_transport"]],
  x = ~ Date,
  marker = list(size = 7),
  name = ~ Measure,
  hoverinfo = ~ "text"
) %>%
  add_trace(
    type = "scatter",
    y = ~ `%`,
    text = ~text,
    mode = "markers+lines",
    line = list(color = col_palette["sg_blue"]),
    marker = list(color = col_palette["sg_blue"])
  ) %>%
  add_style_chart() %>%
  layout(
    yaxis = list(
      tickformat = "%"
    )
  )

# 4 Economy -------------------------------------------------------------------
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
    shapes = shapes[["4a"]],
    annotations = filter(annotations, plot == "4a", dataset == "4a") %>%
      pmap(list) #transpose and convert to list
  )

# Turnover ----------------------------------------------------------------
p <- ggplot(
  data = datasets[["4_turnover"]] %>%
    ungroup(),
  mapping = aes(x = month_short,
                y = turnover,
                group = industry)
) +
  geom_ribbon(mapping = aes(ymin = 50,
                            ymax = turnover),
              fill = col_palette["sg_light_blue"]) +
  geom_line(colour = col_palette["sg_blue"]) +
  geom_point(colour = col_palette["sg_blue"]) +
  facet_wrap( ~ industry, ncol = 5) +
  theme(
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    strip.background = element_rect(fill = "white"),
    panel.background = element_blank()
  )


plots[["4_turnover"]] <- ggplotly(p) %>%
  config(displayModeBar = FALSE,
         showAxisDragHandles = FALSE) %>%
  htmlwidgets::onRender(
    "function(el, x) {
    Plotly.d3.selectAll('.cursor-pointer').style('cursor', 'crosshair')}"
  )

# Unemployment ----------------------------------------------------------------
plots[["4_unemployment"]] <- plot_ly(
  data = datasets[["4_unemployment"]],
  x = ~ year_quarter,
  y = ~ rate,
  marker = list(size = 7),
  hoverinfo = ~ "text",
  text = ~text
) %>%
  add_trace(
    type = "scatter",
    mode = "markers+lines",
    line = list(color = col_palette["sg_blue"]),
    marker = list(color = col_palette["sg_blue"])
  ) %>%
  add_style_chart() %>%
  layout(
    yaxis = list(
      tickformat = "%"
    )
  )

# Claimant counts -------------------------------------------------------------
plots[["4_claimants"]] <- plot_ly(
  data = datasets[["4_claimants"]],
  x = ~ date,
  y = ~ count,
  marker = list(size = 7),
  hoverinfo = ~ "text",
  text = ~text
) %>%
  add_trace(
    type = "scatter",
    mode = "markers+lines",
    line = list(color = col_palette["sg_blue"]),
    marker = list(color = col_palette["sg_blue"])
  ) %>%
  add_style_chart()

# GDP ----------------------------------------------------------------
plots[["4_GDP"]] <- plot_ly(
  data = datasets[["4_GDP"]],
  x = ~ date,
  y = ~ gdp,
  marker = list(size = 7),
  hoverinfo = ~ "text",
  text = ~text
) %>%
  add_trace(
    type = "scatter",
    mode = "markers+lines",
    line = list(color = col_palette["sg_blue"]),
    marker = list(color = col_palette["sg_blue"])
  ) %>%
  add_style_chart()

