# 1 Direct health -------------------------------------------------------------
## R value --------------------------------------------------------------------
plots[["1.1_R"]] <- plot_ly(
  data = datasets[["1.1_R"]],
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
    yaxis = list(tickformat = ".1f"),
    shapes = shapes[["1r"]],
    annotations = filter(annotations, plot == "1r", dataset == "1r") %>%
      pmap(list),
    margin = list(r = 10)
  )
  
## Number of infectious people ------------------------------------------------
plots[["1.2_infectious"]] <- plot_ly(
  data = datasets[["1.2_infectious"]],
  x = ~ date,
  hoverinfo = "text",
  y = ~ midpoint
) %>%
  add_trace(
    type = "scatter",
    mode = "markers",
    marker = list(opacity = 0),
    error_y = ~list(array = upperbound - midpoint,
                    arrayminus = midpoint - lowerbound,
                    color = col_palette["sg_blue"],
                    thickness = 5,
                    width = 6),
    text = ~ text
  ) %>%
  add_style_chart() %>%
  layout(
    xaxis = list(showspikes = TRUE,
                 spikemode = "across",
                 type = "date",
                 tickformat = "%d %b"
                ),
                     # yaxis = list(tickformat = ".1f"),
    # shapes = shapes[["1r"]],
    # annotations = filter(annotations, plot == "1r", dataset == "1r") %>%
    #   pmap(list),
    margin = list(r = 10)
  )
  
# plots[["1.2_infectious_logscale"]] <- plot_ly(
#   data = datasets[["1.2_infectious"]],
#   x = ~ date,
#   hoverinfo = "text",
#   y = ~ midpoint
# ) %>%
#   add_ribbons(
#     ymin = ~ lowerbound,
#     ymax = ~ upperbound,
#     line = list(color = "transparent"),
#     fillcolor = col_palette["sg_light_blue"]
#   ) %>%
#   add_trace(
#     type = "scatter",
#     mode = "lines+markers",
#     marker = list(color = col_palette["sg_blue"]),
#     line = list(color = col_palette["sg_blue"]),
#     text = ~ text
#   ) %>%
#   add_style_chart() %>%
#   layout(
#     showlegend = FALSE,
#     yaxis = list(type = "log",
#                  tickformat = ",.1r",
#                  range = c(0, 4)),
#     annotations = filter(annotations,
#                          plot == "1_infect_logscale",
#                          dataset == "1_infect_logscale")
#   )

## Cases ------------------------------------------------------------
plots[["1.3_cases"]] <- plot_ly(
  data = datasets[["1.3_cases"]],
  x = ~ date,
  hoverinfo = "text"
) %>%
  add_trace(
    name = "Cases",
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
    legend = list(orientation = "h",
                  x = 0, y = 100),
    margin = list(r = 15)
  )

## Cases ------------------------------------------------------------
# plots[["1.3_cases"]] <- plot_ly(
#   data = datasets[["1.3_cases"]],
#   x = ~ date,
#   hoverinfo = "text"
# ) %>%
#   add_trace(
#     name = "Cases",
#     y = ~ count,
#     type = "bar",
#     marker = list(color = col_palette["sg_grey"]),
#     text = ~ count_text
#   ) %>%
#   add_trace(
#     name = "7 day average",
#     y = ~ count_7day_avg,
#     type = "scatter",
#     mode = "markers+lines",
#     marker = list(size = 7, color = col_palette["sg_blue"]),
#     line = list(color = col_palette["sg_blue"]),
#     text = ~ count_7day_avg_text
#   ) %>%
#   add_style_chart() %>%
#   layout(
#     showlegend = FALSE,
#     yaxis = list(type = "log",
#                  tickformat = ",.1r",
#                  range = c(0, log10(450))),
#     shapes = list(
#       list(
#         type = "line",
#         layer = "below",
#         x0 = dates[["lockdown"]],
#         x1 = dates[["lockdown"]],
#         y0 = 0,
#         y1 = 450,
#         line = list(color = col_palette["sg_grey"], dash = "dot")
#       )
#     ),
#     annotations = filter(annotations,
#                          plot == "1_cases",
#                          dataset == "1_cases") %>%
#       mutate(y = log10(y)) %>%
#       pmap(list),
#     legend = list(orientation = "h",
#                   x = 0, y = 100)
#   )

plots[["1.3_cases_logscale"]] <- plot_ly(
  data = datasets[["1.3_cases"]],
  x = ~ date,
  hoverinfo = "text"
) %>%
  add_trace(
    name = "Cases",
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
    yaxis = list(type = "log",
                 tickformat = ",.1r",
                 range = c(0, log10(450))),
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
    annotations = filter(annotations,
                         plot == "1_cases",
                         dataset == "1_cases") %>%
      mutate(y = log10(y)) %>%
      pmap(list),
    legend = list(orientation = "h",
                  x = 0, y = 100),
    margin = list(r = 15)
  )

# Weekly deaths ---------------------------------------------------------------
plots[["1.4_deaths"]] <- plot_ly(
  data = datasets[["1.4_deaths"]],
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
    showlegend = FALSE,
    margin = list(r = 25)
  )

## Hospital admissions --------------------------------------------------------
plots[["1.5_admissions"]] <- plot_ly(
  data = datasets[["1.5_admissions"]],
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
    showlegend = FALSE,
    margin = list(r = 15)
  )

plots[["1.5_admissions_logscale"]] <- plot_ly(
  data = datasets[["1.5_admissions"]],
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
    showlegend = FALSE,
    yaxis = list(type = "log",
                 tickformat = ",.1r",
                 range = c(0, log10(213))),
    margin = list(r = 15)
  )

# 2 Indirect health -----------------------------------------------------------
## A&E attendance -------------------------------------------------------------
# plots[["2.1_A&E"]] <- plot_ly(
#   x = ~ week_ending_date_2020,
#   y = ~ attendance,
#   hoverinfo = ~ "text"
# ) %>%
#   add_trace(data = datasets[["2.1_A&E"]] %>%
#               group_by(year) %>%
#               filter(year < 2020),
#             type = "scatter",
#             mode = "lines",
#             text = ~ text) %>%
#   add_trace(data = datasets[["2.1_A&E"]] %>%
#               filter(year >= 2020),
#             type = "scatter",
#             mode = "markers+lines",
#             marker = list(size = 7),
#             text = ~ text) %>%
#   add_style_chart() %>%
#   layout(
#     showlegend = FALSE,
#     colorway = c(col_palette),
#     shapes = shapes[["2a"]],
#     annotations = filter(annotations, plot == "2a", dataset == "2a") %>%
#       pmap(list) #transpose and convert to list
#   )
plots[["2.1_A&E"]] <- plot_ly(
  x = ~ week_ending_date_2020,
  y = ~ attendance,
  hoverinfo = ~ "text"
) %>%
  add_trace(data = datasets[["2.1_A&E"]] %>% filter(year<2020) %>%
              group_by(year),
            name="2015-2019",
            type = "scatter",
            mode = "lines",
            text = ~ text) %>%
  add_trace(data = datasets[["2.1_A&E"]] %>% filter(year==2020) %>%
              group_by(year),
            name="2020",
            type = "scatter",
            mode = "lines",
            text = ~ text) %>%
  add_trace(data = datasets[["2.1_A&E"]] %>% filter(year==2021) %>%
              group_by(year),
            name="2021",
            type = "scatter",
            mode = "lines",
            line=list(color=rgb(0,176/255,240/255)),
            text = ~ text) %>%
  add_style_chart() %>%
  layout(
    showlegend = TRUE,
    legend=list(
      font=list(size=10),
      x= .7, y= 0
    ),
    colorway = c(col_palette),
    shapes = shapes[["2a"]],
    xaxis=list(tickformat="%b"),
    annotations = filter(annotations, plot == "2a", dataset == "2a",
                         #added this to remove the label from annotations
                         id != 15) %>%
      pmap(list) #transpose and convert to list
  )

## Excess deaths --------------------------------------------------------------
plots[["2.2_excess"]] <- plot_ly(
  data = datasets[["2.2_excess"]],
  x = ~ date,
  y = ~ count,
  hoverinfo = ~ "text"
) %>%
  add_ribbons(
    data = datasets[["2.2_excess_spark"]],
    ymin = ~ avg_2015_19,
    ymax = ~ all_2020,
    text = ~text,
    line = list(color = "transparent"),
    fillcolor = col_palette["sg_light_blue"],
    fill = "tonext"
  ) %>%
  add_trace(
    data = datasets[["2.2_excess"]],
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
      title = "",
      showline = FALSE,
      showgrid = FALSE,
      zeroline = FALSE
    ),
    yaxis = list(
      fixedrange = TRUE,
      showline = FALSE,
      title = "",
      showgrid = FALSE,
      tick0 = 0,
      zeroline = FALSE,
      tickformat = ","
    ),
    shapes = shapes[["2_excess"]],
    annotations = filter(annotations,
                         plot == "2_excess",
                         dataset == "2_excess") %>%
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


## Emergency and planned admissions -------------------------------------------
plots[["2.3_admissions"]] <- plot_ly(
  data = datasets[["2.3_admissions"]],
  x = ~ Week_ending,
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
  )  %>%
  add_style_chart() %>%
  layout(showlegend = FALSE,
         annotations = filter(annotations,
                              plot == "2_admissions",
                              dataset == "2_admissions") %>%
           pmap(list),
         margin = list(r = 10))


# Avoiding GPs and hospitals --------------------------------------------------
# "#333e48" - grey from SG branding guidelines
# "#99A4AE" - same grey as above but lightened 40%
# "#E5F0F8" - sg_light_blue
# "#66CBFF" - sg_blue lightened 40%
# "#0065bd" - sg_blue

plots[["2.4_avoiding"]] <-
  plot_ly(
    data = datasets[["2.4_avoiding"]],
    x = ~ date,
    y = ~ rate,
    name = ~ sentiment,
    marker = list(line = list(color = "white",
                              width = 3)),
    hoverinfo = ~ "text"
  ) %>%
  add_trace(type = "bar",
            text = ~text_2020) %>%
  add_style_chart() %>%
  layout(
    title = "<b>I would avoid</b> contacting GP \nfor immediate non-COVID-19 health concerns",
    showlegend = FALSE,
    barmode = "stack",
    yaxis = list(tickformat = "%"),
    colorway = c("#333e48", "#99A4AE", "#E5F0F8", "#66CBFF", "#0065bd"),
    annotations = datasets[["2.4_avoiding"]] %>%
      filter(date_start == max(date_start)) %>%
      mutate(rate_cum = cumsum(rate) - rate / 2) %>% # Position annotations in the middle of each bar
      select(date, sentiment, rate_cum) %>%
      mutate(text = stringr::str_to_sentence(sentiment) %>%
               stringr::str_wrap(width = 15),
             font = c(list(list(color = "#333e48")),
                      list(list(color = "#99A4AE")),
                      list(list(color = col_palette["sg_grey"])),
                      list(list(color = "#66CBFF")),
                      list(list(color = col_palette["sg_blue"]))),
             plot = "2_GP",
             dataset = "2_GP",
             showarrow = FALSE,
             xanchor = "left",
             xshift = 150, #Was orginally 30 but have changed due to smaller number of bars
             align = "left") %>%
      rename(y = rate_cum,
             x = date) %>%
      pmap(list))

# 3 Society -------------------------------------------------------------------
## Children at school ---------------------------------------------------------
library('stringr')

#-------Separate primary, secondary and special charts from 15 March 

#p <- ggplot(
#  data = datasets[["3.1_schools"]],
#  mapping = aes(x = date,
#                y = count,
#                group = Measure,
#                text = text)
#) +
#  geom_point(colour = col_palette["sg_blue"]) +
#  geom_line(data = datasets[["3.1_schools"]][!is.na(datasets[["3.1_schools"]]$count),],
#            colour = col_palette["sg_blue"]) +
#  facet_wrap( ~ measure2, ncol = 1) +
#  theme(
#    axis.title = element_blank(),
#    axis.ticks = element_blank(),
#    strip.background = element_rect(fill = "white"),
#    panel.background = element_blank()#,
#    #panel.spacing = unit(10, "mm")
#  )
#
#plots[["3.1_schools"]] <- ggplotly(p,
#                                      height = 660,
#                                      tooltip = "text") %>%
#  config(displayModeBar = FALSE,
#         showAxisDragHandles = FALSE) %>%
#  htmlwidgets::onRender(
#    "function(el, x) {
#    Plotly.d3.selectAll('.cursor-pointer').style('cursor', 'crosshair')}"
#  )

#---------Separate specific chart option for 15 March----------
#plots[["3.1.1_schools"]] <- plot_ly(
#  data = datasets[["3.1_schools"]] %>% filter(Measure=="Primary"),
#  x = ~ date,
#  y = ~ count,
#  marker = list(size = 7),
#  hoverinfo = ~ "text",
#  text = ~text
#) %>%
#  add_trace(
#    type = "scatter",
#    mode = "markers+lines",
#    line = list(color = col_palette["sg_blue"]),
#    marker = list(color = col_palette["sg_blue"])
#  ) %>%
#  add_style_chart() %>%
#  layout(
#    yaxis = list(
#      tickformat = "%"
#    ),
#    xaxis = list(
#      tickformat = "%d %b %Y"
#    )
#  )

#Option of hybrid chart for 15 March
#plots[["3.1_schools"]] <- plot_ly(
#  data = datasets[["3.1_schools"]],
#  x = ~ date,
#  y = ~ count,
#  marker = list(size = 7),
#  name = ~ Measure,
#  hoverinfo = ~ "text",
#  text = ~text
#) %>%
#  add_trace(
#    type = "scatter",
#    mode = "markers+lines",
#    connectgaps = TRUE
#  ) %>%
#  add_style_chart() %>%
#  layout(
#    showlegend = FALSE,
#    yaxis = list(tickformat = "%"),
#    colorway = c("#0065bd", "#66CBFF", "#66CBFF", "#8E979C")
#  )

#-------Old bar chart with absences------
# plots[["3.1_schools"]] <-plot_ly(
#   #data = filter(datasets[["3.1_schools"]],Measure!='All_attending'),
#   data = datasets[["3.1_schools"]] %>% filter(Measure!='All_attending'),
#   x = ~ as.Date(date[!is.na(count)],format= '%d/%m/%Y'),# %>% format("%d %b")),
#   y = ~ count[!is.na(count)],
#   name = ~ Measure[!is.na(count)] %>% str_replace_all(c("Non_covid_absence"),c("Non covid-19 related absence")) %>% str_replace_all(c("Covid_absence"),c("Covid-19 related absence")),
#   text = ~ text[!is.na(count)],
#   hoverinfo = ~ "text"
# ) %>%
#   add_style_chart() %>%
#   add_trace(type = "bar",
#             marker = list(size = 7),
#             text = ~ text[!is.na(count)]) %>%
#   layout(barmode = 'group',
#          xaxis = list(type = "category"),
#          yaxis = list(tickformat = "%"),
#          legend = list(xanchor = "left",
#                        yanchor = "bottom",
#                        orientation = "h",
#                        x = 0,
#                        y = 1.05),
#          colorway = c("#66CBFF", "#0065bd"))

#--------------Line graph from 19 April------------ 
 plots[["3.1_schools"]] <- plot_ly(
   data = datasets[["3.1_schools"]],
   x = ~ date,
   y = ~ count,
   name = ~ Measure,
   hoverinfo = ~ "text"
 ) %>%
   add_style_chart() %>%
   add_trace(data = filter(datasets[["3.1_schools"]],
                           date <= as.Date("2020-12-23")),
             type = "scatter",
             mode = "markers+lines",
             marker = list(size = 7),
             text = ~ text,
             connectgaps = TRUE) %>%
  add_trace(data = filter(datasets[["3.1_schools"]],
                          date >= as.Date("2021-03-15"),
                          date <= as.Date("2021-04-11")),
            type = "scatter",
            mode = "markers+lines",
            marker = list(size = 7),
            text = ~ text,
            connectgaps = TRUE) %>%
 add_trace(data = filter(datasets[["3.1_schools"]],
                          date >= as.Date("2021-04-12"),
                          date <= as.Date("2021-06-25")),
            type = "scatter",
            mode = "markers+lines",
            marker = list(size = 7),
            text = ~ text,
            connectgaps = TRUE) %>%
  add_trace(data = filter(datasets[["3.1_schools"]],
                          date >= as.Date("2021-08-18")),
            type = "scatter",
            mode = "markers+lines",
            marker = list(size = 7),
            text = ~ text,
            connectgaps = TRUE) %>%
   layout(
     showlegend = FALSE,
     yaxis = list(tickformat = "%"),
     colorway = c("#0065bd", "#66CBFF", "#8E979C"),
     shapes = shapes[["3_school"]],
     annotations = filter(annotations,
                          plot == "3_school",
                          dataset == "3_school") %>%
       pmap(list)
   )

## Crisis applications --------------------------------------------------------
plots[["3.2_crisis"]] <- plot_ly(
  x = ~ lubridate::month(month_ending_date, label = TRUE),
  y = ~ crisis_applications,
  hoverinfo = ~ "text"
) %>%
  add_trace(data = datasets[["3.2_crisis"]] %>%
              group_by(year = year(month_ending_date)) %>%
              filter(year%in% c(2018,2019)),
            type = "scatter",
            mode = "lines",
            text = ~ text,
            name = "2018-19") %>%
  add_trace(data = datasets[["3.2_crisis"]] %>%
              filter(year(month_ending_date) == 2020),
            type = "scatter",
            mode = "markers+lines",
            marker = list(size = 7),
            text = ~ text,
            name = "2020") %>%
  add_trace(data = datasets[["3.2_crisis"]] %>% #RB adding 2021 line to chart
              filter(year(month_ending_date) == 2021),
            type = "scatter",
            mode = "markers+lines",
            marker = list(size = 7,
            color=rgb(0,176/255,240/255)), 
            line=list(color=rgb(0,176/255,240/255)), 
            text = ~ text,
            name = "2021") %>%
  add_style_chart() %>%
  layout(
    showlegend = TRUE,
    legend = list(font=list(size=10),
                  x = 0.8, y = 0.1),
    colorway = c(col_palette)
  )

## Crime ----------------------------------------------------------------------
# plots[["3.3_crime"]] <- plot_ly(
#   data = filter(datasets[["3.3_crime"]], month == "May"),
#   x = ~ recorded,
#   y = ~ crime_group,
#   name = ~ forcats::as_factor(year),
#   text = ~ text,
#   hoverinfo = ~ "text"
# ) %>%
#   add_trace(type = "bar") %>%
#   add_style_chart() %>%
#   layout(colorway = c(col_palette["sg_grey"], col_palette["sg_blue"]),
#          yaxis = list(autorange = "reversed",
#                       tickformat = "[[ ]<]"),
#          xaxis = list(tickformat = ","))


################################################################################
#--- This is the chart that currently gets plotted (set in detail.Rmd) --------#
p <- ggplot(
  data = datasets[["3.3_crime"]] %>%
    filter(crime_group %in% c("Total crimes",
                              "Total offences")),
  mapping = aes(x = month,
                y = recorded,
                group = year_present,
                colour = as.factor(year_present),
                text = text)
) +
  geom_line() +
  geom_point() +
  facet_wrap( ~ crime_group, ncol = 2) +
  # facet_grid(cols = vars(crime_group)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
  scale_colour_manual(values = c(col_palette[["sg_grey"]],
                                 col_palette[["sg_blue"]],
                                 rgb(0,176/255,240/255))) +
  theme(
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    strip.background = element_rect(fill = "white"),
    panel.background = element_blank(),
    legend.title = element_blank()
  )

plots[["3.3.5_crime"]] <- ggplotly(p,
                                   tooltip = "text") %>% # c("month", "recorded", "year")) %>%
  config(displayModeBar = FALSE,
         showAxisDragHandles = FALSE) %>%
  layout(
    legend = list(xanchor = "left",
                  yanchor = "bottom",
                  orientation = "h",
                  x = 0,
                  y = 1.05)
  ) %>%
  htmlwidgets::onRender(
    "function(el, x) {
    Plotly.d3.selectAll('.cursor-pointer').style('cursor', 'crosshair')}"
  )

# Loneliness ------------------------------------------------------------------
plots[["3.4_loneliness"]] <- plot_ly(
  data = datasets[["3.4_loneliness"]],
  x = ~ date_start,
  marker = list(size = 7),
  name = ~ source,
  hoverinfo = ~ "text"
) %>%
  add_trace(
    type = "scatter",
    y = ~ percent / 100,
    text = ~text_2020,
    mode = "markers+lines",
    line = list(color = col_palette["sg_blue"]),
    marker = list(color = col_palette["sg_blue"])
  ) %>%
  add_style_chart() %>%
  layout(showlegend = FALSE,
         yaxis = list(
           tickformat = "%"
         ),
         annotations = filter(annotations,
                              plot == "3.4_loneliness",
                              dataset == "3.4_loneliness") %>%
           pmap(list))

# Trust in government ---------------------------------------------------------
plots[["3.5_trust"]] <- plot_ly(
  data = datasets[["3.5_trust"]],
  x = ~ date_start,
  marker = list(size = 7),
  name = ~ source,
  hoverinfo = ~ "text"
) %>%
  add_trace(
    type = "scatter",
    y = ~ percent / 100,
    text = ~text_2020,
    mode = "markers+lines",
    line = list(color = col_palette["sg_blue"]),
    marker = list(color = col_palette["sg_blue"])
  ) %>%
  add_style_chart() %>%
  layout(showlegend = FALSE,
         yaxis = list(
           tickformat = "%"
         ),
         annotations = filter(annotations,
                              plot == "3.5_trust",
                              dataset == "3.5_trust") %>%
           pmap(list))

# Threats to Jobs -------------------------------------------------------------
plots[["3.6_job"]] <- plot_ly(
  data = datasets[["3.6_job"]],
  x = ~ date_start,
  marker = list(size = 7),
  name = ~ source,
  hoverinfo = ~ "text"
) %>%
  add_trace(
    type = "scatter",
    y = ~ percent / 100,
    text = ~text_2020,
    mode = "markers+lines",
    line = list(color = col_palette["sg_blue"]),
    marker = list(color = col_palette["sg_blue"])
  ) %>%
  add_style_chart() %>%
  layout(showlegend = FALSE,
         yaxis = list(
           tickformat = "%"
         ),
         annotations = filter(annotations,
                              plot == "3.6_job",
                              dataset == "3.6_job") %>%
           pmap(list),
         margin = list(r = 10))

# Transport -------------------------------------------------------------------
plots[["3.7_transport"]] <- plot_ly(
  data = datasets[["3.7_transport"]],
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
    ),
    margin = list(r = 40)
  )

# 4 Economy -------------------------------------------------------------------
# Turnover --------------------------------------------------------------------
turnover_baseline <- 50

## Turnover All ---------------------------------------------------------------
plots[["4.1.1_turnover"]] <- plot_ly(
  data = datasets[["4.1_turnover"]] %>%
    filter(industry == "All Industries"),
  x = ~ date,
  hoverinfo = ~ "text",
  height = 200
) %>%
  add_ribbons(
    ymin = ~ turnover_baseline,
    ymax = ~ turnover,
    line = list(color = "transparent"),
    fillcolor = col_palette["sg_light_blue"]
  ) %>%
  add_trace(
    type = "scatter",
    y = ~ turnover,
    text = ~text,
    mode = "markers+lines",
    line = list(color = col_palette["sg_blue"]),
    marker = list(size = 7, color = col_palette["sg_blue"])
  ) %>%
  add_style_chart() %>%
  layout(showlegend = FALSE,
         margin = list(r = 10))

## Turnover Broad sectors -----------------------------------------------------
p <- ggplot(
  data = datasets[["4.1_turnover"]] %>%
    filter(parent == "All Industries"),
  mapping = aes(x = date,
                y = turnover,
                group = industry,
                text = text)
) +
  geom_ribbon(mapping = aes(ymin = turnover_baseline,
                            ymax = turnover),
              fill = col_palette["sg_light_blue"]) +
  geom_line(colour = col_palette["sg_blue"]) +
  facet_wrap( ~ industry) +
  theme(
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    strip.background = element_rect(fill = "white"),
    panel.background = element_blank(),
    panel.spacing = unit(10, "mm")
  )

plots[["4.1.2_turnover"]] <- ggplotly(p,
                                      height = 240,
                                      tooltip = "text") %>%
  config(displayModeBar = FALSE,
         showAxisDragHandles = FALSE) %>%
  htmlwidgets::onRender(
    "function(el, x) {
    Plotly.d3.selectAll('.cursor-pointer').style('cursor', 'crosshair')}"
  )

## Turnover Services ----------------------------------------------------------
p <- ggplot(
  data = datasets[["4.1_turnover"]] %>%
    filter(parent == "Services") %>%
    mutate(industry = forcats::fct_reorder(industry, turnover, .fun = min)),
  mapping = aes(x = date,
                y = turnover,
                group = industry,
                text = text)
) +
  geom_ribbon(mapping = aes(ymin = turnover_baseline,
                            ymax = turnover),
              fill = col_palette["sg_light_blue"]) +
  geom_line(colour = col_palette["sg_blue"]) +
  facet_wrap( ~ industry, ncol = 2) +
  theme(
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    strip.background = element_rect(fill = "white"),
    panel.background = element_blank()
  )

# plotly was cutting some of these small multiples off. So set the height to be
# bigger than the default (450).
plots[["4.1.3_turnover"]] <- ggplotly(p,
                                      height = 450 * 1.2,
                                      tooltip = "text") %>%
  config(displayModeBar = FALSE,
         showAxisDragHandles = FALSE) %>%
  htmlwidgets::onRender(
    "function(el, x) {
    Plotly.d3.selectAll('.cursor-pointer').style('cursor', 'crosshair')}"
  )

# GDP ----------------------------------------------------------------
plots[["4.2_GDP"]] <- plot_ly(
  data = datasets[["4.2_GDP"]],
  x = ~ date,
  y = ~ `GDP (2017=100)`,
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
    shapes = shapes[["4.2_GDP"]],
    annotations = filter(annotations,
                     plot == "4.2_GDP",
                     dataset == "4.2_GDP")
  )

# Unemployment ----------------------------------------------------------------
plots[["4.3_unemployment"]] <- plot_ly(
  data = datasets[["4.3_unemployment"]],
  x = ~ date,
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
    ),
    shapes = shapes[["4.3_unemployment"]],
    annotations = filter(annotations,
                         plot == "4.3_unemployment",
                         dataset == "4.3_unemployment")
  )

# Claimant counts -------------------------------------------------------------
plots[["4.4_claimants"]] <- plot_ly(
  data = datasets[["4.4_claimants"]],
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
  add_style_chart() %>%
  layout(
    margin = list(r = 30),
    shapes = shapes[["4.4_claimants"]],
    annotations = filter(annotations,
                         plot == "4.4_claimants",
                         dataset == "4.4_claimants")
  )
