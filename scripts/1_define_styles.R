add_style_spark <- function(p,
                            range = c(dates[["start_sparklines"]],
                                      as.character(Sys.Date()))) {
  p %>% add_trace(
    type = "scatter",
    mode = "lines",
    line = list(color = "black"),
    fill = "tozeroy",
    fillcolor = col_palette["sg_light_blue"],
    hoverinfo = "text"
  ) %>%
    config(displayModeBar = FALSE,
           showAxisDragHandles = FALSE) %>%
    layout(
      showlegend = FALSE,
      paper_bgcolor = "rgba(0, 0, 0, 0)",
      plot_bgcolor = "rgba(0, 0, 0, 0)",
      yaxis = list(
        fixedrange = TRUE,
        title = "",
        showgrid = FALSE,
        zeroline = FALSE,
        rangemode = "tozero",
        showticklabels = FALSE
      ),
      xaxis = list(
        fixedrange = TRUE,
        title = "",
        showgrid = FALSE,
        showticklabels = FALSE,
        range = range
      ),
      margin = list(l = 0, #210,
                    r = 0,
                    t = 0,
                    b = 0)
    ) %>%
    htmlwidgets::onRender(
      "function(el, x) {
      Plotly.d3.selectAll('.cursor-pointer').style('cursor', 'crosshair')}"
    )
}

add_style_chart <- function(p) {
  p %>% config(displayModeBar = FALSE,
               showAxisDragHandles = FALSE) %>%
    layout(
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
        zeroline = FALSE,
        rangemode = "tozero",
        tickformat = ","
      ),
      paper_bgcolor = "rgba(0, 0, 0, 0)",
      plot_bgcolor = "rgba(0, 0, 0, 0)",
      margin = list(l = 0,
                    r = 0)
    ) %>%
    htmlwidgets::onRender(
      "function(el, x) {
      Plotly.d3.selectAll('.cursor-pointer').style('cursor', 'crosshair')}"
    )
}
