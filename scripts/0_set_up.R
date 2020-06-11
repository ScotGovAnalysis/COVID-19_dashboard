# Load libraries ----------------------------------------------------------
library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(here)
library(sf)
library(lubridate)
library(plotly)

# Define data paths -------------------------------------------------------
datasets <- plots <- shapes <- list()

data_path <- function(pattern) {
  list.files(path = "data",
             pattern = pattern) %>%
    paste("data/", ., sep = "") %>%
    here::here()
}

paths <- list(
  sg = data_path("^Trends\\+in\\+daily\\+COVID-19\\+data\\+.*\\.xlsx"),
  nrs = data_path("^covid-deaths-data-week-[0-9][0-9]\\.xlsx"),
  phs = data_path("^202[0-9]-[0-9][0-9]-[0-9][0-9]-ed-weekly-nhsscotland-data.csv$"),
  pop_est = here::here("data/mid-year-pop-est-19-info.xlsx"),
  sitrep = here::here("data/Dashboard - Time series.xlsx"),
  text = here::here("data/text.xlsx")
)

# Define plotting variables -----------------------------------------------
col_palette <- c(sg_grey = "#8e979c", # From SG logo
                 sg_blue = "#0065bd", # From SG logo
                 sg_light_blue = "#E5F0F8") # From menu items on SG website
hoverlabel_spark <- list(
  bgcolor = col_palette[["sg_light_blue"]],
  bordercolor = "white",
  font = list(color = "black")
)
dates <- c(
  lockdown = "2020-03-26",
  WHO_declares_pandemic = "2020-03-11",
  start_sparklines = "2020-03-07"
)
spark_height <- 80


# Create shapes -----------------------------------------------------------
shapes <- list(
  "1r" = list(
    list(
      type = "line",
      layer = "below",
      x0 = "2020-02-19",
      x1 = "2020-05-27",
      y0 = 1,
      y1 = 1,
      line = list(color = "black", dash = "dot")
    ),
    list(
      type = "line",
      layer = "below",
      x0 = "2020-03-26",
      x1 = "2020-03-26",
      y0 = 1,
      y1 = 7,
      line = list(color = col_palette["sg_grey"], dash = "dot")
    )
  ),
  "1_infect" = list(
    type = "line",
    layer = "below",
    x0 = dates[["lockdown"]],
    x1 = dates[["lockdown"]],
    y0 = 0,
    y1 = 25000,
    line = list(color = col_palette["sg_grey"], dash = "dot")
  ),
  "1a" = list(
    type = "line",
    layer = "below",
    x0 = dates[["lockdown"]],
    x1 = dates[["lockdown"]],
    y0 = 0,
    y1 = 160,
    line = list(color = col_palette["sg_grey"], dash = "dot")
  ),
  "1c" = list(
    list(
      type = "rect",
      layer = "below",
      fillcolor = col_palette[["sg_light_blue"]],
      x0 = dates[["lockdown"]],
      x1 = as.Date("2020-04-11"),
      y0 = 0,
      y1 = 1596,
      line = list(width = 0)
    )
  ),
  "2a" = list(
    list(
      type = "line",
      layer = "below",
      x0 = 13,
      x1 = 13,
      y0 = 3000,
      y1 = 11059,
      line = list(color = col_palette["sg_grey"], dash = "dot")
    ),
    list(
      type = "line",
      layer = "below",
      x0 = 17,
      x1 = 17,
      y0 = 3000,
      y1 = 15056,
      line = list(color = col_palette["sg_grey"], dash = "dot")
    )
  ),
  "3a" = list(
    list(
      type = "rect",
      layer = "below",
      fillcolor = col_palette[["sg_light_blue"]],
      x0 = as.Date("2020-04-06"),
      x1 = as.Date("2020-04-20"),
      y0 = 0,
      y1 = 2106,
      line = list(width = 0)
    )
  ),
  "4a" = list(
    list(
      type = "line",
      layer = "below",
      x0 = dates[["WHO_declares_pandemic"]],
      x1 = dates[["WHO_declares_pandemic"]],
      y0 = 0,
      y1 = 5000,
      line = list(color = col_palette["sg_grey"], dash = "dot")
    ),
    list(
      type = "line",
      layer = "below",
      x0 = dates[["lockdown"]],
      x1 = dates[["lockdown"]],
      y0 = 0,
      y1 = 9500,
      line = list(color = col_palette["sg_grey"], dash = "dot")
    )
  )
)


# Define function(s) ------------------------------------------------------
add_style_spark <- function(p) {
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
        range = c(dates[["start_sparklines"]], as.character(Sys.Date()))
      ),
      margin = list(l = 200,
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
