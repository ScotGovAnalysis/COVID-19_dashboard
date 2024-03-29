# Load libraries ----------------------------------------------------------
library(readxl)
library(openxlsx)
library(dplyr)
library(tidyr)
library(purrr)
library(here)
library(sf)
library(lubridate)
library(plotly)
library(markdown)
library(htmltools)


# Define data paths -------------------------------------------------------
datasets <- plots <- shapes <- list()

data_path <- function(pattern) {
  list.files(path = "data",
             pattern = pattern) %>%
    paste("data/", ., sep = "") %>%
    here::here()
}

paths <- list(
  #The below line was in use until 27-10-2020 but would not read in week 43 for deaths
  #sg_template = here::here("data/COVID-19 Public Facing Dashboard - Four Harms Input Data.xlsx")
  sg_template = "data/COVID-19 Public Facing Dashboard - Four Harms Input Data.xlsx"
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
  start_sparklines = "2020-03-07",
  start_sparklines_economy = "2016-01-01",
  data_updated = "2021-04-26"
)
spark_height <- 80 * 1.2 # Temporarily increasing this until we reduce the word
                         # count of some headlines


# Create shapes -----------------------------------------------------------
shapes <- list(
  "1r" = list(
    list(
      type = "line",
      layer = "below",
      xref = "paper",
      # x0 = "2020-03-26",
      # x1 = "2020-06-24",
      x0 = 0,
      x1 = 1,
      y0 = 1,
      y1 = 1,
      line = list(color = "black", dash = "dot")
    )
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
      x0 = "2020-03-29",
      x1 = "2020-03-29",
      y0 = 3000,
      y1 = 11059,
      line = list(color = col_palette["sg_grey"], dash = "dot")
    ),
    list(
      type = "line",
      layer = "below",
      x0 = "2020-04-26",
      x1 = "2020-04-26",
      y0 = 7000,
      y1 = 15056,
      line = list(color = col_palette["sg_grey"], dash = "dot")
    )
  ),
  
  
  "2_GP" = list(
    list(
      type = "line",
      layer = "below",
      x0 = "2020-04-24",
      x1 = "2020-04-24",
      y0 = 25,
      y1 = 45,
      line = list(color = col_palette["sg_grey"], dash = "dot")
    )
  ),
  "3_school" = list(
    list(
      type = "rect",
      layer = "below",
      fillcolor = col_palette[["sg_light_blue"]],
      x0 = as.Date("2020-12-24"),
      x1 = as.Date("2021-01-05"),
      yref = "paper",
      y0 = 0,
      y1 = 1,
      line = list(width = 0)
    ),
    list(
      type = "rect",
      layer = "below",
      fillcolor = col_palette[["sg_light_blue"]],
      x0 = as.Date("2021-03-15"),
      x1 = as.Date("2021-04-01"),
      yref = "paper",
      y0 = 0,
      y1 = 1,
      line = list(width = 0)
    ),
    list(
      type = "rect",
      layer = "below",
      fillcolor = col_palette[["sg_light_blue"]],
      x0 = as.Date("2021-04-02"),
      x1 = as.Date("2021-04-11"),
      yref = "paper",
      y0 = 0,
      y1 = 1,
      line = list(width = 0)
    ),
    list(
      type = "rect",
      layer = "below",
      fillcolor = col_palette[["sg_light_blue"]],
      x0 = as.Date("2021-06-26"),
      x1 = as.Date("2021-08-17"),
      yref = "paper",
      y0 = 0,
      y1 = 1,
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
  ),
  "4.2_GDP" = list(
    list(
      type = "line",
      layer = "below",
      x0 = as.Date("2020-03-01"),
      x1 = as.Date("2020-03-01"),
      y0 = 40,
      y1 = 95,
      line = list(color = col_palette["sg_grey"], dash = "dot")
    )
  ),
  "4.3_unemployment" = list(
    list(
      type = "line",
      layer = "below",
      x0 = as.Date("2020-05-01"),
      x1 = as.Date("2020-05-01"),
      y0 = 0.02,
      y1 = 0.04493,
      line = list(color = col_palette["sg_grey"], dash = "dot")
    )
  ),
  "4.4_claimants" = list(
    list(
      type = "line",
      layer = "below",
      x0 = as.Date("2020-03-01"),
      x1 = as.Date("2020-03-01"),
      y0 = 50000,
      y1 = 111400,
      line = list(color = col_palette["sg_grey"], dash = "dot")
    )
  )
)
