equality_col <- tibble(harm = 1:4, colour = c("#e2f0d9", "#bdd7ee", "#fdbbf7", "#fbe5d6"))
equality_col <- tibble(harm = 1:4, colour = RColorBrewer::brewer.pal(12,"Paired")[c(3,1,5,11)])
#equality_col <- tibble(harm = 1:4, colour = RColorBrewer::brewer.pal(11,"RdYlBu")[c(4,5,6,7)])

equality_title <- narrative %>% 
  filter(harm != 0, section == "Equality") %>%
  select(harm, text)

equality_text <- function(ds) {
  ds <- ds %>%
    inner_join(equality_col, by = "harm") %>%
    left_join(equality_title %>% rename(title = text), by = "harm") %>%
    mutate(column = (harm - 1) %% 2 + 1)
  div(class = "row",
      div(class = "col-md-6",
          style = "padding:0;",
          filter(ds, column == 1) %>%
            select(text, colour, title) %>%
            pmap(~ div(class="equality-box",
                       style = paste0("background-color:", ..2, ";",
                                      "border-radius: 20px;",
                                      "padding: 10px;",
                                      "margin: 10px 5px;"),
                       title = ..3,
                       create_html(..1))
            ) %>%
            tagList()
      ),
      div(class = "col-md-6",
          style = "padding:0;",
          filter(ds, column == 2) %>%
            select(text, colour, title) %>%
            pmap(~ div(class="equality-box",
                       style = paste0("background-color:", ..2, ";",
                                      "border-radius: 20px;",
                                      "padding: 10px;",
                                      "margin: 10px 5px;"),
                       title = ..3,
                       create_html(..1))
            ) %>%
            tagList()
      )
  )
}

equality_text <- function(ds) {
  ds <- ds %>%
    inner_join(equality_col, by = "harm") %>%
    left_join(equality_title %>% rename(title = text), by = "harm") %>%
    mutate(row = (harm - 1) %/% 2 + 1,
           column = (harm - 1) %% 2 + 1)
  ds %>%
    group_by(row) %>%
    nest() %>%
    pmap(~{ #map along grid rows
      row_data <- .y
      div(class = "row",
          map(seq_len(2), ~{ #map along grid cols
            box_text <- filter(row_data, column == .x) %>%
              select(text, colour, title)
            div(class = "col-md-6",
                style = "padding:0;",
                pmap(box_text,
                     ~ div(class = "equality-box",
                           style = paste0("background-color:", ..2, ";"),
                           title = ..3,
                           create_html(..1))
                ) %>%
                  tagList()
            )
          })
      )
    }) %>%
    tagList()
# 
#     select(row, text, colour, title) %>%
# 
#     nest(-row) %>%
# 
# 
# 
# 
# 
#   div(class = "row",
#       map(seq_len(4), ~{
#         box_text <- filter(ds, harm == .x) %>%
#           select(text, colour, title)
#         if(nrow(box_text) == 0) {
#           div(class = "col-md-6",
#               style = "padding:0;",
#               div(span(style = "width:100%", p("fhfgfh")))
#           )
#         } else {
#           div(class = "col-md-6",
#               style = "padding:0;",
#               pmap(box_text,
#                    ~ div(style = paste0("background-color:", ..2, ";",
#                                         "border-radius: 20px;",
#                                         "padding: 10px;",
#                                         "margin: 10px 5px;"),
#                          title = ..3,
#                          create_html(..1))
#               ) %>%
#                 tagList()
#           )
#         }
#       }) %>%
#         tagList()
#   )
}

# equality_text <- function(ds) {
#   ds <- ds %>%
#     inner_join(equality_col, by = "harm") %>%
#     left_join(equality_title %>% rename(title = text), by = "harm") %>%
#     mutate(column = (harm - 1) %% 2 + 1)
#   div(class = "row",
#       div(class = "col-md-6",
#           style = "padding:0;",
#           filter(ds, column == 1) %>%
#             select(text, colour, title) %>%
#             pmap(~ div(style = paste0("background-color:", ..2, ";",
#                                       "border-radius: 20px;",
#                                       "padding: 10px;",
#                                       "margin: 10px 5px;"),
#                        title = ..3,
#                        create_html(..1))
#             ) %>%
#             tagList()
#       ),
#       div(class = "col-md-6",
#           style = "padding:0;",
#           filter(ds, column == 2) %>%
#             select(text, colour, title) %>%
#             pmap(~ div(style = paste0("background-color:", ..2, ";",
#                                       "border-radius: 20px;",
#                                       "padding: 10px;",
#                                       "margin: 10px 5px;"),
#                        title = ..3,
#                        create_html(..1))
#             ) %>%
#             tagList()
#       )
#   )
# }