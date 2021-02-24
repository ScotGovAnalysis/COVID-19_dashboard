equality_title <- narrative %>% 
  filter(harm != 0, section == "Equality") %>%
  mutate(img = paste0("images/harm",harm,"-icon.svg)")) %>% #insert image with name harm1-icon.svg etc. 
  select(harm, text, img)

equality_text <- function(ds) {
  ds <- ds %>%
    left_join(equality_title %>% rename(title = text), by = "harm") 
  
  tagList(
    div(class = "row",style = "font-size:0", #this is a little hacky for getting it to align to the bottom of the row, but couldn't find anything else that works in bootstrap 3
        div(class = "col-sm-12 col-md-6", style = "padding:0; display: inline-block; vertical-align: bottom; float: none;",
            div(
              filter(ds, harm == 1) %>%
                select(text, title) %>%
                pmap( ~ div(class = "equality-box",
                            style = paste0("background-color: #e2f0d9;"),
                            title = ..2,
                            img(src = "images/harm1-icon.svg", height = "20px", width = "20px"),
                            create_html(..1))
                )
            )
        ),
        div(class = "col-sm-12 col-md-6",  style = "padding:0; display: inline-block; vertical-align: bottom; float: none;",
            div(
              filter(ds, harm == 2) %>%
                select(text, title) %>%
                pmap( ~ div(class = "equality-box",
                            style = paste0("background-color: #bdd7ee;"),
                            title = ..2,
                            img(src = "images/harm2-icon.svg", height = "20px", width = "20px"),
                            create_html(..1))
                )
            )
        )
    ),
    div(class = "row", 
        div(class = "col-md-6",
            style = "padding:0;",
            div(filter(ds, harm == 3) %>%
                  select(text, title) %>%
                  pmap( ~ div(class = "equality-box",
                              style = paste0("background-color: #fdbbf7;"),
                              title = ..2,
                              img(src = "images/harm3-icon.svg", height = "20px", width = "20px"),
                              create_html(..1))
                  )
            )
        ),
        div(class = "col-md-6",
            style = "padding:0;",
            div(filter(ds, harm == 4) %>%
                  select(text, title) %>%
                  pmap( ~ div(class = "equality-box",
                              style = paste0("background-color: #fbe5d6;"),
                              title = ..2,
                              img(src = "images/harm4-icon.svg", height = "20px", width = "20px"),
                              create_html(..1))
                  )
            )
        )
    )
  )
}  


