# Add plots and HTML to existing dataset ----------------------------------
datasets[["sg_template"]][["TEXT"]] <-
  datasets[["sg_template"]][["TEXT"]] %>%
  mutate(
    slug = TITLE_max_35_characters %>%
      stringr::str_to_lower() %>%
      stringr::str_replace_all(pattern = " ", replacement = "_") %>%
      stringr::str_remove_all(pattern = "&") %>%
      paste0("detail.html#", .),
    slug = case_when(
      stringr::str_starts(worksheet_name, pattern = "2.3.") ~
        "detail.html#emergency_and_planned_admissions",
      TRUE ~ slug
    ), 
    spark_text = paste0("<H3 style = 'margin:0; font-size:18px;'>",
                        "<a href=\"",
                        slug,
                        "\">",
                        TITLE_max_35_characters,
                        "</a>",
                        "</H3>",
                        HEADLINE_max_60_characters),
    spark_plot = plots[paste0(worksheet_name, "_spark")],
    spark_exists = map(spark_plot, is.list),
    spark_html = map2(.x = spark_text,
                      .y = spark_plot,
                      .f = ~ div(
                        class = "row fluid-row",
                        div(
                          class = "col-sm-7",
                          style = 'padding-right:0;',
                          htmltools::HTML(.x)
                        ),
                        div(class = "col-sm-5",
                            div(alt = "", .y))
                      ))
  )

# Index -------------------------------------------------------------------
h_rule <- htmltools::hr(style="height: 2px;
                         border-width: 0;
                         background-color: #8e979c;")

nav_buttons <-
  div(class = "row fluid-row text-center",
      div(
        class = "col-md-6",
        img(
          src = "images/navigation_summary.svg",
          style = "width: 90%;",
          alt = "The four harms of Covid-19: direct health, indirect health,
          society, economy"
        ),
        a(href = "summary.html", h2("Visual summary"))
        ),
      div(
        class = "col-md-6",
        img(src = "images/navigation_detail.svg",
            style = "width: 90%;",
            alt = ""),
        # alt text left intentionally empty as the image is
        # purely decorative
        a(href = "detail.html", h2("Detailed analysis"))
      ))


harm_panels_1_2 <-
  div(class = "row fluid-row",
      div(class = "col-md-6", div(
        class = "panel panel-default",
        div(class = "panel-heading",
            h2(
              class = "panel-title",
              a("Direct health impacts",
                href = "detail.html#1_direct_health_harms")
            )),
        div(
          class = "panel-body",
          narrative %>%
            filter(harm == 1, section == "Definition") %>%
            pull("text") %>%
            create_html()
        )
      )),
      div(class = "col-md-6", div(
        class = "panel panel-default",
        div(class = "panel-heading",
            h2(
              class = "panel-title",
              a("Health impacts not directly related to COVID-19",
                href = "detail.html#2_indirect_health_harms")
            )),
        div(
          class = "panel-body",
          narrative %>%
            filter(harm == 2, section == "Definition") %>%
            pull("text") %>%
            create_html()
        )
      )))

harm_panels_3_4 <-
  div(class = "row fluid-row",
      div(class = "col-md-6", div(
        class = "panel panel-default",
        div(class = "panel-heading",
            h2(
              class = "panel-title",
              a("Societal impacts", href = "detail.html#3_societal_harms")
            )),
        div(
          class = "panel-body",
          narrative %>%
            filter(harm == 3, section == "Definition") %>%
            pull("text") %>%
            create_html()
        )
      )),
      div(class = "col-md-6", div(
        class = "panel panel-default",
        div(class = "panel-heading",
            h2(
              class = "panel-title",
              a("Economic impacts", href = "detail.html#4_economic_harms")
            )),
        div(
          class = "panel-body",
          narrative %>%
            filter(harm == 4, section == "Definition") %>%
            pull("text") %>%
            create_html()
        )
      )))


# Summary -----------------------------------------------------------------
harm_panels_1_2_spark <-
  div(class = "row fluid-row",
      div(class = "col-md-6", div(
        class = "panel panel-default",
        div(class = "panel-heading",
            h2(
              class = "panel-title",
              a("Direct health impacts",
                href = "detail.html#1_direct_health_harms")
            )),
        div(class = "panel-body",
            datasets[["sg_template"]][["TEXT"]] %>%
              filter(stringr::str_starts(worksheet_name, "1."),
                     spark_exists == TRUE) %>%
              pull(spark_html)
            )
      )),
      div(class = "col-md-6", div(
        class = "panel panel-default",
        div(class = "panel-heading",
            h2(
              class = "panel-title",
              a("Health impacts not directly related to COVID-19",
                href = "detail.html#2_indirect_health_harms")
            )),
        div(class = "panel-body",
            datasets[["sg_template"]][["TEXT"]] %>%
              filter(stringr::str_starts(worksheet_name, "2."),
                     spark_exists == TRUE) %>%
              pull(spark_html))
      )))


harm_panels_3_4_spark <-
  div(class = "row fluid-row",
      div(class = "col-md-6", div(
        class = "panel panel-default",
        div(class = "panel-heading",
            h2(
              class = "panel-title",
              a("Societal impacts", href = "detail.html#3_societal_harms")
            )),
        div(class = "panel-body",
            datasets[["sg_template"]][["TEXT"]] %>%
              filter(stringr::str_starts(worksheet_name, "3."),
                     spark_exists == TRUE) %>%
              pull(spark_html))
      )),
      div(class = "col-md-6", div(
        class = "panel panel-default",
        div(class = "panel-heading",
            h2(
              class = "panel-title",
              a("Economic impacts", href = "detail.html#4_economic_harms")
            )),
        div(class = "panel-body",
            datasets[["sg_template"]][["TEXT"]] %>%
              filter(stringr::str_starts(worksheet_name, "4."),
                     spark_exists == TRUE) %>%
              pull(spark_html))
      )))
