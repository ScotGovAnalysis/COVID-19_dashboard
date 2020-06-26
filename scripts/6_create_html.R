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
        a(href = "summary.html", h3("Visual summary")),
        p("Click on here to see an overview of the four harms")
        ),
      div(
        class = "col-md-6",
        img(src = "images/navigation_detail.svg",
            style = "width: 90%;",
            alt = ""),
        # alt text left intentionally empty as the image is
        # purely decorative
        a(href = "detail.html", h3("Detailed analysis")),
        p("Click on here to explore the four harms")
      ))


harm_panels_1_2 <-
  div(class = "row fluid-row",
      div(class = "col-md-6", div(
        class = "panel panel-default",
        div(class = "panel-heading",
            h3(
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
            h3(
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
            h3(
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
            h3(
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
            h3(
              class = "panel-title",
              a("Direct health impacts",
                href = "detail.html#1_direct_health_harms")
            )),
        div(class = "panel-body", plots[["1_sparklines"]])
      )),
      div(class = "col-md-6", div(
        class = "panel panel-default",
        div(class = "panel-heading",
            h3(
              class = "panel-title",
              a("Health impacts not directly related to COVID-19",
                href = "detail.html#2_indirect_health_harms")
            )),
        div(class = "panel-body", plots[["2_sparklines"]])
      )))


harm_panels_3_4_spark <-
  div(class = "row fluid-row",
      div(class = "col-md-6", div(
        class = "panel panel-default",
        div(class = "panel-heading",
            h3(
              class = "panel-title",
              a("Societal impacts", href = "detail.html#3_societal_harms")
            )),
        div(class = "panel-body", plots[["3_sparklines"]])
      )),
      div(class = "col-md-6", div(
        class = "panel panel-default",
        div(class = "panel-heading",
            h3(
              class = "panel-title",
              a("Economic impacts", href = "detail.html#4_economic_harms")
            )),
        div(class = "panel-body", plots[["4_sparklines"]])
      )))