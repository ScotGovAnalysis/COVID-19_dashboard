# README
This site presents analysis on COVID-19 in Scotland. It is part of our commitment to bring transparency to our work and decisions on the crisis and to support understanding and public engagement with some of the very difficult issues that we face. We have drawn together data and evidence on the various harms and wider impacts – health, societal and economic – caused by the crisis. Much of this evidence is still emerging, and the scale and nature of the impacts will change over time.

## How to update
1. Save the latest copy of the spreadsheet into the data subdirectory:
  - Remove the date from the end of the spreadsheet file name
  - Update the date on [this line](https://github.com/DataScienceScotland/COVID-19_dashboard/blob/0bc0a2399114296fe08530ef877d45916c19366e/scripts/0_define_variables.R#L43) in `0_define_variables.R` 
2. Run `rmarkdown::render_site()`
3. Copy files from the `_site` subdirectory to the shared folder for internal QA
3. Zip files in the `_site` subdirectory and send to the EPCC

## How it works
This is a collection of R Markdown files that make a static website.

## RMarkdown Reference materials
- https://bookdown.org/yihui/rmarkdown/
- https://plotly-r.com/

# Licence
All content is available under the [Open Government Licence v3.0](http://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/), except where otherwise stated.
