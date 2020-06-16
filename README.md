# README
This site presents analysis on COVID-19 in Scotland. It is part of our commitment to bring transparency to our work and decisions on the crisis and to support understanding and public engagement with some of the very difficult issues that we face. We have drawn together data and evidence on the various harms and wider impacts – health, societal and economic – caused by the crisis. Much of this evidence is still emerging, and the scale and nature of the impacts will change over time.

## How to update
1. Save the latest copies of these files into the data subdirectory (not currently on GitHub):
  - [NRS deaths](https://www.nrscotland.gov.uk/covid19stats)
  - [SG daily trends](https://www.gov.scot/publications/trends-in-number-of-people-in-hospital-with-confirmed-or-suspected-covid-19/)
  - [PHS A&E attendances](https://beta.isdscotland.org/find-publications-and-data/health-services/hospital-care/nhs-performs-weekly-update-of-emergency-department-activity-and-waiting-time-statistics/)
  - [PHS planned and emergency admissions](https://scotland.shinyapps.io/phs-covid-wider-impact/)
  - Four Harms Input Data (from eRDM)
2. Run rmarkdown::render_site()
3. Copy files from the _site subdirectory to the shared folder for internal QA

## How it works
This is a collection of R Markdown files that make a static website.

## RMarkdown Reference materials
- https://bookdown.org/yihui/rmarkdown/
- https://plotly-r.com/
