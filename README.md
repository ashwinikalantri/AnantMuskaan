# Instruction for Setup

## Prerequsites
- [R](https://www.r-project.org)
- [RStudio IDE](https://posit.co/products/open-source/rstudio/)
- R Packages
  - shiny
  - bslib
  - shinyWidgets
  - gt
  - dplyr
  - tidyr
  - lubridate
  - RSQLite
  - stringr
  - DBI
  - REDCapR
- RedCap API key

## Setup
Copy the files `app.R`, `data_refresh.R` and `read_data.R` from [Anant Muskaan Repo](https://github.com/ashwinikalantri/AnantMuskaan/), and place them in a single folder.

## First Run
- On first run, the app will create a new local database file `anantmuskaan.sqlite`. This will store some of the data from the Anant Muskaan Redcap Project in this data.
- You will be prompted to enter the Redcap API key. This will be stored in the local database. To change the API Key, use the `Change API Key` link in the footer.

## Data Update
The app will automatically get new data of it doesn't have the latest data. This could also be manually done by clicking the x icon (only visible if data is stale) on the footer. 

<picture>
  <source media="(prefers-color-scheme: dark)" srcset="assets/reload_dark.png">
  <src="assets/reload_light.png">
</picture>

<svg aria-hidden="true" role="img" viewBox="0 0 512 512" style="height:1em;width:1em;vertical-align:-0.125em;margin-left:auto;margin-right:auto;font-size:inherit;fill:currentColor;overflow:visible;position:relative;">
  <path d="M463.5 224H472c13.3 0 24-10.7 24-24V72c0-9.7-5.8-18.5-14.8-22.2s-19.3-1.7-26.2 5.2L413.4 96.6c-87.6-86.5-228.7-86.2-315.8 1c-87.5 87.5-87.5 229.3 0 316.8s229.3 87.5 316.8 0c12.5-12.5 12.5-32.8 0-45.3s-32.8-12.5-45.3 0c-62.5 62.5-163.8 62.5-226.3 0s-62.5-163.8 0-226.3c62.2-62.2 162.7-62.5 225.3-1L327 183c-6.9 6.9-8.9 17.2-5.2 26.2s12.5 14.8 22.2 14.8H463.5z"/>
</svg>

## Data Privacy
The app will pull some of the Redcap data from the Anant Muskaan project. It will also store these data. Make sure that the data is handled in compliance with the data protection laws. One on them being not hosting it on servers outside the country.

## Hosting Options
Due to the above mentioned data privacy concerns, hosting data on free to use public servers [Shinyapps.io](https://www.shinyapps.io) is best avoided. The app can run locally on a PC or Mac, but is more useful when hosted on a server accessible to all team members. Hosting it on a [Shiny Server](https://posit.co/products/open-source/shiny-server/) under your control is the best, but technical option. 

## Support and suggestions
Create an Issue [here](https://github.com/ashwinikalantri/AnantMuskaan/issues)