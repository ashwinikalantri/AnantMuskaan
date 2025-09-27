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
  - fontawesome
  - gh
- RedCap API key
- Github Token

## Setup
Download the `Source Code (zip)` asset from the latest [Anant Muskaan](https://github.com/ashwinikalantri/AnantMuskaan/releases/latest) release, unzip the file in the desired folder.

## First Run
- On first run, the app will create a new local database file `anantmuskaan.sqlite`. This will store some of the data from the Anant Muskaan Redcap Project in this data.
- You will be prompted to enter the Redcap API key. This will be stored in the local database. To change the API Key, use the ![](assets/rotate.png) **Change API Key** link in the footer.
- Version Check (Optional): To enable the option to check of the app is updated to the latest version, a Github Token is needed. It can be entered by clicking the ![](assets/github.png) **Enter GitHub Token** link on the footer. 

## Data Update
The app will automatically get new data of it doesn't have the latest data. This could also be manually done by clicking the ![](assets/reload.png) **Update Data** link on the footer. 

## Updating the App
When a new version is released, you will be notified on the footer. You need to re-download the latest version, and replace only the `app.R`, `data_read.R` and `data_refresh.R` files. You can preserve the old `anantmuskaan.sqlite` file (it contains your API key and the local data). If you delete this file, you will have to redo the initial First Run setup. 

## Data Privacy and Hosting Options
The app will pull some of the Redcap data from the Anant Muskaan project. It will also store these data. Make sure that the data is handled in compliance with the data protection laws and ICMR policy. Due to these data privacy concerns, hosting data on free to use public servers [Shinyapps.io](https://www.shinyapps.io) is best avoided. The app can run locally on a PC or Mac, but is more useful when hosted on a server accessible to all team members. Hosting it on a [Shiny Server](https://posit.co/products/open-source/shiny-server/) under your control is the best, but technical option. 

## Support and suggestions
Create an Issue [here](https://github.com/ashwinikalantri/AnantMuskaan/issues)