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
Download the `Source Code (zip)` asset from the latest [Anant Muskaan](https://github.com/ashwinikalantri/AnantMuskaan/releases/latest) release, unzip the file in the desired folder.
Create a file `.Renviron` with the following content:
```
GH_TOKEN" = "github_pat_XXXX" ## Replace with github secure token, shared via email to site PIs on request.
```

## First Run
- On first run, the app will create a new local database file `anantmuskaan.sqlite`. This will store some of the data from the Anant Muskaan Redcap Project in this data.
- You will be prompted to enter the Redcap API key. This will be stored in the local database. To change the API Key, use the ![](assets/rotate.png) **Change API Key** link in the footer.

## Data Update
The app will automatically get new data of it doesn't have the latest data. This could also be manually done by clicking the ![](assets/reload.png) **Update Data** link on the footer. 

## Data Privacy
The app will pull some of the Redcap data from the Anant Muskaan project. It will also store these data. Make sure that the data is handled in compliance with the data protection laws. One on them being not hosting it on servers outside the country.

## Updating the App
When a new version is released, you need to re-download the latest version, and replace the old files. You can preserve the old `anantmuskaan.sqlite` file (it contains your API key and the local data). If you delete this file, you will have to redo the initial First Run setup. 

## Hosting Options
Due to the above mentioned data privacy concerns, hosting data on free to use public servers [Shinyapps.io](https://www.shinyapps.io) is best avoided. The app can run locally on a PC or Mac, but is more useful when hosted on a server accessible to all team members. Hosting it on a [Shiny Server](https://posit.co/products/open-source/shiny-server/) under your control is the best, but technical option. 

## Support and suggestions
Create an Issue [here](https://github.com/ashwinikalantri/AnantMuskaan/issues)