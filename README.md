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


## Setup Environment variables
Some sensitive information should not be present in your code, and needs to be stored in the environment.

### Method 1

Run these commands in R Console to set environment variables

#### Redcap API Key
- `Sys.setenv("API_KEY" = "XXXXX") ## Replace with the Redcap API`
- `Sys.setenv("REDCAP_URL" = "https://nhrp-rdp.icmr.org.in/api/")`


#### Database
- `Sys.setenv("DB_PATH" = "anantmuskaan.sqlite")`

### Method 2

Create a file named `.Renviron` in the same folder as your files, and add the following code to the file
```
#Redcap API
API_KEY = "XXXXX" ## Replace with the Redcap API
REDCAP_URL = "https://nhrp-rdp.icmr.org.in/api/"

# Database
DB_PATH = "anantmuskaan.sqlite"
```

## First Run
On first run, the app will create a new local database file `anantmuskaan.sqlite`. This file will store some of the data from the Anant Muskaan Redcap Project in this data.

## Data Update
The app will automatically get new data of it doesn't have the latest data. This could also be manually done by clicking the ![](arrow-rotate-right.png) icon (only visible if data is stale) on the footer. 

## Data Privacy
The app will pull some of the Redcap data from the Anant Muskaan project. It will also store these data. Make sure that the data is handled in compliance with the data protection laws. One on them being not hosting it on servers outside the country.

## Support and suggestions
Create an Issue [here](https://github.com/ashwinikalantri/AnantMuskaan/issues)