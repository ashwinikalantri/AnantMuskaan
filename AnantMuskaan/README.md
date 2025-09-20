# Instruction for setup

The following variables needs to be saved in the Environment:

## Method 1
### Redcap API Key
- `Sys.setenv("API_KEY" = "XXXXX") ## Replace with the Redcap API`
- `Sys.setenv("REDCAP_URL" = "https://nhrp-rdp.icmr.org.in/api/")`


### Database
- `Sys.setenv("DB_PATH" = "anantmuskaan.sqlite")`

## Method 2
Create a file named `.Renviron`

Add the following to the file
```
#Redcap API
API_KEY = "XXXXX" ## Replace with the Redcap API
REDCAP_URL = "https://nhrp-rdp.icmr.org.in/api/"

# Database
DB_PATH = "anantmuskaan.sqlite"
```