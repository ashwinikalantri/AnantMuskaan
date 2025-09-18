library(DBI)
amdb <-
  DBI::dbConnect(
    odbc::odbc(),
    Driver = "PostgreSQL",
    Server = Sys.getenv("DB_HOST"),
    Database = Sys.getenv("DB_NAME"),
    UID = Sys.getenv("DB_USER"),
    PWD = Sys.getenv("DB_PASS"),
    Port = 5432
  )

## Data Download ####

d1 <- dbReadTable(amdb, "school")
d2 <- dbReadTable(amdb, "activities")
