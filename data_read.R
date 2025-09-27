library(DBI)
library(RSQLite)

conn <- dbConnect(RSQLite::SQLite(), "anantmuskaan.sqlite")
## Data Download ####
if(dbExistsTable(conn, "school") &&
   dbExistsTable(conn, "activities")) {
  d1 <- dbReadTable(conn, "school")
  
  d2 <- dbReadTable(conn, "activities") %>%
    mutate(
      task_start_date  = as.POSIXct(task_start_date),
      task_schedule_date = as.Date(task_schedule_date),
      activity_date = as.Date(activity_date)
    )
  
} else {
  source("data_refresh.R")
}
