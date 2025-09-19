library(DBI)
library(RSQLite)

conn <- dbConnect(RSQLite::SQLite(), Sys.getenv("DB_PATH"))
## Data Download ####

d1 <- dbReadTable(conn, "school")
d2 <- dbReadTable(conn, "activities") %>% 
  mutate(
    task_start_date  = as.POSIXct(task_start_date),
    task_schedule_date = as.Date(task_schedule_date),
    activity_date = as.Date(activity_date)
    )

str(d1)
str(d2)
