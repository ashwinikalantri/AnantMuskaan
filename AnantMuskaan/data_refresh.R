library(DBI)
library(REDCapR)
library(dplyr)

withProgress(message = 'Data update in progress',
             detail = 'This may take a while...',
             value = 0,
             {
               ## Data Download ####
               token <- Sys.getenv("API_KEY")
               url <- Sys.getenv("REDCAP_URL")
               
               incProgress(30 / 100, detail = "Downloading Redcap Data")
               data <- REDCapR::redcap_read(redcap_uri = url, token = token)$data
               
               incProgress(30 / 100, detail = "Downloading Labels")
               block_lab <- REDCapR::redcap_read(
                 redcap_uri = url,
                 token = token,
                 raw_or_label = "label",
                 fields = c("block1")
               )$data
               
               incProgress(10 / 100, detail = "School Data")
               ### School Data ####
               d1 <- data %>%
                 filter(is.na(redcap_repeat_instrument)) %>%
                 select(record_id, site, area_type, school_type, school1) %>%
                 left_join(block_lab, by = join_by(record_id)) %>%
                 rename(block = block1)
               
               incProgress(10 / 100, detail = "Activity Data")
               ### Activity Data ####
               d2 <- data %>%
                 filter(redcap_repeat_instrument == "implementation_status") %>%
                 select(
                   record_id,
                   task_start_date,
                   task_schedule_date,
                   activity_date,
                   brush_activity_1_5,
                   first_to_fifth_participants
                 )
               
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
               incProgress(20 / 100, detail = "Storing Data")
               dbWriteTable(amdb, "school", d1, overwrite = TRUE)
               dbWriteTable(amdb, "activities", d2, overwrite = TRUE)
             })