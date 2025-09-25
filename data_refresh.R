library(DBI)
library(REDCapR)
library(dplyr)
library(RSQLite)

withProgress(message = 'Data update in progress',
             detail = 'This may take a while...',
             value = 0,
             {
               
               conn <- dbConnect(RSQLite::SQLite(), "anantmuskaan.sqlite")
               
               ## Data Download ####
               token <- dbGetQuery(conn, "SELECT value FROM settings WHERE name = 'api_key'")$value[1]
               
               url <- "https://nhrp-rdp.icmr.org.in/api/"
               
               incProgress(30 / 100, detail = "Downloading Redcap Data")
               
               tryCatch({
                 redcap_result <- REDCapR::redcap_read(redcap_uri = url, token = token)
                 
                 if (redcap_result$success) {
                   data <- redcap_result$data
                   message("Successfully read ", nrow(data), " rows from REDCap.")
                 } else {
                   warning("REDCap API call did not succeed. Message: ", redcap_result$outcome_message)
                 }
                 
               }, error = function(e) {
                 message("An error occurred while fetching data from REDCap.")
                 warning("REDCap Error: ", e$message)
                 showNotification(paste("REDCap Error: ", e$message),
                                  type = "error")
                   
               })
               
               
               incProgress(30 / 100, detail = "Downloading Labels")
               
               tryCatch({
                 redcap_result_b <- REDCapR::redcap_read(redcap_uri = url,
                                                       token = token,
                                                       raw_or_label = "label",
                                                       fields = c("block1")
                                                       )
                 
                 if (redcap_result_b$success) {
                   block_lab <- redcap_result_b$data
                   message("Successfully read ", nrow(block_lab), " rows from REDCap.")
                 } else {
                   warning("REDCap API call did not succeed. Message: ", redcap_result_b$outcome_message)
                 }
                 
               }, error = function(e) {
                 message("An error occurred while fetching data from REDCap.")
                 warning("REDCap Error: ", e$message)
                 showNotification(paste("REDCap Error: ", e$message),
                                  type = "error")
               })
               
               
               incProgress(10 / 100, detail = "School Data")
               
               ### School Data ####
               d1 <- data %>%
                 filter(is.na(redcap_repeat_instrument)) %>%
                 select(record_id, site, area_type, school_type, school1) %>%
                 left_join(block_lab %>% select(record_id, block1), by = join_by(record_id)) %>%
                 rename(block = block1,
                        school = school1)
               
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
                 ) %>% 
                 rename(brush_activity = brush_activity_1_5,
                        participants = first_to_fifth_participants) %>% 
                 mutate(participants = as.numeric(participants))
               
               incProgress(20 / 100, detail = "Storing Data")
               
               if (!dbExistsTable(conn, "school")) {
                 dbCreateTable(conn, "school", list(
                   record_id = "TEXT",
                   site = "TEXT",
                   area_type = "TEXT",
                   school_type = "TEXT",
                   school = "TEXT",
                   block = "TEXT"
                 ))
               }
               
               if (!dbExistsTable(conn, "activities")) {
                 dbCreateTable(conn, "activities", list(
                   record_id = "TEXT",
                   task_start_date = "TEXT",
                   task_schedule_date = "TEXT",
                   activity_date = "TEXT",
                   brush_activity = "INTEGER",
                   participants = "INTEGER"
                 ))
               }
               dbWriteTable(conn, "school", d1, overwrite = TRUE)
               dbWriteTable(conn, "activities", d2, overwrite = TRUE)
               dbExecute(
                 conn,
                 "INSERT OR REPLACE INTO settings (name, value) VALUES ('last_update', ?)",
                 params = list(Sys.time())
               )
               
             })