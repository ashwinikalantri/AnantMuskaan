library(shiny)
library(bslib)
library(shinyWidgets)
library(gt)
library(dplyr)
library(tidyr)
library(lubridate)
library(RSQLite)
library(stringr)
library(shinyalert)
library(DBI)
library(DT)
library(fontawesome)
library(gh)
library(purrr)
library(tibble)
library(plotly)

ver <- "v1.3.4"

conn <- dbConnect(RSQLite::SQLite(), "anantmuskaan.sqlite")

dbExecute(conn,
          "CREATE TABLE IF NOT EXISTS settings (
              name TEXT PRIMARY KEY,
              value TEXT NOT NULL
            )")

dbExecute(
  conn,
  "CREATE TABLE IF NOT EXISTS school (
              record_id TEXT,
              site  TEXT,
              area_type TEXT,
              school_type TEXT,
              school TEXT,
              block TEXT
            )"
)

dbExecute(
  conn,
  "CREATE TABLE IF NOT EXISTS activities (
              record_id TEXT,
              task_start_date TEXT,
              task_schedule_date TEXT,
              activity_date TEXT,
              brush_activity INTEGER,
              participants INTEGER
            )"
)

ui <- page_fluid(
  card(card_header(h4("Anant Muskaan")),
       layout_columns(
         value_box( 
           title = "Total Schools", 
           textOutput("sch_no"),
           uiOutput("sch_ur"),
           uiOutput("sch_type"),
           paste0("MyCap on ",format(Sys.Date()-1,"%d %b")),
           uiOutput("sch_ent_yd"),
           showcase = plotlyOutput("graph_sch"), 
           showcase_layout = "bottom",
           #showcase = icon("school"),
           theme = "primary" 
         ),
         value_box( 
           title = textOutput("block_name_1"),
           textOutput("sch_no_b1"),
           uiOutput("sch_ur_b1"),
           uiOutput("sch_type_b1"),
           paste0("MyCap on ",format(Sys.Date()-1,"%d %b")),
           uiOutput("sch_ent_yd_b1"),
           showcase = plotlyOutput("graph_sch_b1"), 
           showcase_layout = "bottom",
           theme = "teal" 
         ),
         value_box( 
           title = textOutput("block_name_2"),
           textOutput("sch_no_b2"),
           uiOutput("sch_ur_b2"),
           uiOutput("sch_type_b2"),
           paste0("MyCap on ",format(Sys.Date()-1,"%d %b")),
           uiOutput("sch_ent_yd_b2"),
           showcase = plotlyOutput("graph_sch_b2"), 
           showcase_layout = "bottom",
           theme = "teal" 
         ),
         value_box( 
           title = textOutput("block_name_3"),
           textOutput("sch_no_b3"),
           uiOutput("sch_ur_b3"),
           uiOutput("sch_type_b3"),
           paste0("MyCap on ",format(Sys.Date()-1,"%d %b")),
           uiOutput("sch_ent_yd_b3"),
           showcase = plotlyOutput("graph_sch_b3"), 
           showcase_layout = "bottom",
           theme = "teal" 
         )
       ),
       layout_columns(
         card(checkboxGroupInput(
           "block_list",
           "Select Block",
           inline = T,
           choices = NA
         )), 
    card(
      checkboxGroupInput(
        "area_type_list",
        "Select Area",
        inline = T,
        choices = c("Rural" = "R", "Urban" = "U"),
        selected = c("R", "U")
      )
    ),
    card(
      checkboxGroupInput(
        "school_type_list",
        "Select School Type",
        inline = T,
        choices = c(
          "Gov School" = "GS",
          "Gov Aided" = "GA",
          "Pvt School" = "PS"
        ),
        selected = c("GS", "GA", "PS")
      )
    )
  )),
  navset_card_tab(
    nav_panel("Daily Data", card(
      dateInput(
        inputId = "daily_date",
        label = "Select Date",
        value = Sys.Date() - 1,
        max = Sys.Date() - 1,
        min = as.Date("2025-07-01"),
        format = "dd M yyyy"
      )
    ), card(gt_output("daily_table"))),
    nav_panel(
      "Date Range Data",
      card(
        dateRangeInput(
          inputId = "range_date",
          label = "Select Date Range",
          start = Sys.Date() - 7,
          end = Sys.Date() - 1,
          max = Sys.Date() - 1,
          min = as.Date("2025-07-01"),
          weekstart = 1,
          format = "dd M yyyy"
        )
      ),
      navset_card_tab(
        nav_panel("Entries", gt_output("range_ent_table")),
        nav_panel("Activities", gt_output("range_act_table")),
        nav_panel("No Activities", DTOutput("range_noent_table", height = "100%")),
        nav_panel("Schools with Entries", DTOutput("range_school_ent_table", height = "100%"))
      )
    ),
    nav_panel(
      "School-wise Data",
      layout_columns(card(
        dateRangeInput(
          inputId = "school_range_date",
          label = "Select Date Range",
          start = as.Date("2025-07-01"),
          end = Sys.Date() - 1,
          max = Sys.Date() - 1,
          min = as.Date("2025-07-01"),
          weekstart = 1,
          format = "dd M yyyy"
        )
      ), card(
        selectizeInput(
          "selectSchool",
          "Select School:",
          choices = NA ,
          options = list(dropdownParent = 'body')
        )
      )),
      # navset_card_tab(nav_panel(
      #   "Entries",
        layout_columns(
          value_box( 
            title = "No of Entries", 
            textOutput("sch_ent"),
            showcase = icon("calendar"),
            theme = "primary" 
            ),
          value_box( 
            title = "No of Activities", 
            textOutput("sch_act"),
            showcase = icon("tooth"),
            theme = "teal" 
          )
        ),
        #card(uiOutput("sch_ent")),
        DTOutput("school_ent_table", height = "100%")
      #))
    ),
    nav_panel("Monthly Data", card(
      airDatepickerInput(
        inputId = "month_date",
        label = "Select Month",
        view = "months",
        minView = "months",
        value = Sys.Date() - 1,
        max = Sys.Date() - 1,
        min = as.Date("2025-07-01"),
        dateFormat = "MMM yyyy",
        autoClose = TRUE
      )
    ), card(gt_output("monthly_table"))),
    card_footer(uiOutput("footer"))
  ),
  id = "tab"
)

server <- function(input, output, session) {
  api_key <- reactiveVal(NULL)
  
  prompt_for_key <- function() {
    shinyalert(
      inputPlaceholder = "API Key",
      showCancelButton = F,
      confirmButtonText = "Submit",
      title = "Redcap API Key Required",
      text = "Please enter your Redcap API key to continue.",
      type = "input",
      inputType = "password",
      callbackR = save_key_and_proceed,
      showConfirmButton = TRUE,
      session = session
    )
  }
  
  save_key_and_proceed <- function(key_from_input) {
    if (!is.null(key_from_input) &&
        key_from_input != FALSE &&
        nzchar(key_from_input) &&
        grepl("^([0-9A-Fa-f]{32})(?:\n)?$", key_from_input)) {
      dbExecute(
        conn,
        "INSERT OR REPLACE INTO settings (name, value) VALUES ('api_key', ?)",
        params = list(key_from_input)
      )
      
      api_key(key_from_input)
      
      shinyalert("Success!", "API Key has been saved.", type = "success")
    } else {
      shinyalert("Cancelled",
                 "You must provide a correct API key to use the app.",
                 type = "error")
      
      if (isTruthy(dbGetQuery(conn, "SELECT value FROM settings WHERE name = 'api_key'"))) {
        prompt_for_key()
      }
    }
  }
  
  key_from_db <- dbGetQuery(conn, "SELECT value FROM settings WHERE name = 'api_key'")
  
  if (nrow(key_from_db) > 0) {
    message("API key found in database.")
    api_key(key_from_db$value[1])
  } else {
    message("API key not found. Prompting user.")
    prompt_for_key()
  }
  
  observeEvent(api_key(), {
    req(api_key())
    conn <- dbConnect(RSQLite::SQLite(), "anantmuskaan.sqlite")
    
    source("data_read.R")
    
    if (!isTruthy(dbGetQuery(conn, "SELECT value FROM settings WHERE name = 'last_update'")$value)) {
      source("data_refresh.R", local = TRUE)
    } else {
      if (difftime(Sys.time(), as.POSIXct(as.numeric(
        dbGetQuery(
          conn,
          "SELECT value FROM settings WHERE name = 'last_update'"
        )$value
      )), units = "hours") > 24) {
        source("data_refresh.R", local = TRUE)
      }
    }
    
    
    
    ## Update inputs ####
    observe({
      updateCheckboxGroupInput(
        session,
        "block_list",
        choices = unique(d1$block),
        selected = unique(d1$block)
      )
      
    })
    
    
    output$footer <- renderUI({
      if (difftime(Sys.time(), as.POSIXct(as.numeric(
        dbGetQuery(
          conn,
          "SELECT value FROM settings WHERE name = 'last_update'"
        )$value
      )), units = "hours") > 24) {
        color <- "#fc9090" # red
      } else {
        color <- "#50C878" # green
      }
      
      date_gh <- gh(endpoint = "https://api.github.com/repos/ashwinikalantri/anantmuskaan/releases/latest")$published_at
      ver_gh <- gh(endpoint = "https://api.github.com/repos/ashwinikalantri/anantmuskaan/releases/latest")$name
      ver_update <- compareVersion(str_replace(ver_gh, "v", ""), str_replace(ver, "v", ""))
      
      p(
        style = paste0('text-align:center;'),
        tooltip(
          fa(
            name = "circle",
            fill = color,
            prefer_type = "solid"
          ),
          paste0("Data is ", round(
            difftime(Sys.time(), as.POSIXct(as.numeric(
              dbGetQuery(
                conn,
                "SELECT value FROM settings WHERE name = 'last_update'"
              )$value
            )), units = "hours"), 0
          ), " hours old"),
          placement = "top"
        ),
        paste0(
          "Data updated on ",
          format(as.POSIXct(as.numeric(
            dbGetQuery(
              conn,
              "SELECT value FROM settings WHERE name = 'last_update'"
            )$value
          )), "%d %b %Y %I:%M %p"),
          "  "
        ),
        actionLink("updData", label = "Update Data", icon = icon("redo")),
        br(),
        paste0(
          "API Key: ",
          str_replace(api_key(), "(?<=^.{4}).*(?=.{4}$)", " * * * * "),
          "  "
        ),
        actionLink(
          "updateAPI",
          label = "Update API Key",
          icon = icon("rotate")
        ),
        br(),
        if (ver_update == 0) {
          paste0("Anant Muskaan App ",
                 ver,
                 " (dated ",
                 format(as.POSIXct(date_gh), "%d %b %Y"),
                 ")")
        } else if (ver_update == 1) {
          a(
            paste0(
              "Anant Muskaan App ",
              ver,
              " (Updated ",
              ver_gh,
              " dated ",
              format(as.POSIXct(date_gh), "%d %b %Y"),
              " available)"
            ),
            href = "https://github.com/ashwinikalantri/AnantMuskaan/releases/latest"
          )
        } else if (ver_update == -1) {
          paste0(
            "Beta Anant Muskaan App ",
            ver,
            " (Latest ",
            ver_gh,
            " dated ",
            format(as.POSIXct(date_gh), "%d %b %Y"),
            ")"
          )
        },
        br(),
        paste0(
          year(Sys.Date()),
          " © Mahatma Gandhi Institute of Medical Sciences"
        )
      )
    })
    
    observeEvent(input$updateAPI, {
      prompt_for_key()
    })
    
    observeEvent(input$updData, {
      shinyalert::shinyalert(
        title = "Refresh Data",
        type = "info",
        text = paste0(
          "The data is ",
          round(difftime(
            Sys.time(), as.POSIXct(as.numeric(
              dbGetQuery(
                conn,
                "SELECT value FROM settings WHERE name = 'last_update'"
              )$value
            )), units = "hours"
          ), 0),
          " hours old. Do you want to update this data? This may take some time."
        ),
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        showCancelButton = TRUE,
        confirmButtonText = "Refresh",
        confirmButtonCol = "#4CAF50",
        callbackR = function(x) {
          if (x == TRUE)
            source("data_refresh.R", local = TRUE)
        },
        size = "s"
      )
    })
    ## Choices ####
    observe({
      choices_school <- d1 %>%
        filter(block %in% input$block_list) %>%
        filter(area_type %in% input$area_type_list) %>%
        filter(school_type %in% input$school_type_list) %>%
        separate_wider_delim(
          record_id,
          delim = "-",
          names = c("Site", "ID"),
          too_few = "align_start",
          too_many = "merge"
        ) %>%
        split(.$block) %>%
        map(~ deframe(select(., school, ID)))
      updateSelectInput(inputId = "selectSchool", choices = choices_school)
    })
    
    ## Summary: Summary Card Values ####
    card_data <- d1 %>%
      group_by(block) %>%
      count() %>%
      left_join(
        d1 %>%
          group_by(block, area_type) %>%
          count() %>%
          pivot_wider(names_from = area_type, values_from = n),
        by = join_by(block)
      ) %>%
      left_join(
        d1 %>%
          group_by(block, school_type) %>%
          count() %>%
          pivot_wider(names_from = school_type, values_from = n),
        by = join_by(block)
      ) %>%
      left_join(
        d2 %>%
          filter(task_schedule_date == Sys.Date() - 1) %>%
          left_join(d1, by = join_by(record_id)) %>%
          mutate(brush_activity = case_when(
            brush_activity == 1 ~ 1, brush_activity == 2 ~ 0
          )) %>%
          group_by(block) %>%
          summarise(entry = n(), act = sum(brush_activity)),
        by = join_by(block)
      )
    
    output$sch_no <- renderText({
      sum(card_data$n)
    })
    
    output$sch_ur <- renderUI({
      tags$div(style = "display: flex; justify-content: start; gap: 10px; padding: 5px;",
               tags$span(class = "badge bg-info", paste0("Urban: ",sum(card_data$U))),
               tags$span(class = "badge bg-info", paste0("Rural: ",sum(card_data$R)))
      )
    })
    
    output$sch_type <- renderUI({
      tags$div(style = "display: flex; justify-content: start; gap: 10px; padding: 5px;",
               tags$span(class = "badge bg-info", paste0("Gov: ",sum(card_data$GS))),
               tags$span(class = "badge bg-info", paste0("Aided: ",sum(card_data$GA))),
               tags$span(class = "badge bg-info", paste0("Pvt: ",sum(card_data$PS))),
      )
    })
      
    
    #Block 1
    output$block_name_1 <- renderText({
      card_data[1,]$block
    })
    output$sch_no_b1 <- renderText({
      card_data[1,]$n
    })
    
    output$sch_ur_b1 <- renderUI({
      tags$div(style = "display: flex; justify-content: start; gap: 10px; padding: 5px;",
               tags$span(class = "badge bg-success", paste0("Urban: ",card_data[1,]$U)),
               tags$span(class = "badge bg-success", paste0("Rural: ",card_data[1,]$R))
      )
    })
    
    output$sch_type_b1 <- renderUI({
      tags$div(style = "display: flex; justify-content: start; gap: 10px; padding: 5px;",
               tags$span(class = "badge bg-success", paste0("Gov: ",card_data[1,]$GS)),
               tags$span(class = "badge bg-success", paste0("Aided: ",card_data[1,]$GA)),
               tags$span(class = "badge bg-success", paste0("Pvt: ",card_data[1,]$PS)),
      )
    })
    
    
    #Block 2
    output$block_name_2 <- renderText({
      card_data[2,]$block
    })
    output$sch_no_b2 <- renderText({
      card_data[2,]$n
    })
    
    output$sch_ur_b2 <- renderUI({
      tags$div(style = "display: flex; justify-content: start; gap: 10px; padding: 5px;",
               tags$span(class = "badge bg-success", paste0("Urban: ",card_data[2,]$U)),
               tags$span(class = "badge bg-success", paste0("Rural: ",card_data[2,]$R))
      )
    })
    
    output$sch_type_b2 <- renderUI({
      tags$div(style = "display: flex; justify-content: start; gap: 10px; padding: 5px;",
               tags$span(class = "badge bg-success", paste0("Gov: ",card_data[2,]$GS)),
               tags$span(class = "badge bg-success", paste0("Aided: ",card_data[2,]$GA)),
               tags$span(class = "badge bg-success", paste0("Pvt: ",card_data[2,]$PS)),
      )
    })
    
    #Block 3
    output$block_name_3 <- renderText({
      card_data[3,]$block
    })
    output$sch_no_b3 <- renderText({
      card_data[3,]$n
    })
    
    output$sch_ur_b3 <- renderUI({
      tags$div(style = "display: flex; justify-content: start; gap: 10px; padding: 5px;",
               tags$span(class = "badge bg-success", paste0("Urban: ",card_data[3,]$U)),
               tags$span(class = "badge bg-success", paste0("Rural: ",card_data[3,]$R))
      )
    })
    
    output$sch_type_b3 <- renderUI({
      tags$div(style = "display: flex; justify-content: start; gap: 10px; padding: 5px;",
               tags$span(class = "badge bg-success", paste0("Gov: ",card_data[3,]$GS)),
               tags$span(class = "badge bg-success", paste0("Aided: ",card_data[3,]$GA)),
               tags$span(class = "badge bg-success", paste0("Pvt: ",card_data[3,]$PS)),
      )
    })
    
    # Activities and entries
    output$sch_ent_yd <- renderUI({
      tags$div(
        style = "display: flex; justify-content: start; gap: 10px; padding: 5px;",
        tags$span(class = "badge bg-info", paste0("Entries: ", sum(card_data$entry))),
        tags$span(class = "badge bg-info", paste0("Activities: ", sum(card_data$act)))
      )
    })
    
    output$sch_ent_yd_b1 <- renderUI({
      tags$div(
        style = "display: flex; justify-content: start; gap: 10px; padding: 5px;",
        tags$span(class = "badge bg-success", paste0("Entries: ", card_data[1,]$entry)),
        tags$span(class = "badge bg-success", paste0("Activities: ", card_data[1,]$act))
      )
    })
    
    output$sch_ent_yd_b2 <- renderUI({
      tags$div(
        style = "display: flex; justify-content: start; gap: 10px; padding: 5px;",
        tags$span(class = "badge bg-success", paste0("Entries: ", card_data[2,]$entry)),
        tags$span(class = "badge bg-success", paste0("Activities: ", card_data[2,]$act))
      )
    })
    
    output$sch_ent_yd_b3 <- renderUI({
      tags$div(
        style = "display: flex; justify-content: start; gap: 10px; padding: 5px;",
        tags$span(class = "badge bg-success", paste0("Entries: ", card_data[3,]$entry)),
        tags$span(class = "badge bg-success", paste0("Activities: ", card_data[3,]$act))
      )
    })
    
    output$graph_sch_b1 <- renderPlotly({
      
      plot_ly(d2 %>% 
                left_join(d1,by = join_by(record_id)) %>% 
                group_by(block,task_schedule_date) %>% 
                count() %>% 
                filter(block == card_data[1,]$block) %>% 
                select(-block), height = 100) |> 
        add_lines( 
          x = ~task_schedule_date, 
          y = ~n, 
          color = I("#ffffff"), 
          fill = "tozeroy",
          alpha = 0.75
        ) %>% 
        layout( 
          xaxis = list(visible = FALSE, showgrid = FALSE), 
          yaxis = list(visible = FALSE, showgrid = FALSE), 
          hovermode = "x", 
          margin = list(t = 0, r = 0, l = 0, b = 0), 
          paper_bgcolor = "transparent", 
          plot_bgcolor = "transparent" ) %>% 
        config(displayModeBar = F)
    })
    
    output$graph_sch_b2 <- renderPlotly({
      
      plot_ly(d2 %>% 
                left_join(d1,by = join_by(record_id)) %>% 
                group_by(block,task_schedule_date) %>% 
                count() %>% 
                filter(block == card_data[2,]$block) %>% 
                select(-block), height = 100) |> 
        add_lines( 
          x = ~task_schedule_date, 
          y = ~n, 
          color = I("#ffffff"), 
          fill = "tozeroy",
          alpha = 0.75
        ) %>% 
        layout( 
          xaxis = list(visible = FALSE, showgrid = FALSE), 
          yaxis = list(visible = FALSE, showgrid = FALSE), 
          hovermode = "x", 
          margin = list(t = 0, r = 0, l = 0, b = 0), 
          paper_bgcolor = "transparent", 
          plot_bgcolor = "transparent" ) %>% 
        config(displayModeBar = F)
    })
    
    output$graph_sch_b3 <- renderPlotly({
      
      plot_ly(d2 %>% 
                left_join(d1,by = join_by(record_id)) %>% 
                group_by(block,task_schedule_date) %>% 
                count() %>% 
                filter(block == card_data[3,]$block) %>% 
                select(-block), height = 100) |> 
        add_lines( 
          x = ~task_schedule_date, 
          y = ~n, 
          color = I("#ffffff"), 
          fill = "tozeroy",
          alpha = 0.75
        ) %>% 
        layout( 
          xaxis = list(visible = FALSE, showgrid = FALSE), 
          yaxis = list(visible = FALSE, showgrid = FALSE), 
          hovermode = "x", 
          margin = list(t = 0, r = 0, l = 0, b = 0), 
          paper_bgcolor = "transparent", 
          plot_bgcolor = "transparent" ) %>% 
        config(displayModeBar = F)
    })
    
    output$graph_sch <- renderPlotly({
      
      plot_ly(d2 %>% 
                group_by(task_schedule_date) %>% 
                count(), height = 100) |> 
        add_lines( 
          x = ~task_schedule_date, 
          y = ~n, 
          color = I("#ffffff"), 
          fill = "tozeroy",
          alpha = 0.75
        ) %>% 
        layout( 
          xaxis = list(visible = FALSE, showgrid = FALSE), 
          yaxis = list(visible = FALSE, showgrid = FALSE), 
          hovermode = "x", 
          margin = list(t = 0, r = 0, l = 0, b = 0), 
          paper_bgcolor = "transparent", 
          plot_bgcolor = "transparent" ) %>% 
        config(displayModeBar = F)
    })
    
    ## Data: Monthly####
    data_monthly <- reactive({
      d2 %>%
        filter(task_schedule_date >= rollbackward(input$month_date, roll_to_first = TRUE)) %>%
        filter(task_schedule_date <= rollforward(input$month_date)) %>%
        group_by(record_id) %>%
        arrange(record_id,
                desc(task_schedule_date),
                desc(task_start_date)) %>%
        mutate(dup = duplicated(task_schedule_date)) %>%
        filter(dup == FALSE) %>%
        select(record_id,
               task_schedule_date,
               brush_activity,
               participants) %>%
        mutate(brush_activity = case_when(brush_activity == 1 ~ 1, brush_activity == 2 ~ 0)) %>%
        summarise(
          entry = n(),
          participants = sum(as.numeric(participants), na.rm = T),
          activity = sum(brush_activity, na.rm = T)
        ) %>%
        right_join(
          d1 %>%
            dplyr::select(record_id, school, block, area_type, school_type),
          by = join_by(record_id)
        ) %>%
        ungroup() %>%
        filter(block %in% input$block_list) %>%
        filter(area_type %in% input$area_type_list) %>%
        filter(school_type %in% input$school_type_list) %>%
        separate_wider_delim(
          record_id,
          delim = "-",
          names = c("Site", "ID"),
          too_few = "align_start",
          too_many = "merge"
        ) %>%
        mutate(ID = as.numeric(ID)) %>%
        arrange(ID) %>%
        mutate(across(
          .cols = c("entry", "participants", "activity"),
          ~ replace_na(., 0)
        ))
    })
    
    ##Data: School Ent ####
    school_ent_date_range <- reactive({
      d2 %>%
        filter(task_schedule_date >= input$school_range_date[[1]]) %>%
        filter(task_schedule_date <= input$school_range_date[[2]]) %>%
        group_by(record_id) %>%
        arrange(record_id,
                desc(task_schedule_date),
                desc(task_start_date)) %>%
        mutate(dup = duplicated(task_schedule_date)) %>%
        filter(dup == FALSE) %>%
        select(record_id,
               task_schedule_date,
               brush_activity,
               participants) %>%
        mutate(brush_activity = case_when(brush_activity == 1 ~ 1, brush_activity == 2 ~ 0)) %>%
        # summarise(
        #   entry = n(),
        #   participants = sum(as.numeric(participants), na.rm = T),
        #   activity = sum(brush_activity, na.rm = T)
        # ) %>%
        right_join(
          d1 %>%
            dplyr::select(record_id, school, block, area_type, school_type),
          by = join_by(record_id)
        ) %>%
        ungroup() %>%
        filter(block %in% input$block_list) %>%
        filter(area_type %in% input$area_type_list) %>%
        filter(school_type %in% input$school_type_list) %>%
        separate_wider_delim(
          record_id,
          delim = "-",
          names = c("Site", "ID"),
          too_few = "align_start",
          too_many = "merge"
        ) %>%
        mutate(ID = as.numeric(ID)) %>%
        filter(ID == input$selectSchool) %>%
        arrange(ID) %>%
        mutate(across(
          .cols = c("participants", "brush_activity"),
          ~ replace_na(., 0)
        ))
    })
    
    ##Data: Date Range ####
    data_date_range <- reactive({
      d2 %>%
        filter(task_schedule_date >= input$range_date[[1]]) %>%
        filter(task_schedule_date <= input$range_date[[2]]) %>%
        group_by(record_id) %>%
        arrange(record_id,
                desc(task_schedule_date),
                desc(task_start_date)) %>%
        mutate(dup = duplicated(task_schedule_date)) %>%
        filter(dup == FALSE) %>%
        select(record_id,
               task_schedule_date,
               brush_activity,
               participants) %>%
        mutate(brush_activity = case_when(brush_activity == 1 ~ 1, brush_activity == 2 ~ 0)) %>%
        summarise(
          entry = n(),
          participants = sum(as.numeric(participants), na.rm = T),
          activity = sum(brush_activity, na.rm = T)
        ) %>%
        right_join(
          d1 %>%
            dplyr::select(record_id, school, block, area_type, school_type),
          by = join_by(record_id)
        ) %>%
        ungroup() %>%
        filter(block %in% input$block_list) %>%
        filter(area_type %in% input$area_type_list) %>%
        filter(school_type %in% input$school_type_list) %>%
        separate_wider_delim(
          record_id,
          delim = "-",
          names = c("Site", "ID"),
          too_few = "align_start",
          too_many = "merge"
        ) %>%
        mutate(ID = as.numeric(ID)) %>%
        arrange(ID) %>%
        mutate(across(
          .cols = c("entry", "participants", "activity"),
          ~ replace_na(., 0)
        ))
    })
    
    ## Table: Single Date ####
    output$daily_table <- render_gt({
      validate(
        need(input$block_list, "Please select at least one Block."),
        need(
          input$area_type_list,
          "Please select at least one Area type."
        ),
        need(
          input$school_type_list,
          "Please select at least one School type."
        )
      )
      
      groups <- expand.grid(input$area_type_list, input$school_type_list) %>%
        mutate(groups = paste0(Var1, "_", Var2)) %>%
        .$groups
      
      day_data <- d2 %>%
        filter(task_schedule_date == input$daily_date) %>%
        group_by(record_id) %>%
        select(record_id, task_schedule_date) %>%
        unique() %>%
        count(name = "Visits") %>%
        right_join(
          d1 %>%
            dplyr::select(record_id, school, block, area_type, school_type),
          by = join_by(record_id)
        ) %>%
        ungroup() %>%
        filter(block %in% input$block_list) %>%
        filter(area_type %in% input$area_type_list) %>%
        filter(school_type %in% input$school_type_list) %>%
        separate_wider_delim(
          record_id,
          delim = "-",
          names = c("Site", "ID"),
          too_few = "align_start",
          too_many = "merge"
        ) %>%
        mutate(ID = as.numeric(ID)) %>%
        arrange(ID) %>%
        group_by(Visits, block, area_type, school_type) %>%
        count(name = "Schools") %>%
        ungroup() %>%
        pivot_wider(names_from = c(area_type, school_type),
                    values_from = Schools) %>%
        group_by(block) %>%
        rowwise() %>%
        mutate(Total = sum(c_across(any_of(groups)), na.rm = T)) %>%
        group_by(block) %>%
        mutate(Visits = case_when(is.na(Visits) ~ 3, .default = Visits)) %>%
        group_by(block) %>%
        mutate(across(everything(), ~ replace_na(., 0))) %>%
        reframe(per = across(any_of(c(
          groups, "Visits"
        )), ~ . / sum(.)),
        n = across(any_of(c(
          groups, "Visits"
        )), ~ .),
        ) %>%
        filter(per$Visits == .25)
      
      if (nrow(day_data) > 0) {
        day_data <- day_data %>%
          select(block) %>%
          bind_cols(day_data %>%
                      .$n %>%
                      select(-Visits)) %>%
          bind_cols(day_data %>%
                      .$per %>%
                      select(-Visits) %>%
                      rename_with(~ paste0(.x, "_P"), everything()))
        
        
        daily_table <- day_data %>%
          gt(rowname_col = "block") %>%
          tab_header(
            title = paste0("Anant Muskan MyCap"),
            subtitle = paste0(
              "No of Schools with entries for ",
              format(input$daily_date, "%d %b")
            )
          ) %>%
          sub_missing(missing_text = "0") %>%
          tab_spanner(columns = starts_with("R_"), label = "Rural") %>%
          tab_spanner(columns = starts_with("U_"), label = "Urban") %>%
          fmt_percent(columns = ends_with("_P")) %>%
          cols_label(
            ends_with("_GA") ~ "Gov Aided",
            ends_with("_GS") ~ "Gov School",
            ends_with("_PS") ~ "Pvt School"
          ) %>%
          tab_style(style = cell_text(weight = "bold", align = "center"),
                    locations = cells_row_groups()) %>%
          cols_align(align = "center", columns = everything()) %>%
          tab_options(
            table.width = pct(95),
            heading.border.bottom.color = "black",
            column_labels.font.size = "12px",
            column_labels.font.weight = "bold",
            column_labels.border.bottom.color = "black",
            grand_summary_row.border.color = "black",
            row_group.border.top.color = "black",
            row_group.border.bottom.color = "black",
            stub.border.color = "black"
          ) %>%
          opt_table_outline(color = "black") %>%
          tab_style(
            style = cell_borders(
              sides = c("top", "bottom"),
              color = "black",
              weight = px(1.5),
              style = "solid"
            ),
            locations = list(cells_body(), cells_stub())
          ) %>%
          cols_width(everything() ~ pct(14))
        
        for (i in names(day_data)[names(day_data) %in% groups]) {
          daily_table <- daily_table %>%
            cols_merge(columns = starts_with(i), pattern = "{1} ({2})")
        }
      } else {
        daily_table <- data.frame() %>%
          gt()
      }
      
      daily_table
    })
    
    ## Table: Range Entries####
    output$range_ent_table <- render_gt({
      validate(
        need(input$block_list, "Please select at least one Block."),
        need(
          input$area_type_list,
          "Please select at least one Area type."
        ),
        need(
          input$school_type_list,
          "Please select at least one School type."
        )
      )
      if (nrow(data_date_range()) > 0) {
        groups_range <- expand.grid(
          input$area_type_list,
          input$school_type_list,
          c("entry", "participants", "activity")
        ) %>%
          mutate(groups = paste0(Var1, "_", Var2, "_", Var3)) %>%
          .$groups
        
        range_data <- data_date_range() %>%
          mutate(lab = "entry") %>%
          group_by(lab, block, area_type, school_type, entry) %>%
          count() %>%
          pivot_wider(
            names_from = c(area_type, school_type, lab),
            values_from = n
          ) %>%
          group_by(block) %>%
          mutate(across(everything(), ~ replace_na(., 0))) %>%
          reframe(
            per = across(any_of(groups_range), ~ . / sum(.)),
            n = across(any_of(groups_range), ~ .),
            entry = entry
          )
        
        range_data <- range_data %>%
          select(block, entry) %>%
          bind_cols(range_data %>% .$n) %>%
          bind_cols(range_data %>% .$per %>%
                      rename_with(~ paste0(.x, "_P"), everything()))
        
        
        range_table <- range_data %>%
          arrange(entry) %>%
          group_by(block) %>%
          gt(rowname_col = "entry") %>%
          tab_header(
            title = paste0("Anant Muskan MyCap "),
            subtitle = paste0(
              "Entries done from ",
              format(input$range_date[[1]], "%d %b"),
              " to ",
              format(input$range_date[[2]], "%d %b")
            )
          ) %>%
          tab_spanner(columns = starts_with("R_"), label = "Rural") %>%
          tab_spanner(columns = starts_with("U_"), label = "Urbal") %>%
          sub_missing(missing_text = 0) %>%
          fmt_percent(columns = ends_with("_P")) %>%
          tab_style(style = cell_text(weight = "bold", align = "center"),
                    locations = cells_row_groups()) %>%
          cols_align(align = "center", columns = everything()) %>%
          cols_label(
            contains("_GA") ~ "Gov Aided",
            contains("_GS") ~ "Gov School",
            contains("_PS") ~ "Pvt School"
          ) %>%
          tab_options(
            table.width = pct(95),
            heading.border.bottom.color = "black",
            column_labels.font.size = "12px",
            column_labels.font.weight = "bold",
            column_labels.border.bottom.color = "black",
            grand_summary_row.border.color = "black",
            row_group.border.top.color = "black",
            row_group.border.bottom.color = "black",
            stub.border.color = "black"
          ) %>%
          opt_table_outline(color = "black") %>%
          tab_style(
            style = cell_borders(
              sides = c("top", "bottom"),
              color = "black",
              weight = px(1.5),
              style = "solid"
            ),
            locations = list(cells_body(), cells_stub())
          ) %>%
          cols_width(contains("_") ~ pct(15))
        
        for (i in names(range_data)[names(range_data) %in% groups_range]) {
          range_table <- range_table %>%
            cols_merge(columns = starts_with(i), pattern = "{1} ({2})")
        }
      } else {
        range_table <- data.frame() %>% gt()
      }
      range_table
    })
    
    ## Table: Range Activities ####
    output$range_act_table <- render_gt({
      validate(
        need(input$block_list, "Please select at least one Block."),
        need(
          input$area_type_list,
          "Please select at least one Area type."
        ),
        need(
          input$school_type_list,
          "Please select at least one School type."
        )
      )
      
      if (nrow(data_date_range()) > 0) {
        groups_range <- expand.grid(
          input$area_type_list,
          input$school_type_list,
          c("entry", "participants", "activity")
        ) %>%
          mutate(groups = paste0(Var1, "_", Var2, "_", Var3)) %>%
          .$groups
        
        range_data_act = data_date_range() %>%
          mutate(lab = "activity") %>%
          group_by(lab, block, area_type, school_type, activity) %>%
          count() %>%
          pivot_wider(
            names_from = c(area_type, school_type, lab),
            values_from = n
          ) %>%
          full_join(
            data_date_range() %>%
              mutate(lab = "participants") %>%
              group_by(lab, block, area_type, school_type, activity) %>%
              summarise(part = sum(participants, na.rm = T)) %>%
              pivot_wider(
                names_from = c(area_type, school_type, lab),
                values_from = part
              ),
            by = join_by(block, activity)
          ) %>%
          group_by(block) %>%
          arrange(activity) %>%
          mutate(across(everything(), ~ replace_na(., 0))) %>%
          reframe(
            per = across(any_of(groups_range), ~ . / sum(.)),
            n = across(any_of(groups_range), ~ .),
            activity = activity
          )
        
        range_data_act <- range_data_act %>%
          select(block, activity) %>%
          bind_cols(range_data_act %>% .$n) %>%
          bind_cols(range_data_act %>% .$per %>%
                      rename_with(~ paste0(.x, "_P"), everything())) %>%
          relocate(ends_with("_activity"), .after = last_col()) %>%
          relocate(ends_with("_activity_P"), .after = last_col()) %>%
          relocate(ends_with("_participants"), .after = last_col()) %>%
          relocate(ends_with("_participants_P"), .after = last_col())
        
        range_act_table <- range_data_act %>%
          arrange(activity) %>%
          group_by(block) %>%
          gt(rowname_col = "activity") %>%
          tab_header(
            title = paste0("Anant Muskan MyCap "),
            subtitle = paste0(
              "Activities conducted from ",
              format(input$range_date[[1]], "%d %b"),
              " to ",
              format(input$range_date[[2]], "%d %b")
            )
          ) %>%
          tab_spanner(columns = starts_with("R_"), label = "Rural") %>%
          tab_spanner(columns = starts_with("U_"), label = "Urbal") %>%
          fmt_percent(columns = ends_with("_P")) %>%
          sub_missing(missing_text = 0) %>%
          cols_label(
            contains("_GA") ~ "Gov Aided",
            contains("_GS") ~ "Gov School",
            contains("_PS") ~ "Pvt School"
          ) %>%
          tab_style(style = cell_text(weight = "bold", align = "center"),
                    locations = cells_row_groups()) %>%
          cols_align(align = "center", columns = everything()) %>%
          tab_options(
            table.width = pct(95),
            heading.border.bottom.color = "black",
            column_labels.font.size = "12px",
            column_labels.font.weight = "bold",
            column_labels.border.bottom.color = "black",
            grand_summary_row.border.color = "black",
            row_group.border.top.color = "black",
            row_group.border.bottom.color = "black",
            stub.border.color = "black"
          ) %>%
          opt_table_outline(color = "black") %>%
          tab_style(
            style = cell_borders(
              sides = c("top", "bottom"),
              color = "black",
              weight = px(1.5),
              style = "solid"
            ),
            locations = list(cells_body(), cells_stub())
          ) %>%
          cols_width(contains("_") ~ pct(15))
        
        cols <- word(
          names(range_data_act)[names(range_data_act) %in% groups_range],
          start = 1,
          end = -2,
          sep = "_"
        ) %>% unique()
        
        for (i in cols) {
          range_act_table <- range_act_table %>%
            cols_merge(columns = starts_with(i), pattern = "{1} ({2})<br><small>{3} Participants ({4})</small>")
        }
      } else {
        range_act_table <- data.frame() %>% gt()
      }
      
      range_act_table
    })
    
    ## Table: School Entries ####
    output$school_ent_table <- renderDT({
      validate(
        need(input$block_list, "Please select at least one Block."),
        need(
          input$area_type_list,
          "Please select at least one Area type."
        ),
        need(
          input$school_type_list,
          "Please select at least one School type."
        )
      )
      
      
      if (nrow(school_ent_date_range()) > 0) {
        school_ent_table <- school_ent_date_range() %>%
          select(task_schedule_date, brush_activity) %>%
          mutate(brush_activity = case_when(
            brush_activity == 0 ~ "Not done",
            brush_activity == 1 ~ "Done"
          )) %>%
          mutate(task_schedule_date = format(task_schedule_date, "%d %b %Y")) %>%
          #group_by(block) %>%
          datatable(
            rownames = FALSE,
            colnames = c('Date', "Activity"),
            extensions = 'Buttons',
            options = list(dom = 'Bfrtip', buttons = list(
              c('copy'),
              list(extend = "print", title = "Anant Muskaan - Schools with Activities"),
              list(
                extend = 'collection',
                buttons = list(
                  list(
                    extend = "pdf",
                    title = "Anant Muskaan - Schools with Activities",
                    filename = "AM_no_act"
                  ),
                  list(extend = "excel", filename = "AM_act")
                ),
                text = 'Download'
              )
            ))
          )
      } else {
        school_ent_table <- data.frame(Message = "No Data") %>% datatable()
      }
      school_ent_table
    }, server = FALSE)
    
    ## Table: School Entry Cards ####
    output$sch_ent <- renderText({
      validate(
        need(input$block_list, "Please select at least one Block."),
        need(
          input$area_type_list,
          "Please select at least one Area type."
        ),
        need(
          input$school_type_list,
          "Please select at least one School type."
        )
      )
      
      if (nrow(school_ent_date_range()) > 0) {
        sch_ent_val <- school_ent_date_range() %>%
          filter(!is.na(task_schedule_date)) %>% 
          summarise(N = n()) %>% .$N
      } else {
        sch_ent_val <- 0
      }
      
      sch_ent_val
    
  })
  
    
    output$sch_act <- renderText({
      validate(
        need(input$block_list, "Please select at least one Block."),
        need(
          input$area_type_list,
          "Please select at least one Area type."
        ),
        need(
          input$school_type_list,
          "Please select at least one School type."
        )
      )
      
      if (nrow(school_ent_date_range()) > 0) {
        sch_act_val <- school_ent_date_range() %>%
          summarise(N = sum(brush_activity)) %>% .$N
      } else {
        sch_act_val <- 0
      }
      
      sch_act_val
      
    })
  
    ## Table: Range No Entries ####
  output$range_noent_table <- renderDT({
    validate(
      need(input$block_list, "Please select at least one Block."),
      need(input$area_type_list, "Please select at least one Area type."),
      need(
        input$school_type_list,
        "Please select at least one School type."
      )
    )
    
    
    if (nrow(data_date_range()) > 0) {
      no_ent_table <- data_date_range() %>%
        filter(activity == 0) %>%
        select(ID, block, school, entry) %>%
        group_by(block) %>%
        datatable(
          rownames = FALSE,
          colnames = c('ID', "Block", 'School', 'Entries'),
          extensions = 'Buttons',
          options = list(dom = 'Bfrtip', buttons = list(
            c('copy'),
            list(extend = "print", title = "Anant Muskaan - Schools with no Activities"),
            list(
              extend = 'collection',
              buttons = list(
                list(
                  extend = "pdf",
                  title = "Anant Muskaan - Schools with no Activities",
                  filename = "AM_no_act"
                ),
                list(extend = "excel", filename = "AM_no_act")
              ),
              text = 'Download'
            )
          ))
        )
    } else {
      no_ent_table <- data.frame(Message = "No Data") %>%
        datatable()
    }
    no_ent_table
  }, server = FALSE)
  
    ## Table: Range No Entries ####
    output$range_school_ent_table <- renderDT({
      validate(
        need(input$block_list, "Please select at least one Block."),
        need(input$area_type_list, "Please select at least one Area type."),
        need(
          input$school_type_list,
          "Please select at least one School type."
        )
      )
      
      
      if (nrow(data_date_range()) > 0) {
        ent_table <- data_date_range() %>%
          filter(entry != 0) %>%
          select(ID, block, school,activity, entry) %>%
          group_by(block) %>%
          datatable(
            rownames = FALSE,
            colnames = c('ID', "Block", 'School', "Activity",'Entries'),
            extensions = 'Buttons',
            options = list(dom = 'Bfrtip', buttons = list(
              c('copy'),
              list(extend = "print", title = "Anant Muskaan - Schools with Entries"),
              list(
                extend = 'collection',
                buttons = list(
                  list(
                    extend = "pdf",
                    title = "Anant Muskaan - Schools with Entries",
                    filename = "AM_ent"
                  ),
                  list(extend = "excel", filename = "AM_ent")
                ),
                text = 'Download'
              )
            ))
          )
      } else {
        ent_table <- data.frame(Message = "No Data") %>%
          datatable()
      }
      ent_table
    }, server = FALSE)
    
  ## Table: Monthly ####
  output$monthly_table <- render_gt({
    validate(
      need(input$block_list, "Please select at least one Block."),
      need(input$area_type_list, "Please select at least one Area type."),
      need(
        input$school_type_list,
        "Please select at least one School type."
      )
    )
    if (nrow(data_monthly()) > 0) {
      groups_range_m <- expand.grid(
        c("entry", "participants", "activity"),
        input$area_type_list,
        input$school_type_list
      ) %>%
        mutate(groups = paste0(Var1, "_", Var2, "_", Var3)) %>%
        .$groups
      
      month_data <- data_monthly() %>%
        group_by(block, area_type, school_type) %>%
        summarise(
          entry = sum(entry, na.rm = T),
          participants = sum(participants, na.rm = T),
          activity = sum(activity, na.rm = T)
        ) %>%
        pivot_wider(
          names_from = c(area_type, school_type),
          values_from = c(entry, participants, activity)
        ) %>%
        ungroup() %>%
        relocate(starts_with("entry_"), .after = last_col()) %>%
        relocate(starts_with("activity_"), .after = last_col()) %>%
        relocate(starts_with("participants_"), .after = last_col())
      
      month_table <- month_data %>%
        gt(rowname_col = "block") %>%
        tab_spanner(columns = contains("R_"), label = "Rural") %>%
        tab_spanner(columns = contains("U_"), label = "Urbal") %>%
        cols_label(
          ends_with("_GA") ~ "Gov Aided",
          ends_with("_GS") ~ "Gov School",
          ends_with("_PS") ~ "Pvt School"
        ) %>%
        sub_missing(missing_text = "0") %>%
        tab_options(
          table.width = pct(95),
          heading.border.bottom.color = "black",
          column_labels.font.size = "12px",
          column_labels.font.weight = "bold",
          column_labels.border.bottom.color = "black",
          grand_summary_row.border.color = "black",
          row_group.border.top.color = "black",
          row_group.border.bottom.color = "black",
          stub.border.color = "black"
        ) %>%
        opt_table_outline(color = "black") %>%
        tab_style(
          style = cell_borders(
            sides = c("top", "bottom"),
            color = "black",
            weight = px(1.5),
            style = "solid"
          ),
          locations = list(cells_body(), cells_stub())
        ) %>%
        cols_width(block ~ pct(15), contains("_") ~ pct(15)) %>%
        tab_header(
          title = paste0("Anant Muskan MyCap"),
          subtitle = paste0(
            "No of Entries, Activities conducted and Students for ",
            format(input$month_date, "%b %Y")
          )
        ) %>%
        tab_style(style = cell_text(weight = "bold", align = "center"),
                  locations = cells_row_groups()) %>%
        cols_align(align = "center", columns = everything())
      
      for (i in unique(word(
        names(month_data)[names(month_data) %in% groups_range_m],
        start = -2,
        end = -1,
        sep = "_"
      ))) {
        month_table <- month_table %>%
          cols_merge(columns = ends_with(i), pattern = "Entered: {1}<br>Activities: {2} <br> Students: {3}")
      }
    } else {
      month_table <- data.frame() %>% gt()
    }
    
    month_table
  })
})
}

shinyApp(ui = ui, server = server)