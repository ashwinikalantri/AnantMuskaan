library(shiny)
library(bslib)
library(shinyWidgets)
library(gt)
library(dplyr)
library(tidyr)
library(lubridate)
library(RSQLite)

ui <- page_fluid(
  titlePanel("Anant Muskaan"),
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
        nav_panel("No Entries", gt_output("range_noent_table"))
      )
    ),
    nav_panel(
      "Monthly Data",
      card(
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
      ),
      card(gt_output("monthly_table"))
    ),
    card_footer(
        uiOutput("updateData")
      )
  ),
  id = "tab"
)

server <- function(input, output) {
  
  source("read_data.R")
  
  output$updateData <- renderText({
    paste0(
      "<p style='text-align:center;'>",
      "Data available till ",
      format(max(d2$task_schedule_date, na.rm = T), "%d %B %Y"),
      if (round(difftime(
        Sys.Date() - 1, max(d2$task_schedule_date, na.rm = T), units = "days"
      ), 0) > 0) {
        actionLink("updData", label = "", icon = icon("redo"))
      },
      "<br>",year(Sys.Date())," © Mahatma Gandhi Institute of Medical Sciences</p>"
    )
  })
  
  observeEvent(input$updData, {
    shinyalert::shinyalert(
      title = "Refresh Data",
      type = "info",
      text = paste0(
        "The data is ",
        round(difftime(
          Sys.Date() - 1, max(d2$task_schedule_date, na.rm = T), units = "days"
        ), 0),
        " days old. Do you want to update this data? This may take some time."
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
  
  ## Monthly Data####
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
      select(
        record_id,
        task_schedule_date,
        brush_activity,
        participants
      ) %>%
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
  
  ##Date range data ####
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
      select(
        record_id,
        task_schedule_date,
        brush_activity,
        participants
      ) %>%
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
  ## Single Date ####
  output$daily_table <- render_gt({
    d2 %>%
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
      mutate(Total = sum(c(R_GA, R_GS, U_GS, U_PS, R_PS, U_GA), na.rm = T)) %>%
      group_by(block) %>%
      mutate(
        R_GA_P = R_GA / sum(R_GA, na.rm = T),
        R_GS_P = R_GS / sum(R_GS, na.rm = T),
        R_PS_P = R_PS / sum(R_PS, na.rm = T),
        U_GA_P = U_GA / sum(U_GA, na.rm = T),
        U_GS_P = U_GS / sum(U_GS, na.rm = T),
        U_PS_P = U_PS / sum(U_PS, na.rm = T),
      ) %>%
      filter(!is.na(Visits)) %>%
      select(-Visits) %>%
      ungroup() %>%
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
      cols_merge_n_pct(col_n = R_GA, col_pct = R_GA_P) %>%
      cols_merge_n_pct(col_n = R_GS, col_pct = R_GS_P) %>%
      cols_merge_n_pct(col_n = R_PS, col_pct = R_PS_P) %>%
      cols_merge_n_pct(col_n = U_GA, col_pct = U_GA_P) %>%
      cols_merge_n_pct(col_n = U_GS, col_pct = U_GS_P) %>%
      cols_merge_n_pct(col_n = U_PS, col_pct = U_PS_P) %>%
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
      )
  })
  
  ## Range Entries####
  
  output$range_ent_table <- render_gt({
    data_date_range() %>%
      mutate(lab = "entry") %>%
      group_by(lab, block, area_type, school_type, entry) %>%
      count() %>%
      pivot_wider(names_from = c(area_type, school_type, lab),
                  values_from = n) %>%
      group_by(block) %>%
      mutate(
        R_GA_P = R_GA_entry / sum(R_GA_entry, na.rm = T),
        R_GS_P = R_GS_entry / sum(R_GS_entry, na.rm = T),
        R_PS_P = R_PS_entry / sum(R_PS_entry, na.rm = T),
        U_GA_P = U_GA_entry / sum(U_GA_entry, na.rm = T),
        U_GS_P = U_GS_entry / sum(U_GS_entry, na.rm = T),
        U_PS_P = U_PS_entry / sum(U_PS_entry, na.rm = T)
      ) %>%
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
      cols_merge(columns = c(R_GA_entry, R_GA_P),
                 pattern = "{1} <br><small>{2}</small>") %>%
      cols_merge(columns = c(R_GS_entry, R_GS_P),
                 pattern = "{1} <br><small>{2}</small>") %>%
      cols_merge(columns = c(R_PS_entry, R_PS_P),
                 pattern = "{1} <br><small>{2}</small>") %>%
      cols_merge(columns = c(U_GA_entry, U_GA_P),
                 pattern = "{1} <br><small>{2}</small>") %>%
      cols_merge(columns = c(U_GS_entry, U_GS_P),
                 pattern = "{1} <br><small>{2}</small>") %>%
      cols_merge(columns = c(U_PS_entry, U_PS_P),
                 pattern = "{1} <br><small>{2}</small>") %>%
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
  })
  
  ## Range Activities ####
  output$range_act_table <- render_gt({
    data_date_range() %>%
      mutate(lab = "activity") %>%
      group_by(lab, block, area_type, school_type, activity) %>%
      count() %>%
      pivot_wider(names_from = c(area_type, school_type, lab),
                  values_from = n) %>%
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
      mutate(
        R_GA_P = R_GA_activity / sum(R_GA_activity, na.rm = T),
        R_GS_P = R_GS_activity / sum(R_GS_activity, na.rm = T),
        R_PS_P = R_PS_activity / sum(R_PS_activity, na.rm = T),
        U_GA_P = U_GA_activity / sum(U_GA_activity, na.rm = T),
        U_GS_P = U_GS_activity / sum(U_GS_activity, na.rm = T),
        U_PS_P = U_PS_activity / sum(U_PS_activity, na.rm = T)
      ) %>%
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
      cols_merge(
        columns = c(R_GA_activity, R_GA_participants, R_GA_P),
        pattern = "{1} ({3})<br><small>{2} Participants</small>"
      ) %>%
      cols_merge(
        columns = c(R_GS_activity, R_GS_participants, R_GS_P),
        pattern = "{1} ({3})<br><small>{2} Participants</small>"
      ) %>%
      cols_merge(
        columns = c(R_PS_activity, R_PS_participants, R_PS_P),
        pattern = "{1} ({3})<br><small>{2} Participants</small>"
      ) %>%
      cols_merge(
        columns = c(U_GA_activity, U_GA_participants, U_GA_P),
        pattern = "{1} ({3})<br><small>{2} Participants</small>"
      ) %>%
      cols_merge(
        columns = c(U_GS_activity, U_GS_participants, U_GS_P),
        pattern = "{1} ({3})<br><small>{2} Participants</small>"
      ) %>%
      cols_merge(
        columns = c(U_PS_activity, U_PS_participants, U_PS_P),
        pattern = "{1} ({3})<br><small>{2} Participants</small>"
      ) %>%
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
  })
  
  ## Range No Entries ####
  output$range_noent_table <- render_gt({
    data_date_range() %>%
      filter(activity == 0) %>%
      select(ID, block, school, entry) %>%
      group_by(block) %>%
      gt(rowname_col = "ID") %>%
      cols_label(block = "Block",
                 school = "School",
                 entry = "No of Entries") %>%
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
      tab_header(
        title = paste0("Anant Muskan MyCap"),
        subtitle = paste0(
          "Schools with No Activities conducted from ",
          format(input$range_date[[1]], "%d %b"),
          " to ",
          format(input$range_date[[2]], "%d %b")
        )
      )
  })
  
  ## Monthly ####
  output$monthly_table <- render_gt({
    data_monthly() %>%
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
      gt(rowname_col = "block") %>%
      tab_spanner(columns = contains("R_"), label = "Rural") %>%
      tab_spanner(columns = contains("U_"), label = "Urbal") %>%
      cols_label(
        ends_with("_GA") ~ "Gov Aided",
        ends_with("_GS") ~ "Gov School",
        ends_with("_PS") ~ "Pvt School"
      ) %>%
      cols_merge(
        columns = c(entry_R_GA, activity_R_GA, participants_R_GA),
        pattern = "Entered: {1}<br>Activities: {2} <br> Students: {3}"
      ) %>%
      cols_merge(
        columns = c(entry_R_GS, activity_R_GS, participants_R_GS),
        pattern = "Entered: {1}<br>Activities: {2} <br> Students: {3}"
      ) %>%
      cols_merge(
        columns = c(entry_R_PS, activity_R_PS, participants_R_PS),
        pattern = "Entered: {1}<br>Activities: {2} <br> Students: {3}"
      ) %>%
      cols_merge(
        columns = c(entry_U_GA, activity_U_GA, participants_U_GA),
        pattern = "Entered: {1}<br>Activities: {2} <br> Students: {3}"
      ) %>%
      cols_merge(
        columns = c(entry_U_GS, activity_U_GS, participants_U_GS),
        pattern = "Entered: {1}<br>Activities: {2} <br> Students: {3}"
      ) %>%
      cols_merge(
        columns = c(entry_U_PS, activity_U_PS, participants_U_PS),
        pattern = "Entered: {1}<br>Activities: {2} <br> Students: {3}"
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
      cols_width(contains("_") ~ pct(15)) %>%
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
  })
  
}

shinyApp(ui = ui, server = server)
