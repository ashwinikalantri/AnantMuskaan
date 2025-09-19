library(shiny)
library(bslib)
library(shinyWidgets)
library(gt)
library(dplyr)
library(tidyr)
library(lubridate)
library(RSQLite)
library(stringr)

ui <- page_fluid(
  titlePanel("Anant Muskaan"),
  card(layout_columns(
    card(checkboxGroupInput("block_list", "Select Block", choices = NA)),
    card(
      checkboxGroupInput("area_type_list", "Select Area", choices = NA)
    ),
    card(
      checkboxGroupInput("school_type_list", "Select School Type", choices = NA)
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
        nav_panel("No Entries", gt_output("range_noent_table"))
      )
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
    card_footer(uiOutput("updateData"))
  ),
  id = "tab"
)

server <- function(input, output, session) {
  source("read_data.R")
  
  ## Update inputs ####
  observe({
    updateCheckboxGroupInput(
      session,
      "block_list",
      choices = unique(d1$block),
      selected = unique(d1$block)
    )
    
    updateCheckboxGroupInput(
      session,
      "area_type_list",
      choices = unique(d1$area_type),
      selected = unique(d1$area_type)
    )
    
    updateCheckboxGroupInput(
      session,
      "school_type_list",
      choices = unique(d1$school_type),
      selected = unique(d1$school_type)
    )
    
  })
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
      "<br>",
      year(Sys.Date()),
      " © Mahatma Gandhi Institute of Medical Sciences</p>"
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
      need(input$area_type_list, "Please select at least one Area type."),
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
        )
      
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
      need(input$area_type_list, "Please select at least one Area type."),
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
        pivot_wider(names_from = c(area_type, school_type, lab),
                    values_from = n) %>%
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
        # cols_merge(columns = c(R_GA_entry, R_GA_P), pattern = "{1} <br><small>{2}</small>") %>%
        # cols_merge(columns = c(R_GS_entry, R_GS_P), pattern = "{1} <br><small>{2}</small>") %>%
        # cols_merge(columns = c(R_PS_entry, R_PS_P), pattern = "{1} <br><small>{2}</small>") %>%
        # cols_merge(columns = c(U_GA_entry, U_GA_P), pattern = "{1} <br><small>{2}</small>") %>%
        # cols_merge(columns = c(U_GS_entry, U_GS_P), pattern = "{1} <br><small>{2}</small>") %>%
        # cols_merge(columns = c(U_PS_entry, U_PS_P), pattern = "{1} <br><small>{2}</small>") %>%
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
      need(input$area_type_list, "Please select at least one Area type."),
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
                    rename_with(~ paste0(.x, "_P"), everything()))
      
      range_act_table <- range_data_act %>%
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
      
      for (i in names(range_data_act)[names(range_data_act) %in% groups_range]) {
        range_act_table <- range_act_table %>%
          cols_merge(columns = starts_with(word(
            i,
            start = 1,
            end = -2,
            sep = "_"
          )), pattern = "{1} ({3})<br><small>{2} Participants</small>")
      }
    } else {
      range_act_table <- data.frame() %>% gt()
    }
    
    range_act_table
  })
  
  ## Table: Range No Entries ####
  output$range_noent_table <- render_gt({
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
    } else {
      no_ent_table <- data.frame() %>% gt()
    }
    no_ent_table
  })
  
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
        ungroup()
      
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
  
}

shinyApp(ui = ui, server = server)
