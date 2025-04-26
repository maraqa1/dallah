# Kanban Board Module UI
kanbanBoardUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(3, tags$h4("Not Started", class = "text-center"), uiOutput(ns("notStartedColumn"))),
      column(3, tags$h4("In Progress", class = "text-center"), uiOutput(ns("inProgressColumn"))),
      column(3, tags$h4("Complete", class = "text-center"), uiOutput(ns("completeColumn"))),
      column(3, tags$h4("Blocked", class = "text-center"), uiOutput(ns("blockedColumn")))
    )
  )
}

tags$h4("Not Started", style = "text-align: center; font-weight: bold;")


# Kanban Board Module Server
kanbanBoardServer <- function(input, output, session, tasks) {
  ns <- session$ns
  library(lubridate)
  library(dplyr)
  
  normalize_status <- function(status) {
    status <- tolower(trimws(status))
    dplyr::case_when(
      status %in% c("blocked") ~ "Blocked",
      status %in% c("complete", "completed", "done") ~ "Complete",
      status %in% c("in progress", "ongoing", "started", "working") ~ "In Progress",
      status %in% c("not started", "to do", "todo") ~ "Not Started",
      TRUE ~ "Not Started"  # fallback
    )
  }
  
  # Safely parse any date format
  safe_parse_date <- function(x) {
    x <- as.character(x)
    parsed <- suppressWarnings(mdy(x))
    if (is.na(parsed)) {
      parsed <- suppressWarnings(as.Date(x, tryFormats = c("%Y-%m-%d", "%d/%m/%Y", "%m/%d/%Y")))
    }
    parsed
  }
  
  # Render a single Kanban column with sorting by Health → Schedule → Progress
  renderTaskColumn <- function(status) {
    data <- tasks()
    if (nrow(data) == 0) return(h4("No tasks available."))
    
    data$Status <- normalize_status(data$Status)
    filtered <- data[!is.na(data$Status) & data$Status == status, ]
    
    if (nrow(filtered) == 0) return(h4(paste("No tasks in", status, "column.")))
    
    # --- Schedule Health Calculation (for sorting) ---
    filtered$schedule_health <- sapply(filtered$`Start Date`, function(start_date) {
      parsed_date <- safe_parse_date(start_date)
      if (is.na(parsed_date)) return("Unknown")
      days <- as.numeric(difftime(parsed_date, Sys.Date(), units = "days"))
      if (days < 0) return("Overdue")
      if (days < 7) return("At Risk")
      return("On Track")
    })
    
    # --- Sorting Priorities ---
    filtered <- filtered %>%
      mutate(
        Health_Score = dplyr::case_when(
          Health == "Red" ~ 1,
          Health == "Amber" ~ 2,
          Health == "Green" ~ 3,
          Health == "Gray" ~ 4,
          TRUE ~ 5
        ),
        Schedule_Score = dplyr::case_when(
          schedule_health == "Overdue" ~ 1,
          schedule_health == "At Risk" ~ 2,
          schedule_health == "On Track" ~ 3,
          TRUE ~ 4
        ),
        Progress_Score = ifelse(is.na(`% Complete`), 1, 1 - `% Complete`)
      ) %>%
      arrange(Health_Score, Schedule_Score, Progress_Score)
    
    # --- Render task cards ---
    tagList(lapply(seq_len(nrow(filtered)), function(i) {
      task <- filtered[i, ]
      
      # Fields (with fallback defaults)
      task_name <- ifelse(!is.na(task$Primary), task$Primary, "Unnamed Task")
      project_title <- ifelse(!is.na(task$`short name`), task$`short name`, "Untitled Project")
      project_phase <- ifelse(!is.na(task$`Active Phase`), task$`Active Phase`, "Not specified")
      assignee <- ifelse(!is.na(task$`Assigned To`), task$`Assigned To`, "Unassigned")
      wave <- ifelse(!is.na(task$Wave), task$Wave, "Not specified")
      tier <- ifelse(!is.na(task$tier), task$tier, "Not specified")
      risk_level <- ifelse(!is.na(task$Health), task$Health, "Unknown")
      
      # Progress
      progress <- ifelse(!is.na(task$`% Complete`), task$`% Complete`, 0)
      progress_pct <- paste0(round(progress * 100), "%")
      
      # Schedule Health (already calculated)
      schedule_health <- task$schedule_health
      
      # Colors
      schedule_color <- switch(schedule_health,
                               "On Track" = "green",
                               "At Risk" = "orange",
                               "Overdue" = "red",
                               "Unknown" = "gray")
      risk_color <- switch(risk_level,
                           "Red" = "red",
                           "Amber" = "orange",
                           "Green" = "green",
                           "Gray" = "gray",
                           "black")
      bg_color <- switch(tier,
                         "Tier 1 - High Governance" = "#FFD6D6",
                         "Tier 2 - Medium Governance" = "#FFE7C9",
                         "Tier 3 - Low Governance" = "#D6FFD6",
                         "#F5F5F5")
      
      # Task Card UI
      div(
        class = "task-card",
        style = paste(
          "background-color:", bg_color, ";",
          "padding: 15px; margin-bottom: 15px; border-radius: 8px;",
          "box-shadow: 0px 4px 8px rgba(0, 0, 0, 0.1);"
        ),
        h4(paste(project_title, "-", project_phase), style = "margin-bottom: 10px; font-weight: bold;"),
        h5(task_name, style = "margin-bottom: 10px; font-weight: bold;"),
        p(paste("Assignee:", assignee), style = "margin: 5px 0;"),
        p(paste("Wave:", wave), style = "margin: 5px 0;"),
        div(
          class = "badge",
          style = paste(
            "display: inline-block; padding: 5px 10px; border-radius: 8px; margin-bottom: 5px;",
            "background-color:", risk_color, "; color: white; font-weight: bold;"
          ),
          paste("Health:", risk_level)
        ),
        div(
          class = "badge",
          style = paste(
            "display: inline-block; padding: 5px 10px; border-radius: 8px; margin-bottom: 10px;",
            "background-color:", schedule_color, "; color: white; font-weight: bold;"
          ),
          paste("Schedule:", schedule_health)
        ),
        div(
          class = "progress",
          style = "height: 15px; background-color: #f5f5f5; border-radius: 5px; overflow: hidden; margin-bottom: 5px;",
          div(
            class = "progress-bar",
            style = paste0("width:", progress_pct, "; background-color: #007bff; height: 100%;")
          )
        ),
        p(paste("Progress:", progress_pct), style = "font-size: 12px; color: #666;")
      )
    }))
  }
  
  
  output$notStartedColumn <- renderUI({ renderTaskColumn("Not Started") })
  output$inProgressColumn <- renderUI({ renderTaskColumn("In Progress") })
  output$completeColumn <- renderUI({ renderTaskColumn("Complete") })
  output$blockedColumn <- renderUI({ renderTaskColumn("Blocked") })
}

