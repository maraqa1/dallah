# ─────────────────────────────────────────────────────────────
# modules/kanbanBoardModule.R (Optimized for Desktop & Mobile)
# ─────────────────────────────────────────────────────────────
<<<<<<< HEAD

=======
>>>>>>> 199504063c151172a454c2b129edb12eeda88552
kanbanBoardUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    tags$head(
      tags$style(HTML("
<<<<<<< HEAD
        /* Column Header Styles */
=======
>>>>>>> 199504063c151172a454c2b129edb12eeda88552
        .kanban-header {
          background-color: #2E2D62;
          color: white;
          padding: 10px;
          border-radius: 8px;
          text-align: center;
          font-weight: bold;
          margin-bottom: 10px;
          font-size: 18px;
        }

<<<<<<< HEAD
        /* Task Card Styles */
=======
>>>>>>> 199504063c151172a454c2b129edb12eeda88552
        .task-card {
          background-color: #ffffff;
          border-radius: 8px;
          box-shadow: 0 2px 6px rgba(0,0,0,0.1);
          margin-bottom: 15px;
          padding: 10px;
          transition: transform 0.2s;
        }

        .task-card:hover {
          transform: scale(1.02);
        }

<<<<<<< HEAD
        /* Responsive Column Layout */
=======
>>>>>>> 199504063c151172a454c2b129edb12eeda88552
        .kanban-column {
          margin-bottom: 20px;
        }

        @media (min-width: 768px) {
          .kanban-column {
            width: 25%;
            float: left;
            padding: 0 10px;
          }
        }

        @media (max-width: 767px) {
          .kanban-column {
            width: 100%;
            padding: 0 5px;
          }
        }

<<<<<<< HEAD
        /* Badge Style */
=======
>>>>>>> 199504063c151172a454c2b129edb12eeda88552
        .badge {
          display: inline-block;
          padding: 5px 10px;
          border-radius: 8px;
          color: white;
          font-weight: bold;
          margin-bottom: 5px;
          font-size: 12px;
        }
      "))
    ),
    
<<<<<<< HEAD
=======
    # ── Filters ─────────────────────────────────────────────
    fluidRow(
      column(4, selectInput(ns("waveSelect"), "Filter by Wave", choices = NULL)),
      column(4, selectInput(ns("assigneeSelect"), "Filter by Assignee", choices = NULL)),
      column(4, selectInput(ns("programSelect"), "Filter by Program", choices = NULL))
    ),
    
    # ── Columns ─────────────────────────────────────────────
>>>>>>> 199504063c151172a454c2b129edb12eeda88552
    div(class = "row",
        div(class = "kanban-column",
            div(class = "kanban-header", "Not Started"),
            uiOutput(ns("notStartedColumn"))
        ),
        div(class = "kanban-column",
            div(class = "kanban-header", "In Progress"),
            uiOutput(ns("inProgressColumn"))
        ),
        div(class = "kanban-column",
            div(class = "kanban-header", "Complete"),
            uiOutput(ns("completeColumn"))
        ),
        div(class = "kanban-column",
            div(class = "kanban-header", "Blocked"),
            uiOutput(ns("blockedColumn"))
        )
    ),
    
<<<<<<< HEAD
    div(style = "clear: both;") # To clear floats
=======
    div(style = "clear: both;")
>>>>>>> 199504063c151172a454c2b129edb12eeda88552
  )
}

kanbanBoardServer <- function(input, output, session, tasks) {
  ns <- session$ns
<<<<<<< HEAD
=======
  
  # ── Update SelectInput Filter Choices ─────────────────────
  observe({
    req(tasks())
    data <- tasks()
    
    updateSelectInput(session, "waveSelect",
                      choices = c("All", sort(unique(na.omit(data$Wave)))),
                      selected = "All")
    
    updateSelectInput(session, "assigneeSelect",
                      choices = c("All", sort(unique(na.omit(data$`Assigned To`)))),
                      selected = "All")
    
    updateSelectInput(session, "programSelect",
                      choices = c("All", sort(unique(na.omit(data$Program)))),
                      selected = "All")
  })
  
  
  
>>>>>>> 199504063c151172a454c2b129edb12eeda88552
  library(dplyr)
  library(lubridate)
  
  normalize_status <- function(status) {
    status <- tolower(trimws(status))
    dplyr::case_when(
      status %in% c("blocked") ~ "Blocked",
      status %in% c("complete", "completed", "done") ~ "Complete",
      status %in% c("in progress", "ongoing", "started", "working") ~ "In Progress",
      status %in% c("not started", "to do", "todo") ~ "Not Started",
      TRUE ~ "Not Started"
    )
  }
  
  safe_parse_date <- function(x) {
    x <- as.character(x)
    parsed <- suppressWarnings(mdy(x))
    if (is.na(parsed)) {
      parsed <- suppressWarnings(as.Date(x, tryFormats = c("%Y-%m-%d", "%d/%m/%Y", "%m/%d/%Y")))
    }
    parsed
  }
  
  renderTaskColumn <- function(status) {
    data <- tasks()
<<<<<<< HEAD
=======
    
    
    # ── Apply Filters ─────────────────────────────────────────
    if (!is.null(input$waveSelect) && input$waveSelect != "All") {
      data <- data[data$Wave == input$waveSelect, ]
    }
    if (!is.null(input$assigneeSelect) && input$assigneeSelect != "All") {
      data <- data[data$`Assigned To` == input$assigneeSelect, ]
    }
    if (!is.null(input$programSelect) && input$programSelect != "All") {
      data <- data[data$Program == input$programSelect, ]
    }
    
    
    
>>>>>>> 199504063c151172a454c2b129edb12eeda88552
    if (nrow(data) == 0) return(h4("No tasks available."))
    
    data$Status <- normalize_status(data$Status)
    filtered <- data[!is.na(data$Status) & data$Status == status, ]
    
    if (nrow(filtered) == 0) return(h4(paste("No tasks in", status, "column.")))
    
    filtered$schedule_health <- sapply(filtered$`Start Date`, function(start_date) {
      parsed_date <- safe_parse_date(start_date)
      if (is.na(parsed_date)) return("Unknown")
      days <- as.numeric(difftime(parsed_date, Sys.Date(), units = "days"))
      if (days < 0) return("Overdue")
      if (days < 7) return("At Risk")
      return("On Track")
    })
    
    filtered <- filtered %>%
      mutate(
        Health_Score = case_when(
          Health == "Red" ~ 1,
          Health == "Amber" ~ 2,
          Health == "Green" ~ 3,
          Health == "Gray" ~ 4,
          TRUE ~ 5
        ),
        Schedule_Score = case_when(
          schedule_health == "Overdue" ~ 1,
          schedule_health == "At Risk" ~ 2,
          schedule_health == "On Track" ~ 3,
          TRUE ~ 4
        ),
        Progress_Score = ifelse(is.na(`% Complete`), 1, 1 - `% Complete`)
      ) %>%
      arrange(Health_Score, Schedule_Score, Progress_Score)
    
    tagList(lapply(seq_len(nrow(filtered)), function(i) {
      task <- filtered[i, ]
      
      task_name <- ifelse(!is.na(task$Primary), task$Primary, "Unnamed Task")
      project_title <- ifelse(!is.na(task$`short name`), task$`short name`, "Untitled Project")
      project_phase <- ifelse(!is.na(task$`Active Phase`), task$`Active Phase`, "Not specified")
      assignee <- ifelse(!is.na(task$`Assigned To`), task$`Assigned To`, "Unassigned")
      wave <- ifelse(!is.na(task$Wave), task$Wave, "Not specified")
      tier <- ifelse(!is.na(task$tier), task$tier, "Not specified")
      risk_level <- ifelse(!is.na(task$Health), task$Health, "Unknown")
      progress <- ifelse(!is.na(task$`% Complete`), task$`% Complete`, 0)
      progress_pct <- paste0(round(progress * 100), "%")
      schedule_health <- task$schedule_health
      
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
      
      div(
        class = "task-card",
        style = paste("background-color:", bg_color, ";"),
        
        h5(paste(project_title, "-", project_phase), style = "margin-bottom: 8px; font-weight: bold; font-size: 14px;"),
        h6(task_name, style = "margin-bottom: 8px;"),
        
        p(paste("👤", assignee), style = "margin: 4px 0; font-size: 12px;"),
        p(paste("🌊 Wave:", wave), style = "margin: 4px 0; font-size: 12px;"),
        
        div(
          class = "badge",
          style = paste("background-color:", risk_color, ";"),
          paste("Health:", risk_level)
        ),
        
        div(
          class = "badge",
          style = paste("background-color:", schedule_color, ";"),
          paste("Schedule:", schedule_health)
        ),
        
        div(
          class = "progress",
          style = "height: 10px; background-color: #f0f0f0; border-radius: 5px; margin-top: 5px;",
          div(
            class = "progress-bar",
            style = paste0("width:", progress_pct, "; background-color: #007bff; height: 100%;")
          )
        ),
        p(paste("Progress:", progress_pct), style = "font-size: 11px; color: #666; margin-top: 3px;")
      )
    }))
  }
  
  output$notStartedColumn <- renderUI({ renderTaskColumn("Not Started") })
  output$inProgressColumn <- renderUI({ renderTaskColumn("In Progress") })
  output$completeColumn <- renderUI({ renderTaskColumn("Complete") })
  output$blockedColumn <- renderUI({ renderTaskColumn("Blocked") })
}
