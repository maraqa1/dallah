# app.R
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(DT)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)       # For Excel file uploads
library(shinyjqui)    # For drag-and-drop (optional)
library(patchwork)
library(openxlsx)
library(bslib)        # Ensure bslib loaded for mobile theming
library(shinyBS)
library(ggtext) 
library(jsonlite)
library(lubridate)

# --- Load Modules ---
source("modules/taskInputModule.R")
source("modules/kanbanBoardModule.R")
source("modules/dashboardAnalyticsModule.R")
source("modules/steeringWaveGanttModule.R")
source("modules/themeModule.R")


# --- Define UI ---
ui <- navbarPage(
  title = "Kanban Dashboard",
  theme = bs_theme(bootswatch = "flatly"),
  
  tabPanel("Task Input", taskInputUI("taskInput")),
  tabPanel("Kanban Board", kanbanBoardUI("kanbanBoard")),
  tabPanel("Dashboard Analytics", dashboardAnalyticsUI("dashboardAnalytics")),
  tabPanel("Gantt by Wave", steeringWaveGanttUI("waveGantt"))
)

# --- Define Server ---
server <- function(input, output, session) {
  
  # Initialize shared task data
  task_data <- reactiveVal(
    data.frame(
      Primary = character(),
      `Assigned To` = character(),
      `% Complete` = character(),
      Health = character(),
      Progress = character(),
      Status = character(),
      `Active Phase` = character(),
      `Start Date` = character(),
      Wave = character(),
      Code = character(),
      `short name` = character(),
      tier = character(),
      Program = character(),
      stringsAsFactors = FALSE
    )
  )
  
  # --- Load legacy saved tasks if available ---
  observe({
    if (file.exists("saved_tasks.csv")) {
      task_data(read.csv("saved_tasks.csv", stringsAsFactors = FALSE))
      message("✅ Loaded saved task data (CSV).")
    } else if (file.exists("saved_tasks.rds")) {
      task_data(readRDS("saved_tasks.rds"))
      message("✅ Loaded saved task data (RDS).")
    }
  })
  
  # --- Debug: Show structure (optional) ---
  observe({
    message("📋 Current task_data structure:")
    str(task_data())
  })
  
  # --- Modules Calls ---
  taskInputServer("taskInput", task_data = task_data)  # ✅ Using modern moduleServer()
  
  callModule(kanbanBoardServer, "kanbanBoard", tasks = task_data)
  # kanbanBoardServer("kanbanBoard", tasks = task_data)
  
  callModule(dashboardAnalyticsServer, "dashboardAnalytics", tasks = task_data)
  callModule(steeringWaveGanttServer, "waveGantt", tasks = task_data)
}

# --- Launch App ---
shinyApp(ui = ui, server = server)
