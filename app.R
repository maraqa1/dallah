library(shinyjs)
library(shinyWidgets)
library(DT)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)       # Required for Excel file uploads
library(shinyjqui)    # Optional: For drag-and-drop UI interactions
library(patchwork)
library(openxlsx)


# --- Load Modules ---
source("modules/taskInputModule.R")
source("modules/kanbanBoardModule.R")
source("modules/dashboardAnalyticsModule.R")
# At top of app.R
source("modules/steeringWaveGanttModule.R")

# --- Define UI ---
ui <- navbarPage(
  title = "Kanban Dashboard",
  theme = bslib::bs_theme(bootswatch = "flatly"),
  tabPanel("Task Input", taskInputUI("taskInput")),
  tabPanel("Kanban Board", kanbanBoardUI("kanbanBoard")),               # Placeholder for future use
  tabPanel("Dashboard Analytics", dashboardAnalyticsUI("dashboardAnalytics")),  # Placeholder
  # In UI
  tabPanel("Gantt by Wave", steeringWaveGanttUI("waveGantt"))
  
)

# --- Define Server ---
server <- function(input, output, session) {
  
  # Initialize shared task data in Smartsheet format
  task_data <- reactiveVal(
    data.frame(
      Primary = character(),
      `Assigned To` = character(),
      `% Complete` = character(),    # as character initially (we'll clean it on upload)
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
  
  # Load saved data if it exists (legacy support, can be removed later)
  observe({
    if (file.exists("saved_tasks.csv")) {
      task_data(read.csv("saved_tasks.csv", stringsAsFactors = FALSE))
      print("Loaded saved task data (CSV).")
    } else if (file.exists("saved_tasks.rds")) {
      task_data(readRDS("saved_tasks.rds"))
      print("Loaded saved task data (RDS).")
    }
  })
  
  # Debug: Show structure of task_data in console
  observe({
    print("Current task_data structure:")
    print(str(task_data()))
  })
  
  # --- Modules ---
  #callModule(taskInputServer, "taskInput", task_data = task_data)
  taskInputServer("taskInput", task_data = task_data)
  
  # Uncomment these when you're ready to use:
  callModule(kanbanBoardServer, "kanbanBoard", tasks = task_data)
  callModule(dashboardAnalyticsServer, "dashboardAnalytics", tasks = task_data)
  # In server
  callModule(steeringWaveGanttServer, "waveGantt", tasks = task_data)
}

# --- Launch App ---
shinyApp(ui, server)