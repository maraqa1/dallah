# modules/taskInputModule.R

taskInputUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    h3("Task Management"),
    br(),
    
    fluidRow(
      column(12,
             radioButtons(ns("dataSource"), "Step 1: Choose Data Source:",
                          choices = c("Smartsheet" = "smartsheet",
                                      "Google Sheet" = "google",
                                      "Local File" = "local",
                                      "Upload File" = "upload"),
                          selected = "smartsheet",
                          inline = TRUE),
             uiOutput(ns("sourceInstructions"))
      )
    ),
    
    fluidRow(
      conditionalPanel(
        condition = sprintf("input['%s'] == 'upload'", ns("dataSource")),
        column(12,
               fileInput(ns("fileUpload"), "Step 2: Upload Smartsheet File (.csv or .xlsx)", accept = c(".csv", ".xlsx"))
        )
      )
    ),
    
    fluidRow(
      column(6, actionButton(ns("loadData"), "Step 3: Load Data", class = "btn-primary btn-block")),
      column(6, actionButton(ns("clearData"), "Clear All Tasks", class = "btn-danger btn-block"))
    ),
    
    hr(),
    h3("Add / Edit Task"),
    br(),
    
    fluidRow(
      column(4, textInput(ns("taskName"), "Initiative Name (Primary)")),
      column(4, textInput(ns("projectTitle"), "Short Name")),
      column(4, selectInput(ns("projectPhase"), "Active Phase", 
                            choices = c("Planning", "Execution", "Closure", "Assessment", "Not Started")))
    ),
    
    fluidRow(
      column(4, selectInput(ns("status"), "Status", choices = c("Not Started", "In Progress", "Completed"))),
      column(4, textInput(ns("assignee"), "Assigned To")),
      column(4, dateInput(ns("startDate"), "Start Date", value = Sys.Date(), format = "yyyy-mm-dd"))
    ),
    
    fluidRow(
      column(4, actionButton(ns("addTask"), "Add Task", class = "btn-success btn-block")),
      column(4, actionButton(ns("updateTask"), "Update Selected Task", class = "btn-primary btn-block")),
      column(4, actionButton(ns("deleteTask"), "Delete Selected Task", class = "btn-danger btn-block"))
    ),
    
    hr(),
    h3("Task Preview Table"),
    br(),
    
    dataTableOutput(ns("previewTable"))
  )
}

####
read_smartsheet_sheet <- function(api_key, sheet_id) {
  library(httr)
  library(jsonlite)
  library(dplyr)
  
  url <- paste0("https://api.smartsheet.com/2.0/sheets/", sheet_id)
  `%||%` <- function(x, y) if (!is.null(x)) x else y
  
  response <- GET(url, add_headers(
    Authorization = paste("Bearer", api_key),
    Accept = "application/json"
  ))
  sheet_data <- content(response, as = "parsed", simplifyVector = FALSE)
  
  column_map <- setNames(
    vapply(sheet_data$columns, function(col) col$title, character(1)),
    vapply(sheet_data$columns, function(col) as.character(col$id), character(1))
  )
  
  convert_row <- function(row) {
    row_values <- setNames(rep(NA, length(column_map)), column_map)
    for (cell in row$cells) {
      col_id <- as.character(cell$columnId)
      val <- cell$displayValue %||% cell$value
      if (!is.null(val) && col_id %in% names(column_map)) {
        row_values[[ column_map[[col_id]] ]] <- val
      }
    }
    as.data.frame(as.list(row_values), stringsAsFactors = FALSE)
  }
  
  rows_df <- bind_rows(lapply(sheet_data$rows, convert_row))
  names(rows_df) <- gsub(" ", "_", names(rows_df))
  
  rows_df[] <- lapply(rows_df, function(col) {
    if (is.character(col)) {
      col[col == "NA" | col == ""] <- NA
    }
    return(col)
  })
  
  
  
  return(rows_df)
}

####

taskInputServer <- function(id, task_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    # --- Auto-Load from Smartsheet first, fallback to Google Sheet ---
    observe({
      df <- NULL
      tryCatch({
        
       
        # --- Try Smartsheet ---
        api_key <- "qmX7yeYQbYEoTgSX5s7vusvsWMM916URXcNUN"
        sheet_id <- "951355355123588"
        
        df <- read_smartsheet_sheet(api_key, sheet_id)
        
        build_df_clean_smartsheet <- function(df_raw) {
          df_clean <- tibble::tibble(
            `Level Code`           = as.numeric(df_raw$Level.Code),
            `WBS Phase1`           = as.character(df_raw$WBS.Phase1),
            `WBS Phase`            = as.logical(df_raw$WBS.Phase),
            `Ancestors`            = as.character(df_raw$Ancestors),
            `Skip WBS`             = as.logical(df_raw$Skip.WBS),
            `Row ID`               = as.character(df_raw$Row.ID),
            `WBS`                  = as.character(df_raw$WBS),
            `Code`                 = as.character(df_raw$Code),
            `Task`                 = as.character(df_raw$Task),
            `Description`          = as.character(df_raw$Description),
            `Wave`                 = as.character(df_raw$Wave),
            `Program`              = as.character(df_raw$Program),
            `Assigned To`          = as.character(df_raw$Assigned.To),
            `% Allocation`         = as.logical(df_raw$X..Allocation),
            `Status`               = as.character(df_raw$Status),
            `% Complete`           = readr::parse_number(as.character(df_raw$X..Complete)) / 100,
            `Progress`             = as.character(df_raw$Progress),
            `Health`               = as.character(df_raw$Health),
            `Start Date`           = as.POSIXct(gsub("T.*", "", df_raw$Start.Date), format = "%Y-%m-%d"),
            `End Date`             = as.POSIXct(gsub("T.*", "", df_raw$End.Date), format = "%Y-%m-%d"),
            `Duration`             = as.character(df_raw$Duration),
            `Predecessors`         = as.logical(df_raw$Predecessors),
            `Gate Approval`        = as.character(df_raw$Gate.Approval),
            `Escalated`            = as.logical(df_raw$Escalated),
            `Date Escalated`       = as.POSIXct(df_raw$Date.Escalated, format = "%Y-%m-%d"),
            `At Risk`              = as.logical(df_raw$At.Risk),
            `Date Risk Identified` = as.POSIXct(df_raw$Date.Risk.Identified, format = "%Y-%m-%d"),
            `Risk Description`     = as.character(df_raw$Risk.Description),
            `Issue`                = as.logical(df_raw$Issue),
            `Date Isse Identified` = as.POSIXct(df_raw$Date.Isse.Identified, format = "%Y-%m-%d"),
            `Issue Description`    = as.character(df_raw$Issue.Description),
            `All Not Started`      = as.numeric(df_raw$All.Not.Started),
            `Phase Status Sort`    = as.numeric(df_raw$Phase.Status.Sort),
            `Active Phase`         = as.character(df_raw$Active.Phase),
            `short name`           = as.character(df_raw$short.name),
            `tier`                 = as.character(df_raw$tier),
            `New Progress Health`  = as.character(df_raw$New.Progress.Health),
            `Previous Status`      = as.numeric(df_raw$Previous.Status)
          )
          return(df_clean)
        }
        
        
        
        df <- build_df_clean_smartsheet(df)
        

        if ("Task" %in% colnames(df)) {
          df <- df %>% dplyr::rename(Primary = Task)
        }
        
        df <- df %>% dplyr::filter(grepl("^Initiative [0-9]+:", Primary))
        
        if ("%_Complete" %in% names(df)) {
          df$`%_Complete` <- readr::parse_number(as.character(df$`%_Complete`)) / 1
          df$Progress <- df$`%_Complete`
        }
        
        
        task_data(df)
        showNotification("✅ Auto-loaded from Smartsheet successfully!", type = "message")
        
      }, error = function(e1) {
        # --- If Smartsheet fails, try Google Sheet ---
        tryCatch({
          googlesheets4::gs4_deauth()
          google_sheet_id <- "1B7jwdy9nmTjVPN9glEe4qLMIoxFyNARd6MeMY9yjdzs"
          df <- googlesheets4::read_sheet(google_sheet_id)
          
          if ("Task" %in% colnames(df)) {
            df <- df %>% dplyr::rename(Primary = Task)
          }
          
          df <- df %>% dplyr::filter(grepl("^Initiative [0-9]+:", Primary))
          
          df$`% Complete` <- readr::parse_number(as.character(df$`% Complete`)) / 1
          df$Progress <- df$`% Complete`
          
          task_data(df)
          showNotification("⚠️ Smartsheet unavailable. Fallback to Google Sheet successful.", type = "warning")
          
        }, error = function(e2) {
          showNotification("❌ Failed to load from both Smartsheet and Google Sheet.", type = "error")
        })
      })
    })
    
    
    
    
    # --- Auto-Load from Google on Startup ---
    #   observe({
    #    df <- NULL
    #    tryCatch({
    #     googlesheets4::gs4_deauth()
    #     sheet_id <- "1cvemCZnWoeq_OCKKs379Jrjt34NfKfm5RRNllaMvqf4"
    #     sheet_id <- "1B7jwdy9nmTjVPN9glEe4qLMIoxFyNARd6MeMY9yjdzs"
    #     df <- googlesheets4::read_sheet(sheet_id)
    #browser()
    #     if ("Task" %in% colnames(df)) {
    #        df <- df %>% dplyr::rename(Primary = Task)
    #    }
    #     
    #      df <- df %>%
    #        dplyr::filter(grepl("^Initiative [0-9]+:", Primary))
    #      
    #     df$`% Complete` <- readr::parse_number(as.character(df$`% Complete`)) / 1
    #      df$Progress <- df$`% Complete`
    
    #       task_data(df)
    
    #       showNotification("✅ Auto-loaded data from Google successfully!", type = "message")
    
    #  }, error = function(e) {
    #     showNotification("⚠️ Auto-load from Google failed. Please load manually.", type = "warning")
    #   })
    #  })
    
    
    # File paths
    local_file_path <- "./data/Stage_report.xlsx"
    google_sheet_id <- "1B7jwdy9nmTjVPN9glEe4qLMIoxFyNARd6MeMY9yjdzs"
    
    # Columns expected
    required_cols <- c("Primary", "Assigned To", "% Complete", "Health", "Status",
                       "Active Phase", "Start Date", "Wave", "Code", "short name", "tier", "Program")
    
    # --- Step Instructions ---
    output$sourceInstructions <- renderUI({
      req(input$dataSource)
      
      switch(input$dataSource,
             "smartsheet" = helpText("➡️ Will auto-load data from Smartsheet using API key and Sheet ID."),
             "upload" = helpText("➡️ Upload a .csv or .xlsx Smartsheet file."),
             "local" = helpText("➡️ Will load from './data/Stage_report.xlsx'. Ensure the file exists."),
             "google" = helpText("➡️ Will load from the public Google Sheet shared link."),
             NULL)
    })
    
    # --- Load Data Button ---
    observeEvent(input$loadData, {
      req(input$dataSource)
      df <- NULL
      
      if (input$dataSource == "upload") {
        req(input$fileUpload)
        
        ext <- tools::file_ext(input$fileUpload$name)
        df <- if (ext == "csv") {
          read.csv(input$fileUpload$datapath, stringsAsFactors = FALSE, check.names = FALSE)
        } else if (ext %in% c("xlsx", "xls")) {
          readxl::read_excel(input$fileUpload$datapath)
        } else {
          showNotification("❌ Unsupported file format.", type = "error")
          return()
        }
        
      } else if (input$dataSource == "local") {
        if (file.exists(local_file_path)) {
          df <- readxl::read_excel(local_file_path)
        } else {
          showNotification("❌ Local file not found!", type = "error")
          return()
        }
        
      } else if (input$dataSource == "google") {
        tryCatch({
          googlesheets4::gs4_deauth()
          df <- googlesheets4::read_sheet(google_sheet_id)
          
          if ("Task" %in% colnames(df)) {
            df <- df %>% dplyr::rename(Primary = Task)
          } 
          
          df <- df %>%
            dplyr::filter(grepl("^Initiative [0-9]+:", Primary))
          
        }, error = function(e) {
          showNotification(paste("❌ Google Sheet load error:", e$message), type = "error")
          df <- NULL
        })
      } else if (input$dataSource == "smartsheet") {
        tryCatch({
          api_key <- "qmX7yeYQbYEoTgSX5s7vusvsWMM916URXcNUN"
          sheet_id <- "951355355123588"
          
          df <- read_smartsheet_sheet(api_key, sheet_id)
          
          if ("Task" %in% colnames(df)) {
            df <- df %>% dplyr::rename(Primary = Task)
          }
          
          df <- df %>% dplyr::filter(grepl("^Initiative [0-9]+:", Primary))
          
          # Convert % Complete
          if ("%_Complete" %in% names(df)) {
            df$`%_Complete` <- readr::parse_number(as.character(df$`%_Complete`)) / 1
            df$Progress <- df$`%_Complete`
          }
          
        }, error = function(e) {
          showNotification(paste("❌ Smartsheet load error:", e$message), type = "error")
          df <- NULL
        })
      }
      
    
      # Final validation
      if (!is.null(df)) {
        missing_cols <- setdiff(required_cols, colnames(df))
        if (length(missing_cols) > 0) {
          showNotification(paste("❌ Missing columns:", paste(missing_cols, collapse = ", ")), type = "error")
          return()
        }
        
        df$`% Complete` <- readr::parse_number(as.character(df$`% Complete`)) / 1
        df$Progress <- df$`% Complete`
        
        task_data(df)
        showNotification("✅ Data loaded successfully.", type = "message")
      }
    })
    
    # --- Add / Update / Delete Tasks ---
    observeEvent(input$addTask, {
      req(task_data())
      new_task <- data.frame(
        Primary = input$taskName,
        `Assigned To` = input$assignee,
        `% Complete` = 0,
        Health = "Green",
        Status = input$status,
        `Active Phase` = input$projectPhase,
        `Start Date` = as.character(input$startDate),
        Wave = NA,
        Code = NA,
        `short name` = input$projectTitle,
        tier = "Tier 2 - Medium Governance",
        Program = "Default Program",
        Progress = 0,
        stringsAsFactors = FALSE
      )
      
      new_task <- new_task[, names(task_data()), drop = FALSE]
      task_data(rbind(task_data(), new_task))
      showNotification("✅ Task added!", type = "message")
    })
    
    observeEvent(input$updateTask, {
      req(input$previewTable_rows_selected)
      df <- task_data()
      i <- input$previewTable_rows_selected
      
      df[i, ] <- list(
        Primary = input$taskName,
        `Assigned To` = input$assignee,
        `% Complete` = 0,
        Health = "Green",
        Status = input$status,
        `Active Phase` = input$projectPhase,
        `Start Date` = as.character(input$startDate),
        Wave = NA,
        Code = NA,
        `short name` = input$projectTitle,
        tier = "Tier 2 - Medium Governance",
        Program = "Default Program",
        Progress = 0
      )
      
      task_data(df)
      showNotification("✅ Task updated!", type = "message")
    })
    
    observeEvent(input$clearData, {
      task_data(data.frame(
        Primary = character(),
        `Assigned To` = character(),
        `% Complete` = character(),
        Health = character(),
        Status = character(),
        `Active Phase` = character(),
        `Start Date` = character(),
        Wave = character(),
        Code = character(),
        `short name` = character(),
        tier = character(),
        Program = character(),
        Progress = numeric(),
        stringsAsFactors = FALSE
      ))
      showNotification("⚠️ All tasks cleared.", type = "warning")
    })
    
    observeEvent(input$deleteTask, {
      req(input$previewTable_rows_selected)
      
      showModal(modalDialog(
        title = "Confirm Deletion",
        "Are you sure you want to delete the selected task?",
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirmDelete"), "Yes, Delete", class = "btn-danger")
        )
      ))
    })
    
    observeEvent(input$confirmDelete, {
      selected_row <- isolate(input$previewTable_rows_selected)
      task_data(task_data()[-selected_row, , drop = FALSE])
      removeModal()
      showNotification("⚠️ Task deleted.", type = "warning")
    })
    
    # --- Render Table ---
    output$previewTable <- renderDataTable({
      req(task_data())
      datatable(task_data(), options = list(pageLength = 5, scrollX = TRUE), selection = "single")
    })
  })
}
