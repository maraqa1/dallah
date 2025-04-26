# modules/taskInputModule.R

taskInputUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    h3("Task Management"),
    br(),
    
    fluidRow(
      column(12,
             radioButtons(ns("dataSource"), "Step 1: Choose Data Source:",
                          choices = c("Google Sheet" = "google",
                                      "Local File" = "local",
                                      "Upload File" = "upload"),
                          selected = "google",
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

taskInputServer <- function(id, task_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # --- Auto-Load from Google on Startup ---
    observe({
      df <- NULL
      tryCatch({
        googlesheets4::gs4_deauth()
        sheet_id <- "1cvemCZnWoeq_OCKKs379Jrjt34NfKfm5RRNllaMvqf4"
        df <- googlesheets4::read_sheet(sheet_id)
        
        if ("Task" %in% colnames(df)) {
          df <- df %>% dplyr::rename(Primary = Task)
        }
        
        df <- df %>%
          dplyr::filter(grepl("^Initiative [0-9]+:", Primary))
        
        df$`% Complete` <- readr::parse_number(as.character(df$`% Complete`)) / 1
        df$Progress <- df$`% Complete`
        
        task_data(df)
        
        showNotification("✅ Auto-loaded data from Google successfully!", type = "message")
        
      }, error = function(e) {
        showNotification("⚠️ Auto-load from Google failed. Please load manually.", type = "warning")
      })
    })
    
    
    # File paths
    local_file_path <- "./data/Stage_report.xlsx"
    google_sheet_id <- "1cvemCZnWoeq_OCKKs379Jrjt34NfKfm5RRNllaMvqf4"
    
    # Columns expected
    required_cols <- c("Primary", "Assigned To", "% Complete", "Health", "Status",
                       "Active Phase", "Start Date", "Wave", "Code", "short name", "tier", "Program")
    
    # --- Step Instructions ---
    output$sourceInstructions <- renderUI({
      req(input$dataSource)
      
      switch(input$dataSource,
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
