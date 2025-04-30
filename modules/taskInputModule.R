taskInputUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    h3("Task Management"),
    
    radioButtons(ns("dataSource"), "Step 1: Choose Data Source:",
                 choices = c("Upload File" = "upload",
                             "Local File" = "local",
                             "Google Sheet" = "google"),
                 selected = "upload",
                 inline = TRUE),
    
    # Dynamic Help Text
    uiOutput(ns("sourceInstructions")),
    
    fluidRow(
      conditionalPanel(
        condition = sprintf("input['%s'] == 'upload'", ns("dataSource")),
        column(6, fileInput(ns("fileUpload"), "Step 2: Upload Smartsheet File (.csv or .xlsx)", accept = c(".csv", ".xlsx")))
      ),
      
      column(6, actionButton(ns("loadData"), "Step 3: Load Data", class = "btn-primary")),
      column(6, actionButton(ns("clearData"), "Clear All Tasks", class = "btn-danger"))
    ),
    
    hr(),
    h3("Add / Edit Task"),
    
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
      column(4, actionButton(ns("addTask"), "Add Task", class = "btn-success")),
      column(4, actionButton(ns("updateTask"), "Update Selected Task", class = "btn-primary")),
      column(4, actionButton(ns("deleteTask"), "Delete Selected Task", class = "btn-danger"))
    ),
    
    hr(),
    h3("Task Preview Table"),
    dataTableOutput(ns("previewTable"))
  )
}


taskInputServer <- function(id, task_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # --- INITIAL LOAD: Try Google, then Local, then wait for Upload ---
    observe({
      df <- NULL
      
      # Try Google first
      tryCatch({
        googlesheets4::gs4_deauth()
        sheet_id <- "1cvemCZnWoeq_OCKKs379Jrjt34NfKfm5RRNllaMvqf4"
        df <- googlesheets4::read_sheet(sheet_id)
        
        # Rename Task to Primary
        if ("Task" %in% colnames(df)) {
          df <- df %>% dplyr::rename(Primary = Task)
        }
        
        # Filter only Initiative rows
        df <- df %>%
          dplyr::filter(grepl("^Initiative [0-9]+:", Primary))
        
        showNotification("✅ Data loaded successfully from Google.", type = "message")
        
      }, error = function(e) {
        # If Google fails, try Local
        showNotification("⚠️ Failed to load from Google. Trying Local file...", type = "warning")
        
        if (file.exists("./data/Stage_report.xlsx")) {
          df <- readxl::read_excel("./data/Stage_report.xlsx")
          
          df$`% Complete` <- readr::parse_number(as.character(df$`% Complete`)) / 1
          df$Progress <- df$`% Complete`
          
          showNotification("✅ Data loaded successfully from Local File.", type = "message")
        } else {
          showNotification("❌ Local file not found. Please upload manually.", type = "error")
        }
      })
      
      if (!is.null(df)) {
        task_data(df)
      }
    })
    
    
    
    
    # Set paths
    local_file_path <- "./data/Stage_report.xlsx"
    google_sheet_url <- "https://docs.google.com/spreadsheets/d/1cvemCZnWoeq_OCKKs379Jrjt34NfKfm5RRNllaMvqf4/edit?usp=sharing"
    
    # Columns expected
    required_cols <- c("Primary", "Assigned To", "% Complete", "Health", "Status",
                       "Active Phase", "Start Date", "Wave", "Code", "short name", "tier", "Program")
    
    #✅ Dynamically displays the correct guidance for the user depending on the selection.
    
    output$sourceInstructions <- renderUI({
      req(input$dataSource)
      
      if (input$dataSource == "upload") {
        helpText("➡️ Please upload a .csv or .xlsx Smartsheet file, then click 'Load Data'.")
      } else if (input$dataSource == "local") {
        helpText("➡️ System will load from local file './data/Stage_report.xlsx'. Simply click 'Load Data'.")
      } else if (input$dataSource == "google") {
        helpText("➡️ System will load from the public Google Sheet. Make sure your sheet is shared publicly.")
      } else {
        NULL
      }
    })
    
    
    
    # --- Load Data ---
    
    observeEvent(input$loadData, {
      req(input$dataSource)
      source_choice <- input$dataSource
      df <- NULL
      
      if (source_choice == "upload") {
        req(input$fileUpload)
        
        ext <- tools::file_ext(input$fileUpload$name)
        if (ext == "csv") {
          df <- read.csv(input$fileUpload$datapath, stringsAsFactors = FALSE, check.names = FALSE)
        } else if (ext %in% c("xlsx", "xls")) {
          df <- readxl::read_excel(input$fileUpload$datapath)
        } else {
          showNotification("❌ Unsupported file format. Only CSV and Excel supported.", type = "error")
          return()
        }
        
      } else if (source_choice == "local") {
        if (file.exists(local_file_path)) {
          df <- readxl::read_excel(local_file_path)
        } else {
          showNotification("❌ Local file not found!", type = "error")
          return()
        }
        
      } else if (source_choice == "google") {
          tryCatch({
            googlesheets4::gs4_deauth()
            sheet_id <- "1cvemCZnWoeq_OCKKs379Jrjt34NfKfm5RRNllaMvqf4"
            df <- googlesheets4::read_sheet(sheet_id) #%>%
            
            # Rename Task to Primary to match local format
            if ("Task" %in% colnames(df)) {
              df <- df %>% dplyr::rename(Primary = Task)
            }
            
            # Filter only initiatives
            df <- df %>%
              dplyr::filter(grepl("^Initiative [0-9]+:", Primary))
            
  
          }, error = function(e) {
            showNotification(paste("❌ Google Sheet load error:", e$message), type = "error")
            df <- NULL
          })
        
        
        
        #-----
        
        
      }
      
      if (!is.null(df)) {
        # Check if required columns exist
        missing_cols <- setdiff(required_cols, colnames(df))
        if (length(missing_cols) > 0) {
          showNotification(paste("❌ Missing columns:", paste(missing_cols, collapse = ", ")), type = "error")
          return()
        }
        
        df$`% Complete` <- readr::parse_number(as.character(df$`% Complete`)) / 1
        df$Progress <- df$`% Complete`
        
        task_data(df)
        showNotification(paste("✅ Loaded tasks from", source_choice), type = "message")
      }
    })
    
    # --- Add Task ---
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
      
      missing_cols <- setdiff(names(task_data()), names(new_task))
      for (col in missing_cols) {
        new_task[[col]] <- NA
      }
      
      task_data(rbind(task_data(), new_task[, names(task_data()), drop = FALSE]))
      showNotification("✅ Task added!", type = "message")
    })
    
    # --- Populate form when selecting a row ---
    observeEvent(input$previewTable_rows_selected, {
      req(input$previewTable_rows_selected)
      row <- task_data()[input$previewTable_rows_selected, ]
      
      updateTextInput(session, "taskName", value = row$Primary)
      updateTextInput(session, "projectTitle", value = row$`short name`)
      updateSelectInput(session, "projectPhase", selected = row$`Active Phase`)
      updateSelectInput(session, "status", selected = row$Status)
      updateTextInput(session, "assignee", value = row$`Assigned To`)
      updateDateInput(session, "startDate", value = as.Date(row$`Start Date`))
    })
    
    # --- Update Task ---
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
    
    # --- Clear All Tasks ---
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
    
    # --- Delete Task ---
    observeEvent(input$deleteTask, {
      req(input$previewTable_rows_selected)
      
      showModal(modalDialog(
        title = "Delete Task",
        "Are you sure you want to delete this task?",
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirmDelete"), "Yes, Delete", class = "btn-danger")
        )
      ))
    })
    
    observeEvent(input$confirmDelete, {
      selected_row <- isolate(input$previewTable_rows_selected)
      updated_data <- task_data()[-selected_row, , drop = FALSE]
      task_data(updated_data)
      
      removeModal()
      showNotification("⚠️ Task deleted.", type = "warning")
    })
    
    # --- Table Output ---
    output$previewTable <- renderDataTable({
      req(task_data())
      datatable(task_data(), options = list(pageLength = 5, autoWidth = TRUE), selection = "single")
    })
  })
}
