
taskInputUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    h3("Task Management"),
    fluidRow(
      column(6, fileInput(ns("fileUpload"), "Upload Smartsheet File (.csv or .xlsx)", accept = c(".csv", ".xlsx"))),
      column(6, actionButton(ns("clearData"), "Clear All Tasks", class = "btn-danger"))
    ),
    hr(),
    h3("Add / Edit Task"),
    fluidRow(
      column(4, textInput(ns("taskName"), "Initiative Name (Primary)")),
      column(4, textInput(ns("projectTitle"), "Short Name")),
      column(4, selectInput(ns("projectPhase"), "Active Phase", choices = c("Planning", "Execution", "Closure", "Assessment", "Not Started")))
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

taskInputServer <- function(input, output, session, task_data) {
  ns <- session$ns
  
  # File Upload Handler
  observeEvent(input$fileUpload, {
    req(input$fileUpload)
    
    ext <- tools::file_ext(input$fileUpload$name)
    df <- tryCatch({
      if (ext == "csv") {
        read.csv(input$fileUpload$datapath, stringsAsFactors = FALSE, check.names = FALSE)
      } else {
        readxl::read_excel(input$fileUpload$datapath)
      }
    }, error = function(e) {
      showNotification(paste("Upload error:", e$message), type = "error")
      return(NULL)
    })
    
    if (!is.null(df)) {
      required_cols <- c("Primary", "Assigned To", "% Complete", "Health", "Status",
                         "Active Phase", "Start Date", "Wave", "Code", "short name", "tier", "Program")
      
      missing_cols <- setdiff(required_cols, colnames(df))
      if (length(missing_cols) > 0) {
        showNotification(paste("Invalid file structure. Missing:", paste(missing_cols, collapse = ", ")), type = "error")
        return()
      }
      

      df$`% Complete` <- readr::parse_number(as.character(df$`% Complete`)) / 1
      df$Progress <- df$`% Complete`
      task_data(df)
      showNotification("Smartsheet data uploaded successfully.", type = "message")
    }
  })
  
  # Add New Task
  observeEvent(input$addTask, {
    req(task_data())  # Ensure base data is loaded
    
    # Create new task
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
    
    # Fill in any missing columns with NA
    existing_cols <- names(task_data())
    missing_cols <- setdiff(existing_cols, names(new_task))
    
    for (col in missing_cols) {
      new_task[[col]] <- NA
    }
    
    # Reorder columns to match task_data()
    new_task <- new_task[, existing_cols, drop = FALSE]
    
    # Append the new row
    task_data(rbind(task_data(), new_task))
    showNotification("Task added successfully!", type = "message")
  })
  
  
  # Populate form when selecting a row
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
  
  # Update Existing Task
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
    showNotification("Task updated.", type = "message")
  })
  
  # Clear Tasks
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
    showNotification("All tasks cleared.", type = "warning")
  })
  
  
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
    showNotification("Task deleted.", type = "warning")
  })
  
  
  
  # Preview Table
  output$previewTable <- renderDataTable({
    req(task_data())
    datatable(task_data(), options = list(pageLength = 5, autoWidth = TRUE), selection = "single")
  })
}
