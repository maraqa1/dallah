dashboardAnalyticsUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    h3("Dashboard Analytics", style = "font-weight: bold; text-align: center; margin-bottom: 20px;"),
    hr(style = "border-top: 3px solid #007bff;"),
    
    # Filters
    fluidRow(
      column(6, selectInput(ns("projectFilter"), "Filter by Project:", choices = NULL, selected = "All")),
      column(6, selectInput(ns("assigneeFilter"), "Filter by Assignee:", choices = NULL, selected = "All"))
    ),
    
    hr(style = "border-top: 1px solid #ccc;"),
    
    # Value Boxes
    fluidRow(
      column(2, uiOutput(ns("totalTasksBox"))),
      column(2, uiOutput(ns("blockedTasksBox"))),
      column(2, uiOutput(ns("highRiskTasksBox"))),
      column(2, uiOutput(ns("overdueTasksBox"))),
      column(2, uiOutput(ns("inProgressTasksBox"))),
      column(2, uiOutput(ns("averageProgressBox")))
    ),
    
    hr(style = "border-top: 3px solid #007bff;"),
    
    # Visual Insights Section
    h4("Visual Insights", style = "font-weight: bold; text-align: center; margin-top: 20px;"),
    
    # --- Progress by Project Plot ---
    fluidRow(
      column(12,
             plotOutput(ns("progressByProjectPlot"), height = "600px")
      )
    ),
    
    # --- Risk Levels by Assignee Plot ---
    fluidRow(
      column(12,
             plotOutput(ns("riskLevelsByAssigneePlot"), height = "auto")  # ✅ no need for extra UIOutput now
      )
    ),
    
    hr(style = "border-top: 3px solid #007bff;"),
    
    fluidRow(
      column(12,
             downloadButton(ns("downloadPMReport"), "Download PM Recommendations", class = "btn-primary", style = "margin-bottom:20px;")
      )
    ),
    
    
    # --- Heatmap Table ---
    h4("PM Recommendation Report", style = "font-weight: bold; text-align: center; margin-top: 20px;"),
    
    fluidRow(
      column(12,
             div(
               style = "padding-top: 30px; padding-bottom: 30px;",
               dataTableOutput(ns("heatmapTable"))
             )
      )
    )
  )
}


dashboardAnalyticsServer <- function(input, output, session, tasks) {
  ns <- session$ns
  
  
  observe({
    req(tasks())
    data <- tasks()
    updateSelectInput(session, "projectFilter", choices = c("All", unique(data$`short name`)))
    updateSelectInput(session, "assigneeFilter", choices = c("All", unique(data$`Assigned To`)))
  })
  
  filtered_data <- reactive({
    req(tasks())
    data <- tasks()
    
    if (!is.null(input$projectFilter) && input$projectFilter != "All") {
      data <- data[data$`short name` == input$projectFilter, ]
    }
    if (!is.null(input$assigneeFilter) && input$assigneeFilter != "All") {
      data <- data[data$`Assigned To` == input$assigneeFilter, ]
    }
    
    # ✅ Safe % Complete parsing
    if (!is.numeric(data$`% Complete`)) {
      data$`% Complete` <- suppressWarnings(readr::parse_number(as.character(data$`% Complete`)))
    }
    data$`% Complete` <- data$`% Complete` * 100
    
    data
  })
  
  
  # --- Value Boxes ---
  output$totalTasksBox <- renderUI({
    
    #browser()
    n <- nrow(filtered_data())
    div(style = "background:#007bff;color:white;padding:20px;border-radius:10px;text-align:center;", h3(n), p("Total Tasks"))
  })
  
  output$blockedTasksBox <- renderUI({
    n <- sum(tolower(trimws(filtered_data()$Health)) == "red", na.rm = TRUE)
    div(style = "background:#dc3545;color:white;padding:20px;border-radius:10px;text-align:center;", h3(n), p("Critical Health (Red)"))
  })
  
  output$highRiskTasksBox <- renderUI({
    n <- sum(tolower(trimws(filtered_data()$Health)) == "amber", na.rm = TRUE)
    div(style = "background:#ffc107;color:white;padding:20px;border-radius:10px;text-align:center;", h3(n), p("Medium Risk (Amber)"))
  })
#-------------------------------------------------------------------------------  
  output$overdueTasksBox <- renderUI({
    data <- filtered_data()
    
    # Safe parse of dates
    start_dates <- suppressWarnings(as.Date(data$`Start Date`, tryFormats = c("%Y-%m-%d", "%d/%m/%Y", "%m/%d/%Y")))
    end_dates   <- suppressWarnings(as.Date(data$`End Date`, tryFormats = c("%Y-%m-%d", "%d/%m/%Y", "%m/%d/%Y")))
    
    # Clean status
    status <- tolower(trimws(data$Status))
    
    # Overdue logic (PM view)
    overdue <- (
      (!is.na(start_dates) & start_dates < Sys.Date() & status == "not started") |
        (!is.na(end_dates)   & end_dates   < Sys.Date() & status != "completed")
    )
    
    count <- sum(overdue, na.rm = TRUE)
    
    div(
      style = "background:#dc3545;color:white;padding:20px;border-radius:10px;text-align:center;",
      h3(count),
      p("Overdue Tasks (Not Started / Incomplete)")
    )
  })
  #-------------------------------------------------------------------------------   
  
  output$inProgressTasksBox <- renderUI({
    n <- sum(tolower(trimws(filtered_data()$Status)) == "in progress", na.rm = TRUE)
    div(style = "background:#17a2b8;color:white;padding:20px;border-radius:10px;text-align:center;", h3(n), p("In Progress"))
  })
  
  output$averageProgressBox <- renderUI({
    avg <- round(mean(filtered_data()$`% Complete`, na.rm = TRUE), 1)
    div(style = "background:#28a745;color:white;padding:20px;border-radius:10px;text-align:center;", h3(paste0(avg, "%")), p("Avg. Completion"))
  })
  
  # --- Plot: Progress by Project ---
  
  project_theme <- theme_minimal(base_size = 14) +
    theme(
      panel.background = element_rect(fill = "#f7f7f7"),
      plot.background = element_rect(fill = "#f7f7f7"),
      panel.grid.major.x = element_line(color = "grey80"),
      panel.grid.major.y = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
      axis.text.y = element_text(face = "bold", size = 12),
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      plot.margin = margin(40, 40, 40, 40, "pt")
    )
  

  
  
  
  library(tidytext)  # for reorder_within and scale_y_reordered
  
  output$progressByProjectPlot <- renderPlot({
    data <- filtered_data()
    req(nrow(data) > 0)
    
    # Step 1: Parse and clean fields safely
    data$`% Complete` <- suppressWarnings(readr::parse_number(as.character(data$`% Complete`)))
    data$Start_Date <- suppressWarnings(as.Date(data$`Start Date`, tryFormats = c("%Y-%m-%d", "%d/%m/%Y", "%m/%d/%Y")))
    data$End_Date <- suppressWarnings(as.Date(data$`End Date`, tryFormats = c("%Y-%m-%d", "%d/%m/%Y", "%m/%d/%Y")))
    
    # Step 2: Mutate inside the pipeline, create safe lowercased status
    library(dplyr)
    library(lubridate)
    
    data <- data %>%
      mutate(
        # --- Normalize and prepare fields ---
        Status_clean = tolower(trimws(as.character(Status))),      # Ensure status is lowercase for logic
        Start_Date = as.Date(`Start Date`, tryFormats = c("%Y-%m-%d", "%d/%m/%Y", "%m/%d/%Y")),
        End_Date   = as.Date(`End Date`, tryFormats = c("%Y-%m-%d", "%d/%m/%Y", "%m/%d/%Y")),
        Progress   = suppressWarnings(readr::parse_number(as.character(`% Complete`)) / 100),  # Convert % to proportion
        
        # --- Timeline-based progress estimation ---
        Duration_days = as.numeric(End_Date - Start_Date),         # Total duration of the task
        Elapsed_days  = as.numeric(Sys.Date() - Start_Date),       # How many days have passed
        Expected_Progress = ifelse(Duration_days > 0, Elapsed_days / Duration_days, NA),  # % of time elapsed
        
        # --- Business logic for schedule health ---
        # This logic uses status, actual vs expected progress, and timelines
        Schedule_Health = case_when(
          # ✅ Completed tasks or 100% done are always on track
          Status_clean %in% c("completed", "done") | Progress >= 1 ~ "On Track",
          
          # ❓ If we can't calculate progress or timeline, mark as unknown
          is.na(Expected_Progress) | is.na(Progress) ~ "Unknown",
          
          # 🔴 Significantly behind schedule (more than 20% behind)
          Expected_Progress - Progress > 0.2 ~ "Delayed",
          
          # 🟠 Slightly behind (0–20% behind)
          Expected_Progress - Progress > 0 ~ "At Risk",
          
          # ✅ Otherwise, you're on schedule
          TRUE ~ "On Track"
        )
      )
    
    
    
    # Step 3: Group and summarize
    project_summary <- data %>%
      group_by(Wave, `short name`) %>%
      summarise(
        avg_progress = mean(`% Complete`, na.rm = TRUE),
        Schedule_Health = names(which.max(table(Schedule_Health))),
        .groups = "drop"
      )
    
    # Step 4: Plot
    ggplot(project_summary, aes(x = reorder(`short name`, avg_progress), y = avg_progress, fill = Schedule_Health)) +
      geom_col() +
      coord_flip() +
      facet_wrap(~ Wave, scales = "free_y", nrow = 1) +
      scale_fill_manual(
        values = c("On Track" = "forestgreen", "At Risk" = "orange", "Delayed" = "firebrick", "Unknown" = "gray")
      ) +
      labs(
        title = "Project Completion Progress by Wave",
        x = "Project",
        y = "Average % Complete",
        fill = "Schedule Health"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        axis.text.y = element_text(size = 9, face = "bold"),
        legend.position = "bottom"
      )
  })
  
  
  # --- Plot: Health Levels by Assignee ---
  steering_gantt_theme <- theme_minimal(base_size = 13) +
    theme(
      panel.background = element_rect(fill = "#f8f8f8"),
      plot.background = element_rect(fill = "#f8f8f8"),
      panel.grid.major.x = element_line(color = "gray80"),
      panel.grid.major.y = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.text.y = element_text(face = "bold"),
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      legend.title = element_text(face = "bold"),
      legend.position = "bottom"
    )
  
  
  output$riskLevelsByAssigneePlot <- renderPlot({
    req(tasks())
    data <- tasks()
    if (nrow(data) == 0) return(NULL)
    
    data <- data %>%
      mutate(
        # Correct Progress parsing
        `% Complete` = suppressWarnings(readr::parse_number(as.character(`% Complete`))),
        
        Progress = case_when(
          `% Complete` > 1 ~ `% Complete` / 100,  # If looks like 80%, convert
          TRUE ~ `% Complete`                     # If already 0.8, keep
        ),
        
        # Date parsing
        Start_Date = as.Date(`Start Date`, tryFormats = c("%Y-%m-%d", "%d/%m/%Y")),
        End_Date   = as.Date(`End Date`, tryFormats = c("%Y-%m-%d", "%d/%m/%Y")),
        
        # Status cleaning
        Status_clean = tolower(trimws(Status)),
        
        # Progress expectation based on timeline
        Duration_days = as.numeric(End_Date - Start_Date),
        Elapsed_days = as.numeric(pmin(Sys.Date(), End_Date) - Start_Date),
        Expected_Progress = ifelse(Duration_days > 0, pmax(0, pmin(1, Elapsed_days / Duration_days)), NA),
        
        # Business logic for Schedule_Health
        Schedule_Health = case_when(
          Status_clean %in% c("completed", "done", "complete") | Progress >= 1 ~ "On Track",
          is.na(Expected_Progress) | is.na(Progress) ~ "Unknown",
          Expected_Progress - Progress > 0.2 ~ "Delayed",
          Expected_Progress - Progress > 0 ~ "At Risk",
          TRUE ~ "On Track"
        ),
        
        # Force Schedule_Health factor order
        Schedule_Health = factor(Schedule_Health, levels = c("On Track", "At Risk", "Delayed", "Unknown")),
        
        # Extra fields for downstream plotting
        Assignee = `Assigned To`,
        Project = `short name`,
        Wave = as.character(Wave)
      ) %>%
      filter(
        !is.na(Project),
        !is.na(Start_Date),
        !is.na(End_Date),
        !is.na(Assignee)
      )
    
    
    
  
    gantt_theme <- theme_minimal(base_size = 12) +
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#f7f7f7", color = NA),
        strip.background = element_rect(fill = "#003366"),
        strip.text = element_text(color = "white", face = "bold"),
        axis.text.y = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = "bottom"
      )
    
    ggplot(data, aes(y = reorder(Project, Start_Date))) +
      geom_segment(aes(x = Start_Date, xend = End_Date, yend = Project, color = Schedule_Health), linewidth = 6) +
      geom_text(aes(x = End_Date + 5, label = Wave), size = 3, hjust = 0, fontface = "bold", color = "gray40") +
      
      # ✅ Add today's date marker
      geom_vline(xintercept = as.numeric(Sys.Date()), linetype = "dashed", color = "red", linewidth = 1) +
      annotate("text", x = Sys.Date(), y = 1, label = "Today", vjust = -1, color = "red", fontface = "bold", angle = 90) +
      
      scale_color_manual(
        values = c("On Track" = "forestgreen", "At Risk" = "orange", "Delayed" = "firebrick", "Unknown" = "gray")
      ) +
      facet_wrap(~ Assignee, scales = "free_y", ncol = 2) +
      scale_x_date(
        name = "Timeline",
        breaks = scales::pretty_breaks(n = 6),
        date_labels = "%b %Y"
      ) +
     # labs(
     #   title = "Task Health by Project and Assignee",
     #   y = "Project",
     #   color = "Schedule Health"
     # ) +
      
      labs(
        title = "Task Health by Project Manager (with Business Logic)",
        x = NULL, 
        y = NULL, 
        fill = "Schedule Health",
        caption = "🟢 On Track: Completed or as expected | 🟠 At Risk: ≤20% behind | 🔴 Delayed: >20% behind | ⚪ Unknown: Missing data"
      ) +
      coord_cartesian(clip = "off") +
      gantt_theme
  }, height = function() {
    assignees <- unique(tasks()$`Assigned To`)
    rows_needed <- ceiling(length(assignees) / 2)
    plot_height <- 300 + rows_needed * 320
    return(min(plot_height, 3000))
  })
  
  
  
  # --- Table: PM Recommendations ---
  output$heatmapTable <- renderDT({
    data <- filtered_data()
    req(nrow(data) > 0)
    
    # --- Clean and prepare ---
    data <- data %>%
      mutate(
        Progress = suppressWarnings(readr::parse_number(as.character(`% Complete`)) / 100),
        Start_Date = as.Date(`Start Date`, tryFormats = c("%Y-%m-%d", "%d/%m/%Y")),
        End_Date = as.Date(`End Date`, tryFormats = c("%Y-%m-%d", "%d/%m/%Y")),
        Duration_days = as.numeric(End_Date - Start_Date),
        Elapsed_days = as.numeric(pmin(Sys.Date(), End_Date) - Start_Date),
        Expected_Progress = ifelse(Duration_days > 0, pmax(0, pmin(1, Elapsed_days / Duration_days)), NA),
        Status_clean = tolower(trimws(Status))
      )
    
    # --- Build PM Recommendations Table ---
    recommendations <- data %>%
      filter(!is.na(`Assigned To`), !is.na(`short name`)) %>%
      group_by(`Assigned To`, `short name`) %>%
      summarize(
        Actual_Progress = round(mean(Progress, na.rm = TRUE) * 100, 1),
        Expected_Progress = round(mean(Expected_Progress, na.rm = TRUE) * 100, 1),
        Gap = round(Expected_Progress - Actual_Progress, 1),
        Active_Phase = first(`Active Phase`),
        Status = first(Status_clean),
        .groups = "drop"
      ) %>%
      mutate(
        Recommendation = case_when(
          Status %in% c("completed", "done") ~ "✅ Project completed.",
          is.na(Expected_Progress) | is.na(Actual_Progress) ~ "❓ Data incomplete.",
          Gap < -20 ~ paste0("🚀 Ahead by ", abs(Gap), "%, good job!"),
          Gap > 20 ~ paste0("⚠️ Behind by ", Gap, "%. Focus on accelerating '", Active_Phase, "' phase."),
          TRUE ~ "✅ On track, keep momentum!"
        )
      ) %>%
      select(
        `Project Manager` = `Assigned To`,
        `Project` = `short name`,
        `Actual Progress (%)` = Actual_Progress,
        `Expected Progress (%)` = Expected_Progress,
        `Progress Gap (%)` = Gap,
        `Active Phase` = Active_Phase,
        `Recommendation`
      )
    
    # --- Render as datatable nicely ---
    datatable(
      recommendations,
      options = list(
        pageLength = 10,
        autoWidth = TRUE,
        scrollX = TRUE,
        rowCallback = JS(
          "function(row, data) {",
          "  if (data[5] == 'Closure') { $('td:eq(5)', row).css('color', 'green'); }",
          "  if (data[5] == 'Procurement') { $('td:eq(5)', row).css('color', 'orange'); }",
          "  if (data[5] == 'Implementation') { $('td:eq(5)', row).css('color', 'blue'); }",
          "  if (data[5] == 'Assessment') { $('td:eq(5)', row).css('color', 'purple'); }",
          "}"
        )
      ),
      rownames = FALSE
    )
  })
  
  
  output$downloadPMReport <- downloadHandler(
    filename = function() {
      paste0("PM_Recommendations_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      library(dplyr)
      library(openxlsx)
      
      data <- filtered_data()
      
      recommendations <- data %>%
        mutate(
          Progress = suppressWarnings(readr::parse_number(as.character(`% Complete`)) / 100),
          Start_Date = as.Date(`Start Date`, tryFormats = c("%Y-%m-%d", "%d/%m/%Y")),
          End_Date = as.Date(`End Date`, tryFormats = c("%Y-%m-%d", "%d/%m/%Y")),
          Duration_days = as.numeric(End_Date - Start_Date),
          Elapsed_days = as.numeric(pmin(Sys.Date(), End_Date) - Start_Date),
          Expected_Progress = ifelse(Duration_days > 0, pmax(0, pmin(1, Elapsed_days / Duration_days)), NA),
          Status_clean = tolower(trimws(Status))
        ) %>%
        group_by(`Assigned To`, `short name`) %>%
        summarize(
          `Actual Progress (%)` = round(mean(Progress, na.rm = TRUE) * 100, 1),
          `Expected Progress (%)` = round(mean(Expected_Progress, na.rm = TRUE) * 100, 1),
          `Progress Gap (%)` = round(`Expected Progress (%)` - `Actual Progress (%)`, 1),
          `Active Phase` = first(`Active Phase`),
          Status = first(Status_clean),
          .groups = "drop"
        ) %>%
        mutate(
          Recommendation = case_when(
            Status %in% c("completed", "done") ~ "✅ Project completed.",
            is.na(`Expected Progress (%)`) | is.na(`Actual Progress (%)`) ~ "❓ Data incomplete.",
            `Progress Gap (%)` < -20 ~ paste0("🚀 Ahead by ", abs(`Progress Gap (%)`), "% - Excellent!"),
            `Progress Gap (%)` > 20 ~ paste0("⚠️ Behind by ", `Progress Gap (%)`, "% - Focus on '", `Active Phase`, "'!"),
            TRUE ~ "✅ On track, maintain momentum."
          )
        ) %>%
        rename(
          `Project Manager` = `Assigned To`,
          `Project Name` = `short name`
        )
      
      ## --- Create Workbook ---
      wb <- createWorkbook()
      
      addWorksheet(wb, "PM Recommendations")
      
      # Write timestamp
      writeData(wb, sheet = 1, x = paste("Generated on:", Sys.Date()), startRow = 1, startCol = 1)
      
      # Write Table
      writeDataTable(wb, sheet = 1, x = recommendations, startRow = 3, startCol = 1, tableStyle = "TableStyleMedium9")
      
      ## --- Styling ---
      setColWidths(wb, 1, cols = 1:ncol(recommendations), widths = "auto")
      freezePane(wb, 1, firstRow = TRUE)
      
      # Style header
      headerStyle <- createStyle(
        fontSize = 12, fontColour = "white", fgFill = "#003366",
        halign = "center", valign = "center", textDecoration = "bold", border = "Bottom"
      )
      addStyle(wb, 1, headerStyle, rows = 3, cols = 1:ncol(recommendations), gridExpand = TRUE)

      # --- Correct Conditional Formatting ---
      # Highlight delayed or ahead rows based on recommendation
      
      # Apply red for rows where the Recommendation has ⚠️ (Behind)
      conditionalFormatting(
        wb, sheet = 1,
        cols = 1:ncol(recommendations), rows = 4:(nrow(recommendations) + 3),
        rule = 'ISNUMBER(SEARCH("⚠️", $G4))',
        style = createStyle(bgFill = "#ffe5e5") # Light Red
      )
      
      # Apply green for rows where the Recommendation has 🚀 (Ahead)
      conditionalFormatting(
        wb, sheet = 1,
        cols = 1:ncol(recommendations), rows = 4:(nrow(recommendations) + 3),
        rule = 'ISNUMBER(SEARCH("🚀", $G4))',
        style = createStyle(bgFill = "#e6ffe6") # Light Green
      )
      
      
      # Save workbook
      openxlsx::writeData(wb, sheet = 1, recommendations, startRow = 4, colNames = TRUE)
      openxlsx::addStyle(wb, sheet = 1, style = headerStyle, rows = 4, cols = 1:ncol(recommendations), gridExpand = TRUE)
      
      # 👉 Insert the conditional formatting block here 👆
      
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      
    }
  )
  
  
  
  
}
