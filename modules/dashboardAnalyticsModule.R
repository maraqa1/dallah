dashboardAnalyticsUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    tags$style(HTML("
      .value-box-small h4 { font-size: 20px; margin: 5px 0; }
      .value-box-small p { font-size: 12px; margin: 0; }
      .section-divider { border-top: 3px solid #007bff; margin-top: 30px; margin-bottom: 30px; }
    ")),
    
    h3("Dashboard Analytics", style = "font-weight: bold; text-align: center; margin-bottom: 20px;"),
    div(class = "section-divider"),
    
    # Filters
    fluidRow(
      column(6, selectInput(ns("projectFilter"), "Filter by Project:", choices = NULL, selected = "All")),
      column(6, selectInput(ns("assigneeFilter"), "Filter by Assignee:", choices = NULL, selected = "All"))
    ),
    
    div(class = "section-divider"),
    
    # Value Boxes - Smaller
    fluidRow(
      column(2, div(class = "value-box-small", uiOutput(ns("totalTasksBox")))),
      column(2, div(class = "value-box-small", uiOutput(ns("blockedTasksBox")))),
      column(2, div(class = "value-box-small", uiOutput(ns("highRiskTasksBox")))),
      column(2, div(class = "value-box-small", uiOutput(ns("overdueTasksBox")))),
      column(2, div(class = "value-box-small", uiOutput(ns("inProgressTasksBox")))),
      column(2, div(class = "value-box-small", uiOutput(ns("averageProgressBox"))))
    ),
    
    div(class = "section-divider"),
    
    # Visual Insights
    h4("Overall Progress for each project", style = "font-weight: bold; text-align: center; margin-bottom: 20px;"),
    
    fluidRow(
      column(12,
             plotOutput(ns("progressByProjectPlot"), height = "500px")
      )
    ),
    
    br(),
    br(),
    
    div(class = "section-divider"),
    
    # Visual Insights
    h4("Progress for each PM", style = "font-weight: bold; text-align: center; margin-bottom: 20px;"),
    
    fluidRow(
      column(12,
             div(
               style = "overflow-x: auto; overflow-y: auto; max-height: 800px; margin-bottom: 40px;",
               plotOutput(ns("riskLevelsByAssigneePlot"), height = "auto")
             )
             
      )
    ),
    
    div(class = "section-divider"),
    
    fluidRow(
      column(12,
             downloadButton(ns("downloadPMReport"), "Download PM Recommendations", class = "btn btn-primary", style = "margin-bottom:20px;")
      )
    ),
    
    hr(style = "border-top: 3px solid #003366; margin-top: 40px;"),
    
    h4("PM Recommendation Report", style = "font-weight: bold; text-align: center; margin-top: 20px;"),
    fluidRow(
      column(12,
             div(style = "margin-top: 20px;",
                 dataTableOutput(ns("heatmapTable"))
             )
      )
    )
  )
}

dashboardAnalyticsServer <- function(input, output, session, tasks) {
  ns <- session$ns
  
  
  # ───────────────────────────────
  # Value Box Styling Helper
  # ───────────────────────────────
  value_box_style <- function(color) {
    paste0(
      "background-color:", color, ";",
      "color:white;",
      "padding:10px 15px;",
      "border-radius:8px;",
      "text-align:center;",
      "box-shadow:0 2px 6px rgba(0,0,0,0.1);",
      "font-size:14px;"
    )
  }
  
  
  # Update filter options
  observe({
    req(tasks())
    data <- tasks()
    updateSelectInput(session, "projectFilter", choices = c("All", unique(data$`short name`)))
    updateSelectInput(session, "assigneeFilter", choices = c("All", unique(data$`Assigned To`)))
  })
  
  # Reactive filtered data
  filtered_data <- reactive({
    req(tasks())
    data <- tasks()
    
    if (!is.null(input$projectFilter) && input$projectFilter != "All") {
      data <- data[data$`short name` == input$projectFilter, ]
    }
    if (!is.null(input$assigneeFilter) && input$assigneeFilter != "All") {
      data <- data[data$`Assigned To` == input$assigneeFilter, ]
    }
    
    data$`% Complete` <- suppressWarnings(readr::parse_number(as.character(data$`% Complete`))) * 100
    data
  })
  
  # Total Tasks
  output$totalTasksBox <- renderUI({
    div(style = value_box_style("#007bff"),
        h6("Total Tasks", style = "margin-bottom:4px;"),
        h4(nrow(filtered_data()), style = "font-weight:bold;"))
  })
  
  # Critical Health
  output$blockedTasksBox <- renderUI({
    n <- sum(tolower(trimws(filtered_data()$Health)) == "red", na.rm = TRUE)
    div(style = value_box_style("#dc3545"),
        h6("Critical Health", style = "margin-bottom:4px;"),
        h4(n, style = "font-weight:bold;"))
  })
  
  # Medium Risk
  output$highRiskTasksBox <- renderUI({
    n <- sum(tolower(trimws(filtered_data()$Health)) == "amber", na.rm = TRUE)
    div(style = value_box_style("#fd7e14"),
        h6("Medium Risk", style = "margin-bottom:4px;"),
        h4(n, style = "font-weight:bold;"))
  })
  
  # Overdue Tasks
  output$overdueTasksBox <- renderUI({
    data <- filtered_data()
    start <- suppressWarnings(as.Date(data$`Start Date`, tryFormats = c("%Y-%m-%d", "%d/%m/%Y")))
    end   <- suppressWarnings(as.Date(data$`End Date`, tryFormats = c("%Y-%m-%d", "%d/%m/%Y")))
    status <- tolower(trimws(data$Status))
    overdue <- ((!is.na(start) & start < Sys.Date() & status == "not started") |
                  (!is.na(end) & end < Sys.Date() & status != "completed"))
    div(style = value_box_style("#ffc107"),
        h6("Overdue", style = "margin-bottom:4px;"),
        h4(sum(overdue, na.rm = TRUE), style = "font-weight:bold;"))
  })
  
  # In Progress
  output$inProgressTasksBox <- renderUI({
    n <- sum(tolower(trimws(filtered_data()$Status)) == "in progress", na.rm = TRUE)
    div(style = value_box_style("#17a2b8"),
        h6("In Progress", style = "margin-bottom:4px;"),
        h4(n, style = "font-weight:bold;"))
  })
  
  # Average Completion
  output$averageProgressBox <- renderUI({
    avg <- round(mean(filtered_data()$`% Complete`, na.rm = TRUE), 1)
    div(style = value_box_style("#28a745"),
        h6("Avg. Completion", style = "margin-bottom:4px;"),
        h4(paste0(avg, "%"), style = "font-weight:bold;"))
  })

    
  # Plot: Progress by Project
  output$progressByProjectPlot <- renderPlot({
    data <- filtered_data()
    req(nrow(data) > 0)
    
    data$`% Complete` <- suppressWarnings(readr::parse_number(as.character(data$`% Complete`)))
    
    data <- data %>%
      mutate(
        Start_Date = as.Date(`Start Date`, tryFormats = c("%Y-%m-%d", "%d/%m/%Y", "%m/%d/%Y")),
        End_Date = as.Date(`End Date`, tryFormats = c("%Y-%m-%d", "%d/%m/%Y", "%m/%d/%Y")),
        Progress = `% Complete` / 100,
        Status_clean = tolower(trimws(Status)),
        Duration_days = as.numeric(End_Date - Start_Date),
        Elapsed_days = as.numeric(Sys.Date() - Start_Date),
        Expected_Progress = ifelse(Duration_days > 0, Elapsed_days / Duration_days, NA),
        Schedule_Health = case_when(
          Status_clean %in% c("completed", "done") | Progress >= 1 ~ "On Track",
          is.na(Expected_Progress) | is.na(Progress) ~ "Unknown",
          Expected_Progress - Progress > 0.2 ~ "Delayed",
          Expected_Progress - Progress > 0 ~ "At Risk",
          TRUE ~ "On Track"
        )
      )
    
    project_summary <- data %>%
      group_by(Wave, `short name`) %>%
      summarise(
        avg_progress = mean(`% Complete`, na.rm = TRUE),
        Schedule_Health = names(which.max(table(Schedule_Health))),
        .groups = "drop"
      )
    
    ggplot(project_summary, aes(x = reorder(`short name`, avg_progress), y = avg_progress, fill = Schedule_Health)) +
      geom_col() +
      coord_flip() +
      facet_wrap(~ Wave, scales = "free_y") +
      scale_fill_manual(values = c(
        "On Track" = "forestgreen",
        "At Risk" = "orange",
        "Delayed" = "firebrick",
        "Unknown" = "gray"
      )) +
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
        strip.text = element_text(face = "bold"),
        legend.position = "bottom",
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "grey90")
      ) +
       theme_dashboard()
  }, height = function() {
    num_projects <- nrow(filtered_data())
    height <- min(600, 100 + num_projects * 20)
    max(height, 300)
  })
  
  output$riskLevelsByAssigneePlot <- renderPlot({
    data <- filtered_data()
    req(nrow(data) > 0)
    
    data <- data %>%
      mutate(
        `% Complete` = suppressWarnings(readr::parse_number(as.character(`% Complete`))),
        Progress = ifelse(`% Complete` > 1, `% Complete` / 100, `% Complete`),
        Start_Date = as.Date(`Start Date`, tryFormats = c("%Y-%m-%d", "%d/%m/%Y")),
        End_Date = as.Date(`End Date`, tryFormats = c("%Y-%m-%d", "%d/%m/%Y")),
        Duration_days = as.numeric(End_Date - Start_Date),
        Elapsed_days = as.numeric(pmin(Sys.Date(), End_Date) - Start_Date),
        Expected_Progress = ifelse(Duration_days > 0, pmax(0, pmin(1, Elapsed_days / Duration_days)), NA),
        Status_clean = tolower(trimws(Status)),
        Schedule_Health = case_when(
          Status_clean %in% c("completed", "done") | Progress >= 1 ~ "On Track",
          is.na(Expected_Progress) | is.na(Progress) ~ "Unknown",
          Expected_Progress - Progress > 0.2 ~ "Delayed",
          Expected_Progress - Progress > 0 ~ "At Risk",
          TRUE ~ "On Track"
        ),
        Assignee = `Assigned To`,
        Project = `short name`,
        Wave = as.character(Wave),
        Phase = `Active Phase`,
        Progress_Label = paste0(round(Progress * 100), "%"),
        Health_Icon = case_when(
          Schedule_Health == "On Track" ~ "🟢",
          Schedule_Health == "At Risk" ~ "🟠",
          Schedule_Health == "Delayed" ~ "🔴",
          TRUE ~ "⚫"
        ),
        Badge_Label = paste(Health_Icon, Progress_Label, "|", Phase)
      ) %>%
      filter(!is.na(Project), !is.na(Start_Date), !is.na(End_Date), !is.na(Assignee))
    
    ggplot(data, aes(y = reorder(Project, Start_Date))) +
      geom_segment(aes(x = Start_Date, xend = End_Date, yend = Project, color = Schedule_Health), linewidth = 5) +
      geom_text(aes(x = End_Date + 5, label = Badge_Label), hjust = 0, size = 3.5, fontface = "bold") +
      
      geom_vline(xintercept = as.numeric(Sys.Date()), linetype = "dashed", color = "red", linewidth = 1) +
      annotate("text", x = Sys.Date(), y = 1, label = "Today", vjust = -1, color = "red", fontface = "bold", angle = 90) +
      
      facet_wrap(~ Assignee, scales = "free_y", ncol = 2) +
      scale_color_manual(values = c(
        "On Track" = "forestgreen",
        "At Risk"  = "orange",
        "Delayed"  = "firebrick",
        "Unknown"  = "gray"
      )) +
      scale_x_date(date_labels = "%b %Y") +
      labs(
        title = "Task Health by Project Manager (Progress & Phase)",
        y = NULL,
        x = NULL,
        color = "Schedule Health"
      ) +
      theme_dashboard()  # Apply your centralized theme
  }, height = function() {
    assignees <- unique(filtered_data()$`Assigned To`)
    rows_needed <- ceiling(length(assignees) / 2)
    height <- 350 + rows_needed * 280
    max(height, 600)
  })
  
  
  output$heatmapTable <- renderDT({
    data <- filtered_data()
    req(nrow(data) > 0)
    
    summary <- data %>%
      mutate(
        Progress = suppressWarnings(readr::parse_number(as.character(`% Complete`))) / 100,
        Start = as.Date(`Start Date`, tryFormats = c("%Y-%m-%d", "%d/%m/%Y")),
        End   = as.Date(`End Date`, tryFormats = c("%Y-%m-%d", "%d/%m/%Y")),
        Duration = as.numeric(End - Start),
        Elapsed = as.numeric(pmin(Sys.Date(), End) - Start),
        Expected = ifelse(Duration > 0, Elapsed / Duration, NA)
      ) %>%
      group_by(`Assigned To`, `short name`) %>%
      summarise(
        Actual_Progress = round(mean(Progress, na.rm = TRUE) * 100, 1),
        Expected_Progress = round(mean(Expected, na.rm = TRUE) * 100, 1),
        Gap = round(Expected_Progress - Actual_Progress, 1),
        Phase = first(`Active Phase`),
        Recommendation = case_when(
          is.na(Expected_Progress) | is.na(Actual_Progress) ~ "❓ Incomplete",
          Gap > 20 ~ paste("⚠️ Behind by", Gap, "%"),
          Gap < -20 ~ paste("🚀 Ahead by", abs(Gap), "%"),
          TRUE ~ "✅ On Track"
        ),
        
     
          Recommendation = case_when(
            Status %in% c("completed", "done") ~ "✅ Project completed.",
            is.na(Expected_Progress) | is.na(Actual_Progress) ~ "❓ Data incomplete.",
            Gap > 20 ~ paste0("⚠️ Behind by ", Gap, "%. Focus on accelerating '", Phase, "' phase."),
            Gap < -20 ~ paste0("🚀 Ahead by ", abs(Gap), "%. Excellent progress!"),
            TRUE ~ "✅ On track, keep momentum!"
          ),
          
        
        .groups = "drop"
      ) %>%
      rename(
        `Project Manager` = `Assigned To`,
        `Project` = `short name`,
        `Actual Progress (%)` = Actual_Progress,
        `Expected Progress (%)` = Expected_Progress,
        `Progress Gap (%)` = Gap,
        `Active Phase` = Phase
      )
    
    datatable(
      summary,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        rowCallback = JS(
          "function(row, data) {",
          "  if (parseFloat(data[4]) > 20) { $('td:eq(4)', row).css('color', 'red'); }",
          "  if (parseFloat(data[4]) < -20) { $('td:eq(4)', row).css('color', 'green'); }",
          "}"
        )
      ),
      rownames = FALSE
    )
  })
}
