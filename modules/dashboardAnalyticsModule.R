dashboardAnalyticsUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    # --- PROFESSIONAL TITLE SECTION ---
    div(
      style = "margin-bottom:18px;",
      tags$h2(
        "Portfolio Project Progress Dashboard",
        style = "font-weight:800; color:#1A237E; letter-spacing:1px; margin-bottom:3px; margin-top:8px; line-height:1.1; font-family: 'Segoe UI', Arial, sans-serif;"
      ),
      tags$div(
        "Track real-time progress, risk, and schedule health across all key projects.",
        style = "font-size:18px; color:#555; font-weight:400; margin-bottom:3px; margin-left:2px; font-family: 'Segoe UI', Arial, sans-serif;"
      ),
      tags$hr(style = "border-top: 3px solid #1A237E; width: 90%; margin-top:6px; margin-bottom:16px;")
    ),
    
    tags$style(HTML("
      .value-box-small h4 { font-size: 20px; margin: 5px 0; }
      .value-box-small p { font-size: 12px; margin: 0; }
      .section-divider { border-top: 3px solid #007bff; margin-top: 30px; margin-bottom: 30px; }
    ")),
    
    # FILTERS
    fluidRow(
      column(6, selectInput(ns("projectFilter"), "Filter by Project:", choices = NULL, selected = "All")),
      column(6, selectInput(ns("assigneeFilter"), "Filter by Assignee:", choices = NULL, selected = "All"))
    ),
    
    div(class = "section-divider"),
    
    # VALUE BOXES
    fluidRow(
      column(2, div(class = "value-box-small", uiOutput(ns("totalTasksBox")))),
      column(2, div(class = "value-box-small", uiOutput(ns("blockedTasksBox")))),
      column(2, div(class = "value-box-small", uiOutput(ns("highRiskTasksBox")))),
      column(2, div(class = "value-box-small", uiOutput(ns("overdueTasksBox")))),
      column(2, div(class = "value-box-small", uiOutput(ns("inProgressTasksBox")))),
      column(2, div(class = "value-box-small", uiOutput(ns("averageProgressBox"))))
    ),
    
    div(class = "section-divider"),
    
    # MAIN PROGRESS PLOT & GUIDANCE
    h4("Overall Progress for Each Project", style = "font-weight: bold; text-align: center; margin-bottom: 20px;"),
    fluidRow(
      column(
        width = 2,
        div(
          style = "background: #f5f7fa; border-radius: 10px; border: 1px solid #d0dae5; padding: 18px; margin-right: 20px; color: #212529; font-size: 12px;",
          tags$strong("How to Read This Chart"),
          tags$ul(
            tags$li("The % at right is the gap between expected progress (◆) and actual completion today."),
            tags$li("Positive gap: Behind schedule. Negative gap: Ahead of schedule."),
            tags$li(tags$span(style = "color:#d7263d;font-weight:bold;", "🚩 Large positive gap (+50% or more): Escalate—far behind schedule.")),
            tags$li(tags$span(style = "color:#fdae1a;font-weight:bold;", "⚠️ Small positive gap (+10% to +50%): Monitor and request a catch-up plan.")),
            tags$li(tags$span(style = "color:#28a745;font-weight:bold;", "✅ Near zero gap (–10% to +10%): On track.")),
            tags$li(tags$span(style = "color:#1695a3;font-weight:bold;", "⏩ Negative gap (–10% or less): Ahead. Confirm, consider resource reallocation."))
          ),
          tags$p("◆ = Where project should be today (expected progress).", style="margin-bottom:0;"),
          tags$p("Gap = Expected – Actual.", style="margin-bottom:0;")
        )
      ),
      column(
        width = 10,
        plotOutput(ns("progressByProjectPlot"), height = "600px"),
        tags$hr()
      )
    ),
    
    div(class = "section-divider"),
    
    # PROGRESS BY PM
    h4("Progress for Each Project Manager", style = "font-weight: bold; text-align: center; margin-bottom: 20px;"),
    fluidRow(
      column(12,
             div(
               style = "overflow-x: auto; overflow-y: auto; max-height: 800px; margin-bottom: 40px;",
               plotOutput(ns("riskLevelsByAssigneePlot"), height = "auto")
             )
      )
    ),
    
    div(class = "section-divider"),
    
    # DOWNLOAD BUTTON
    fluidRow(
      column(12,
             downloadButton(ns("downloadPMReport"), "Download PM Recommendations", class = "btn btn-primary", style = "margin-bottom:20px;")
      )
    ),
    
    hr(style = "border-top: 3px solid #003366; margin-top: 40px;"),
    
    # STYLED TABLES
    tags$style(HTML("
      table.dataTable {
        font-size: 12px;
        font-family: 'Segoe UI', sans-serif;
      }
      table.dataTable tbody td {
        padding: 6px 10px;
      }
      table.dataTable thead th {
        background-color: #003366;
        color: white;
        font-weight: bold;
        text-align: center;
      }
      table.dataTable tbody tr:nth-child(odd) {
        background-color: #f9f9f9;
      }
      table.dataTable tbody tr:nth-child(even) {
        background-color: #ffffff;
      }
    ")),
    
    h4("PM Recommendation Report", style = "font-weight: bold; text-align: center; margin-top: 20px;"),
    fluidRow(
      column(12,
             div(style = "margin-top: 20px;",
                 DTOutput(ns("heatmapTable"))  # use DTOutput for consistency with DT package
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
        avg_expected_progress = mean(Expected_Progress * 100, na.rm = TRUE),
        Schedule_Health = names(which.max(table(Schedule_Health))),
        .groups = "drop"
      ) %>%
      mutate(
        progress_gap = avg_expected_progress - avg_progress
      )
    
    ggplot(project_summary, aes(x = reorder(`short name`, avg_progress), y = avg_progress, fill = Schedule_Health)) +
      geom_col(width = 0.6) +
      geom_point(aes(y = avg_expected_progress), color = "deepskyblue", size = 4, shape = 18) +
      geom_label(
        aes(y = 105, 
            label = ifelse(!is.na(progress_gap), sprintf("%+d%%  ", round(progress_gap)), ""),
            fill = case_when(
              progress_gap > 30  ~ "Delayed",
              progress_gap > 10  ~ "At Risk",
              progress_gap > -10 ~ "On Track",
              TRUE               ~ "Ahead"
            )
        ),
        color = "white", fontface = "bold", size = 3.5, hjust = 0
      ) +
      coord_flip() +
      facet_wrap(~ Wave, scales = "free_y") +
      scale_fill_manual(values = c(
        "On Track" = "forestgreen",
        "At Risk" = "orange",
        "Delayed" = "firebrick",
        "Unknown" = "gray",
        "Ahead" = "deepskyblue"
      ), guide = "none") +
      ylim(0, 110) +    # Clamp axis at 0-110%
      labs(
        title = "Project Completion Progress vs. Expected (with Gap)",
        x = "Project",
        y = "Average % Complete",
        fill = "Schedule Health",
        caption = "Badge = schedule gap (expected – actual): red = delayed, orange = at risk, green = on track, blue = ahead."
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
  
  
  # Plot: Progress by Project
  output$progressByProjectPlot_1 <- renderPlot({
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
    library(ggtext)  # Needed for geom_richtext
    
    data <- filtered_data()
    req(nrow(data) > 0)
    
    today <- Sys.Date()
    
    data <- data %>%
      mutate(
        `% Complete` = suppressWarnings(readr::parse_number(as.character(`% Complete`))),
        Progress = ifelse(`% Complete` > 1, `% Complete` / 100, `% Complete`),
        Start_Date = as.Date(`Start Date`, tryFormats = c("%Y-%m-%d", "%d/%m/%Y")),
        End_Date = as.Date(`End Date`, tryFormats = c("%Y-%m-%d", "%d/%m/%Y")),
        Duration_days = as.numeric(End_Date - Start_Date),
        Elapsed_days = as.numeric(pmin(today, End_Date) - Start_Date),
        Expected_Progress = ifelse(Duration_days > 0, pmax(0, pmin(1, Elapsed_days / Duration_days)), NA),
        Status_clean = tolower(trimws(Status)),
        Schedule_Health = case_when(
          Status_clean %in% c("completed", "done") | Progress >= 1 ~ "On Track",
          is.na(Expected_Progress) | is.na(Progress) ~ "Unknown",
          Expected_Progress - Progress > 0.2 ~ "Delayed",
          Expected_Progress - Progress > 0 ~ "At Risk",
          TRUE ~ "On Track"
        ),
        Is_Blocked = Status_clean == "blocked",
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
        Blocked_Icon = ifelse(Is_Blocked, "<span style='color:red;font-weight:bold;'>🔒 BLOCKED</span>", ""),
        Badge_Label = paste(Health_Icon, Progress_Label, "|", Phase, Blocked_Icon)
      ) %>%
      filter(!is.na(Project), !is.na(Start_Date), !is.na(End_Date), !is.na(Assignee))
    
    ggplot(data, aes(y = reorder(Project, Start_Date))) +
      geom_segment(aes(x = Start_Date, xend = End_Date, yend = Project, color = Schedule_Health), linewidth = 5) +
      
      # Use geom_richtext instead of geom_text for colored label
      geom_richtext(
        aes(x = End_Date + 5, label = Badge_Label),
        hjust = 0, size = 3.5, fontface = "bold",
        fill = NA, label.color = NA  # transparent background and no border
      ) +
      
      geom_vline(xintercept = as.numeric(today), linetype = "dashed", color = "red", linewidth = 1) +
      annotate("text", x = today, y = 1, label = "Today", vjust = -1, color = "red", fontface = "bold", angle = 90) +
      
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
      theme_dashboard()
  }, height = function() {
    assignees <- unique(filtered_data()$`Assigned To`)
    rows_needed <- ceiling(length(assignees) / 2)
    height <- 350 + rows_needed * 280
    max(height, 600)
  })
  
  
    
  output$riskLevelsByAssigneePlot_old <- renderPlot({
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
  
  
  # In your SERVER code
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
      group_by(`Assigned To`, `short name`, Code) %>%
      summarise(
        Actual_Progress = round(mean(Progress, na.rm = TRUE) * 100, 1),
        Expected_Progress = round(mean(Expected, na.rm = TRUE) * 100, 1),
        Gap = round(Expected_Progress - Actual_Progress, 1),
        Phase = first(`Active Phase`),
        Status = first(Status),
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
      ) %>%
      select(
        `Project Manager`, Code, Project,
        `Actual Progress (%)`, `Expected Progress (%)`, `Progress Gap (%)`,
        `Active Phase`, Recommendation
      )
    datatable(
      summary,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'tip',
        rowCallback = JS(
          "function(row, data) {",
          "  if (parseFloat(data[5]) > 20) { $('td:eq(5)', row).css({'color': '#b30000', 'font-weight': 'bold'}); }",
          "  if (parseFloat(data[5]) < -20) { $('td:eq(5)', row).css({'color': 'darkgreen', 'font-weight': 'bold'}); }",
          "}"
        )
      ),
      rownames = FALSE,
      class = 'compact stripe hover'
    ) %>%
      formatStyle(
        columns = c("Actual Progress (%)", "Expected Progress (%)", "Progress Gap (%)"),
        `text-align` = 'center'
      ) %>%
      formatStyle(
        columns = c("Project Manager", "Code", "Project", "Active Phase", "Recommendation"),
        `text-align` = 'left'
      )
    
  })  
  
  
  output$downloadPMReport <- downloadHandler(
    filename = function() {
      paste0("PM_Heatmap_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      library(openxlsx)
      
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
        group_by(`Assigned To`, `short name`, Code) %>%
        summarise(
          Actual_Progress = round(mean(Progress, na.rm = TRUE) * 100, 1),
          Expected_Progress = round(mean(Expected, na.rm = TRUE) * 100, 1),
          Gap = round(Expected_Progress - Actual_Progress, 1),
          Phase = first(`Active Phase`),
          Status = first(Status),
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
        ) %>%
        select(`Project Manager`, Code, Project, `Actual Progress (%)`, `Expected Progress (%)`,
               `Progress Gap (%)`, `Active Phase`, Recommendation)
      
      # Create styled Excel workbook
      wb <- createWorkbook()
      addWorksheet(wb, "PM Heatmap")
      
      headerStyle <- createStyle(
        fontSize = 11, fontColour = "white", fgFill = "#0073C2",
        halign = "center", textDecoration = "bold", border = "Bottom"
      )
      
      # Write data
      writeData(wb, sheet = 1, x = summary, startRow = 1, headerStyle = headerStyle)
      
      # Auto width
      setColWidths(wb, sheet = 1, cols = 1:ncol(summary), widths = "auto")
      
      # Apply conditional formatting to Gap column
      gap_col <- which(colnames(summary) == "Progress Gap (%)")
      conditionalFormatting(wb, sheet = 1, cols = gap_col, rows = 2:(nrow(summary) + 1),
                            rule = ">20", style = createStyle(fontColour = "red"))
      conditionalFormatting(wb, sheet = 1, cols = gap_col, rows = 2:(nrow(summary) + 1),
                            rule = "<-20", style = createStyle(fontColour = "darkgreen"))
      
      # Freeze header row
      freezePane(wb, sheet = 1, firstRow = TRUE)
      
      # Save file
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
}

