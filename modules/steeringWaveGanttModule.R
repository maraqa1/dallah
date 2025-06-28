steeringWaveGanttUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    tags$style(HTML("
      .legend-box {
        padding: 10px;
        margin-bottom: 20px;
        border: 1px solid #ccc;
        border-radius: 8px;
        background-color: #f9f9f9;
      }
      .legend-item {
        display: inline-block;
        margin-right: 20px;
        font-size: 14px;
      }
      .legend-color {
        display: inline-block;
        width: 15px;
        height: 15px;
        margin-right: 6px;
        border: 1px solid #ccc;
        vertical-align: middle;
      }
    ")),
    
    h3("Steering Committee Gantt Chart", style = "text-align: center; font-weight: bold; margin-bottom: 20px;"),
    
    # 🔽 Wave + Facet Selectors
    fluidRow(
      column(6, offset = 1,
             selectInput(ns("waveSelect"), "Select Wave:", choices = NULL, selected = "All")
      ),
      column(5,
             selectInput(ns("facetBy"), "Slice Gantt by:", choices = c(
               "Wave", "Program", "tier", "Active Phase", "Status", "Assigned To"
             ), selected = "Wave")
      )
    ),
    
    hr(),
    
    # 🔍 Legend Section
    fluidRow(
      column(6,
             div(class = "legend-box",
                 tags$h5("Health Status", style = "margin-bottom: 10px;"),
                 div(class = "legend-item", span(class = "legend-color", style = "background-color: forestgreen;"), "Green"),
                 div(class = "legend-item", span(class = "legend-color", style = "background-color: firebrick;"), "Red"),
                 div(class = "legend-item", span(class = "legend-color", style = "background-color: gray60;"), "Gray")
             )
      ),
      column(6,
             div(class = "legend-box",
                 tags$h5("Project Phase", style = "margin-bottom: 10px;"),
                 div(class = "legend-item", span(class = "legend-color", style = "background-color: blue;"), "Planning"),
                 div(class = "legend-item", span(class = "legend-color", style = "background-color: purple;"), "Assessment"),
                 div(class = "legend-item", span(class = "legend-color", style = "background-color: orange;"), "Implementation"),
                 div(class = "legend-item", span(class = "legend-color", style = "background-color: #ca7a13;"), "Procurement"),
                 div(class = "legend-item", span(class = "legend-color", style = "background-color: #a5e914;"), "Closure"),
                 div(class = "legend-item", span(class = "legend-color", style = "background-color: gray;"), "Not Started")
             )
      )
    ),
    
    hr(),
    
    fluidRow(
      column(12, plotOutput(ns("waveGanttPlot"), height = "800px"))
    )
  )
}


steeringWaveGanttServer <- function(input, output, session, tasks) {
  ns <- session$ns
  
  # Update wave choices
  observe({
    req(tasks())
    waves <- sort(unique(na.omit(tasks()$Wave)))
    updateSelectInput(session, "waveSelect", choices = c("All", waves), selected = "All")
  })
  
  
  # Dynamically calculate plot height based on data volume
  reactiveHeight_1 <- reactive({
    req(tasks())
    data <- tasks()
    
    # Apply wave filter
    if (!is.null(input$waveSelect) && input$waveSelect != "All") {
      data <- data[data$Wave == input$waveSelect, ]
    }
    
    facet_col <- input$facetBy
    if (is.null(facet_col) || !(facet_col %in% names(data))) {
      facet_col <- "Wave"
    }
    
    data <- data[!is.na(data$`short name`), ]
    
    rows_per_facet <- data %>%
      group_by(.data[[facet_col]]) %>%
      summarize(n = n()) %>%
      pull(n)
    
    n_facets <- length(rows_per_facet)
    max_rows <- max(rows_per_facet, na.rm = TRUE)
    
    base_height <- 100  # space for headers
    per_row_height <- 40
    
    height <- n_facets * base_height + max_rows * per_row_height
    max(height, 500)
  })
  
  
  reactiveHeight_2 <- reactive({
    req(tasks())
    data <- tasks()
    
    # Wave filter
    if (!is.null(input$waveSelect) && input$waveSelect != "All") {
      data <- data[data$Wave == input$waveSelect, ]
    }
    
    # Remove blank labels
    data <- data[!is.na(data$`short name`), ]
    
    facet_col <- input$facetBy
    if (is.null(facet_col) || !(facet_col %in% names(data))) {
      facet_col <- "Wave"
    }
    
    # Count rows per facet
    rows_per_facet <- data %>%
      group_by(.data[[facet_col]]) %>%
      summarize(n = n(), .groups = "drop") %>%
      pull(n)
    
    n_facets <- length(rows_per_facet)
    max_rows <- max(rows_per_facet, na.rm = TRUE)
    
    base_height <- 100
    row_height <- 35  # Raise back up from 22 to 35 for label room
    
    height <- n_facets * base_height + max_rows * row_height
    height <- max(min(height, 2500), 2000)  # allow taller height
    return(height)
  })
  
  
  reactiveHeight_3 <- reactive({
    req(tasks())
    data <- tasks()
    
    # Apply wave filter
    if (!is.null(input$waveSelect) && input$waveSelect != "All") {
      data <- data[data$Wave == input$waveSelect, ]
    }
    
    data <- data[!is.na(data$`short name`), ]
    
    facet_col <- input$facetBy
    if (is.null(facet_col) || !(facet_col %in% names(data))) {
      facet_col <- "Wave"
    }
    
    # Count total visible items across all facets
    total_items <- data %>%
      group_by(.data[[facet_col]]) %>%
      summarize(n_rows = n(), .groups = "drop") %>%
      pull(n_rows) %>%
      sum()
    
    base_buffer <- 100   # e.g. for headers/margins
    row_height <- 35     # height per item
    
    height <- total_items * row_height + base_buffer
    height <- max(min(height, 2500), 800)  # Clamp
    
    return(height)
  })
  
  
  reactiveHeight <- reactive({
    req(tasks())
    data <- tasks()
    
    # Filter by selected wave
    if (!is.null(input$waveSelect) && input$waveSelect != "All") {
      data <- data[data$Wave == input$waveSelect, ]
    }
    
    data <- data[!is.na(data$`short name`), ]
    
    # Get selected facet column
    facet_col <- input$facetBy
    if (is.null(facet_col) || !(facet_col %in% names(data))) {
      facet_col <- "Wave"
    }
    
    # Compute max rows per facet (this is what matters most visually)
    max_rows_per_facet <- data %>%
      group_by(.data[[facet_col]]) %>%
      summarize(n = n(), .groups = "drop") %>%
      pull(n) %>%
      max(na.rm = TRUE)
    
    # Count number of facets
    n_facets <- data %>%
      pull(.data[[facet_col]]) %>%
      unique() %>%
      length()
    
    # Define height model
    row_height <- 20     # height per initiative (per row)
    facet_header_height <- 90  # for strip/facet title
    buffer <- 100        # top/bottom margins etc.
    
    height <- (max_rows_per_facet * row_height * n_facets) + (n_facets * facet_header_height) + buffer
    
    # Clamp it
    height <- max(min(height, 3000), 800)
    
    return(height)
  })
  
  
  
  # Render Gantt Plot
  output$waveGanttPlot <- renderPlot({
    req(tasks())
    data <- tasks()
    
    if (!is.null(input$waveSelect) && input$waveSelect != "All") {
      data <- data[data$Wave == input$waveSelect, ]
    }
    
    req(nrow(data) > 0)
    
    facet_var <- input$facetBy
    if (is.null(facet_var) || !(facet_var %in% names(data))) {
      facet_var <- "Wave"
    }
    
    # Clean & prep
    phase_levels <- c("Initiation", "Planning", "Assessment", "Procurement",
                      "Implementation", "Closure", "Completed", "Not Started")
    phase_colors <- c(
      "Initiation" = "pink", "Planning" = "blue", "Assessment" = "purple",
      "Procurement" = "#ca7a13", "Implementation" = "orange",
      "Closure" = "#a5e914", "Completed" = "green", "Not Started" = "gray"
    )
    health_colors <- c("Green" = "forestgreen", "Red" = "firebrick", "Gray" = "gray60")
    
    data <- data %>%
      rename(Start = `Start Date`, End = `End Date`) %>%
      mutate(
        Start = as.Date(Start),
        End = as.Date(End),
        Health = ifelse(is.na(Health) | Health == "", "Gray", Health),
        Task_Name = stringr::str_wrap(`short name`, 25),
        Active_Phase = case_when(
          `Active Phase` == "Assesment" ~ "Assessment",
          TRUE ~ `Active Phase`
        ),
        Active_Phase = factor(Active_Phase, levels = phase_levels),
        `% Complete` = ifelse(is.na(`% Complete`), 0, `% Complete`),
        Progress = `% Complete` * 100
      ) %>%
      filter(!is.na(Start) & !is.na(End)) %>%
      arrange(Start)
    
    # Average progress per facet
    facet_summary <- data %>%
      group_by(GroupValue = .data[[facet_var]]) %>%
      summarize(AvgProgress = round(mean(Progress, na.rm = TRUE))) %>%
      mutate(facet_label = paste0(GroupValue, " (", AvgProgress, "%)"))
    
    data <- data %>%
      left_join(facet_summary, by = setNames("GroupValue", facet_var))
    
    quarter_labels <- function(x) paste0("Q", lubridate::quarter(x), " ", lubridate::year(x))
    
    ggplot(data, aes(
      x = Start, xend = End, y = reorder(Task_Name, Start),
      yend = Task_Name, color = Health
    )) +
      geom_segment(position = position_nudge(y = 0.1), size = 4) +
      geom_label(
        aes(x = End + 10, label = paste0(Active_Phase, " (", round(Progress), "%)"), fill = Active_Phase),
        color = "white", fontface = "bold", size = 3.5, hjust = 0, label.size = 0.1
      ) +
      geom_vline(xintercept = as.numeric(Sys.Date()), linetype = "dashed", color = "red", linewidth = 1) +
      annotate("text", x = Sys.Date(), y = -Inf, label = "Today", vjust = -1, color = "red", fontface = "bold") +
      scale_color_manual(values = health_colors) +
      scale_fill_manual(values = phase_colors) +
      scale_x_date(
        breaks = seq(min(data$Start), max(data$End), by = "3 months"),
        labels = quarter_labels,
        expand = expansion(mult = c(0, 0.1))
      ) +
      labs(
        title = paste("Gantt Chart by", facet_var),
        x = "Date", y = "Initiative", color = "Health", fill = "Active Phase"
      ) +
      facet_wrap(vars(facet_label), scales = "free_y", ncol = ifelse(facet_var %in% c("Wave", "Program"), 1, 2)) +
      theme_minimal(base_size = 13) +
      theme(
        plot.background = element_rect(fill = "#f4f4f4", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        strip.background = element_rect(fill = "#003366"),
        strip.text = element_text(color = "white", face = "bold", size = 14),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "gray85"),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.title = element_text(color = "#222222"),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(face = "bold"),
        legend.text = element_text(),
        legend.background = element_rect(fill = "#f4f4f4", color = NA),
        plot.margin = margin(40, 40, 40, 40, "pt")
      ) +
      coord_cartesian(clip = "off")
  }, height = reactiveHeight)
}
