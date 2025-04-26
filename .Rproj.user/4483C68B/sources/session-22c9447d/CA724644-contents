# ─────────────────────────────────────────────
# googleSheetLoaderModule.R
# ─────────────────────────────────────────────

# Load required packages
library(shiny)
library(DT)
library(googlesheets4)
library(janitor)

googleSheetLoaderUI <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Google Sheet Loader"),
    actionButton(ns("refreshData"), "🔄 Refresh from Google Sheet"),
    br(), br(),
    DTOutput(ns("dataPreview"))
  )
}

googleSheetLoaderServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ---- Reference to your Google Sheet URL (internal) ----
    sheet_url <- "https://docs.google.com/spreadsheets/d/1cvemCZnWoeq_OCKKs379Jrjt34NfKfm5RRNllaMvqf4/edit?usp=sharing"
    
    sheet_data <- reactiveVal()
    
    load_google_sheet <- function() {
      # No authentication needed for public sheets
      googlesheets4::gs4_deauth()
      
      tryCatch({
        df <- googlesheets4::read_sheet(sheet_url)
        df <- janitor::clean_names(df)
        
        sheet_data(df)
        showNotification("✅ Public Google Sheet loaded successfully!", type = "message")
      }, error = function(e) {
        showNotification(paste("❌ Error loading Google Sheet:", e$message), type = "error")
      })
    }
    
    # Load once automatically when module starts
    observeEvent(TRUE, {
      load_google_sheet()
    }, once = TRUE)
    
    # Reload manually when user clicks Refresh
    observeEvent(input$refreshData, {
      load_google_sheet()
    })
    
    # Show data in DataTable
    output$dataPreview <- renderDT({
      req(sheet_data())
      datatable(sheet_data(), options = list(pageLength = 10, scrollX = TRUE))
    })
    
    return(sheet_data)
  })
}
