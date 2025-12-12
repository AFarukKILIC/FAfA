#' Data Selection UI Module
#'
#' @param id A character string, the module id.
#' @import shiny
#' @import bslib
#' @importFrom bsicons bs_icon
#' @importFrom shinycssloaders withSpinner 
#' @noRd
data_selection_ui <- function(id) {
  ns <- NS(id) 

  tagList(
    layout_columns(
      col_widths = c(5, 7),
      
      # --- Left Column: Upload & Preview ---
      tagList(
        card(
          card_header(
            "Data Upload", 
            class = "bg-primary text-white",
            bs_icon("upload")
          ),
          card_body(
            markdown("
            Please upload your data file. Supported formats: **.csv, .txt, .dat, .sav, .xlsx, .xls**.
            
            *Ensure missing values are represented as NA.*
            "),
            fileInput(
              ns("file1"),
              label = "Choose Data File:",
              accept = c(".csv", ".txt", ".dat", ".sav", ".xlsx", ".xls"),
              placeholder = "No file selected"
            ),
            checkboxInput(
              ns("has_header_checkbox"), 
              label = "My data has a header row", 
              value = TRUE
            ),
            actionButton(ns("analyze_data"), "Analyze Data", icon = icon("play"), class = "btn-success w-100")
          )
        ),
        card(
          card_header("Data Preview", bs_icon("table")),
          card_body(
            tableOutput(ns("mydatatable")) %>% withSpinner(type = 8, color = "#2C3E50"),
            style = "max-height: 400px; overflow-y: auto;"
          )
        )
      ),
      
      # --- Right Column: Summary Statistics (Value Boxes) ---
      card(
        card_header("Summary Statistics", bs_icon("bar-chart")),
        card_body(
          layout_columns(
            col_widths = c(6, 6, 6, 6, 12),
            row_heights = "auto",
            uiOutput(ns("n_var_box")),     # Variables
            uiOutput(ns("n_obs_box")),     # Sample Size
            uiOutput(ns("min_val_box")),   # Min
            uiOutput(ns("max_val_box")),   # Max
            uiOutput(ns("cat_range_box"))  # Category Range
          )
        )
      )
    )
  )
}