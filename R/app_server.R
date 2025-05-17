#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
# Server Logic
app_server <- function(input, output, session) {
  # message("Main server function started. Session class: ", class(session)[1]) # For debugging

  # --- Initial Data Loading and Cleaning (Supporting Multiple Formats with Dynamic Header) ---
  shared_data_reactive <- reactive({
    # message("shared_data_reactive: Evaluating...") # For debugging
    inFile <- input[["data_selection-file1"]] # From data_selection_ui module

    # Get the header choice from the data_selection_ui module's checkbox
    # The ID is ns("has_header_checkbox"), so in app.R it's "data_selection-has_header_checkbox"
    # Default to TRUE if the input is not available yet (e.g., on app startup)
    user_has_header <- input[["data_selection-has_header_checkbox"]]
    if(is.null(user_has_header)) {
      user_has_header <- TRUE # Default if checkbox not rendered yet
      # message("shared_data_reactive: user_has_header was NULL, defaulted to TRUE") # For debugging
    }
    # message("shared_data_reactive: User indicated header presence: ", user_has_header) # For debugging

    if (is.null(inFile)) {
      # message("shared_data_reactive: No file uploaded, returning NULL.") # For debugging
      return(NULL)
    }
    # message("shared_data_reactive: File '", inFile$name, "' detected.") # For debugging

    tryCatch({
      ext <- tools::file_ext(tolower(inFile$name))

      df <- switch(ext,
                   "csv" = utils::read.csv(inFile$datapath, header = user_has_header, stringsAsFactors = FALSE, na.strings = c("", "NA", "N/A", " ", ".", "-", "?", "missing")),
                   "dat" = utils::read.table(inFile$datapath, header = user_has_header, stringsAsFactors = FALSE, na.strings = c("", "NA", "N/A", " ", ".", "-", "?", "missing")),
                   "txt" = utils::read.delim(inFile$datapath, header = user_has_header, stringsAsFactors = FALSE, na.strings = c("", "NA", "N/A", " ", ".", "-", "?", "missing")),
                   "sav" = {
                     if (!requireNamespace("haven", quietly = TRUE)) {
                       showNotification("Package 'haven' is required to load .sav files. Please install it.", type = "error", duration = 7)
                       return(NULL)
                     }
                     # haven::read_sav typically handles headers correctly from the .sav file structure
                     haven::read_sav(inFile$datapath)
                   },
                   "xlsx" = {
                     if (!requireNamespace("readxl", quietly = TRUE)) {
                       showNotification("Package 'readxl' is required to load .xlsx files. Please install it.", type = "error", duration = 7)
                       return(NULL)
                     }
                     # For Excel, col_names = TRUE reads first row as header, col_names = FALSE reads all as data
                     readxl::read_excel(inFile$datapath, col_names = user_has_header, na = c("", "NA", "N/A", " ", ".", "-", "?", "missing"))
                   },
                   "xls" = {
                     if (!requireNamespace("readxl", quietly = TRUE)) {
                       showNotification("Package 'readxl' is required to load .xls files. Please install it.", type = "error", duration = 7)
                       return(NULL)
                     }
                     readxl::read_excel(inFile$datapath, col_names = user_has_header, na = c("", "NA", "N/A", " ", ".", "-", "?", "missing"))
                   },
                   {
                     showNotification(
                       paste("Unsupported file type: '", ext, "'. Please upload a supported file (CSV, DAT, TXT, SAV, XLS, XLSX).", sep=""),
                       type = "error",
                       duration = 7
                     )
                     # message("shared_data_reactive: Unsupported file type '", ext, "'.") # For debugging
                     return(NULL)
                   }
      )

      if (is.null(df)) {
        # message("shared_data_reactive: df is NULL after read function.") # For debugging
        return(NULL)
      }
      # message("shared_data_reactive: File loaded, rows: ", nrow(df), ", cols: ", ncol(df)) # For debugging
      # message("shared_data_reactive: Column names after load: ", paste(colnames(df), collapse=", ")) # For debugging

      if ("clean_missing_data" %in% ls(envir = .GlobalEnv)) {
        # message("shared_data_reactive: Calling clean_missing_data...") # For debugging
        cleaned_result <- clean_missing_data(df)

        if(!is.null(cleaned_result$cleaned_data)){
          # message("shared_data_reactive: clean_missing_data returned data, rows: ", nrow(cleaned_result$cleaned_data), ", removed: ", cleaned_result$removed_rows) # For debugging
        } else {
          # message("shared_data_reactive: clean_missing_data returned NULL for cleaned_data.") # For debugging
        }
        return(cleaned_result$cleaned_data)
      } else {
        showNotification("'clean_missing_data' function not found in utils.R. Critical error. Data will not be cleaned.", type = "error", duration=10)
        # message("shared_data_reactive: CRITICAL ERROR - clean_missing_data not found. Returning raw loaded df.") # For debugging
        return(df) # Return raw df if cleaning function is missing, though this is not ideal
      }
    }, error = function(e) {
      showNotification(paste("Error loading or processing file:", e$message), type = "error", duration = 7)
      # message("shared_data_reactive: ERROR - ", e$message) # For debugging
      return(NULL)
    })
  })

  # --- Data Selection Module ---
  callModule(data_selection_server, "data_selection", data = shared_data_reactive)

  # --- Wrangling Data Modules Chain ---
  # Assumes wrangling modules RETURN their processed data as reactives
  data_after_exclusion_reactive <- callModule(
    module = wrangling_server_ex_var,
    id = "wrangling_ex_var",
    data = shared_data_reactive
  )

  data_for_outliers_module_input <- reactive({
    # message("data_for_outliers_module_input: Evaluating...") # For debugging
    excluded_data_val <- data_after_exclusion_reactive()
    if (!is.null(excluded_data_val)) {
      # message("data_for_outliers_module_input: Using data_after_exclusion_reactive. Rows: ", nrow(excluded_data_val)) # For debugging
      return(excluded_data_val)
    }
    sdr_val <- shared_data_reactive()
    # message("data_for_outliers_module_input: Fallback to shared_data_reactive. Is it NULL? ", is.null(sdr_val), if(!is.null(sdr_val)) paste0(" Rows: ", nrow(sdr_val))) # For debugging
    return(sdr_val)
  })

  data_without_outliers_reactive <- callModule(
    module = wrangling_server_outliers,
    id = "wrangling_outliers",
    data = data_for_outliers_module_input
  )

  final_wrangled_data_reactive <- reactive({
    # message("final_wrangled_data_reactive: Evaluating...") # For debugging
    data_after_outliers_val <- data_without_outliers_reactive()
    if (!is.null(data_after_outliers_val)) {
      # message("final_wrangled_data_reactive: Using data_without_outliers_reactive. Rows: ", nrow(data_after_outliers_val)) # For debugging
      return(data_after_outliers_val)
    }
    # message("final_wrangled_data_reactive: data_without_outliers_reactive was NULL.") # For debugging

    excluded_data_val <- data_after_exclusion_reactive()
    if (!is.null(excluded_data_val)) {
      # message("final_wrangled_data_reactive: Using data_after_exclusion_reactive. Rows: ", nrow(excluded_data_val)) # For debugging
      return(excluded_data_val)
    }
    # message("final_wrangled_data_reactive: data_after_exclusion_reactive was NULL.") # For debugging

    sdr_val <- shared_data_reactive()
    if (!is.null(sdr_val)) {
      # message("final_wrangled_data_reactive: Using shared_data_reactive. Rows: ", nrow(sdr_val)) # For debugging
    } else {
      # message("final_wrangled_data_reactive: shared_data_reactive was also NULL. Returning NULL.") # For debugging
    }
    return(sdr_val)
  })

  callModule(
    module = wrangling_server_split,
    id = "wrangling_split",
    data = final_wrangled_data_reactive
  )

  callModule(assumptions_server, "assumptions", data = final_wrangled_data_reactive)
  callModule(efa_server_fac_ret, "efa_fac_ret", data = final_wrangled_data_reactive)

  efa_settings_reactive <- callModule(
    module = function(input, output, session) {
      reactive({
        list(
          number_factor = input$number_factor,
          rotating_method = input$rotating_method,
          fact_method = input$fact_method,
          cor_kind = input$cor_kind
        )
      })
    },
    id = "efa_analysis"
  )

  returned_efa_object_reactive <- callModule(
    module = efa_server_analysis,
    id = "efa_analysis",
    data = final_wrangled_data_reactive
  )

  callModule(
    module = efa_server_report,
    id = "efa_report",
    data = final_wrangled_data_reactive,
    efa_output_reactive = returned_efa_object_reactive,
    efa_settings_reactive = efa_settings_reactive
  )

  callModule(ega_server, "ega", data = final_wrangled_data_reactive)
  callModule(cfa_server, "cfa", data = final_wrangled_data_reactive)
  callModule(inv_server, "inv", data = final_wrangled_data_reactive)
  callModule(reliability_server, "reliability", data = final_wrangled_data_reactive)
  callModule(item_weighting_server, "item_weighting", data = final_wrangled_data_reactive)
  callModule(about_server, "about")
}

