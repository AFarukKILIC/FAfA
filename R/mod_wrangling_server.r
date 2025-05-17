# mod_wrangling_server.R dosyasının içeriği (varsa library() çağrıları silinmiş olarak)

# ---------------------------------------------------------------------------
# Server Module for Excluding Variables
# ---------------------------------------------------------------------------
#' Server Logic for Excluding Variables
#'
#' Handles the logic for parsing user input for column exclusion,
#' updating the dataset, and providing a download for the modified dataset.
#'
#' @param input Shiny input.
#' @param output Shiny output.
#' @param session Shiny session.
#' @param data A reactive expression returning the current dataset.
#'
#' @return A reactive expression returning the dataset after variable exclusion.
#' @import shiny
#' @importFrom utils write.csv
#' @importFrom stats na.omit
#' @noRd
wrangling_server_ex_var <- function(input, output, session, data) {
  # reactiveVal to store the data after variable exclusion
  data_after_exclusion_rv <- reactiveVal()

  observeEvent(input$exclude_button, {
    # Ensure data is available
    validate(need(data(), "Please upload your dataset first before excluding variables."))
    validate(need(input$excluded_variables_input, "Please enter column numbers to exclude."))

    current_data <- data()
    excluded_vars_str <- input$excluded_variables_input

    # Attempt to parse the input string (e.g., "1,3,5" or "1, 3:5, 7")
    tryCatch({
      # A more robust way to parse mixed single numbers and ranges:
      # Split by comma, then process each part.
      parts <- trimws(unlist(strsplit(excluded_vars_str, ",")))
      user_excluded_indices <- integer(0)

      for (part in parts) {
        if (grepl(":", part)) { # It's a range like "3:5"
          range_parts <- as.integer(trimws(unlist(strsplit(part, ":"))))
          if (length(range_parts) == 2 && !any(is.na(range_parts)) && range_parts[1] <= range_parts[2]) {
            user_excluded_indices <- c(user_excluded_indices, seq(range_parts[1], range_parts[2]))
          } else {
            stop(paste("Invalid range format:", part), call. = FALSE)
          }
        } else { # It's a single number
          single_num <- as.integer(part)
          if (!is.na(single_num)) {
            user_excluded_indices <- c(user_excluded_indices, single_num)
          } else {
            stop(paste("Invalid number format:", part), call. = FALSE)
          }
        }
      }
      user_excluded_indices <- sort(unique(user_excluded_indices)) # Remove duplicates and sort

      # Validate indices
      if (length(user_excluded_indices) == 0) {
        stop("No valid column numbers were entered for exclusion.", call. = FALSE)
      }
      if (any(user_excluded_indices < 1) || any(user_excluded_indices > ncol(current_data))) {
        stop(paste0("Invalid column number(s) provided. Please ensure numbers are between 1 and ", ncol(current_data), "."), call. = FALSE)
      }
      if (length(user_excluded_indices) >= ncol(current_data)) {
          stop("You cannot exclude all columns from the dataset.", call. = FALSE)
      }

      # Exclude the variables
      data_after_exclusion_rv(current_data[, -user_excluded_indices, drop = FALSE]) # Use drop = FALSE to keep it a data.frame

      showNotification(
        paste0(
          "Variables excluded. The dataset now has ",
          ncol(data_after_exclusion_rv()), " variables and ",
          nrow(data_after_exclusion_rv()), " observations."
        ), type = "message"
      )
    }, error = function(e) {
      showNotification(paste("Error excluding variables:", e$message), type = "error", duration = 7)
      data_after_exclusion_rv(NULL) # Clear previous result on error
    })
  })

  # Download handler for the data with excluded variables
  output$download_excluded_data_button <- downloadHandler(
    filename = function() {
      paste0("dataset_vars_excluded_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(data_after_exclusion_rv()) # Ensure data is available
      write.csv(data_after_exclusion_rv(), file, row.names = FALSE)
    }
  )
  # Return the reactive data for chaining in app.R
  return(reactive(data_after_exclusion_rv()))
}


# ---------------------------------------------------------------------------
# Server Module for Splitting Data
# ---------------------------------------------------------------------------
#' Server Logic for Splitting Dataset
#'
#' Handles the logic for randomly splitting the dataset into two subsets
#' based on a user-defined percentage and provides downloads for them.
#'
#' @param input Shiny input.
#' @param output Shiny output.
#' @param session Shiny session.
#' @param data A reactive expression returning the current dataset.
#'
#' @return Does not directly return a single reactive for chaining in the main app flow,
#'         as it produces two datasets. Download handlers are provided.
#' @import shiny
#' @importFrom utils write.csv
#' @importFrom stats runif
#' @noRd
wrangling_server_split <- function(input, output, session, data) {
  # reactiveValues to store the two split datasets
  split_datasets_rv <- reactiveValues(first_half = NULL, second_half = NULL)

  observeEvent(input$split_data_button, {
    validate(need(data(), "Please upload your dataset first before splitting."))
    validate(need(input$split_percentage_slider, "Please set the split percentage."))

    current_data <- data()
    split_prop <- input$split_percentage_slider / 100 # Convert percentage to proportion

    tryCatch({
      if (nrow(current_data) < 2) {
        stop("Cannot split data with fewer than 2 rows.", call. = FALSE)
      }
      # Ensure reproducibility of the split if desired (e.g., by setting a seed)
      # set.seed(123) # Optional: for reproducible splits

      num_rows_first_half <- floor(split_prop * nrow(current_data))
      # Ensure each split has at least one row if the dataset is very small and split is extreme
      if (nrow(current_data) >= 2) { # Only adjust if there are at least 2 rows to split
          if (num_rows_first_half == 0) num_rows_first_half <- 1
          if (num_rows_first_half == nrow(current_data)) num_rows_first_half <- nrow(current_data) - 1
      }

      if (num_rows_first_half <= 0 || num_rows_first_half >= nrow(current_data) ) {
           # This condition might still be met if nrow(current_data) is 1, but the earlier check for < 2 rows handles that.
           # This mainly catches issues if the percentage slider somehow allows 0% or 100% leading to an empty set.
           stop(paste0("Cannot create a meaningful split resulting in an empty subset. Current data size: ", nrow(current_data) ,", requested for first set: ", num_rows_first_half,". Adjust percentage."), call. = FALSE)
      }

      selected_indices <- sample.int(n = nrow(current_data), size = num_rows_first_half, replace = FALSE)

      split_datasets_rv$first_half <- current_data[selected_indices, , drop = FALSE]
      split_datasets_rv$second_half <- current_data[-selected_indices, , drop = FALSE]

      showNotification(
        paste0(
          "Data successfully split. First subset has ",
          nrow(split_datasets_rv$first_half), " observations. Second subset has ",
          nrow(split_datasets_rv$second_half), " observations."
        ), type = "message"
      )
    }, error = function(e) {
      showNotification(paste("Error splitting data:", e$message), type = "error", duration = 7)
      split_datasets_rv$first_half <- NULL # Clear on error
      split_datasets_rv$second_half <- NULL
    })
  })

  # Download handler for the first half
  output$download_first_subset_button <- downloadHandler(
    filename = function() {
      paste0("dataset_subset1_", input$split_percentage_slider, "pct_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(split_datasets_rv$first_half)
      write.csv(split_datasets_rv$first_half, file, row.names = FALSE)
    }
  )

  # Download handler for the second half
  output$download_second_subset_button <- downloadHandler(
    filename = function() {
      paste0("dataset_subset2_", 100 - input$split_percentage_slider, "pct_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(split_datasets_rv$second_half)
      write.csv(split_datasets_rv$second_half, file, row.names = FALSE)
    }
  )
  # This module primarily provides downloads of two datasets.
  # If one of these splits were to be the primary data for the *next step in the main chain*,
  # app.R would need to be structured to select which split to use.
  # For now, not returning a single reactive for the main chain.
  # return(reactive(list(first_half = split_datasets_rv$first_half, second_half = split_datasets_rv$second_half)))
}


# ---------------------------------------------------------------------------
# Server Module for Handling Outliers
# ---------------------------------------------------------------------------
#' Server Logic for Handling Multivariate Outliers
#'
#' Detects multivariate outliers using Mahalanobis Distance via the 'assumptions'
#' utility function. Allows users to view and remove identified outliers.
#'
#' @param input Shiny input.
#' @param output Shiny output.
#' @param session Shiny session.
#' @param data A reactive expression returning the current dataset.
#'
#' @return A reactive expression returning the dataset after potential outlier removal.
#' @import shiny
#' @importFrom utils write.csv
#' @importFrom stats na.omit
#' @noRd
wrangling_server_outliers <- function(input, output, session, data) {
  # reactiveValues to store outlier analysis results and data without outliers
  outlier_info_rv <- reactiveValues(
    identified_outliers_table = NULL,
    outlier_count = NULL,
    data_without_outliers = NULL,
    original_row_indices_of_outliers = NULL
  )

  observeEvent(input$check_outliers_button, {
    validate(need(data(), "Please upload your dataset first to check for outliers."))
    validate(need(input$mah_p_value_threshold_input, "Please specify a p-value threshold for Mahalanobis distance."))

    current_data <- data()
    # Ensure current_data is not NULL before proceeding
    if(is.null(current_data)){
        showNotification("Data is not available for outlier detection.", type="warning")
        return()
    }

    p_threshold <- as.numeric(input$mah_p_value_threshold_input)

    if (!exists("assumptions")) {
      showNotification("Error: The 'assumptions' function for outlier detection is not found. Please check 'utils.R'.", type = "error", duration = 10)
      return()
    }

    numeric_cols <- sapply(current_data, is.numeric)
    if(sum(numeric_cols) < 2 ){ # Loosened from ncol(current_data) to allow for non-numeric columns if assumptions() handles it
        showNotification(paste0("Mahalanobis distance requires at least two numeric columns for calculation. Found ", sum(numeric_cols), " numeric columns. Ensure relevant data is numeric."), type = "warning", duration=10)
        # The assumptions function in utils.R should ideally handle subsetting to numeric columns if necessary,
        # or this check should be more stringent if all columns passed to assumptions must be numeric.
        # For now, this is a warning.
    }
    if(nrow(current_data) < sum(numeric_cols) && sum(numeric_cols) > 0){ # Check if N < P for numeric subset
        showNotification("Warning: The number of observations is less than the number of numeric variables. Mahalanobis distance may not be stable or calculable.", type="warning", duration=10)
    }


    tryCatch({
      # Pass the p_threshold to the assumptions function
      # Ensure your assumptions function in utils.R accepts and uses mah_p_threshold
      analysis_results <- assumptions(current_data, mah_p_threshold = p_threshold)

      outlier_info_rv$identified_outliers_table <- analysis_results$Mah_significant_outliers # Adjusted name from utils.R
      outlier_info_rv$outlier_count <- analysis_results$n_outliers_found # Adjusted name from utils.R

      # The 'assumptions' function in utils.R now returns 'Row_Number_In_Complete_Data'
      # which refers to row numbers *after* na.omit was applied within that function.
      # This is what we need for removing rows from the 'current_data' if it was also na.omitted
      # at the start of the assumptions() function.
      if (!is.null(analysis_results$Mah_significant_outliers) && "Row_Number_In_Complete_Data" %in% colnames(analysis_results$Mah_significant_outliers)) {
        outlier_info_rv$original_row_indices_of_outliers <- analysis_results$Mah_significant_outliers$Row_Number_In_Complete_Data
      } else {
        outlier_info_rv$original_row_indices_of_outliers <- NULL
      }

      output$outliers_table <- renderTable({
        if (is.null(outlier_info_rv$identified_outliers_table) || nrow(outlier_info_rv$identified_outliers_table) == 0) {
          data.frame(Message = "No outliers detected at the specified p-value threshold.")
        } else {
          # Display relevant columns, e.g., Row_Number_In_Complete_Data, MD, MD_p
          # Ensure these column names match what `assumptions` function returns
          outlier_info_rv$identified_outliers_table[, c("Row_Number_In_Complete_Data", "MD", "MD_p"), drop = FALSE]
        }
      }, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = "s", digits = 3)

      output$outlier_count_text <- renderText({
        paste("Number of outliers identified (p <", p_threshold, "):", outlier_info_rv$outlier_count %||% 0)
      })

      showNotification("Outlier check completed.", type="message")

    }, error = function(e) {
      showNotification(paste("Error during outlier detection:", e$message), type = "error", duration = 7)
      outlier_info_rv$identified_outliers_table <- NULL
      outlier_info_rv$outlier_count <- NULL
      outlier_info_rv$original_row_indices_of_outliers <- NULL
      output$outliers_table <- renderTable({ data.frame(Error = e$message) })
      output$outlier_count_text <- renderText({"Error in outlier detection."})
    })
  })

  observeEvent(input$remove_outliers_button, {
    validate(
      need(data(), "Please upload data first."),
      # Check outlier_count instead of the table itself for deciding if removal is possible
      need(!is.null(outlier_info_rv$outlier_count), "Please identify outliers first using the 'Identify Outliers' button.")
    )

    current_data_for_removal <- data() # This is the data *before* na.omit inside assumptions()

    # The outlier_info_rv$original_row_indices_of_outliers are indices from the
    # na.omitted data *within* the assumptions() function.
    # To remove them from `current_data_for_removal`, we first need to apply the same na.omit
    # that assumptions() did, then remove, OR ensure assumptions() returns indices
    # relative to the data it received if that data was not na.omitted by this module.
    # Given utils.R's assumptions() now does na.omit internally:

    if (is.null(current_data_for_removal)) {
        showNotification("Original data not available for outlier removal.", type="error")
        return()
    }

    # Re-create the 'complete case' version of current_data, similar to what assumptions() does
    # This is crucial if current_data_for_removal could have NAs that assumptions() would have removed
    # before calculating Mahalanobis distances.
    # The `clean_missing_data` in app.R should already provide complete data to this module.
    # So, `current_data_for_removal` should ideally be complete cases.

    data_complete_cases_for_removal <- stats::na.omit(current_data_for_removal)
    # ^ This line is key. If current_data_for_removal from app.R is already na.omitted by clean_missing_data,
    # then this na.omit might be redundant but harmless. If not, it's essential.
    # The `Row_Number_In_Complete_Data` from utils.R refers to rows in `data_complete_cases_for_removal`.

    if (is.null(outlier_info_rv$original_row_indices_of_outliers) || length(outlier_info_rv$original_row_indices_of_outliers) == 0) {
      outlier_info_rv$data_without_outliers <- current_data_for_removal # No outliers to remove, return original
      showNotification("No outliers were identified or selected for removal. Using the current dataset.", type = "info")
    } else {
      indices_to_remove_from_complete <- outlier_info_rv$original_row_indices_of_outliers

      if(length(indices_to_remove_from_complete) > 0 && length(indices_to_remove_from_complete) < nrow(data_complete_cases_for_removal)) {
          # Remove rows from the 'data_complete_cases_for_removal'
          data_after_removing_from_complete <- data_complete_cases_for_removal[-indices_to_remove_from_complete, , drop = FALSE]
          outlier_info_rv$data_without_outliers <- data_after_removing_from_complete

          showNotification(
            paste0(
              length(indices_to_remove_from_complete), " outlier(s) removed. The dataset (based on complete cases used for MD) now has ",
              nrow(outlier_info_rv$data_without_outliers), " observations."
            ), type = "message"
          )
      } else if (length(indices_to_remove_from_complete) >= nrow(data_complete_cases_for_removal)) {
          showNotification("Error: Attempting to remove all or more rows than available in the complete-case dataset. Outliers not removed.", type="error")
          outlier_info_rv$data_without_outliers <- current_data_for_removal # Fallback to original
      } else { # length is 0
          outlier_info_rv$data_without_outliers <- current_data_for_removal
          showNotification("No valid outlier indices found for removal. Using the current dataset.", type = "warning")
      }
    }
  })

  output$download_data_no_outliers_button <- downloadHandler(
    filename = function() {
      paste0("dataset_no_outliers_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(outlier_info_rv$data_without_outliers)
      write.csv(outlier_info_rv$data_without_outliers, file, row.names = FALSE)
    }
  )
  # Return the reactive data for chaining in app.R
  return(reactive(outlier_info_rv$data_without_outliers))
}

# Helper for default NULL values (used in outlier_count_text)
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x

