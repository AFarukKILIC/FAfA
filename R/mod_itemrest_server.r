#' ItemRest Analysis Server Module
#'
#' @param id Module namespace ID.
#' @param data Reactive containing the input dataset.
#' @import shiny
#' @importFrom stats na.omit
#' @export
mod_itemrest_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    # Store results from ItemRest package
    analysis_results <- reactiveVal(NULL)
    
    observeEvent(input$run_itemrest, {
      req(data())
      
      # Check if ItemRest package is available
      if (!requireNamespace("ItemRest", quietly = TRUE)) {
        showNotification("Package 'ItemRest' is required but not installed.", type = "error")
        return()
      }
      
      # Data preparation: Ensure numeric and handle NAs if not done upstream
      # The manual mentions removing rows with missing values [cite: 116]
      df <- data()
      if(!all(sapply(df, is.numeric))) {
        showNotification("ItemRest requires all variables to be numeric.", type = "error")
        return()
      }
      df_clean <- stats::na.omit(df)
      
      # Prepare arguments 
      num_factors_arg <- if(is.na(input$n_factors)) NULL else input$n_factors
      
      showNotification("Running ItemRest automation strategies...", type = "message", duration = NULL, id = "ir_progress")
      
      tryCatch({
        # Calling itemrest function
        res <- ItemRest::itemrest(
          data = df_clean,
          cor_method = input$cor_method,
          n_factors = num_factors_arg,
          extract = input$extraction_method,
          rotate = input$rotation_method
        )
        
        analysis_results(res)
        removeNotification("ir_progress")
        showNotification("Analysis Complete!", type = "message")
        
      }, error = function(e) {
        removeNotification("ir_progress")
        showNotification(paste("Analysis Failed:", e$message), type = "error", duration = 10)
      })
    })
    
    # Output 1: Optimal Strategy Text [cite: 100]
    output$optimal_strategy_text <- renderPrint({
      req(analysis_results())
      res <- analysis_results()
      
      # Assuming 'optimal_strategy' component exists based on manual [cite: 100]
      if("optimal_strategy" %in% names(res)) {
        print(res$optimal_strategy)
      } else {
        cat("Optimal strategy details not found in result object.")
      }
    })
    
    # Output 2: Removal Summary Table [cite: 99]
    output$removal_summary_table <- renderTable({
      req(analysis_results())
      res <- analysis_results()
      
      # The manual states 'removal_summary' is a data.frame [cite: 101]
      if("removal_summary" %in% names(res)) {
        return(res$removal_summary)
      } else {
        return(data.frame(Message = "Summary table not returned by analysis."))
      }
    }, striped = TRUE, hover = TRUE, digits = 3)
    
  })
}