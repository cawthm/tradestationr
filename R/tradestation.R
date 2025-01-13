#' @title TradeStation API Client
#' @description A high-performance R6 class for interacting with the TradeStation API
#' @importFrom R6 R6Class
#' @importFrom httr2 request
#' @importFrom data.table data.table as.data.table
#' @importFrom jsonlite fromJSON
#' @importFrom logger log_info log_error
#' @importFrom checkmate assert_string assert_choice assert_directory_exists
#'
#' @export
TradeStation <- R6::R6Class(
  "TradeStation",
  public = list(
    initialize = function(secrets_dir = ".secrets") {
      message("Initializing with secrets_dir: ", secrets_dir)
      assert_directory_exists(secrets_dir)
      private$.secrets_dir <- secrets_dir
      
      # Initialize token manager
      private$.token_manager <- TokenManager$new(secrets_dir = secrets_dir)
      
      # Initialize market data handler
      private$.market_data <- MarketData$new(self)
      
      invisible(self)
    },
    
    authenticate = function(force = FALSE) {
      private$.token_manager$authenticate(force = force)
      invisible(self)
    },
    
    is_authenticated = function() {
      private$.token_manager$has_tokens()
    },
    
    update_token = function() {
      private$.token_manager$update_access_token()
    },
    
    market_data = function() {
      private$.market_data
    }
  ),
  
  private = list(
    .base_url = "https://api.tradestation.com/v3",
    .secrets_dir = NULL,
    .token_manager = NULL,
    .market_data = NULL,
    
    # Handle API requests with automatic token refresh
    .request = function(path, method = "GET", query = NULL, body = NULL, stream = FALSE, callback = NULL) {
      if (!private$.token_manager$has_tokens()) {
        stop("Not authenticated. Call authenticate() first.")
      }
      
      full_url <- paste0(private$.base_url, path)
      cat(sprintf("\nMaking request to: %s\n", full_url))
      
      req <- request(full_url) %>%
        httr2::req_auth_bearer_token(private$.token_manager$get_access_token())
      
      if (!is.null(query)) {
        req <- req %>% httr2::req_url_query(!!!query)
        cat(sprintf("With query params: %s\n", 
            paste(names(query), query, sep = "=", collapse = ", ")))
      }
      
      if (!is.null(body)) {
        req <- req %>% 
          httr2::req_body_json(body) %>%
          httr2::req_method(method)
      }
      
      if (stream) {
        if (is.null(callback)) {
          stop("Callback function required for streaming requests")
        }
        
        cat("Setting up streaming request...\n")
        
        # Add required headers for streaming
        req <- req %>%
          httr2::req_headers(
            Accept = "text/event-stream",
            `Cache-Control` = "no-cache",
            Connection = "keep-alive"
          )
        
        cat("\nStarting stream...\n")
        
        # Simple streaming handler - process each chunk directly
        tryCatch({
          httr2::req_perform_stream(
            req,
            function(x) {
              # Convert raw data to character
              if (is.raw(x)) {
                data <- rawToChar(x)
                if (nchar(data) > 0) {
                  callback(data)
                }
              }
              TRUE  # Continue streaming
            },
            buffer_kb = 0.5  # Small buffer to get data more frequently
          )
        }, error = function(e) {
          cat(sprintf("\nStreaming error: %s\n", e$message))
          if (inherits(e, "httr2_error")) {
            cat(sprintf("Response status: %d\n", httr2::resp_status(e$resp)))
            cat(sprintf("Response body: %s\n", rawToChar(e$resp$body)))
          }
          stop(e)
        })
        
        return(invisible(NULL))
      }
      
      tryCatch({
        resp <- httr2::req_perform(req)
        resp
      }, error = function(e) {
        if (inherits(e, "httr2_error")) {
          if (httr2::resp_status(e$resp) == 401) {
            # Token expired, refresh and retry
            cat("Token expired, attempting refresh...\n")
            if (private$.token_manager$update_access_token()) {
              req <- req %>% httr2::req_auth_bearer_token(private$.token_manager$get_access_token())
              httr2::req_perform(req)
            } else {
              stop("Failed to refresh token")
            }
          } else {
            # Try to get error details from response
            error_body <- tryCatch({
              fromJSON(rawToChar(e$resp$body))
            }, error = function(e2) NULL)
            
            error_msg <- if (!is.null(error_body) && !is.null(error_body$Message)) {
              sprintf("API request failed: %s - %s", httr2::resp_status_desc(e$resp), error_body$Message)
            } else {
              sprintf("API request failed: %s", httr2::resp_status_desc(e$resp))
            }
            
            stop(error_msg)
          }
        } else {
          stop(sprintf("Request failed: %s", e$message))
        }
      })
    }
  ),
  
  active = list(
    #' @field request Internal request method for use by other classes
    request = function() private$.request
  )
) 