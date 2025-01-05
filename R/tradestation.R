#' @title TradeStation API Client
#' @description A high-performance R6 class for interacting with the TradeStation API
#' @importFrom R6 R6Class
#' @importFrom httr2 request
#' @importFrom data.table data.table as.data.table
#' @importFrom jsonlite fromJSON
#' @importFrom logger log_info log_error
#' @importFrom checkmate assert_string assert_choice
#'
#' @export
TradeStation <- R6::R6Class(
  "TradeStation",
  
  private = list(
    .base_url = "https://api.tradestation.com/v3",
    .token_manager = NULL,
    .market_data = NULL,
    
    # Handle API requests with automatic token refresh
    .request = function(path, method = "GET", query = NULL, body = NULL, stream = FALSE) {
      if (!private$.token_manager$has_tokens()) {
        stop("Not authenticated. Call authenticate() first.")
      }
      
      req <- request(paste0(private$.base_url, path)) %>%
        httr2::req_auth_bearer_token(private$.token_manager$get_access_token())
      
      if (!is.null(query)) {
        req <- req %>% httr2::req_url_query(!!!query)
      }
      
      if (!is.null(body)) {
        req <- req %>% 
          httr2::req_body_json(body) %>%
          httr2::req_method(method)
      }
      
      if (stream) {
        req <- req %>% httr2::req_stream()
      }
      
      resp <- tryCatch({
        httr2::req_perform(req)
      }, error = function(e) {
        if (httr2::resp_status(e$response) == 401) {
          # Token expired, refresh and retry
          if (private$.token_manager$refresh_token()) {
            req <- req %>% httr2::req_auth_bearer_token(private$.token_manager$get_access_token())
            httr2::req_perform(req)
          } else {
            stop("Failed to refresh token")
          }
        } else {
          stop(sprintf("API request failed: %s", e$message))
        }
      })
      
      resp
    }
  ),
  
  public = list(
    #' @description Initialize a new TradeStation API client
    #' @param client_id Your TradeStation API client ID
    #' @param client_secret Your TradeStation API client secret
    #' @param token_path Path to save/load tokens (default: .tokens)
    initialize = function(client_id = Sys.getenv("TS_CLIENT_ID"),
                         client_secret = Sys.getenv("TS_CLIENT_SECRET"),
                         token_path = ".tokens") {
      private$.token_manager <- TokenManager$new(
        client_id = client_id,
        client_secret = client_secret,
        token_path = token_path
      )
      
      private$.market_data <- MarketData$new(self)
      
      log_info("TradeStation API client initialized")
    },
    
    #' @description Authenticate with the TradeStation API
    #' @param force If TRUE, force a new authentication even if cached token exists
    authenticate = function(force = FALSE) {
      private$.token_manager$authenticate(force = force)
      invisible(self)
    },
    
    #' @description Check if the client is authenticated
    #' @return logical indicating if the client is authenticated
    is_authenticated = function() {
      private$.token_manager$has_tokens()
    },
    
    #' @description Get the market data handler
    #' @return MarketData instance
    market_data = function() {
      private$.market_data
    }
  )
) 