#' @title Authentication Handler
#' @description Manages OAuth2 authentication and token lifecycle
#' @importFrom httr2 oauth_client oauth_flow_auth_code oauth_flow_refresh oauth_token_cached
#' @importFrom jsonlite write_json read_json
#' @importFrom logger log_info log_error
#' @importFrom checkmate assert_string assert_path_for_output

#' @export
TokenManager <- R6::R6Class(
  "TokenManager",
  
  private = list(
    .client = NULL,
    .token_path = NULL,
    .token = NULL,
    
    # Save tokens to disk
    .save_tokens = function() {
      if (!is.null(private$.token)) {
        tryCatch({
          write_json(private$.token, private$.token_path, auto_unbox = TRUE)
          log_info("Tokens saved to disk")
        }, error = function(e) {
          log_error(sprintf("Failed to save tokens: %s", e$message))
        })
      }
    },
    
    # Load tokens from disk
    .load_tokens = function() {
      if (file.exists(private$.token_path)) {
        tryCatch({
          private$.token <- read_json(private$.token_path)
          log_info("Tokens loaded from disk")
          TRUE
        }, error = function(e) {
          log_error(sprintf("Failed to load tokens: %s", e$message))
          FALSE
        })
      } else {
        FALSE
      }
    }
  ),
  
  public = list(
    #' @description Initialize token manager
    #' @param client_id TradeStation API client ID
    #' @param client_secret TradeStation API client secret
    #' @param token_path Path to save/load tokens (default: .tokens)
    initialize = function(client_id, client_secret, token_path = ".tokens") {
      assert_string(client_id, min.chars = 1)
      assert_string(client_secret, min.chars = 1)
      assert_path_for_output(token_path)
      
      private$.token_path <- token_path
      private$.client <- oauth_client(
        id = client_id,
        secret = client_secret,
        token_url = "https://signin.tradestation.com/oauth/token",
        auth_url = "https://signin.tradestation.com/authorize",
        name = "tradestationr"
      )
      
      # Try to load existing tokens
      private$.load_tokens()
    },
    
    #' @description Get current access token
    #' @return Access token string or NULL if not authenticated
    get_access_token = function() {
      if (!is.null(private$.token)) {
        private$.token$access_token
      } else {
        NULL
      }
    },
    
    #' @description Check if we have valid tokens
    #' @return logical indicating if we have tokens
    has_tokens = function() {
      !is.null(private$.token)
    },
    
    #' @description Perform initial OAuth2 authentication
    #' @param force If TRUE, force new authentication even if tokens exist
    authenticate = function(force = FALSE) {
      if (!force && self$has_tokens()) {
        log_info("Using existing tokens")
        return(invisible(self))
      }
      
      private$.token <- oauth_flow_auth_code(
        client = private$.client,
        scope = c("openid", "offline_access", "MarketData", "ReadAccount", "Trade")
      )
      
      private$.save_tokens()
      invisible(self)
    },
    
    #' @description Refresh access token using refresh token
    #' @return logical indicating if refresh was successful
    refresh_token = function() {
      if (!self$has_tokens()) {
        log_error("No tokens available to refresh")
        return(FALSE)
      }
      
      tryCatch({
        private$.token <- oauth_flow_refresh(
          client = private$.client,
          refresh_token = private$.token$refresh_token
        )
        private$.save_tokens()
        log_info("Access token refreshed successfully")
        TRUE
      }, error = function(e) {
        log_error(sprintf("Failed to refresh token: %s", e$message))
        FALSE
      })
    },
    
    #' @description Clear all stored tokens
    clear_tokens = function() {
      private$.token <- NULL
      if (file.exists(private$.token_path)) {
        unlink(private$.token_path)
      }
      log_info("Tokens cleared")
      invisible(self)
    }
  )
) 