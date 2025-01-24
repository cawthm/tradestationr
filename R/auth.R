#' @title Authentication Handler
#' @description Manages OAuth2 authentication and token lifecycle
#' @importFrom httr2 oauth_client oauth_flow_auth_code oauth_flow_refresh oauth_token_cached
#' @importFrom logger log_info log_error
#' @importFrom checkmate assert_string assert_path_for_output assert_directory_exists

#' @export
TokenManager <- R6::R6Class(
  "TokenManager",
  
  public = list(
    #' @description Initialize token manager
    #' @param secrets_dir Path to .secrets directory
    initialize = function(secrets_dir = ".secrets") {
      assert_directory_exists(secrets_dir)
      
      private$.secrets_dir <- secrets_dir
      private$.client_creds_path <- file.path(secrets_dir, ".client_creds.rds")
      private$.refresh_token_path <- file.path(secrets_dir, ".refresh_token.rds")
      private$.access_token_path <- file.path(secrets_dir, ".ts_tokens.rds")
      
      # Initialize refresh cooldown tracking
      private$.last_refresh_time <- 0
      private$.refresh_cooldown <- 60  # Minimum seconds between refresh attempts
      
      # Load credentials and initialize client
      creds <- private$.load_credentials()
      private$.client <- oauth_client(
        id = creds$client_id,
        secret = creds$client_secret,
        token_url = "https://signin.tradestation.com/oauth/token",
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
      
      # Save both tokens
      private$.save_access_token()
      private$.save_refresh_token()
      private$.last_refresh_time <- as.numeric(Sys.time())
      
      invisible(self)
    },
    
    #' @description Refresh access token using refresh token
    #' @param refresh_token Optional refresh token to use (otherwise uses stored token)
    #' @param force If TRUE, bypass cooldown period
    #' @return logical indicating if refresh was successful
    refresh_token = function(refresh_token = NULL, force = FALSE) {
      # Check cooldown period unless forced
      current_time <- as.numeric(Sys.time())
      time_since_refresh <- current_time - private$.last_refresh_time
      
      if (!force && time_since_refresh < private$.refresh_cooldown) {
        log_info(sprintf("Skipping refresh, in cooldown period (%.1f seconds remaining)", 
                        private$.refresh_cooldown - time_since_refresh))
        return(TRUE)  # Return true to prevent cascading refresh attempts
      }
      
      if (is.null(refresh_token)) {
        if (!file.exists(private$.refresh_token_path)) {
          log_error("No refresh token available")
          return(FALSE)
        }
        refresh_token <- readRDS(private$.refresh_token_path)
      }
      
      tryCatch({
        private$.token <- oauth_flow_refresh(
          client = private$.client,
          refresh_token = refresh_token
        )
        private$.save_access_token()  # Only update access token
        private$.last_refresh_time <- current_time
        log_info("Access token refreshed successfully")
        TRUE
      }, error = function(e) {
        log_error(sprintf("Failed to refresh token: %s", e$message))
        FALSE
      })
    },
    
    #' @description Force update of access token
    #' @return logical indicating if update was successful
    update_access_token = function() {
      self$refresh_token(force = FALSE)  # Use cooldown by default
    },
    
    #' @description Clear all stored tokens
    clear_tokens = function() {
      private$.token <- NULL
      if (file.exists(private$.access_token_path)) {
        unlink(private$.access_token_path)
      }
      if (file.exists(private$.refresh_token_path)) {
        unlink(private$.refresh_token_path)
      }
      log_info("Tokens cleared")
      invisible(self)
    }
  ),
  
  private = list(
    .client = NULL,
    .secrets_dir = NULL,
    .client_creds_path = NULL,
    .refresh_token_path = NULL,
    .access_token_path = NULL,
    .token = NULL,
    .last_refresh_time = NULL,
    .refresh_cooldown = NULL,
    
    # Load credentials from .secrets
    .load_credentials = function() {
      if (!file.exists(private$.client_creds_path)) {
        stop("Client credentials not found at: ", private$.client_creds_path)
      }
      
      creds <- readRDS(private$.client_creds_path)
      if (is.null(creds$client_id) || is.null(creds$client_secret)) {
        stop("Invalid credentials format in .client_creds.rds")
      }
      
      creds
    },
    
    # Save access token to disk
    .save_access_token = function() {
      if (!is.null(private$.token)) {
        tryCatch({
          saveRDS(private$.token, private$.access_token_path)
          log_info("Access token saved")
        }, error = function(e) {
          log_error(sprintf("Failed to save access token: %s", e$message))
        })
      }
    },
    
    # Save refresh token to disk
    .save_refresh_token = function() {
      if (!is.null(private$.token$refresh_token)) {
        tryCatch({
          saveRDS(private$.token$refresh_token, private$.refresh_token_path)
          log_info("Refresh token saved")
        }, error = function(e) {
          log_error(sprintf("Failed to save refresh token: %s", e$message))
        })
      }
    },
    
    # Load tokens from disk
    .load_tokens = function() {
      # Try to load refresh token first
      if (file.exists(private$.refresh_token_path)) {
        refresh_token <- readRDS(private$.refresh_token_path)
        if (!is.null(refresh_token)) {
          # Use refresh token to get new access token
          return(self$refresh_token(refresh_token, force = TRUE))  # Force initial refresh
        }
      }
      
      # Fall back to access token if available
      if (file.exists(private$.access_token_path)) {
        private$.token <- readRDS(private$.access_token_path)
        return(!is.null(private$.token))
      }
      
      FALSE
    }
  )
) 