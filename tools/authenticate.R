library(googlesheets4)


#' Authenticates the user for access to the Google Sheet.
#' 
#' One of `service_account_key` and `gmail` must be passed to the function for authentication to be successful. If both are passed, the email will only be used if service account authentication fails.
#' 
#' @param service_account_key The string path to the Google Cloud Console service account key. Defaults to `NULL`.
#' @param gmail The GMail account address to use if `service_account_key` fails or is missing. Defaults to `NULL`.
#' @return A boolean indicating whether authentication was successful.
google_authenticate <- function(service_account_key = NULL, gmail = NULL) {
  authenticated <- FALSE
  
  tryCatch(
    {
      # Attempt Google Cloud Console service account authentication (primary)
      gs4_auth(path = service_account_key, cache = FALSE)
      message("Successfully authenticated with service account!")
      authenticated <- TRUE
    },
    error = function(err) {
      message("Service account authentication failed. Trying email...")
      
      # Attempt email authentication as fallback
      tryCatch(
        {
          gs4_auth(email  = gmail,
                   scopes = "https://www.googleapis.com/auth/spreadsheets.readonly",
                   cache  = ".secrets")
          message("Successfully authenticated with email!")
          authenticated <- TRUE
        },
        error = function(err2) {
          message("Email authentication also failed. User is not authenticated.")
        }
      )
    }
  )
  
  return(authenticated)
}
