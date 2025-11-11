library(tidyverse)
library(googlesheets4)
source("tools/generate_sample_data.R")


#' Retrieve grocery data, either from Google Sheet or by simulation
#' 
#' If the user is authenticated, the function will retrieve the data stored in the Google Sheet. If not, sample data will be generated using `generate_sample_grocery_data()` (in *tools/generate_sample_data.R*).
#' 
#' @param authenticated A boolean indicator for whether the user has successfully been authenticated.
#' @return A data frame containing the date, total cost, number of items, and store for each grocery trip, along with a boolean indicator for whether the data is real.
get_grocery_data <- function(authenticated = FALSE) {
  if (authenticated) {
    # Read from Google sheet if possible
    data <- read_sheet("https://docs.google.com/spreadsheets/d/1-qP05bK-Vwapjy7cE382MNJpsaJebitlniGzDfrw-7k/edit?gid=0#gid=0",
                       range = "Groceries") %>% 
      mutate(real = TRUE)
    return(data)
  }
  else {
    # Use sample data if unable to read sheet
    message("Unable to access data; generating sample data instead.")
    sample_data <- generate_sample_grocery_data()
    return(sample_data)
  }
}

#' Retrieve credit card data, either from Google Sheet or by simulation
#' 
#' If the user is authenticated, the function will retrieve the data stored in the Google Sheet. If not, sample data will be generated using `generate_sample_credit_card_data()` (in *tools/generate_sample_data.R*).
#' 
#' @param authenticated A boolean indicator for whether the user has successfully been authenticated.
#' @return A data frame containing the date, cost, and type of utility for each bill, along with a boolean indicator for whether the data is real.
get_credit_card_data <- function(authenticated = FALSE) {
  if (authenticated) {
    # Read from Google sheet if possible
    data <- read_sheet("https://docs.google.com/spreadsheets/d/1-qP05bK-Vwapjy7cE382MNJpsaJebitlniGzDfrw-7k/edit?gid=0#gid=0",
                       range = "Credit Cards") %>% 
      mutate(real = TRUE)
    return(data)
  }
  else {
    # Use sample data if unable to read sheet
    message("Unable to access data; generating sample data instead.")
    sample_data <- generate_sample_credit_card_data()  ## Function does not exist yet!!
    return(sample_data)
  }
}


#' Retrieve utility data, either from Google Sheet or by simulation
#' 
#' If the user is authenticated, the function will retrieve the data stored in the Google Sheet. If not, sample data will be generated using `generate_sample_utility_data()` (in *tools/generate_sample_data.R*).
#' 
#' @param authenticated A boolean indicator for whether the user has successfully been authenticated.
#' @return A data frame containing the date, cost, and type of utility for each bill, along with a boolean indicator for whether the data is real.
get_utility_data <- function(authenticated = FALSE) {
  if (authenticated) {
    # Read from Google sheet if possible
    data <- read_sheet("https://docs.google.com/spreadsheets/d/1-qP05bK-Vwapjy7cE382MNJpsaJebitlniGzDfrw-7k/edit?gid=0#gid=0",
                       range = "Utilities") %>% 
      # Auto-generate Internet billing data, since cost ($89.99) and billing date (4th of the month) are fixed
      bind_rows(data.frame(date = seq.Date(as.Date("10/4/2024", "%m/%d/%Y"),
                                           floor_date(today()) + 3, 
                                           by = "month"),
                           cost = 89.99,
                           utility = "Internet")) %>% 
      mutate(real = TRUE)
    return(data)
  }
  else {
    # Use sample data if unable to read sheet
    message("Unable to access data; generating sample data instead.")
    sample_data <- generate_sample_utility_data()  ## Function does not exist yet!!
    return(sample_data)
  }
}

#' Retrieve income data, either from Google Sheet or by simulation
#' 
#' If the user is authenticated, the function will retrieve the data stored in the Google Sheet. If not, sample data will be generated using `generate_sample_income_data()` (in *tools/generate_sample_data.R*).
#' 
#' @param authenticated A boolean indicator for whether the user has successfully been authenticated.
#' @return A data frame containing the date, income amount, and income source for each instance, along with a boolean indicator for whether the data is real.
get_income_data <- function(authenticated = FALSE) {
  if (authenticated) {
    # Read from Google sheet if possible
    data <- read_sheet("https://docs.google.com/spreadsheets/d/1-qP05bK-Vwapjy7cE382MNJpsaJebitlniGzDfrw-7k/edit?gid=0#gid=0",
                       range = "Income") %>% 
      mutate(real = TRUE)
    return(data)
  }
  else {
    # Use sample data if unable to read sheet
    message("Unable to access data; generating sample data instead.")
    sample_data <- generate_sample_income_data()
    return(sample_data)
  }
}
