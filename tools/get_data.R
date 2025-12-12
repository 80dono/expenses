library(tidyverse)
library(googlesheets4)
library(tsibble)
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

#' Apply basic formatting that is relevant to all expense and income data sheets.
#' 
#' @param df The data frme you wish to format.
#' @param add_week A boolean indicating whether to add a parameter for the week of the date; default is FALSE.
#' @param add_month A boolean indicating whether to add a parameter for the month of the date; default is TRUE.
#' @param add_quarter A boolean indicating whether to add a parameter for the quarter of the date; default is FALSE.
#' @param add_year A boolean indicating whether to add a parameter for the year of the date; default is TRUE.
#' @return A data frame with properly formatted dates.
format_date <- function(df, add_week = FALSE, add_month = TRUE, add_quarter = FALSE, add_year = TRUE) {
  df_fmt <- mutate(df, date = as.Date(date))
  df_fmt$week <- if (add_week) floor_date(df_fmt$date, unit = "week")
  df_fmt$month <- if (add_month) floor_date(df_fmt$date, unit = "month")
  df_fmt$quarter <- if(add_quarter) yearquarter(df_fmt$date)
  df_fmt$year <- if (add_year) year(df_fmt$date)
  
  return(df_fmt)
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
      mutate(cost = if_else(utility == "Internet" & date > as.Date("2025-12-01"), # ADD END DATE
                            64.99, cost),
             real = TRUE)
    return(data)
  }
  else {
    # Use sample data if unable to read sheet
    message("Unable to access data; generating sample data instead.")
    sample_data <- generate_sample_utility_data()  ## Function does not exist yet!!
    return(sample_data)
  }
}


#' Retrieve insurance data, either from Google Sheet or by simulation
#' 
#' If the user is authenticated, the function will retrieve the data stored in the Google Sheet. If not, sample data will be generated using `generate_sample_insurance_data()` (in *tools/generate_sample_data.R*).
#' 
#' @param authenticated A boolean indicator for whether the user has successfully been authenticated.
#' @return A data frame containing the date, cost, and type of insurance for each instance, along with a boolean indicator for whether the data is real.
get_insurance_data <- function(authenticated = FALSE) {
  if (authenticated) {
    # Read from Google sheet if possible
    data <- read_sheet("https://docs.google.com/spreadsheets/d/1-qP05bK-Vwapjy7cE382MNJpsaJebitlniGzDfrw-7k/edit?gid=0#gid=0",
                       range = "Insurance") %>% 
      mutate(date = as.Date(date), real = TRUE)
    return(data)
  }
  else {
    # Use sample data if unable to read sheet
    message("Unable to access data; generating sample data instead.")
    sample_data <- generate_sample_insurance_data()
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
      mutate(date = as.Date(date), real = TRUE)
    return(data)
  }
  else {
    # Use sample data if unable to read sheet
    message("Unable to access data; generating sample data instead.")
    sample_data <- generate_sample_income_data()
    return(sample_data)
  }
}
