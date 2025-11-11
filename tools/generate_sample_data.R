library(tidyverse)


#' Generate sample grocery data
#' 
#' This function creates fake data resembling the actual grocery data recorded, for the purposes of demonstrating capability without revealing personal information.
#' 
#' @param months The number of months worth of data you wish to generate; default is 5.
#' @param trips_per_month The average number of trips to the grocery per month that you wish to simulate; default is 15.
#' @return A data frame containing the date, cost, and type of utility for each bill, along with a boolean indicator for whether the data is real.
generate_sample_grocery_data <- function(months = 5, trips_per_month = 15) {
  trips = months * trips_per_month
  date <- seq(as.Date(today() - months(months)), today(), by = "day") %>% 
    sample(trips, replace = TRUE)
  items <- round(rbeta(trips, 3, 11) * 40) # Right-skewed distribution with a maximum of 40 items
  store <- sample(c("Food Lion", "Giant", "Harris Teeter", "Safeway", "Trader Joe's", 
                    "Wegmans", "Whole Foods"), 
                  trips, replace = TRUE, prob = c(0.1, 0.15, 0.25, 0.1, 0.3, 0.05, 0.05))
  # Total cost is a function of the number of items
  # Cost per item is right skewed with a minimum of $2 and maximum of $15
  cost <- items * (2 + (rbeta(trips, 6, 30) * 14)) %>% 
    round(2)
  shopper <- sample(c("Jack", "Jill", "Both"), trips, prob = c(0.4, 0.45, 0.15))
  
  return(data.frame(date, cost, items, store, shopper, real = FALSE))
}

#' Generate sample utility data
#' 
#' This function creates fake data resembling the actual utility data recorded, for the purposes of demonstrating capability without revealing personal information.
#' 
#' @param months The number of months worth of data you wish to generate; default is 5.
#' @return A data frame containing the date, cost, and type of utility for each bill, along with a boolean indicator for whether the data is real.
generate_sample_utility_data <- function(months = 5) {
  return(data.frame())
}

#' Generate sample income data
#' 
#' This function creates fake data resembling the actual income data recorded, for the purposes of demonstrating capability without revealing personal information.
#' 
#' @param salary Annual salary for the simulation (after taxes).
#' @param months The number of months worth of data you wish to generate; default is 5.
#' @param interval Frequency at which the salary is paid out; default is monthly ("1 month").
#' @return A data frame containing the date, income amount, and source of income, along with a boolean indicator for whether the data is real.
generate_sample_income_data <- function(salary, months = 5, interval = "1 month") {
  dates <- seq(today() - months(months - 1), today(), by = interval)
  paycheck <- (salary / 12) / (length(dates) / months)
  birthday <- sample(seq.Date(min(dates), max(dates), by = "day"), 1)
  df <- data.frame(date = dates, income = round(paycheck, 2), source = "Dunder Mifflin", real = FALSE) %>% 
    add_row(date = birthday, income = 100, source = "Gift", real = FALSE)
  
  return(df)
}
