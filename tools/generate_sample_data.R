library(tidyverse)


#' Generate sample grocery data
#' 
#' This function creates fake data resembling the actual grocery data recorded, for the purposes of demonstrating capability without revealing personal information.
#' 
#' @param months The number of months worth of data you wish to generate; default is 5.
#' @param trips_per_month The average number of trips to the grocery per month that you wish to simulate; default is 15.
#' @return A data frame containing the date, cost, and type of utility for each bill, along with a boolean indicator that the data is not real.
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
  
  return(tibble(date, cost, items, store, shopper, real = FALSE))
}

#' Generate sample utility data
#' 
#' This function creates fake data resembling the actual utility data recorded, for the purposes of demonstrating capability without revealing personal information.
#' 
#' @param months The number of months worth of data you wish to generate; default is 5.
#' @return A data frame containing the date, cost, and type of utility for each bill, along with a boolean indicator that the data is not real.
generate_sample_utility_data <- function(months = 5) {
  # Utilities and their base monthly cost ranges
  utility_profiles <- list(Phone = list(base = 60, noise = 5),
                           Internet = list(base = 80, noise = 8),
                           Water = list(base = 40, noise = 10),
                           Electricity = list(base = 120, noise = 40),
                           Gas = list(base = 50, noise = 30))
  
  dates <- seq(today() - months(months - 1), today(), by = "month")
  
  #' Seasonal multiplier for simulated utility data
  #' 
  #' @param utility Utility type
  #' @param month_num Numeric representation of the month for that utility bill (1 = Jan, ..., 12 = Dec)
  #' @return A numeric representing the dollar cost of that utility for the given month
  seasonal_factor <- function(utility, month_num) {
    if (utility == "Electricity") {
      # High in winter & summer
      return(1 + 0.3 * cos((month_num - 1) * pi/6))
    }
    if (utility == "Gas") {
      # High in winter, low in summer
      return(1 + 0.5 * cos((month_num - 1) * pi/6))
    }
    if (utility == "Water") {
      # Slightly higher in summer
      return(1 + 0.15 * sin((month_num - 1) * pi/6))
    }
    # Phone & Internet stable
    return(1)
  }
  
  # Generate data for each month
  df <- map_df(dates, function(d) {
    month_num <- month(d)
    
    map_df(names(utility_profiles), function(u) {
      base <- utility_profiles[[u]]$base
      noise <- utility_profiles[[u]]$noise
      season <- seasonal_factor(u, month_num)
      
      tibble(date = d, utility = u, cost = round(rnorm(1, mean = base * season, sd = noise), 2))
    })
  })
  
  
  df <- mutate(df, utility = as.factor(utility), real = FALSE)
  
  
  return(df)
}


#' Generate sample insurance data
#' 
#' This function creates fake data resembling the actual insurance data recorded, for the purposes of demonstrating capability without revealing personal information.
#' 
#' @param costs A float or vector of floats indicating the monthly cost of the type(s) of insurance.
#' @param types A string or vector of strings indicating the types of insurance associated with `costs`.
#' @param months The number of months worth of data you wish to generate; default is 5.
#' @return A data frame containing the date, cost, and type of insurance for each instance, along with a boolean indicator that the data is not real.
generate_sample_insurance_data <- function(costs, types, months = 5) {
  # Convert single values to vectors
  if (!is.vector(costs)) {
    costs <- c(as.numeric(costs))
  }
  if (!is.vector(types)) {
    types <- c(as.character(types))
  }
  
  # Add distributions for insurance costs by type (medical, dental, auto, etc.)
  
  dates <- seq(today() - months(months - 1), today(), by = "month")
  df <- tibble(date = dates, cost = costs, insurance = types, real = FALSE)
  
  return(df)
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
  df <- tibble(date = dates, income = round(paycheck, 2), source = "Dunder Mifflin", real = FALSE) %>% 
    add_row(date = birthday, income = 100, source = "Gift", real = FALSE)
  
  return(df)
}
