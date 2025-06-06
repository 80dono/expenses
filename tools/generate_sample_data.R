library(tidyverse)

# ADD DOCSTRINGS!!
#' Generate sample grocery data
#' 
#' This function creates fake data resembling the actual grocery data recorded, for the purposes of demonstrating capability without revealing personal information.
#' 
#' @param months The number of months worth of data you wish to generate; assumes an average of 20 trips per month.
#' @return A data frame containing the date, total cost, number of items, and store for each example grocery trip.
generate_sample_grocery_data <- function(months = 5) {
  date <- seq(as.Date(today() - months(months)), today(), by = "day") %>% 
    sample(20 * months, replace = TRUE)
  items <- round(rbeta(20 * months, 3, 11) * 40) # Right-skewed distribution with a maximum of 40 items
  store <- sample(c("Aldi", "Food Lion", "Giant", "Harris Teeter", "Safeway", "Trader Joe's", "Whole Foods"), 20 * months, 
                  replace = TRUE, prob = c(0.05, 0.1, 0.15, 0.25, 0.1, 0.3, 0.05))
  # Total cost is a function of the number of items
  # Cost per item is right skewed with a minimum of $2 and maximum of $15
  cost <- items * (2 + (rbeta(20 * months, 6, 30) * 14)) %>% 
    round(2)
  
  return(data.frame(date, cost, items, store, real = FALSE))
}
