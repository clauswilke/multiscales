library(tidycensus)
library(tidyverse)

options(tigris_use_cache = TRUE)

# B25077_001: Median house value in the past 12 months (in 2015 Inflation-adjusted dollars)
FL_house_values <- get_acs(state = "FL",  geography = "county", year = 2015, variables = "B25077_001", geometry = TRUE)

devtools::use_data(FL_house_values, overwrite = TRUE)
