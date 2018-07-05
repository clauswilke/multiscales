# 2016 US polling data
# taken from:
#   https://github.com/uwdata/papers-vsup/tree/master/examples
#   Value-Suppressing Uncertainty Palettes
#   Michael Correll, Dominik Moritz, Jeffrey Heer
#   ACM Human Factors in Computing Systems (CHI), 2018

# Electoral Votes taken from: https://state.1keydata.com/state-electoral-votes.php

library(sf)
library(readr)
library(dplyr)
library(here)


usa_albers <- sf::st_transform(
  albersusa::usa_sf(),
  "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"
)

polling <- read_csv(here("data-raw", "US_polling", "polling.csv")) %>%
  mutate(
    Clinton = as.numeric(sub("%", "", `Hillary Clinton`)),
    moe = as.numeric(sub("%", "", `Margin of error`)),
    Clinton_ahead = sign(Clinton - as.numeric(sub("%", "", `Donald Trump`))),
    lead = ifelse(Lead == "Tied", 0, as.numeric(Lead)),
    moe_normalized = ifelse(moe / lead >= 1, 1, moe / lead),
    Clinton_lead = Clinton_ahead * lead
  ) %>%
  select(State, Clinton_lead, moe_normalized)

US_polling <- left_join(polling, rename(usa_albers, State = name))

ec_votes <- read_csv(here("data-raw", "US_polling", "electoral_votes.csv")) %>%
  rename(ec_votes = `Electoral Votes`)

US_polling <- left_join(US_polling, ec_votes)

# turn into sf data frame
US_polling <- st_as_sf(US_polling)

devtools::use_data(US_polling, overwrite = TRUE)


# also make cartogram version
library(cartogram)
US_polling_cartogram <- cartogram_cont(US_polling, 'ec_votes')

devtools::use_data(US_polling_cartogram, overwrite = TRUE)

