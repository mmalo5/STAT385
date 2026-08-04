library(shiny)
library(tidyverse)
library(plotly)
imported_data <- read_csv("https://uofi.box.com/shared/static/0twe4rndqm2r45lkxhy3h74uypj06tmm.csv")
#facility type = Restaurant
#data_restaurant <- filter(imported_data["Facility Type" = 'Restaurant'])
#data_restaurant <- filter([])
#imported_data["Facility Type" = 'Restaurant']

mutated |>
  select(results) |>
  mutate(imported_data, ifelse(results = 'pass', 1, 0))
           
                                   
#pass_or_not
#if results = pass, then its a 1, otherwise a 0
