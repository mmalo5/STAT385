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
imported_data_1 <- imported_data |>
  mutate(pass_or_not = ifelse(Results == "Pass", 1, 0))

imported_data_1 |>
  select(pass_or_not)

#rename columns
names(imported_data_1)
#all lowercase, no whitespace
imported_data_2 <- imported_data_1 |> 
  rename('Inspection_ID' = `Inspection I`)
  #mutate(lowercase = names(imported_data_1)

#w/ base r, names function
names(imported_data_1) <-  
  