# Libraries
library(tidyverse)
library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(broom)
library(tidymodels)
library(leaflet)
library(sf)

# Import external data
runner_data <- read_csv(unzip("data/race_results.csv.zip", "data/race_results.csv")) 
weather_data <- read_csv("data/weather.csv")
gpx_map_2000 <- st_read("data/marathon_route_2000.gpx", layer = "tracks", quiet = TRUE)
gpx_map_2025 <- st_read("data/marathon_route_2025.gpx", layer = "tracks", quiet = TRUE)