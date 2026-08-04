---
title: "Chicago Marathon Analysis"
author: "Miles Maloney"
format: html
server: shiny
---
  
#```{r}
#| label: setup
#| include: false
#| message: false
#| warning: false

library(shiny)
library(tidyverse)
library(plotly)
library(DT)
library(broom)
library(leaflet)
library(sf)

runner_data <- read_csv("STAT385 Project (2000 to 2025 results).csv")
weather_data <- read_csv("STAT385 Project (weather working set).csv")
gpx_map_2000 <- st_read("2000 Chicago Marathon Route.gpx", layer = "tracks", quiet = TRUE)
gpx_map_2025 <- st_read("2025 Chicago Marathon Route.gpx", layer = "tracks", quiet = TRUE)

# general data
set.seed(1)
runners_finished <- runner_data %>%
  drop_na(finish_time_seconds) %>%
  filter(finish_time_seconds <= 23400) %>%
  group_by(year) %>%
  mutate(quartile=ntile(finish_time_seconds, 4)) %>%
  group_by(year, quartile) %>%
  slice_sample(n = 1000) %>%
  ungroup() %>%
  mutate(
    start_time_seconds = case_when(
      quartile == 1 ~ (7*3600) + 30*60,
      quartile == 2 ~ (8*3600),
      quartile %in% c(3,4) ~ (8*3600) + 35*60
    ), 
    end_time_seconds = start_time_seconds + finish_time_seconds
  )

weather_needed <- weather_data %>%
  mutate(year = year(time), hour = hour(time))

racing_weather <- weather_needed %>%
  filter(hour >= 7 & hour <= 15) %>%
  group_by(year) %>%
  summarize(
    avg_race_temp_f = mean(temperature_2m_f, na.rm = TRUE), 
    avg_humidity_pct = mean(relative_humidity_2m_pct, na.rm = TRUE),
    total_precip_in = sum(precipitation_inch, na.rm = TRUE),
    .groups = "drop"
  )

final_data <- runners_finished %>%
  left_join(racing_weather, by = "year")

top_1percent <- runner_data %>%
  drop_na(finish_time_seconds) %>%
  filter(finish_time_seconds <= 23400) %>%
  group_by(year) %>%
  mutate(percentile = ntile(finish_time_seconds, 100)) %>%
  filter(percentile == 1) %>% 
  ungroup() %>%
  left_join(racing_weather, by ="year")

spread_data <- bind_rows(
  final_data %>% mutate(runner_group = "General Sample"),
  top_1percent %>% mutate(runner_group = "Top 1% Elites")
)

# data table
weather_story_data <- weather_data %>%
  mutate(year = year(time), hour = hour(time)) %>%
  filter(hour >= 7 & hour <= 15) %>% 
  group_by(year) %>%
  summarize(
    `Average Temperature (°F)` = round(mean(temperature_2m_f, na.rm = TRUE), 1),
    `8am Start Temperature (°F)` = round(mean(temperature_2m_f[hour == 8], na.rm = TRUE), 1),
    `11am Middle Temperature (°F)` = round(mean(temperature_2m_f[hour == 11], na.rm = TRUE), 1),
    `2pm End Temperature (°F)` = round(mean(temperature_2m_f[hour == 14], na.rm = TRUE), 1),
    `Avg Humidity (%)` = round(mean(relative_humidity_2m_pct), 1),
    `Total Precip (in)` = round(sum(precipitation_inch), 2),
    .groups = "drop"
  ) %>%
  arrange(desc(year))

race_story_data <- runner_data %>%
  drop_na(finish_time_seconds) %>%
  filter(finish_time_seconds <= 23400) %>% 
  group_by(year) %>%
  summarize(
    `Men's Winning (Hrs)` = round(min(finish_time_seconds[gender == "Man"]) / 3600, 2),`Men's Avg (Hrs)` = round(mean(finish_time_seconds[gender == "Man"]) / 3600, 2),
    `Women's Winning (Hrs)` = round(min(finish_time_seconds[gender == "Woman"]) / 3600, 2),
    `Women's Avg (Hrs)` = round(mean(finish_time_seconds[gender == "Woman"]) / 3600, 2),
    `Overall Avg (Hrs)` = round(mean(finish_time_seconds) / 3600, 2),
    .groups = "drop"
  ) %>%
  left_join(
    top_1percent %>%
      group_by(year) %>%
      summarize(`Elite Avg (Hrs)` = round(mean(finish_time_seconds) / 3600, 2)),
    by = "year"
  ) %>%
  select(year, `Men's Winning (Hrs)`, `Men's Avg (Hrs)`, `Women's Winning (Hrs)`, 
         `Women's Avg (Hrs)`, `Elite Avg (Hrs)`, `Overall Avg (Hrs)`) %>%
  arrange(desc(year))
#```

#```{r}
#| label: ui
#| echo: false

# UI
navbarPage(
  title = "Chicago Marathon Explorer",
  
  tabPanel("Chicago Marathon and Weather Data from 2000-2025",
           h3("Welcome to the Chicago Marathon Dashboard"),
           p("This project analyzes the impact of weather on runner performance.")
  ),
  
  tabPanel("Data Tables",
           h3("Weather Data"),
           DTOutput("weather_table"),
           br(),
           h3("Race Results"),
           DTOutput("race_table")
  ),
  
  tabPanel("Results/Findings",
           h3("Temperature Reaction: General vs. Top 1% Elites"),
           sidebarLayout(
             sidebarPanel(
               h4("Controls & Filters"),
               selectInput("group", "Choose Runner Group:", 
                           choices = list("General Sample", "Top 1% Elites")),
               selectInput("weather_metric", "Select Weather Factor:",
                           choices = list("Temperature (°F)" = "avg_race_temp_f",
                                          "Relative Humidity (%)" = "avg_humidity_pct",
                                          "Precipitation (in)" = "total_precip_in"),
                           selected = "avg_race_temp_f"),
               sliderInput("year_range", "Select Year Range:",
                           min = 2000, max = 2025, value = c(2000, 2025), sep = "")
             ),
             mainPanel(
               plotlyOutput("scatter_plot_1"),
               br(),
               tableOutput("summary_table_1")
             )
           ),
           hr(),
           h3("Temperature Reaction by Quartile"),
           sidebarLayout(
             sidebarPanel(
               selectInput("quartile_choice", "Choose Runner Quartile:", 
                           choices = list("Q1 (Fastest)" = 1, "Q2 (Average)" = 2, 
                                          "Q3 (Average)" = 3, "Q4 (Slowest)" = 4))
             ),
             mainPanel(
               plotlyOutput("scatter_plot_2"),
               br(),
               tableOutput("summary_table_2")
             )
           )
  ),
  tabPanel("Course Map Comparison",
           h3("Interactive Route Evolution: 2000 vs. 2025"),
           p("Toggle routes on and off with the checkboxes"),
           leafletOutput("course_map", height = 600)
  ),
  tabPanel("About the Project",
           h3("Methodology and Data Processing"),
           p("Data was sourced from Chicago Marathon results and weather APIs from 2000-2025.")
  )
)
#```

#```{r}
#| context: server

# server
output$weather_table <- renderDT({
  datatable(weather_story_data, rownames = FALSE, 
            options = list(pageLength = 5, searching = TRUE, scrollX = TRUE))
})

output$race_table <- renderDT({
  datatable(race_story_data, rownames = FALSE, 
            options = list(pageLength = 5, searching = TRUE, scrollX = TRUE))
})

yearly_data_1 <- reactive({
  spread_data %>% 
    filter(
      runner_group == input$group,
      year >= input$year_range[1],
      year <= input$year_range[2]
    ) %>%
    group_by(year) %>%
    summarize(
      metric_val = mean(.data[[input$weather_metric]], na.rm = TRUE),
      avg_time_hours = mean(finish_time_seconds, na.rm = TRUE) / 3600,
      .groups = "drop"
    ) %>%
    mutate(
      hover_info = paste("Year:", year, "\nWeather Val:", round(metric_val, 1),
                         "\nTime:", round(avg_time_hours, 2), "Hrs")
    )
})

output$scatter_plot_1 <- renderPlotly({
  p <- ggplot(yearly_data_1(), aes(x = metric_val, y = avg_time_hours)) +
    geom_point(aes(text = hover_info), alpha = 1, color = "steelblue", size = 3) + 
    geom_smooth(method = "lm", formula = y ~ x, color = "black", se = FALSE) +
    labs(title = paste("Yearly Performance of", input$group),
         x = "Selected Weather Factor", y = "Finish Time Hours") + theme_minimal()
  ggplotly(p, tooltip = "text") %>% 
    layout(margin = list(t=70))
})

output$summary_table_1 <- renderTable({
  plot_data <- yearly_data_1()
  stat_model <- lm(avg_time_hours~metric_val, data = plot_data)
  r_squared <- round(summary(stat_model)$r.squared, 5)
  spread_data %>%
    filter(runner_group == input$group, year >= input$year_range[1], year <= input$year_range[2]) %>%
    summarize(`Overall Avg Time (Hours)` = mean(finish_time_seconds, na.rm = TRUE)/3600,
              `Selected Metric Avg` = mean(.data[[input$weather_metric]], na.rm = TRUE)) %>% mutate(`R-squared` = r_squared)
})

yearly_data_2 <- reactive({
  final_data %>%
    filter(quartile == input$quartile_choice) %>%
    group_by(year) %>%
    summarize(
      avg_temp = mean(avg_race_temp_f, na.rm = TRUE),
      avg_time_hours = mean(finish_time_seconds, na.rm = TRUE)/3600,
      .groups = "drop"
    ) %>%
    mutate(
      hover_info = paste("Year:", year, "\nTemp:", round(avg_temp, 1), "°F",
                         "\nTime:", round(avg_time_hours, 2), "Hrs")
    )
})

output$scatter_plot_2 <- renderPlotly({
  p <- ggplot(yearly_data_2(), aes(x = avg_temp, y = avg_time_hours)) +
    geom_point(aes(text = hover_info), alpha = 1, color = "maroon", size = 3) + 
    geom_smooth(method = "lm", formula = y ~ x, color = "black", se = FALSE) +
    labs(title = paste("Yearly Performance of Quartile", input$quartile_choice),
         x = "Average Temperature (°F)", y = "Finish Time (Hours)") + theme_minimal()
  ggplotly(p, tooltip = "text") %>% 
    layout(margin = list(t=70))
}) 

output$summary_table_2 <- renderTable({
  plot_data <- yearly_data_2()
  stat_model <- lm(avg_time_hours ~ avg_temp, data = plot_data)
  r_squared <- round(summary(stat_model)$r.squared, 5)
  final_data %>%
    filter(quartile == input$quartile_choice) %>%
    summarize(`Overall Avg Time (Hours)` = mean(finish_time_seconds, na.rm = TRUE)/3600,
              `Overall Avg Temp (°F)` = mean(avg_race_temp_f, na.rm = TRUE)) %>%
    mutate(`R-squared` = r_squared)
})

output$course_map <- renderLeaflet({
  leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron) %>% 
    addPolylines(data = gpx_map_2000, color = "orange", weight = 5, opacity = 0.9, 
                 label = "2000 Course", group = "2000 Course") %>%
    addPolylines(data = gpx_map_2025, color = "blue", weight = 5, opacity = 0.9, 
                 label = "2025 Course", group = "2025 Course") %>%
    addLayersControl(
      overlayGroups = c("2000 Course", "2025 Course"),
      options = layersControlOptions(collapsed = FALSE)
    )
})
#figuring out map
#install.packages(c("sf", "mapview"))
#library(sf)
#library(mapview)
#mapview(gpx_map_2000)
#mapview(gpx_map_2025)

#```