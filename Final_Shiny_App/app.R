library(shiny)
library(tidyverse)
library(tidymodels)
library(plotly)
library(shinydashboard)
library(DT)
library(broom)
library(leaflet)
library(sf)

#GLOBAL

runner_data <- read_csv(unzip("STAT385 Project (2000 to 2025 results).csv.zip", "STAT385 Project (2000 to 2025 results).csv"))
weather_data <- read_csv("STAT385 Project (weather working set).csv")
gpx_map_2000 <- st_read("2000 Chicago Marathon Route.gpx", layer = "tracks", quiet = TRUE)
gpx_map_2025 <- st_read("2025 Chicago Marathon Route.gpx", layer = "tracks", quiet = TRUE)

set.seed(1)
runners_finished <- runner_data %>%
  drop_na(finish_time_seconds) %>%
  filter(finish_time_seconds <= 23400) %>%
  group_by(year) %>%
  mutate(quartile = ntile(finish_time_seconds, 4)) %>%
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
    avg_apparent_temp_f = mean(apparent_temperature_f, na.rm = TRUE),
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
  left_join(racing_weather, by = "year")

spread_data <- bind_rows(
  final_data %>% mutate(runner_group = "General Sample"),
  top_1percent %>% mutate(runner_group = "Top 1% Elites")
)

# General sample Men vs. Women
gender_yearly <- final_data %>%
  drop_na(finish_time_seconds, gender) %>%
  filter(gender %in% c("Man", "Woman")) %>%
  group_by(year, gender) %>%
  summarize(
    avg_time_hours = mean(finish_time_seconds) / 3600,
    avg_temp = mean(avg_race_temp_f, na.rm = TRUE),
    avg_apparent_temp = mean(avg_apparent_temp_f, na.rm = TRUE),
    .groups = "drop"
  )

# Top 1% sample Men vs. Women
elite_gender_yearly <- top_1percent %>%
  drop_na(finish_time_seconds, gender) %>%
  filter(gender %in% c("Man", "Woman")) %>%
  group_by(year, gender) %>%
  summarize(
    avg_time_hours = mean(finish_time_seconds) / 3600,
    avg_temp = mean(avg_race_temp_f, na.rm = TRUE),
    avg_apparent_temp = mean(avg_apparent_temp_f, na.rm = TRUE),
    .groups = "drop"
  )

weather_story_data <- weather_data %>%
  mutate(year = year(time), hour = hour(time)) %>%
  filter(hour >= 7 & hour <= 15) %>% 
  group_by(year) %>%
  summarize(
    `Average Temperature (°F)` = round(mean(temperature_2m_f, na.rm = TRUE), 1),
    `Average Feels-Like Temp (°F)` = round(mean(apparent_temperature_f, na.rm = TRUE), 1),
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
    `Men's Winning (Hrs)` = round(min(finish_time_seconds[gender == "Man"]) / 3600, 2),
    `Men's Avg (Hrs)` = round(mean(finish_time_seconds[gender == "Man"]) / 3600, 2),
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

#significance and model fit
general_model <- lm(avg_time_hours ~ avg_temp, data = final_data %>% 
                      group_by(year) %>% 
                      summarize(avg_time_hours = mean(finish_time_seconds)/3600, 
                                avg_temp = mean(avg_race_temp_f)))
elite_model <- lm(avg_time_hours ~ avg_temp, data = top_1percent %>% 
                    group_by(year) %>% 
                    summarize(avg_time_hours = mean(finish_time_seconds)/3600, 
                              avg_temp = mean(avg_race_temp_f)))

#p-vals, t-stats, 95% CIs
general_stats <- tidy(general_model, conf.int = TRUE, level = 0.95)
elite_stats <- tidy(elite_model, conf.int = TRUE, level = 0.95)

#group by temp
temp_grouping <- final_data %>%
  mutate(temp_bin = cut(avg_race_temp_f, breaks = seq(30, 90, by = 5))) %>%
  group_by(temp_bin) %>%
  summarize(
    avg_finish_hours = mean(finish_time_seconds, na.rm = TRUE)/3600,
    runner_count = n(),
    .groups = "drop"
  )

#group by humidity
humidity_grouping <- final_data %>%
  mutate(humidity_bin = cut(avg_humidity_pct, breaks = seq(30, 100, by = 10))) %>%
  group_by(humidity_bin) %>%
  summarize(
    avg_finish_hours = mean(finish_time_seconds, na.rm = TRUE)/3600,
    runner_count = n(),
    .groups = "drop"
  )

#group by gender
gender_grouping <- final_data %>%
  drop_na(finish_time_seconds, gender) %>%
  filter(gender %in% c("Man", "Woman")) %>%
  group_by(year, gender) %>%
  summarize(
    avg_time = mean(finish_time_seconds) / 3600,
    avg_temp = mean(avg_race_temp_f),
    .groups = "drop"
  )

#bins of temp impact
temp_binned_plot <- ggplot(temp_grouping, aes(x=temp_bin, y= avg_finish_hours)) +
  geom_col(fill = "steelblue", alpha = 1) +
  labs(
    title = "Average Marathon Finish Time by Temperature Brackets",
    subtitle = "5°F Temperature Bins",
    x = "Temperature Bin (°F)",
    y = "Average Finish Time (Hours)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#static plot for gender
gender_plot <- ggplot(gender_yearly, aes(x = avg_temp, y = avg_time_hours, color = gender)) +
  geom_point(size = 2.5, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  scale_color_manual(values = c("Man" = "blue", "Woman" = "pink")) +
  labs(
    title = "Gender Heat Vulnerability Comparison",
    x = "Average Race Temperature (°F)",
    y = "Average Finish Time (Hours)",
    color = "Gender"
  ) +
  theme_minimal()

man_runners <- lm(avg_time ~ avg_temp, data = gender_grouping %>% filter(gender == "Man"))
woman_runners <- lm(avg_time ~ avg_temp, data = gender_grouping %>% filter(gender == "Woman"))

#year 2007 event
runners_2007 <- runner_data %>% filter(year == 2007)
finished_2007 <- runners_2007 %>% drop_na(finish_time_seconds) %>% nrow()
total_2007 <- nrow(runners_2007)

without_2007_data <- final_data %>% filter(year != 2007) %>% group_by(year) %>% summarize(time = mean(finish_time_seconds)/3600, temp = mean(avg_race_temp_f))
without_2007_model <- lm(time ~ temp, data = without_2007_data)

#2007 comparison
compare2007 <- final_data %>%
  mutate(is_2007 = ifelse(year == 2007, "2007", "Normal Years")) %>%
  group_by(is_2007) %>%
  summarize(
    avg_finish_hours = mean(finish_time_seconds, na.rm = TRUE)/3600,
    avg_temp_f = mean(avg_race_temp_f, na.rm = TRUE),
    runner_count = n()
  )

#statistical tests to compare 2007 and others years
test2007_data <- final_data %>%
  mutate(is_2007 = ifelse(year == 2007, "2007 Heat", "Normal Years"))
t_test_result <- t.test(finish_time_seconds~is_2007, data=test2007_data)
var_test_result <- var.test(finish_time_seconds~is_2007, data=test2007_data)

compare_2007_elites <- spread_data %>%
  filter(year == 2007) %>%
  group_by(runner_group) %>%
  summarize(
    avg_finish_hours = mean(finish_time_seconds, na.rm = TRUE) / 3600,
    avg_temp_f = mean(avg_race_temp_f, na.rm = TRUE),
    runner_count = n(),
    .groups = "drop"
  )

true_temp_model <- lm(avg_time_hours ~ avg_temp, data = final_data %>% group_by(year) %>% summarize(avg_time_hours = mean(finish_time_seconds)/3600, avg_temp = mean(avg_race_temp_f)))
apparent_temp_model <- lm(avg_time_hours ~ avg_apparent_temp, data = final_data %>% group_by(year) %>% summarize(avg_time_hours = mean(finish_time_seconds)/3600, avg_apparent_temp = mean(avg_apparent_temp_f)))

#difference in p-values
gender_diff_model <- lm(avg_time_hours ~ avg_temp*gender, data=gender_yearly)

#visualizing quartiles
viualize_quartiles <- final_data %>%
  group_by(year) %>%
  summarize(q5  = quantile(finish_time_seconds, 0.05, na.rm = TRUE)/3600,
            q25 = quantile(finish_time_seconds, 0.25, na.rm = TRUE)/3600,
            q50 = quantile(finish_time_seconds, 0.50, na.rm = TRUE)/3600,
            q75 = quantile(finish_time_seconds, 0.75, na.rm = TRUE)/3600,
            q95 = quantile(finish_time_seconds, 0.95, na.rm = TRUE)/3600,
            .groups = "drop") %>%
  arrange(desc(year))

#Tidymodels linear regression fitting
regression_simple <- linear_reg() |> set_engine("lm")
regression_general_sample_fit <- regression_simple |> fit(finish_time_seconds ~ avg_race_temp_f, data = final_data)
regression_top1percent_fit <- regression_simple |> fit(finish_time_seconds ~ avg_race_temp_f, data = top_1percent)

gen_slope <- tidy(regression_general_sample_fit)$estimate[2]  
top1_slope <- tidy(regression_top1percent_fit)$estimate[2] 

# UI - New Dashboard

ui <- dashboardPage(
  dashboardHeader(title = "Chicago Marathon Explorer"),
  
  # Sidebar Navigation - icons are from FontAwesome: https://fontawesome.com/
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar",
      menuItem("Overview", tabName = "overview", icon = icon("list")),
      menuItem("Data Explorer", 
               icon = icon("chart-simple"),
               menuSubItem("Weather Story", tabName = "weather_story"),
               menuSubItem("Race Story", tabName = "race_story"),
               menuSubItem("Apparent Temperature", tabName = "res_apparent"),
               menuSubItem("Temperature", tabName = "res_temp"),
               menuSubItem("Quartile Performance", tabName = "res_quartiles"),
               menuSubItem("Cohort & Demographics", tabName = "res_demographics"),
               menuSubItem("Distributions", tabName = "res_distributions")
      ),
      menuItem("Results/Findings", tabName = "results_findings", icon = icon("chart-bar")),
      menuItem("Course Map", tabName = "map", icon = icon("map")),
      menuItem("About", tabName = "about", icon = icon("circle-info"))
    )
  ),
  
  
  #  Body Contents
  dashboardBody(
    tabItems(
      # Overview Page
      tabItem(tabName = "overview",
              h2("The Effect of Weather on Performance at the Chicago Marathon", style = "font-size: 24px; font-weight: bold;"),
              p("This project analyzes the impact of weather on runner performance spanning from 2000 through 2025."),
              p("The Chicago Marathon is one of the world's most popular races, with over 50,000 runners participating each year."), 
              p("The course has changed over the years to better navigate the city and it crosses across many communities in the city. The primary external factor that impacts marathon finish time is weather, which impacts athletes both physically and mentally."),
              p("This project references a dataset of over 930,000 Chicago Marathon results and hourly weather from historical weather logs from the years 2000 to 2025. The goal of this project is to analyze how weather factors such as temperature, humidity, wind speed, and precipitation impact marathon finish times across different groups of runners.")
      ),
      
      # Data/Weather Story Page
      tabItem(tabName = "weather_story",
              h2("Weather Story"),
              p("This table shows hourly weather data recorded during the Chicago Marathon from 2000 to 2025."),
              p("*Note: 2020 Chicago Marathon Race was cancelled due to COVID.", style = "font-style: italic; color: #555; margin-bottom: 12px;"),
              DTOutput("weather_table")
      ),
      
      # Data/Race Story Page
      tabItem(tabName = "race_story",
              h2("Race Story"),
              p("This table shows runners' yearly performance metrics including winning times and averages."),
              p("*Note: 2020 Chicago Marathon Race was cancelled due to COVID.", style = "font-style: italic; color: #555; margin-bottom: 12px;"),
              DTOutput("race_table")
      ),
      
      # Results Sub-page 1: Apparent Temperature
      tabItem(tabName = "res_apparent",
              fluidRow(
                column(width = 3,
                       box(
                         title = "Controls & Filters",
                         status = "primary",
                         width = 12,
                         selectInput("group_app", "Choose Runner Group:",
                                     choices = list("General Sample", "Top 1% Elites")),
                         sliderInput("year_range_app", "Select Year Range:",
                                     min = 2000, max = 2025, value = c(2000, 2025), sep = "")
                       )
                ),
                column(width = 9,
                       plotlyOutput("scatter_plot_apparent"),
                       br(),
                       h4("Apparent Temperature Impact on Marathon"),
                       p("This scatter plot shows how apparent temperature impacts marathon finishing times"),
                       uiOutput("dynamic_summary_app"),
                       br(),
                       fluidRow(
                         valueBoxOutput("overall_avg_time_box_app", width=4),
                         valueBoxOutput("metric_avg_box_app", width=4),
                         valueBoxOutput("rsquared_box_app", width=4)
                       )
                )
              )
      ),
      
      # Results Sub-page 2: Temperature
      tabItem(tabName = "res_temp",
              fluidRow(
                column(width = 3,
                       box(
                         title = "Controls & Filters",
                         status = "primary",
                         width = 12,
                         selectInput("group_temp", "Choose Runner Group:",
                                     choices = list("General Sample", "Top 1% Elites")),
                         sliderInput("year_range_temp", "Select Year Range:",
                                     min = 2000, max = 2025, value = c(2000, 2025), sep = "")
                       )
                ),
                column(width = 9,
                       plotlyOutput("scatter_plot_temp"),
                       br(),
                       h4("Temperature Impact on Marathon"),
                       p("This scatter plot shows how ambient temperature impacts marathon finishing times"),
                       br(),
                       fluidRow(
                         valueBoxOutput("overall_avg_time_box_temp", width=4),
                         valueBoxOutput("metric_avg_box_temp", width=4),
                         valueBoxOutput("rsquared_box_temp", width=4)
                       )
                )
              )
      ),
      
      # Results Sub-page 3: Quartiles
      tabItem(tabName = "res_quartiles",
              fluidRow(
                column(width = 3,
                       box(
                         title = "Controls & Filters",
                         status = "primary",
                         width = 12,
                         selectInput("quartile_choice", "Choose Runner Quartile:",
                                     choices = list("Q1 (Fastest)" = 1, "Q2 (Average)" = 2,
                                                    "Q3 (Average)" = 3, "Q4 (Slowest)" = 4))
                       )
                ),
                column(width = 9,
                       plotlyOutput("scatter_plot_2"),
                       br(),
                       h4("Performance by Quartiles"),
                       p("This scatter plot shows how weather factors impact marathon finishing times for the selected runner quartile"),
                       br(),
                       fluidRow(
                         valueBoxOutput("quartile_avg_time_box", width=4),
                         valueBoxOutput("quartile_temp_box", width=4),
                         valueBoxOutput("rsquared_box_2", width=4)
                       )
                )
              )
      ),
      
      # Results Sub-page 4: Cohort & Demographics
      tabItem(tabName = "res_demographics",
              h3("Cohort & Demographic Comparisons"),
              plotlyOutput("elites_vs_general_plot"),
              br(),
              plotlyOutput("men_vs_women_plot"),
              br(),
              plotlyOutput("elite_men_vs_women_plot")
      ),
      
      # Results Sub-page 5: Distributions
      tabItem(tabName = "res_distributions",
              h3("Distributions"),
              plotOutput("percentile_plot")
      ),
      
      # Static Results/Findings
      tabItem(tabName = "results_findings",
              h2("Results and Findings"),
              p("This section goes over statistical conclusions with static regression visualizations and model outputs."),
              br(),
              #Temperature groups
              h4("1. Temperature Brackets and Marathon Performance"),
              plotOutput("static_temp_plot", height = 400),
              p("Temperatures on race day grouped into 5°F bins. There is a clear increasing trend in average finish times as the weather gets warmer."),
              br(),
              hr(),
              
              #Gender groups
              h4("2. Gender and Marathon Performance"),
              plotOutput("static_gender_plot", height = 400),
              p("This demonstrates how rising temperatures affect male and female runners at different rates.")
      ),      
      
      # Race Map Page
      tabItem(tabName = "map",
              h2("Course Map Comparison"),
              h3("Interactive Route Evolution: 2000 vs. 2025"),
              leafletOutput("course_map", height = 600)
      ),
      
      # About Page
      tabItem(tabName = "about",
              h2("About the Project"),
              h3("Methodology and Data Processing"),
              p("Data was sourced from official Chicago Marathon results and hourly Open-Meteo weather APIs from 2000–2025."),
              br(),
              h4("The 2007 Heat"),
              p("Note about the 2007 Chicago Marathon: Temperature spiked mid race at over 88°F which forced race organizers to stop the race early. Many runners suffered from heat stroke and one runner died.")
      )
    )
  )
)

#SERVER

server <- function(input, output, session) {
  
  output$weather_table <- renderDT({
    datatable(weather_story_data, rownames = FALSE,
              options = list(pageLength = 25, lengthChange = FALSE, searching = TRUE, scrollX = TRUE))
  })
  
  output$race_table <- renderDT({
    datatable(race_story_data, rownames = FALSE,
              options = list(pageLength = 25, lengthChange = FALSE, searching = TRUE, scrollX = TRUE))
  })
  
  # Value boxes for Apparent Temperature Page
  output$overall_avg_time_box_app <- renderValueBox({
    plot_data <- yearly_data_app()
    avg_val <- round(mean(plot_data$avg_time_hours, na.rm = TRUE), 2)
    valueBox(avg_val, "Overall Avg Time (Hours)")
  })
  
  output$metric_avg_box_app <- renderValueBox({
    plot_data <- yearly_data_app()
    avg_val <- round(mean(plot_data$metric_val, na.rm = TRUE), 2)
    valueBox(avg_val, "Selected Apparent Temp Avg (°F)")
  })
  
  output$rsquared_box_app <- renderValueBox({
    plot_data <- yearly_data_app()
    stat_model <- lm(avg_time_hours ~ metric_val, data = plot_data)
    r_sq <- round(summary(stat_model)$r.squared, 2)
    valueBox(r_sq, "R-squared")
  })
  
  output$dynamic_summary_app <- renderUI({
    plot_data <- yearly_data_app()
    stat_model <- lm(avg_time_hours ~ metric_val, data = plot_data)
    r_sq <- round(summary(stat_model)$r.squared*100, 2)
    slope_val_mins <- round(coef(stat_model)[2]*60, 2)
    
    tagList(
      p(paste0(
        "For the group (", input$group_app, "), the apparent temperature explains ", r_sq, 
        "% of the year to year variance in finish times. ",
        "Each 1°F increase in apparent temperature changes finish times by ≈", 
        slope_val_mins, " minutes. This demonstrates the impact humidity and temperatures has on runners."
      ))
    )
  })
  
  # Value boxes for Temperature Page
  output$overall_avg_time_box_temp <- renderValueBox({
    plot_data <- yearly_data_temp()
    avg_val <- round(mean(plot_data$avg_time_hours, na.rm = TRUE), 2)
    valueBox(avg_val, "Overall Avg Time (Hours)")
  })
  
  output$metric_avg_box_temp <- renderValueBox({
    plot_data <- yearly_data_temp()
    avg_val <- round(mean(plot_data$metric_val, na.rm = TRUE), 2)
    valueBox(avg_val, "Selected Temp Avg (°F)")
  })
  
  output$rsquared_box_temp <- renderValueBox({
    plot_data <- yearly_data_temp()
    stat_model <- lm(avg_time_hours ~ metric_val, data = plot_data)
    r_sq <- round(summary(stat_model)$r.squared, 2)
    valueBox(r_sq, "R-squared")
  })
  
  # Value boxes for Quartiles
  output$quartile_avg_time_box <- renderValueBox({
    plot_data <- yearly_data_2()
    avg_val <- round(mean(plot_data$avg_time_hours, na.rm = TRUE), 2)
    valueBox(avg_val, paste("Quartile", input$quartile_choice, "Avg Time (Hrs)"))
  })
  
  output$quartile_temp_box <- renderValueBox({
    plot_data <- yearly_data_2()
    avg_val <- round(mean(plot_data$avg_temp, na.rm = TRUE), 1)
    valueBox(avg_val, "Overall Avg Temp (°F)")
  })
  
  output$rsquared_box_2 <- renderValueBox({
    plot_data <- yearly_data_2()
    stat_model <- lm(avg_time_hours ~ avg_temp, data = plot_data)
    r_sq <- round(summary(stat_model)$r.squared, 2)
    valueBox(r_sq, "R-squared")
  })
  
  yearly_data_app <- reactive({
    spread_data %>%
      filter(
        runner_group == input$group_app,
        year >= input$year_range_app[1],
        year <= input$year_range_app[2]
      ) %>%
      group_by(year) %>%
      summarize(
        metric_val = mean(avg_apparent_temp_f, na.rm = TRUE),
        avg_time_hours = mean(finish_time_seconds, na.rm = TRUE) / 3600,
        .groups = "drop"
      ) %>%
      mutate(
        hover_info = paste("Year:", year, "\nApparent Temp:", round(metric_val, 1), "°F",
                           "\nTime:", round(avg_time_hours, 2), "Hrs")
      )
  })
  
  yearly_data_temp <- reactive({
    spread_data %>%
      filter(
        runner_group == input$group_temp,
        year >= input$year_range_temp[1],
        year <= input$year_range_temp[2]
      ) %>%
      group_by(year) %>%
      summarize(
        metric_val = mean(avg_race_temp_f, na.rm = TRUE),
        avg_time_hours = mean(finish_time_seconds, na.rm = TRUE) / 3600,
        .groups = "drop"
      ) %>%
      mutate(
        hover_info = paste("Year:", year, "\nTemperature:", round(metric_val, 1), "°F",
                           "\nTime:", round(avg_time_hours, 2), "Hrs")
      )
  })
  
  output$scatter_plot_apparent <- renderPlotly({
    p <- ggplot(yearly_data_app(), aes(x = metric_val, y = avg_time_hours)) +
      geom_point(aes(text = hover_info), alpha = 1, color = "steelblue", size = 3) + 
      geom_smooth(method = "lm", formula = y ~ x, color = "black", se = FALSE) +
      labs(title = paste("Different Weather Factors' Impact on Weather Performance (", input$group_app, ")"),
           x = "Apparent Temperature (°F)", y = "Finish Time Hours") + theme_minimal()
    ggplotly(p, tooltip = "text") %>%
      layout(margin = list(t=70))
  })
  
  output$scatter_plot_temp <- renderPlotly({
    p <- ggplot(yearly_data_temp(), aes(x = metric_val, y = avg_time_hours)) +
      geom_point(aes(text = hover_info), alpha = 1, color = "steelblue", size = 3) + 
      geom_smooth(method = "lm", formula = y ~ x, color = "black", se = FALSE) +
      labs(title = paste("Different Weather Factors' Impact on Weather Performance (", input$group_temp, ")"),
           x = "Temperature (°F)", y = "Finish Time Hours") + theme_minimal()
    ggplotly(p, tooltip = "text") %>%
      layout(margin = list(t=70))
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
        hover_info = paste("Year:", year, "\nTemperature:", round(avg_temp, 1), "°F",
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
  
  #elite vs general plot
  output$elites_vs_general_plot <- renderPlotly({
    gen_vs_elite_summary <- spread_data %>%
      group_by(year, runner_group) %>%
      summarize(
        avg_time = mean(finish_time_seconds) / 3600,
        avg_temp = mean(avg_race_temp_f, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        hover_info = paste("Year:", year,
                           "\nCohort:", runner_group,
                           "\nAvg Temp:", round(avg_temp, 2), "°F",
                           "\nAvg Time:", round(avg_time, 2), "Hrs")
      )
    p <- ggplot(gen_vs_elite_summary, aes(x = avg_temp, y = avg_time, color = runner_group)) +
      geom_point(aes(text = hover_info), size = 3, alpha = 1) +
      geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
      scale_color_manual(values = c("General Sample" = "steelblue", "Top 1% Elites" = "orange")) +
      labs(title = "Elite vs. General Runners Performance by Temperature",
           x = "Avg Race Temperature (°F)", y = "Avg Finish Time (Hours)", color = "Cohort") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text") %>% layout(margin=list(t = 50), legend = list(orientation="h", y = -0.3))
  })
  
  #men vs women plot
  output$men_vs_women_plot <- renderPlotly({
    gender_yearly_hover <- gender_yearly %>%
      mutate(
        hover_info = paste("Year:", year,
                           "\nGender:", gender,
                           "\nAvg Temp:", round(avg_temp,2), "°F",
                           "\nAvg Time:", round(avg_time_hours,2), "Hrs")
      )
    p <- ggplot(gender_yearly_hover, aes(x = avg_temp, y = avg_time_hours, color = gender)) +
      geom_point(aes(text = hover_info), size = 3, alpha = 1) +
      geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
      scale_color_manual(values = c("Man" = "blue", "Woman" = "pink")) +
      labs(title = "Men vs. Women Performance by Temperature",
           x = "Avg Race Temperature (°F)", y = "Avg Finish Time (Hours)", color = "Gender") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text") %>% layout(margin = list(t = 50), legend = list(orientation = "h", y = -0.3))
  })
  
  output$elite_men_vs_women_plot <- renderPlotly({
    elite_gender_yearly_hover <- elite_gender_yearly %>%
      mutate(
        hover_info = paste("Year:", year,
                           "\nGender:", gender,
                           "\nAvg Temp:", round(avg_temp,2), "°F",
                           "\nAvg Time:", round(avg_time_hours,2), "Hrs")
      )
    p <- ggplot(elite_gender_yearly_hover, aes(x = avg_temp, y = avg_time_hours, color = gender)) +
      geom_point(aes(text = hover_info), size = 3, alpha = 1) +
      geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
      scale_color_manual(values = c("Man" = "darkblue", "Woman" = "purple")) +
      labs(title = "Top 1% Elites: Men vs. Women Performance",
           x = "Avg Race Temperature (°F)", y = "Avg Finish Time (Hours)", color = "Gender") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text") %>% layout(margin = list(t = 50), legend = list(orientation = "h", y = -0.3))
  })
  
  #quartile_plot
  output$percentile_plot <- renderPlot({
    ggplot(viualize_quartiles, aes(y=as.factor(year))) +
      geom_segment(aes(x=q5, xend=q95, y=as.factor(year), yend=as.factor(year)), 
                   linewidth = 10, color = "lightblue", alpha = 0.5) +
      geom_segment(aes(x=q25, xend=q75, y = as.factor(year), yend = as.factor(year)), 
                   linewidth = 10, color = "deepskyblue2", alpha = 0.5) +
      geom_point(aes(x=q50, y=as.factor(year)), color = "blue", size = 2.5) +
      labs(
        title = "Chicago Marathon Finish Time Distribution by Year",
        subtitle = "Median finish times (medium blue), IQR 25th to 75th percentile (sky blue), 5th to 95th percentile spread (light blue)",
        x = "Finish Time (Hours)",
        y = "Year"
      ) +
      theme_minimal() +
      theme(
        panel.grid.minor = element_blank(),
        text = element_text(size = 12),
        plot.title = element_text(face = "bold", color= "purple")
      )
  })
  
  #Static temperature brackets
  output$static_temp_plot <- renderPlot({
    ggplot(temp_grouping, aes(x=temp_bin, y= avg_finish_hours)) +
      geom_col(fill = "steelblue", alpha = 1) +
      labs(
        title = "Average Marathon Finish Time by Temperature Brackets",
        subtitle = "5°F Temperature Bins",
        x = "Temperature Bin (°F)",
        y = "Average Finish Time (Hours)"
      ) +
      theme_minimal() +
      theme(
        text = element_text(size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(face = "bold", size = 16)
      )
  })

  
  #Static gender heat Plot
  output$static_gender_plot <- renderPlot({
    ggplot(gender_yearly, aes(x = avg_temp, y = avg_time_hours, color = gender)) +
      geom_point(size = 2.5, alpha = 1) +
      geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
      scale_color_manual(values = c("Man" = "blue", "Woman" = "pink")) +
      labs(
        title = "Gender Heat Vulnerability Comparison",
        x = "Average Race Temperature (°F)",
        y = "Average Finish Time (Hours)",
        color = "Gender"
      ) +
      theme_minimal() +
      theme(
        text = element_text(size = 14),
        axis.text = element_text(size = 12),
        plot.title = element_text(face = "bold", size = 16),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14, face = "bold")
      )
  })
  
  
  #map
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
}

#gender summary
final_data %>%
  filter(gender %in% c("Man", "Woman")) %>%
  group_by(gender) %>%
  summarize(
    avg_finish_hours = round(mean(finish_time_seconds, na.rm = TRUE) / 3600,2),
    avg_finish_minutes = sprintf("%.2f", mean(finish_time_seconds, na.rm = TRUE) / 60)
  )

shinyApp(ui, server)