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

runner_data <- read_csv("STAT385 Project (2000 to 2025 results).csv")
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
#print(general_stats)
#print(elite_stats)

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

man_runners <- lm(avg_time ~ avg_temp, data = gender_grouping %>% filter(gender == "Man"))
woman_runners <- lm(avg_time ~ avg_temp, data = gender_grouping %>% filter(gender == "Woman"))

#year 2007 event
runners_2007 <- runner_data %>% filter(year == 2007)
finished_2007 <- runners_2007 %>% drop_na(finish_time_seconds) %>% nrow()
total_2007 <- nrow(runners_2007)

cat("2007 Runners:", total_2007, "\n")
cat("2007 Finishers:", finished_2007, "\n")

without_2007_data <- final_data %>% filter(year != 2007) %>% group_by(year) %>% summarize(time = mean(finish_time_seconds)/3600, temp = mean(avg_race_temp_f))
without_2007_model <- lm(time ~ temp, data = without_2007_data)
print(summary(without_2007_model))

#difference in p-values
gender_diff_model <- lm(avg_time_hours ~ avg_temp*gender, data=gender_yearly)
print(summary(gender_diff_model))

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

gen_t_stat <- tidy(regression_general_sample_fit)$statistic[2]
gen_df <- glance(regression_general_sample_fit)$df.residual
gen_log_p <- log(2) + pt(-abs(gen_t_stat), df=gen_df, log.p=TRUE)
gen_log10 <- gen_log_p/log(10) 

top1_t_stat <- tidy(regression_top1percent_fit)$statistic[2]
top1_df <- glance(regression_top1percent_fit)$df.residual
top1_log_p <- log(2) + pt(-abs(top1_t_stat), df=top1_df, log.p=TRUE)
top1_log10 <- top1_log_p/log(10)

#Slopes and r.squared metrics
gen_slope <- tidy(regression_general_sample_fit)$estimate[2]  
gen_rsquared <- glance(regression_general_sample_fit)$r.squared 
top1_slope <- tidy(regression_top1percent_fit)$estimate[2] 
top1_rsquared <- glance(regression_top1percent_fit)$r.squared

#Temperature increase impact table
temp_incr <- c(1,2,3,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75)
heat_incr_table <- tibble(
  temp_incr_f = temp_incr,
  gen_sample_added_minutes = (gen_slope*temp_incr)/60,
  top_1percent_added_minutes = (top1_slope*temp_incr)/60
)

#quartile regressions
quartile_regressions <- final_data %>%
  group_by(quartile) %>%
  summarize(
    slope_minutes_per_degree = coef(lm(finish_time_seconds ~ avg_race_temp_f))[2]/60,
    r_squared = summary(lm(finish_time_seconds ~ avg_race_temp_f))$r.squared
  )

summary_stats_text <- paste0(
  "For the Chicago Marathon, race-day temperature explains ", round(summary(without_2007_model)$r.squared * 100, 3), 
  "% of the year-over-year variance in average finish times across our dataset of ", format(nrow(final_data), big.mark=","), " runners. ",
  "For an increase of 1°F, the general running population slows down their marathon time by ", round(gen_slope / 60, 3), 
  " minutes on average, and the elite top 1% of runners slow down by ", round(top1_slope/60, 3), " minutes on average."
)
summary_stats_text

# UI - New Dashboard
ui <- dashboardPage(
  dashboardHeader(title = "Chicago Marathon Explorer"),

  # Sidebar Navigation - icons are from FontAwesome: https://fontawesome.com/
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar",
      menuItem("Overview", tabName = "overview", icon = icon("list")),
      menuItem("Data Explorer", 
        icon = icon("compass"),
        menuSubItem("Weather Story", tabName = "weather_story"),
        menuSubItem("Race Story", tabName = "race_story")
      ),
      menuItem("Results/Findings", tabName = "results", icon = icon("chart-simple")),
      menuItem("Course Map", tabName = "map", icon = icon("map")),
      menuItem("About", tabName = "about", icon = icon("circle-info"))
    )
  ),

#  Body Contents
  dashboardBody(
    tabItems(
      # Overview Page
      tabItem(tabName = "overview",
        h2("Welcome to the Chicago Marathon Dashboard"),
        p("This project analyzes the impact of weather on runner performance spanning from 2000 through 2025.")
      ),

      # Data/Weather Story Page
      tabItem(tabName = "weather_story",
        h2("Weather Story"),
        p("*Note: 2020 Chicago Marathon Race was cancelled due to COVID.", style = "font-style: italic; color: #555; margin-bottom: 12px;"),
        DTOutput("weather_table")
      ),

      # Data/Race Story Page
      tabItem(tabName = "race_story",
        h2("Race Story"),
        p("*Note: 2020 Chicago Marathon Race was cancelled due to COVID.", style = "font-style: italic; color: #555; margin-bottom: 12px;"),
        DTOutput("race_table")
      ),

      # Results/Findings Page
      tabItem(tabName = "results",
        # Yearly Performance
        fluidRow(
          column(width = 3,
            box(
              title = "Controls & Filters",
              status = "primary",
              width = 12,
              selectInput("group", "Choose Runner Group:",
                choices = list("General Sample", "Top 1% Elites")),
              selectInput("weather_metric", "Select Weather Factor:",
                choices = list("Temperature (°F)" = "avg_race_temp_f",
                              "Relative Humidity (%)" = "avg_humidity_pct",
                              "Precipitation (in)" = "total_precip_in"),
                selected = "avg_race_temp_f"),
              sliderInput("year_range", "Select Year Range:",
                min = 2000, max = 2025, value = c(2000, 2025), sep = "")
            )
          ),
          column(width = 9,
            plotlyOutput("scatter_plot_1"),
            br(),
            h4("Weather Impact on Marathon"),
            p("This scatter plot shows how weather factors impact marathon finishing times"),
            tableOutput("summary_table_1")
          )
        ),

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
            tableOutput("summary_table_2")
          )
        ),

        # Demographic Charts
        br(),
        h3("Cohort & Demographic Comparisons"),
        plotlyOutput("elites_vs_general_plot"),
        br(),
        plotlyOutput("men_vs_women_plot"),
        br(),
        plotlyOutput("elite_men_vs_women_plot"),

        # Distribution Chart
        br(),
        h3("Distributions"),
        plotOutput("percentile_plot")
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

#UI

# ui <- navbarPage(
#   title = "Chicago Marathon Explorer",
#   tabPanel("Overview",
#            div(style = "padding: 30px;",
#                h3("Welcome to the Chicago Marathon Dashboard"),
#                p("This project analyzes the impact of weather on runner performance spanning from 2000 through 2025.")
#            )
#   ),
  
#   tabPanel("Data Explorer",
#            navlistPanel("Stories",
#                         tabPanel("Weather Story",
#                                  div(style = "padding: 20px;",
#                                      h3("Marathon Weather Conditions 2000-2025"),
#                                      p("*Note: 2020 Chicago Marathon Race was cancelled due to COVID.", style = "font-style: italic; color: #555; margin-bottom: 12px;"),
#                                      DTOutput("weather_table")
#                                  )
#                         ),
#                         tabPanel("Race Story",
#                                  div(style = "padding: 20px;",
#                                      h3("Marathon Finish Time Records & Cohort Story"),
#                                      p("*Note: 2020 Chicago Marathon Race was cancelled due to COVID.", style = "font-style: italic; color: #555; margin-bottom: 12px;"),
#                                      DTOutput("race_table")
#                                  )
#                         )
#            )
#   ),
  
#   tabPanel("Results/Findings",
#            sidebarLayout(
#              sidebarPanel(
#                h4("Controls & Filters"),
#                selectInput("group", "Choose Runner Group:",
#                            choices = list("General Sample", "Top 1% Elites")),
#                selectInput("weather_metric", "Select Weather Factor:",
#                            choices = list("Temperature (°F)" = "avg_race_temp_f",
#                                           "Relative Humidity (%)" = "avg_humidity_pct",
#                                           "Precipitation (in)" = "total_precip_in"),
#                            selected = "avg_race_temp_f"),
#                sliderInput("year_range", "Select Year Range:",
#                            min = 2000, max = 2025, value = c(2000, 2025), sep = "")
#              ),
#              mainPanel(
#                plotlyOutput("scatter_plot_1"),
#                br(),
#                h4("Weather Impact on Marathon"),
#                p("This scatter plot shows how weather factors impact marathon finishing times"),
#                tableOutput("summary_table_1")
#              )
#            ),
#            hr(),
#            sidebarLayout(
#              sidebarPanel(
#                selectInput("quartile_choice", "Choose Runner Quartile:",
#                            choices = list("Q1 (Fastest)" = 1, "Q2 (Average)" = 2,
#                                           "Q3 (Average)" = 3, "Q4 (Slowest)" = 4))
#              ),
#              mainPanel(
#                plotlyOutput("scatter_plot_2"),
#                br(),
#                tableOutput("summary_table_2")
#              )
#            ),
#            hr(), 
#            h3("Cohort & Demographic Comparisons"),
#            fluidRow(
#              column(6, plotlyOutput("elites_vs_general_plot")),
#              column(6, plotlyOutput("men_vs_women_plot"))
#            ),
#            br(),
#            fluidRow(
#              column(6, plotlyOutput("elite_men_vs_women_plot"))
#            ),
#            plotOutput("percentile_plot")
#   ),
  
#   tabPanel("Course Map Comparison",
#            div(style = "padding: 20px;",
#                h3("Interactive Route Evolution: 2000 vs. 2025"),
#                p("Toggle routes on and off with the checkboxes"),
#                leafletOutput("course_map", height = 600)
#            )
#   ),
  
#   tabPanel("About the Project",
#            div(style = "padding: 30px;",
#                h3("Methodology and Data Processing"),
#                p("Data was sourced from official Chicago Marathon results and hourly Open-Meteo weather APIs from 2000–2025."),
#                br(),
#                h4("The 2007 Heat"),
#                p("Note about the 2007 Chicago Marathon: Temperature spiked mid race at over 88°F which forced race organizers to stop the race early. Many runners suffered from heat stroke and one runner died.")
#            )
#   )
# )

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
    stat_model <- lm(avg_time_hours ~ metric_val, data = plot_data)
    r_squared <- round(summary(stat_model)$r.squared, 5)
    spread_data %>%
      filter(runner_group == input$group, year >= input$year_range[1], year <= input$year_range[2]) %>%
      summarize(`Overall Avg Time (Hours)` = mean(finish_time_seconds, na.rm = TRUE)/3600,
                `Selected Metric Avg` = mean(.data[[input$weather_metric]], na.rm = TRUE)) %>%
      mutate(`R-squared` = r_squared)
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
  
  #elite vs general plot
  output$elites_vs_general_plot <- renderPlotly({
    gen_vs_elite_summary <- spread_data %>%
      group_by(year, runner_group) %>%
      summarize(
        avg_time = mean(finish_time_seconds) / 3600,
        avg_temp = mean(avg_race_temp_f, na.rm = TRUE),
        .groups = "drop"
      )
    p <- ggplot(gen_vs_elite_summary, aes(x = avg_temp, y = avg_time, color = runner_group)) +
      geom_point(size = 3, alpha = 1) +
      geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
      scale_color_manual(values = c("General Sample" = "steelblue", "Top 1% Elites" = "orange")) +
      labs(title = "Elite vs. General Runners Performance by Temperature",
           x = "Avg Race Temperature (°F)", y = "Avg Finish Time (Hours)", color = "Cohort") +
      theme_minimal()
    
    ggplotly(p) %>% layout(margin=list(t = 50), legend = list(orientation="h", y = -0.3))
  })
  
  #men vs women plot
  output$men_vs_women_plot <- renderPlotly({
    p <- ggplot(gender_yearly, aes(x = avg_temp, y = avg_time_hours, color = gender)) +
      geom_point(size = 3, alpha = 1) +
      geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
      scale_color_manual(values = c("Man" = "blue", "Woman" = "pink")) +
      labs(title = "Men vs. Women Performance by Temperature",
           x = "Avg Race Temperature (°F)", y = "Avg Finish Time (Hours)", color = "Gender") +
      theme_minimal()
    
    ggplotly(p) %>% layout(margin = list(t = 50), legend = list(orientation = "h", y = -0.3))
  })
  
  output$elite_men_vs_women_plot <- renderPlotly({
    p <- ggplot(elite_gender_yearly, aes(x = avg_temp, y = avg_time_hours, color = gender)) +
      geom_point(size = 3, alpha = 1) +
      geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
      scale_color_manual(values = c("Man" = "darkblue", "Woman" = "purple")) +
      labs(title = "Top 1% Elites: Men vs. Women Performance",
           x = "Avg Race Temperature (°F)", y = "Avg Finish Time (Hours)", color = "Gender") +
      theme_minimal()
    
    ggplotly(p) %>% layout(margin = list(t = 50), legend = list(orientation = "h", y = -0.3))
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
        plot.title = element_text(face = "bold", color = "purple")
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

shinyApp(ui, server)