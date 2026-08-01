library(tidyverse)
library(shiny)
library(plotly)
#runner_data <- read_csv("STAT385 Project (2000 to 2025 results).csv")
runner_data <- read_csv(unzip("STAT385 Project (2000 to 2025 results).csv.zip", "STAT385 Project (2000 to 2025 results).csv"))

#weather_data <- read_csv("STAT385 Project (weather working set).csv")
weather_data <- read_csv("STAT385 Project (weather working set).csv")
#seed for reproducible sampling
set.seed(1)
runners_finished <- runner_data %>%
  drop_na(finish_time_seconds) %>%
  #The Chicago Marathon officially enforces a 6 hour and 30 minute (23400sec) time limit.
  filter(finish_time_seconds <= 23400) %>%
  #grouping by each year
  group_by(year) %>%
  mutate(quartile=ntile(finish_time_seconds, 4)) %>%
  group_by(year, quartile) %>%
  #sample of 1000 per year
  slice_sample(n = 1000) %>%
  ungroup() %>%
  mutate(
    start_time_seconds = case_when(
      quartile == 1 ~ (7*3600) + 30*60, #7:30am
      quartile == 2 ~ (8*3600), #8am
      quartile %in% c(3,4) ~ (8*3600) + 35*60 #8:35am
    ), 
    end_time_seconds = start_time_seconds + finish_time_seconds
  )
#prepping weather data & getting year and hour 
weather_needed <- weather_data %>%
  mutate(year = year(time), hour = hour(time))

#weather averages from each day 
racing_weather <- weather_needed %>%
  filter(hour >= 7 & hour <= 15) %>%
  group_by(year) %>%
  summarize(avg_race_temp_f = mean(temperature_2m_f), avg_humidity_pct = mean(relative_humidity_2m_pct))

#merging by year, this has the sample of 1000 runners per year
final_data <- runners_finished %>%
  left_join(racing_weather, by = "year")

#summary statistics by year (in hours instead of seconds)
yearly_statistics_quartiles <- final_data %>%
  group_by(year) %>%
  summarize(
    avg_time_hours = mean(finish_time_seconds)/3600,
    stdev_hours = sd(finish_time_seconds)/3600,
    q1_hours = quantile(finish_time_seconds, 0.25)/3600,
    median_hours = median(finish_time_seconds)/3600,
    q3_hours = quantile(finish_time_seconds, 0.75)/3600,.groups="drop"
  )
as.data.frame(yearly_statistics_quartiles)

#plotting temperature vs quartiles
quartiles <- final_data %>%
  group_by(year,quartile) %>%
  summarize(
    avg_time_hours = mean(finish_time_seconds)/3600, avg_temp = mean(avg_race_temp_f), 
    .groups = "drop")
#quartiles separate
ggplot(data = quartiles, aes(x = avg_temp, y = avg_time_hours)) +
  geom_point(alpha=0.5) + geom_smooth(method="lm", color="black", se=FALSE) +
  facet_wrap(~quartile) +
  labs(
    title = "Quartiles of Temperature vs. Time",
    x = "Avg Temperature (°F)",
    y = "Avg Finishing Time (hours)"
  )
#quartiles overlapped
ggplot(data=quartiles, aes(x=avg_temp, y=avg_time_hours, color=as.factor(quartile))) +
  geom_point(alpha=0.5) + geom_smooth(method="lm", se = FALSE) + 
  scale_color_manual(
    values = c("1"="navy", "2"="blue", "3"="skyblue4", "4"="skyblue1"),
    labels = c("Q1", "Q2", "Q3", "Q4") ) +
  labs(
    title = "Temperature and Results Quartiles",
    x = "Avg Temperature (°F)",
    y = "Avg Finishing Time (hours)",
    color = "Running group quartile"
  ) + theme(panel.background = element_blank())

#slope, r^2, p-value
library(broom)
model <- lm(finish_time_seconds~avg_race_temp_f, data=final_data)
model_slope <- tidy(model)
model_glance <- glance(model)
print(model_slope)
print(model_glance)
exact_model_slope_seconds <- model_slope$estimate[2]
exact_model_slope_minutes <- exact_model_slope_seconds/60
exact_rsquared <- model_glance$r.squared[1]
print(exact_model_slope_seconds)
print(exact_model_slope_minutes)

#bell curve distribution of marathon times (100 bins turned out the best instead of much more(200) or less(20))
ggplot(final_data, aes(x=finish_time_seconds/3600)) + 
  geom_histogram(aes(y=after_stat(density)), bins=100, color = "gray") + geom_density(color ="blue", linewidth = 1) +
  labs(
    title = "Marathon Finish Times Distribution",
    x = "Finishing Time (hours)",
    y = "Density"
  )


#boxplot of finishing times by year
ggplot(final_data, aes(x=as.factor(year), y=finish_time_seconds/3600)) + 
  geom_boxplot(fill = "lightblue", color = "navy", outlier.size = 0.5, outlier.alpha = 0.25) +
  labs(
    title = "Marathon Finish Times Yearly Distribution Boxplot",
    x = "Year",
    y = "Finishing Time (hours)"
  )
#boxplot of weather by year
weather_needed %>%
  filter(hour>=7 & hour <=17) %>%
  ggplot(aes(x=as.factor(year), y=temperature_2m_f)) +
  geom_boxplot(fill = "coral1", color = "darkred", outlier.size = 0.5) +
  labs(
    title = "Marathon Temperature Distribution Boxplot 7am-5pm CT",
    x = "Year",
    y = "Temperature (°F)"
  )



#(overall) scatter plot summarizing each year
final_data %>%
  group_by(year) %>%
  summarize(
    avg_finish_hours = mean(finish_time_seconds)/3600,
    avg_temp = mean(avg_race_temp_f),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = avg_temp, y = avg_finish_hours, label = year)) + 
  geom_point(color = "maroon", size = 4) +
  geom_text(vjust = 2, size = 3) +
  geom_smooth(method = "lm", color = "black", linetype = "dashed", se = FALSE) +
  labs(
    title = "Yearly Marathon Finishing Times vs. Race Day Temperature",
    x = "Average Race Temperature (°F)",
    y = "Average Finishing Time (hours)"
  ) + theme(panel.background = element_blank())

#top 1% of runners each year
top_1percent <- runner_data %>%
  drop_na(finish_time_seconds) %>%
  filter(finish_time_seconds <= 23400) %>%
  group_by(year) %>%
  mutate(percentile = ntile(finish_time_seconds,100)) %>%
  filter(percentile==1) %>% 
  ungroup() %>%
  left_join(racing_weather, by ="year")
top_1percent_fit <- lm(finish_time_seconds~avg_race_temp_f, data = top_1percent)
tidy(top_1percent_fit)
#top 1% scatter plot summarizing each year
top_1percent %>%
  group_by(year) %>%
  summarize(
    avg_finish_hours = mean(finish_time_seconds)/3600,
    avg_temp = mean(avg_race_temp_f),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = avg_temp, y = avg_finish_hours, label = year)) + 
  geom_point(color = "maroon", size = 4) +
  geom_text(vjust = 2, size = 3) +
  geom_smooth(method = "lm", color = "black", linetype = "dashed", se = FALSE) +
  labs(
    title = "Top 1% Elite Yearly Marathon Finishing Times vs. Race Day Temperature",
    x = "Average Race Temperature (°F)",
    y = "Average Finishing Time (hours)"
  ) + theme(panel.background = element_blank())

#general sample statistics (n=100,000)
general_summary_statistics <- final_data %>%
  summarize(
    group = "General Sample",
    total_number_of_runners = n(),
    mean_time_hours = mean(finish_time_seconds)/3600,
    median_time_hours = median(finish_time_seconds)/3600,
    stdev_time_hours = sd(finish_time_seconds)/3600,
    iqr_time_hours = IQR(finish_time_seconds)/3600,
    q1_time_hours = quantile(finish_time_seconds,0.25)/3600,
    q3_time_hours = quantile(finish_time_seconds, 0.75)/3600,
    min_time_hours = min(finish_time_seconds)/3600,
    max_time_hours = max(finish_time_seconds)/3600,
    mean_temp_f = mean(avg_race_temp_f),
    sd_temp_f = sd(avg_race_temp_f),
    min_temp_f = min(avg_race_temp_f),
    max_temp_f = max(avg_race_temp_f)
  )


#top1% statistics (n=9082)
top_1percent_summary_statistics <- top_1percent %>%
  summarize(
    group = "Top 1%",
    total_number_of_runners = n(),
    mean_time_hours = mean(finish_time_seconds)/3600,
    median_time_hours = median(finish_time_seconds)/3600,
    stdev_time_hours = sd(finish_time_seconds)/3600,
    iqr_time_hours = IQR(finish_time_seconds)/3600,
    q1_time_hours = quantile(finish_time_seconds,0.25)/3600,
    q3_time_hours = quantile(finish_time_seconds, 0.75)/3600,
    min_time_hours = min(finish_time_seconds)/3600,
    max_time_hours = max(finish_time_seconds)/3600,
    mean_temp_f = mean(avg_race_temp_f),
    sd_temp_f = sd(avg_race_temp_f),
    min_temp_f = min(avg_race_temp_f),
    max_temp_f = max(avg_race_temp_f)    
  )

#combining and printing general and top1% statistics
general_and_top_1percent <- bind_rows(general_summary_statistics, top_1percent_summary_statistics)
print(as.data.frame(general_and_top_1percent))

library(tidymodels)
#general sample regression statistics (n=100,000)
regression_simple <- 
  linear_reg() |> 
  set_engine("lm")
regression_general_sample_fit <- 
  regression_simple |> 
  fit(finish_time_seconds ~ avg_race_temp_f, data = final_data)
regression_general_results <- 
  regression_general_sample_fit |> 
  extract_fit_engine() |> 
  summary()
coef(regression_general_results)
tidy(regression_general_sample_fit)
glance(regression_general_sample_fit)

#log p-value calculation of general sample
gen_t_stat <- tidy(regression_general_sample_fit)$statistic[2]
gen_df <- glance(regression_general_sample_fit)$df.residual
#ln(a*b) = ln(a) + ln(b), log_10(p) = ln(p)/ln(10)
gen_log_p <- log(2) + pt(-abs(gen_t_stat), df=gen_df, log.p=TRUE)
gen_log10 <- gen_log_p/log(10) #log_10(p) = -358.758167513122 , p = 10^-358.758167513122

#top1% regression statistics (n=9,082)
#model_top_1percent <- lm(finish_time_seconds~avg_race_temp_f, data=top_1percent)
regression_top1percent_fit <- 
  regression_simple |> 
  fit(finish_time_seconds ~ avg_race_temp_f, data=top_1percent)
regression_top1percent_results <- 
  regression_top1percent_fit |> 
  extract_fit_engine() |> 
  summary()
coef(regression_top1percent_results)
tidy(regression_top1percent_fit)
glance(regression_top1percent_fit)

#log p-value calculation of top1% 
top1_t_stat <- tidy(regression_top1percent_fit)$statistic[2]
top1_df <- glance(regression_top1percent_fit)$df.residual
top1_log_p <- log(2) + pt(-abs(top1_t_stat), df=top1_df, log.p=TRUE)
top1_log10 <- top1_log_p/log(10) #log_10(p) = -70.29292 , p = 10^-70.29292

#slopes and R^2 
gen_slope <- tidy(regression_general_sample_fit)$estimate[2]  #39.47348 sec/1 deg F
gen_rsquared <- glance(regression_general_sample_fit)$r.squared #r^2 = 0.0163088

top1_slope <- tidy(regression_top1percent_fit)$estimate[2] #12.26814 sec/1 deg F
top1_rsquared <- glance(regression_top1percent_fit)$r.squared #r^2 = 0.03436374

#temperature increase table
temp_incr <- c(1,2,3,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75)
heat_incr_table <- tibble(
  temp_incr_f = temp_incr,
  gen_sample_added_minutes = (gen_slope*temp_incr)/60,
  top_1percect_added_minutes = (top1_slope*temp_incr)/60
)
print(heat_incr_table)
#!!!maybe see if I can get like r^2 value for this or some way to measure confidence


#regressions with humidity
regression_multiple_fit <- 
  regression_simple |> 
  fit(finish_time_seconds ~ avg_race_temp_f + avg_humidity_pct, data = final_data)
tidy(regression_multiple_fit)
glance(regression_multiple_fit)
#!!!check the results when back home and see if good to write about/visualize


#overlaid plot comparing the trend line of the General Sample vs. 
#Top 1% Elites on the same chart scale

yearly_gen <- final_data %>%
  group_by(year) %>%
  summarize(avg_finish_hours = mean(finish_time_seconds)/3600, avg_temp = mean(avg_race_temp_f), group = "General Runner Sample")
yearly_top1 <- top_1percent %>%
  group_by(year) %>%
  summarize(avg_finish_hours = mean(finish_time_seconds)/3600, avg_temp = mean(avg_race_temp_f), group = "Top 1% Runner Sample")
bind_rows(yearly_gen, yearly_top1) %>%
  ggplot(aes(x = avg_temp, y = avg_finish_hours, color = group)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Thermal Sensitivity: General Sample vs. Top 1% Elites",
    x = "Average Race Temperature (°F)",
    y = "Average Finish Time (hours)",
    color = "Runner Group"
  ) 
#!!!see if there's a way to follow like a log graph or non-linear fit to see what the ideal temp is, also clean up code to make it mine

#extract residuals (the differences between actual and predicted times) to check if your linear regression assumptions hold true.
augment(regression_general_sample_fit) %>%
  ggplot(aes(x = .pred, y = .resid)) +
  geom_point(alpha = 0.2, color = "midnightblue") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(
    title = "Residuals vs. Fitted Values (General Sample)",
    x = "Predicted Finish Time (seconds)",
    y = "Residual Error (seconds)"
  )
#!!!check for augment arguments

#model evaluation, root mean squared error (RMSE) and mean absolute error (mae)
model_predictions <- final_data %>%
  select(finish_time_seconds) %>%
  bind_cols(predict(regression_general_sample_fit, final_data)) %>%
  mutate(
    squared_error = (finish_time_seconds - .pred)^2,
    absolute_error = abs(finish_time_seconds - .pred)
  ) %>%
  summarize(
    mse = mean(squared_error),
    rmse_seconds = sqrt(mean(squared_error)),
    rmse_minutes = sqrt(mean(squared_error)) / 60,
    mae_minutes  = mean(absolute_error) / 60
  )
print(model_predictions)

# predicted vs. actual finish times (in hours)
final_data %>%
  bind_cols(predict(regression_general_sample_fit, final_data)) %>%
  ggplot(aes(x = .pred / 3600, y = finish_time_seconds / 3600)) +
  geom_point(alpha = 0.1, color = "steelblue") +
  geom_abline(slope = 1, intercept = 0, color = "darkred", linetype = "dashed") +
  labs(
    title = "Actual vs. Predicted Finish Times",
    x = "Predicted Time (hours)",
    y = "Actual Time (hours)"
  ) +
  theme_minimal()


#Faceted Density Distribution by Temperature Bin
final_data %>%
  mutate(temp_category = case_when(
    avg_race_temp_f < 40 ~ "Very Cold (<40°F)",
    avg_race_temp_f < 50 & avg_race_temp_f >= 40  ~ "Cool (40-50°F)",
    avg_race_temp_f < 60 & avg_race_temp_f >= 50 ~ "Moderate (50-60°F)",
    avg_race_temp_f < 70 & avg_race_temp_f >= 60 ~ "Warm (60-70°F)",
    TRUE ~ "Hot (>70°F)"
  )) %>%
  ggplot(aes(x = finish_time_seconds / 3600, fill = temp_category)) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Finish Time Density Distributions across Temperature Ranges",
    x = "Finish Time (hours)",
    y = "Density",
    fill = "Temperature Group"
  ) +
  theme_minimal()
#!!! clean up colors and formal maybe

# Spread metrics comparison table
spread_comparison_hrs <- final_data %>%
  summarize(
    group = "General Sample",
    mean_hours = mean(finish_time_seconds) / 3600,
    median_hours = median(finish_time_seconds) / 3600,
    sd_hours = sd(finish_time_seconds) / 3600,
    iqr_hours = IQR(finish_time_seconds) / 3600
  )
print(spread_comparison_hrs)

#!!! compare this spread/make a spread for elite and compare, maybe move to min or sec...
# Spread metrics in minutes
spread_data <- bind_rows(
  final_data %>% mutate(runner_group = "General Sample"),
  top_1percent %>% mutate(runner_group = "Top 1% Elites")
)
spread_comparison_mins <- spread_data %>%
  group_by(runner_group) %>%
  summarize(
    mean_mins = mean(finish_time_seconds)/60,
    median_mins = median(finish_time_seconds)/60,
    sd_mins = sd(finish_time_seconds)/60,
    iqr_mins = IQR(finish_time_seconds)/60
  )
print(spread_comparison_mins)

#!!!... and can visualize the spread in a good way
#visualizing spread with density plot (mins)
ggplot(spread_data, aes(x = finish_time_seconds/60, fill = runner_group)) +
  geom_density(alpha = 0.5, color = "black") +
  scale_fill_manual(
    values = c("General Sample" = "steelblue", "Top 1% Elites" = "orange")
  ) +
  labs(
    title = "Variance Comparison: Top 1% Elites vs. General Sample",
    x = "Finish Time (Minutes)",
    y = "Density",
    fill = "Runner Category"
  ) +
  theme_minimal()


#another visual with Quartiles
# Faceted Scatter Plot w/ trendlines
ggplot(data = quartiles, aes(
  x = avg_temp, 
  y = avg_time_hours, 
  color = as.factor(quartile), 
  shape = as.factor(quartile)
)) +
  geom_point(size = 3, alpha = 0.8) + 
  geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 0.8) +
  facet_wrap(~quartile, 
             ncol = 4, 
             labeller = as_labeller(c("1" = "Q1 (Fastest)", "2" = "Q2", "3" = "Q3", "4" = "Q4 (Slowest)"))) +
  scale_color_manual(
    values = c("1" = "navy", "2" = "steelblue", "3" = "darkorange", "4" = "firebrick")
  ) +
  labs(
    title = "Temperature vs. Finish Time by Runner Quartile",
    x = "Average Race Temperature (°F)",
    y = "Average Finishing Time (hours)",
    color = "Quartile",
    shape = "Quartile"
  ) +
  theme_bw() + 
  theme(
    legend.position = "right",
    strip.background = element_rect(fill = "grey90"),
    strip.text = element_text(face = "bold")
  )

#Quartile regressions
quartile_regressions <- final_data %>%
  group_by(quartile) %>%
  summarize(
    slope_minutes_per_degree = coef(lm(finish_time_seconds ~ avg_race_temp_f))[2]/60,
    r_squared = summary(lm(finish_time_seconds ~ avg_race_temp_f))$r.squared
  )
print(quartile_regressions)

#general confidence intervals
general_conf_int_95 <- tidy(regression_general_sample_fit, conf.int = TRUE)
general_conf_int_997 <- tidy(regression_general_sample_fit, conf.int = TRUE, conf.level = 0.997)

#general Quartile regressions with 95% confidence intervals
general_quartile_regressions <- final_data %>%
  group_by(quartile) %>%
  summarize(
    slope_mins = coef(lm(finish_time_seconds ~ avg_race_temp_f))[2] / 60,
    conf_low_mins = confint(lm(finish_time_seconds ~ avg_race_temp_f))[2,1] / 60,
    conf_high_mins = confint(lm(finish_time_seconds ~ avg_race_temp_f))[2,2] / 60,
    r_squared = summary(lm(finish_time_seconds ~ avg_race_temp_f))$r.squared
  )

#top 1% confidence intervals
top1_conf_int_95 <- tidy(regression_top1percent_fit, conf.int = TRUE)
top1_conf_int_997 <- tidy(regression_top1percent_fit, conf.int = TRUE, conf.level = 0.997)

#top 1% Quartile regressions with 95% confidence intervals
top1_overall_formatted_regressions <- top_1percent %>%
  summarize(
    group = "Top 1% Overall",
    slope_mins = coef(lm(finish_time_seconds ~ avg_race_temp_f))[2] / 60,
    conf_low_mins = confint(lm(finish_time_seconds ~ avg_race_temp_f))[2,1] / 60,
    conf_high_mins = confint(lm(finish_time_seconds ~ avg_race_temp_f))[2,2] / 60,
    r_squared = summary(lm(finish_time_seconds ~ avg_race_temp_f))$r.squared
  )

#figure out how to publish these and make the plots interactive 
#do dashboard first to figure out visually what the arguments are 
#toggle between elite and top 1%, compare the 2
#look for others factors like humidity on outlier years
#look at finishing times over each year, is there any overall trend/correlation
#talk to a statistician and make sure i am making sound arguments/demonstrations 
#look at other factors about outliers, could that be because of the sample
#could I do average of every year
#conclude what the idea temperature is each year, back that up with overall and top 1%
#maybe think about if it impacted men vs women more
#!!!look at the other weather factors, see if there were factors
#- how did humidity and precipitation impact performance
#shiny dashboard readings in week 8
#figure out how to be able to publish the dashboards as well, figure out how to make interactive, practice exporting the Quarto slides

#SHINY: Comparison dataset of general temp vs finish time
ui <- fluidPage(
  titlePanel("Temperature vs. Finish Time (Yearly Averages)"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "group", 
        label = "Choose Runner Group:", 
        choices = list("General Sample", "Top 1% Elites")
      )
    ),
    mainPanel(
      plotlyOutput("scatter_plot"),
      br(),
      tableOutput("summary_table")
    )
  )
)

server <- function(input, output, session) {
  yearly_data <- reactive({
    spread_data %>% 
      filter(runner_group == input$group) %>%
      group_by(year) %>%
      summarize(
        avg_temp = mean(avg_race_temp_f),
        avg_time_hours = mean(finish_time_seconds) / 3600,
        .groups = "drop"
      ) %>%
      mutate(
        hover_info = paste(
          "Year:", year,
          "\nTemp:", round(avg_temp, 1), "°F",
          "\nTime:", round(avg_time_hours, 2), "Hrs"
        )
      )
  })
  
  output$scatter_plot <- renderPlotly({
    
    p <- ggplot(yearly_data(), aes(x = avg_temp, y = avg_time_hours)) +
      geom_point(aes(text = hover_info), alpha = 0.8, color = "steelblue", size = 3) + 
      geom_smooth(method = "lm", color = "black", se = FALSE) +
      labs(
        title = paste("Yearly Performance of", input$group),
        x = "Average Temperature °F",
        y = "Finish Time Hours"
      ) +
      theme_minimal()
        ggplotly(p, tooltip = "text")
    
  }) 

  # simple summary table
  output$summary_table <- renderTable({
    spread_data %>%
      filter(runner_group == input$group) %>%
      summarize(
        `Overall Avg Time (Hours)` = mean(finish_time_seconds)/3600,
        `Overall Avg Temp (°F)` = mean(avg_race_temp_f)
      )
  })
}
shinyApp(ui, server)


#SHINY: Quartile dataset
library(shiny)
library(tidyverse)
library(plotly)

ui <- fluidPage(
  titlePanel("Temperature vs. Finish Time by Quartile"), 
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "quartile_choice", 
        label = "Choose Runner Quartile:", 
        choices = list("Q1 (Fastest)" = 1, 
                       "Q2 (Average)" = 2, 
                       "Q3 (Average)" = 3, 
                       "Q4 (Slowest)" = 4)
      )
    ),
    mainPanel(
      plotlyOutput("scatter_plot"), # <--- CHANGED: Swapped to plotlyOutput
      br(),
      tableOutput("summary_table")
    )
  )
)

server <- function(input, output, session) {
  yearly_data <- reactive({
    final_data %>%
      filter(quartile == input$quartile_choice) %>%
      group_by(year) %>%
      summarize(
        avg_temp = mean(avg_race_temp_f),
        avg_time_hours = mean(finish_time_seconds) / 3600,
        .groups = "drop"
      ) %>%
      mutate(
        hover_info = paste(
          "Year:", year,
          "\nTemp:", round(avg_temp, 1), "°F",
          "\nTime:", round(avg_time_hours, 2), "Hrs"
        )
      )
  })
  
  output$scatter_plot <- renderPlotly({
    
    p <- ggplot(yearly_data(), aes(x = avg_temp, y = avg_time_hours)) +
      geom_point(aes(text = hover_info), alpha = 0.8, color = "maroon", size = 3) + 
      geom_smooth(method = "lm", formula = y ~ x, color = "black", se = FALSE) +
      labs(
        title = paste("Yearly Performance of Quartile", input$quartile_choice),
        x = "Average Temperature (°F)",
        y = "Finish Time (Hours)"
      ) +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  }) 
  
  # simple summary table
  output$summary_table <- renderTable({
    final_data %>%
      filter(quartile == input$quartile_choice) %>%
      summarize(
        `Overall Avg Time (Hours)` = mean(finish_time_seconds)/3600,
        `Overall Avg Temp (°F)` = mean(avg_race_temp_f)
      )
  })
}
shinyApp(ui, server)

#have publishing workflow figured out
#have dropdowns (later)



