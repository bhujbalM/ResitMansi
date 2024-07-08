library(readr)
library(dplyr)
library(ggplot2)
library(maps)
library(lubridate)
library(purrr) 

install.packages("tidyverse")
library(tidyverse)

install.packages("gapminder")
library(gapminder)

data("gapminder")
head(gapminder)

options(scipen = 999)

# Read the CSV file
df <- read.csv('country_data.csv')

# Display the structure of the dataframe
str(df)

# Display the first few rows
head(df)
write.csv(x = my_file_object, file = "my_file_name.csv")
write.csv(my_file_object, "my_file_name.csv")
library(dplyr)
library(ggplot2)
library(maps)
library(lubridate)
library(purrr) 

# Prepare data for Ireland
ireland_data <- df %>%
  filter(location == 'Ireland') %>%
  mutate(date = as.Date(date))

# Display the first few rows and structure of ireland_data
print(head(ireland_data))
str(ireland_data)

# Prepare data for the world map
latest_data <- df %>%
  group_by(location) %>%
  filter(date == max(date)) %>%
  select(location, total_cases_per_million)

# Get world map data
world_map <- map_data('world')

# Merge data with world map
map_data <- left_join(world_map, latest_data, by = c('region' = 'location'))

# Create the world map chart
world_map_chart <- ggplot(map_data, aes(x = long, y = lat, group = group, fill = total_cases_per_million)) +
  geom_polygon(color = 'white', size = 0.1) +
  scale_fill_viridis_c(option = 'plasma', name = 'Total Cases per Million', na.value = 'gray90') +
  theme_minimal() +
  labs(title = 'COVID-19 Total Cases per Million by Country',
       subtitle = paste('Data as of', max(df$date)),
       x = '', y = '') +
  theme(legend.position = 'bottom')

# Display the chart
print(world_map_chart)


library(dplyr)
library(ggplot2)
library(lubridate)

# Select Ireland and 9 other countries
countries <- c('Ireland', 'United Kingdom', 'France', 'Germany', 'Italy', 'Spain', 'Sweden', 'United States', 'Canada', 'Australia')

# Prepare data for the line chart
line_data <- df %>%
  filter(location %in% countries) %>%
  mutate(date = as.Date(date))

# Create the line chart
line_chart <- ggplot(line_data, aes(x = date, y = new_cases_smoothed_per_million, color = location)) +
  geom_line() +
  scale_color_viridis_d() +
  theme_minimal() +
  labs(title = 'Daily New COVID-19 Cases per Million',
       subtitle = 'Comparison of Ireland and 9 other countries',
       x = 'Date',
       y = 'New Cases per Million (7-day smoothed)',
       color = 'Country') +
  theme(legend.position = 'bottom')

# Display the chart
print(line_chart)


library(dplyr)
library(ggplot2)
library(lubridate)

# Prepare data for Ireland
ireland_data <- df %>%
  filter(location == 'Ireland') %>%
  mutate(date = as.Date(date))

# Create the multi-line chart for Ireland
ireland_chart <- ggplot(ireland_data, aes(x = date)) +
  geom_line(aes(y = new_cases_smoothed_per_million, color = 'New Cases')) +
  geom_line(aes(y = new_deaths_smoothed_per_million * 10, color = 'Deaths (x10)')) +
  geom_line(aes(y = people_vaccinated_per_hundred, color = 'Vaccinated (%)')) +
  scale_y_continuous(
    name = 'New Cases / Vaccinated (%)',
    sec.axis = sec_axis(~./10, name = 'Deaths')
  ) +
  scale_color_manual(values = c('New Cases' = 'blue', 'Deaths (x10)' = 'red', 'Vaccinated (%)' = 'green')) +
  theme_minimal() +
  labs(title = 'COVID-19 in Ireland: Cases, Deaths, and Vaccination Progress',
       x = 'Date',
       color = 'Metric') +
  theme(legend.position = 'bottom')

# Display the chart
print(ireland_chart)


    library(dplyr)
    library(lubridate)

    # Prepare data for Ireland
    ireland_data <- df %>%
      filter(location == 'Ireland') %>%
      mutate(date = as.Date(date))

    # Define time periods
    periods <- list(
      'First 3 months' = c(min(ireland_data$date[!is.na(ireland_data$total_cases)]), min(ireland_data$date[!is.na(ireland_data$total_cases)]) + months(3)),
      '2020' = c(as.Date('2020-01-01'), as.Date('2020-12-31')),
      '2021' = c(as.Date('2021-01-01'), as.Date('2021-12-31')),
      '2022' = c(as.Date('2022-01-01'), as.Date('2022-12-31')),
      'Last 3 months' = c(max(ireland_data$date) - months(3), max(ireland_data$date))
    )

    # Calculate summary statistics for each period
    summary_stats <- lapply(names(periods), function(period) {
      start_date <- periods[[period]][1]
      end_date <- periods[[period]][2]
      
      period_data <- ireland_data %>%
        filter(date >= start_date & date <= end_date)
      
      tibble(
        Period = period,
        Total_Cases = max(period_data$total_cases, na.rm = TRUE),
        Total_Deaths = max(period_data$total_deaths, na.rm = TRUE),
        Max_Daily_Cases = max(period_data$new_cases, na.rm = TRUE),
        Max_Daily_Deaths = max(period_data$new_deaths, na.rm = TRUE),
        Vaccination_Rate = max(period_data$people_fully_vaccinated_per_hundred, na.rm = TRUE)
      ) %>%
      mutate_all(~ifelse(is.infinite(.), NA, .))
    }) %>% bind_rows()

    # Display the summary table
    print(summary_stats)
    

