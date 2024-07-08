
# Load required libraries
library(tidyverse)
library(sf)
library(tmap)
library(lubridate)

# Read the data
country_data <- read.csv('country_data.csv')

# Display the first few rows and structure of the data
print(head(country_data))
print(str(country_data))

# Install and load required packages
if (!require(tidyverse)) install.packages('tidyverse')
if (!require(sf)) install.packages('sf')
if (!require(rnaturalearth)) install.packages('rnaturalearth')
if (!require(rnaturalearthdata)) install.packages('rnaturalearthdata')

library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# Read the data
country_data <- read.csv('country_data.csv')

# Display the first few rows and structure of the data
print(head(country_data))
print(str(country_data))


# Load required libraries
library(tidyverse)
library(lubridate)

# Read the data
country_data <- read.csv('country_data.csv')

# Convert date to proper date format
country_data$date <- as.Date(country_data$date)

# Display the first few rows and structure of the data
print(head(country_data))
print(str(country_data))

# Get unique countries in the dataset
unique_countries <- unique(country_data$location)
print(paste('Unique countries in the dataset:', toString(unique_countries)))


library(tidyverse)
library(lubridate)

# Calculate latest total cases and deaths for each country
latest_stats <- country_data %>%
  group_by(location) %>%
  summarize(
    latest_date = max(date),
    total_cases = max(total_cases, na.rm = TRUE),
    total_deaths = max(total_deaths, na.rm = TRUE)
  ) %>%
  arrange(desc(total_cases))

print(latest_stats)

# Create a line plot of total cases over time for each country
ggplot(country_data, aes(x = date, y = total_cases, color = location)) +
  geom_line() +
  scale_y_log10(labels = scales::comma_format(big.mark = ',')) +
  theme_minimal() +
  labs(title = 'Total COVID-19 Cases Over Time',
       x = 'Date',
       y = 'Total Cases (log scale)',
       color = 'Country') +
  theme(legend.position = 'bottom')

ggsave('total_cases_over_time.png', width = 10, height = 6)

# Create a line plot of total deaths over time for each country
ggplot(country_data, aes(x = date, y = total_deaths, color = location)) +
  geom_line() +
  scale_y_log10(labels = scales::comma_format(big.mark = ',')) +
  theme_minimal() +
  labs(title = 'Total COVID-19 Deaths Over Time',
       x = 'Date',
       y = 'Total Deaths (log scale)',
       color = 'Country') +
  theme(legend.position = 'bottom')

ggsave('total_deaths_over_time.png', width = 10, height = 6)


library(dplyr)
library(ggplot2)
library(tidyr)

# Read the CSV file and prepare data (same as before)
df <- read.csv('country_data.csv')
df$date <- as.Date(df$date)
countries <- c('Ireland', 'Kuwait', 'Myanmar', 'Senegal', 'New Caledonia', 'Seychelles')
df_filtered <- df %>% filter(location %in% countries)

# Function to calculate 7-day rolling average
calculate_rolling_average <- function(data) {
  data %>%
    arrange(date) %>%
    mutate(new_tests_7day_avg = zoo::rollmean(new_tests, k = 7, fill = NA, align = 'right'))
}

# Calculate rolling average for each country
df_with_avg <- df_filtered %>%
  group_by(location) %>%
  group_modify(~calculate_rolling_average(.x)) %>%
  ungroup()

# Plot the results
ggplot(df_with_avg, aes(x = date, y = new_tests_7day_avg, color = location)) +
  geom_line() +
  labs(title = '7-Day Rolling Average of New Tests by Country',
       x = 'Date',
       y = 'New Tests (7-day average)',
       color = 'Country') +
  theme_minimal() +
  theme(legend.position = 'bottom')

# Display the first few rows of the result
print(head(df_with_avg))

# Save the plot
ggsave('new_tests_7day_avg.png', width = 10, height = 6)

{
  "outputs_dict": {
    "325f90ca": "[JULIUS_TABLE]: [\"{\\\"columns\\\":[\\\"location\\\",\\\"correlation\\\"],\\\"index\\\":[\\\"1\\\",\\\"2\\\",\\\"3\\\",\\\"4\\\"],\\\"data\\\":[[\\\"Ireland\\\",\\\"0.137359254652786\\\"],[\\\"Kuwait\\\",\\\"0.103095129938648\\\"],[\\\"Myanmar\\\",\\\"0.150182637633527\\\"],[\\\"Senegal\\\",\\\"0.291260206372651\\\"]]}\"]",
    "afd2a63b": "\u001b[1m\u001b[22m`geom_smooth()` using formula = 'y ~ x'\n",
    "82422c28": "Warning message:\n“\u001b[1m\u001b[22mRemoved 1609 rows containing non-finite outside the scale range\n(`stat_smooth()`).”\n",
    "556c712a": "Warning message:\n“\u001b[1m\u001b[22mRemoved 1609 rows containing missing values or values outside the scale range\n(`geom_point()`).”\n",
    "5ca147e3": "\u001b[1m\u001b[22m`geom_smooth()` using formula = 'y ~ x'\n",
    "e681aef1": "Warning message:\n“\u001b[1m\u001b[22mRemoved 1609 rows containing non-finite outside the scale range\n(`stat_smooth()`).”\n",
    "7982a912": "Warning message:\n“\u001b[1m\u001b[22mRemoved 1609 rows containing missing values or values outside the scale range\n(`geom_point()`).”\n"
  },
  "image_urls_dict": {
    "e335016b": "https://api.chatwithyourdata.io/images/e335016b-1fb4-4337-ae7d-f6e0a90f99be.png"
  }
}