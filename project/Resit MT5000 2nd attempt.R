
# Load required libraries
library(tidyverse)
library(sf)
library(tmap)
library(ggplot2)
library(lubridate)

# Read the data files
country_data <- read.csv('country_data.csv')
country_metadata <- read.csv('country_metadata.csv')

# Display the first few rows of each dataset
print(head(country_data))
print(head(country_metadata))

# Get a summary of the country_data
summary(country_data)
# Install tmap package
install.packages('tmap', repos='https://cran.rstudio.com/', dependencies=TRUE)

# Load required libraries
library(tidyverse)
library(sf)
library(tmap)
library(ggplot2)
library(lubridate)

# Print loaded packages
print((.packages()))

# Install tmap package
install.packages('tmap', repos='https://cran.rstudio.com/', dependencies=TRUE)

# Load required libraries
library(tidyverse)
library(sf)
library(tmap)
library(ggplot2)
library(lubridate)

# Print loaded packages
print((.packages()))

# Read the CSV file
country_data <- read.csv('country_data.csv')

# Display the structure of the data
str(country_data)

# Show the first few rows of the data
print(head(country_data))

# Display summary statistics
summary(country_data)


library(ggplot2)
library(dplyr)

# Convert date to Date type
country_data$date <- as.Date(country_data$date)

# Create the plot
p <- ggplot(country_data, aes(x = date)) +
  geom_line(aes(y = new_cases_smoothed, color = 'New Cases')) +
  geom_line(aes(y = new_deaths_smoothed * 10, color = 'New Deaths (x10)')) +
  scale_y_log10(labels = scales::comma_format(accuracy = 1)) +
  scale_color_manual(values = c('New Cases' = 'blue', 'New Deaths (x10)' = 'red')) +
  labs(title = 'COVID-19 New Cases and Deaths in Ireland',
       x = 'Date',
       y = 'Count (Log Scale)',
       color = 'Metric') +
  theme_minimal() +
  theme(legend.position = 'bottom')

# Display the plot
print(p)


