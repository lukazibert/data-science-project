# Load necessary libraries
library(tidyverse)
library(ggplot2)

# Read the dataset
aqi_data <- read.csv("project/city_day.csv")

# Preview the dataset
head(aqi_data)

# Explore variable types and descriptions
str(aqi_data)

# Aggregation of data

# 1. Calculate the average AQI for each city
average_aqi_by_city <- aqi_data %>%
  group_by(City) %>%
  summarise(Average_AQI = mean(AQI, na.rm = TRUE))

# Preview the aggregated table
print(average_aqi_by_city)

# 2. Determine cities with the highest and lowest AQI values
highest_aqi_cities <- average_aqi_by_city %>%
  arrange(desc(Average_AQI)) %>%
  head()

lowest_aqi_cities <- average_aqi_by_city %>%
  arrange(Average_AQI) %>%
  head()

# Print the cities with the highest and lowest AQI values
print(highest_aqi_cities)
print(lowest_aqi_cities)

# Exploratory data analysis

# 1. Time series plot of AQI trends in a specific city
specific_city <- "Delhi" # Choose a city for analysis
specific_city_data <- aqi_data %>%
  filter(City == specific_city)

ggplot(aqi_data, aes(x = Date, y = AQI)) +
  geom_line() +
  labs(
    title = paste("AQI Trends in", specific_city),
    x = "Date",
    y = "AQI"
  )

# 2. Boxplot of AQI values across different cities
ggplot(aqi_data, aes(x = City, y = AQI)) +
  geom_boxplot() +
  labs(
    title = "Distribution of AQI across Cities",
    x = "City",
    y = "AQI"
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  theme(axis.text.y = element_text(angle = 90, vjust = 0.5, hjust = 1))

# 3. Heatmap of AQI values in different regions of India
ggplot(aqi_data, aes(x = City, y = Date, fill = AQI)) +
  geom_tile() +
  labs(
    title = "Spatial Distribution of AQI in India",
    x = "City",
    y = "Date",
    fill = "AQI"
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  theme(axis.text.y = element_text(angle = 90, vjust = 0.5, hjust = 1))

# 4. Bar chart of AQI buckets across different cities
ggplot(aqi_data, aes(x = City, fill = AQI_Bucket)) +
  geom_bar() +
  labs(
    title = "AQI Bucket Distribution across Cities",
    x = "City",
    fill = "AQI Bucket"
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  theme(axis.text.y = element_text(angle = 90, vjust = 0.5, hjust = 1))

# 5. Scatter plot of AQI vs. PM2.5
ggplot(aqi_data, aes(x = PM2.5, y = AQI)) +
  geom_point() +
  labs(
    title = "AQI vs. PM2.5",
    x = "PM2.5",
    y = "AQI"
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  theme(axis.text.y = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Interpretation / Conclusion:
# Based on the analysis and visualizations, draw conclusions and provide interpretations of the findings.

# Save the code and results to an HTML file using R Markdown
# Publish the HTML file online using platforms like Netlify or GitHub Pages
