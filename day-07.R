#Joshua Puyear
#February 17, 2025

# Question 1
# Reading COVID-19 data
library(tidyverse)
library(readr)
url = 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties-recent.csv'
covid = read_csv(url)

#Identify the six states with the most current cases
top_states <- covid %>% 
  filter(date == max(date)) %>% 
  arrange(desc(cases)) %>% 
  slice_max(order_by = cases, n = 6) %>% 
  pull (state)
  
# Top six states are California, New York, Florida, Illinois, Arizona, and Texas
  

# Filter the raw data to those six states
filtered_data <- covid %>% 
  filter(state %in% top_states) %>% 
  group_by(date, state, .groups = "drop") %>% 
  summarize(total_cases = sum(cases, na.rm = TRUE))
  
# Set up a ggplot -> add layers -> add labels -> add a facet -> add a theme
library(ggplot2)
library(dplyr)
  
plot1 <- ggplot(filtered_data, aes(x = date, y = total_cases, color = state)) +
  geom_smooth(size = 1.5) +
  facet_wrap (~ state, scales = "free_y") +
  labs(title = "Six Most Prevalent Covid States",
       x = "date",
       y = "cases",
       color = "state") +
  theme_bw()
print(plot1)


  
# Save image to img directory
ggsave("Six_Most_Prevalent_Covid_States.png", plot = plot1, width = 10, height = 6, dpi = 300, path = ("C:/Users/Joshua Puyear/Documents/csu-undergrad/ess-330-joshp-2025/github/ess-330-daily-exercises/06-and-up-exercise-330/imgs"))


# Question 2
# Identify the total cases each day in the whole country
total_cases_by_day <- covid %>% 
  group_by(date) %>% 
  summarize(total_cases= sum(cases, na.rm = TRUE))

# Set up a ggplot -> add layers -> add labels -> add a theme
plot2 <- ggplot(total_cases_by_day, aes(x = date, y = total_cases)) +
  geom_line(color = "darkblue", size = 1.2) +
  labs(title = "Total US Covid-19 Cases by Day",
       x = "Date",
       y = "Total Cases") +
  theme_bw()
print(plot2)

#Save image to img directory
ggsave("Total_US_Covid-19_cases_by_Day.png", plot = plot2, width = 10, height = 6, dpi = 300, path = ("C:/Users/Joshua Puyear/Documents/csu-undergrad/ess-330-joshp-2025/github/ess-330-daily-exercises/06-and-up-exercise-330/imgs"))



