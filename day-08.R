#Josh Puyear
#02/24/2025
#Practice making joins and pivots

Make a faceted plot of the cumulative cases & deaths by USA region. Your x axis should be the date and the y axis value/count. To do this you will need to join and pivot the COVID-19 data. We can break this task into 7 steps:

#Read in the COVID-19 Data
url = 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties-recent.csv'
covid = read_csv(url)
head(covid, 5)

#Create a new data.frame using the available state.abb, state.name, state.region objects in  base R. Be intentional about creating a primary key to match to the COVID data!


# example code
#df = data.frame(region = state.region,
#                ...,
#          ...)

df <- data.frame(region = state.region,
                 abbr = state.abb,
                 state = state.name)
head(df)



#Join your new data.frame to the raw COVID data. Think about right, inner, left, or full join…

covid_region <- inner_join(df, covid, by = "state") |>
  group_by(region, date) |>  
# split-apply the joined data to determine the daily,
  summarize(cases = sum(cases),  
# cumulative, cases and deaths for each region
            deaths = sum(deaths), 
            .groups = "drop") |>  
# Remove grouping to prevent issues
  pivot_longer(cols = c(cases, deaths),
# Pivot your data from wide format to long
               names_to = "type",
               values_to = "count") |>
  ggplot(aes(x = date, y = count, group = type, color = type)) +  # Ensure grouping
  geom_line() +                 
# Plot your data in a compelling way (setup, layers,
# labels, facets, themes)
  facet_grid(type ~ region) +
  theme_bw()

# Save the image to your img directory with a good
# file name and extension!

ggsave("covid_cases.png", plot = covid_region, width = 10, height = 6, dpi = 300, 
       path = "C:/Users/Joshua Puyear/Documents/csu-undergrad/ess-330-joshp-2025/github/ess-330-daily-exercises/06-and-up-exercise-330/imgs")

