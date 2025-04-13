# The Assignment

# Starting with readr::read_csv('https://raw.githubusercontent.com/mikejohnson51/csu-ess-330/refs/heads/main/resources/co-est2023-alldata.csv'), and the material we went over, implement a full ML workflow including data cleaning, feature engineering, splitting/resampling, model testing and evaluation to predict deaths from the available COVID and Census data. Submit you R script, and your final scatter plot showing truth vs predicted to Canvas.

## Data Cleaning

library(tidyverse)
library(tidymodels)
library(readr)
library(parsnip)
library(randomForest)
library(NeuralNetTools)

# Ingest Data
# URLs for COVID-19 case data and census population data

covid_url <-  'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv'

pop_url  <- 'https://raw.githubusercontent.com/mikejohnson51/csu-ess-330/refs/heads/main/resources/co-est2023-alldata.csv'


data <- readr::read_csv(covid_url)

census <- readr::read_csv(pop_url)

census = census |> 
  filter(COUNTY == "000") |>  # Filter for state-level data only
  mutate(fips = STATE) |>      # Create a new FIPS column for merging
  select(fips, contains("2021"))  # Select relevant columns for 2021 data

## Feature Engineering

# Process COVID-19 Data
state_data <-  data |> 
  group_by(fips) |> 
  mutate(
    new_cases  = pmax(0, cases - lag(cases)),   # Compute new cases, ensuring no negative values
    new_deaths = pmax(0, deaths - lag(deaths))  # Compute new deaths, ensuring no negative values
  ) |> 
  ungroup() |> 
  left_join(census, by = "fips") |>  # Merge with census data
  mutate(
    m = month(date), y = year(date),
    season = case_when(   # Define seasons based on month
      m %in% 3:5 ~ "Spring",
      m %in% 6:8 ~ "Summer",
      m %in% 9:11 ~ "Fall",
      m %in% c(12, 1, 2) ~ "Winter"
    )
  ) |> 
  group_by(state, y, season) |> 
  mutate(
    season_cases  = sum(new_cases, na.rm = TRUE),  # Aggregate seasonal cases
    season_deaths = sum(new_deaths, na.rm = TRUE)  # Aggregate seasonal deaths
  )  |> 
  distinct(state, y, season, .keep_all = TRUE) |>  # Keep only distinct rows by state, year, season
  ungroup() |> 
  select(state, contains('season'), y, POPESTIMATE2021, BIRTHS2021, DEATHS2021) |>  # Select relevant columns
  drop_na() |>  # Remove rows with missing values
  mutate(logC = log(season_cases +1)) |>
  mutate(logD = log(season_deaths +1))# Log-transform case numbers for modeling


## Splitting/Resampling
# Inspect Data Summary
skimr::skim(state_data)  # Summarize dataset

# Data Splitting for Modeling
splitD <- initial_split(state_data, prop = 0.8, strata = season)  # 80/20 train-test split
trainD <- training(splitD)  # Training set
testD <- testing(splitD)  # Test set
foldsD <- vfold_cv(trainD, v = 10)  # 10-fold cross-validation

# Feature Engineering
recD = recipe(logD ~ . , data = trainD) |> 
  step_rm(state, season_cases) |>  # Remove non-predictive columns
  step_dummy(all_nominal()) |>  # Convert categorical variables to dummy variables
  step_scale(all_numeric_predictors()) |>  # Scale numeric predictors
  step_center(all_numeric_predictors())  # Center numeric predictors

## Model Testing
# Define Regression Models
lm_mod <- linear_reg() |> 
  set_engine("lm") |> 
  set_mode("regression")

rf_model <- rand_forest() |> 
  set_engine("ranger") |> 
  set_mode("regression")

rf_model2 <- rand_forest() |> 
  set_engine("randomForest") |> 
  set_mode("regression")

b_mod <- boost_tree() |> 
  set_engine("xgboost") |> 
  set_mode("regression")

nn_mod <- mlp(hidden_units = 10) |> 
  set_engine("nnet") |> 
  set_mode("regression")

## Visualization

# Create Workflow Set
wfD = workflow_set(list(recD), list(lm_mod, 
                                    rf_model, 
                                    rf_model2,
                                    b_mod, 
                                    nn_mod
)) |> 
  workflow_map(resamples = foldsD)  # Apply workflows across resamples

# Visualize Model Performance
autoplot(wfD)

# Fit Selected Model (Neural Network)
fitD <- workflow() |> 
  add_recipe(recD) |> 
  add_model(b_mod) |> 
  fit(data = trainD)


# Feature Importance
vip::vip(fitD)

# Model Evaluation
predictionsD <- augment(fitD, new_data = testD) |> 
  mutate(diff = abs(logD - .pred))  # Compute absolute differences

metrics(predictionsD, truth = logD, estimate = .pred)  # Compute regression metrics

ggplot(predictionsD, aes(x = logD, y = .pred)) + 
  geom_point() + 
  geom_abline() +
  #geom_label(aes(label = paste(state, season), nudge_x = 0.1, nudge_y = 0.1)) +
  labs(title = "Boosted Forest Model", 
       x = "Actual (Log10)", 
       y = "Predicted (Log10)") + 
  theme_minimal()

ggsave(filename = "C:/Users/Joshua Puyear/Documents/csu-undergrad/ess-330-joshp-2025/github/ess-330-daily-exercises/06-and-up-exercise-330/imgs/scatter_death.jpg", plot = last_plot(), scale = 1, width = 8,
       height = 6, units = "in",
       dpi = 300)
