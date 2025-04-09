#Josh Puyear
#04/07/2025
#Practice making joins and pivots

#Exercise 15
#accessing Palmer Penguins

library(tidymodels)
library(palmerpenguins)
library(ranger)
library(dplyr)
library(ggplot2)
library(tidyr)
library(visdat)
library(ggpubr)

penguins <- na.omit(palmerpenguins::penguins)

names(penguins)


shapiro.test(penguins$flipper_length_mm)
#not normally distributed, so shouldn't I do some feature engineering (recipe,
#prep, bake?)

set.seed(3155)
(resample_split <- initial_split(penguins, prop = 0.7))

#create training data
penguins_train <- training(resample_split)
glimpse(penguins_train)

#create testing data
penguins_test <- testing(resample_split)
glimpse(penguins_test)

#Checking that data proportions match
(table(penguins$species) / nrow(penguins))

# Training Data
(table(penguins_train$species) / nrow(penguins_train))

# Testing Data
(table(penguins_test$species) / nrow(penguins_test))

#They don't really match

#Making strata
set.seed(142)

penguins <- drop_na(penguins)
penguins_strata <- initial_split(penguins, strata = species, prop = .7)

# Extract the training and testing sets
train_strata <- training(penguins_strata)
test_strata  <- testing(penguins_strata)

# Check the proportions of stratified data
# Dataset
table(penguins$species) / nrow(penguins)

table(train_strata$species) / nrow(train_strata)

table(test_strata$species) / nrow(test_strata)

#Create a 10-fold cross validation based on the training data

#this is how many rows will be in each fold
nrow(train_strata) * 1/10

penguins_folds <- vfold_cv(train_strata, v = 10) |>
  glimpse()




#Exercise 16- fitting the model

# Open your R script from the last daily assignment

# Add a new section for the model fitting and workflow
#setting the model's engine and mode

# Define a logistic regression model and a rand_forest model
#rand_forest is new, so, think about the engine (use the default) and mode needed for our problem
#define multinomial logistic regression model, there are 3 species types
multinom_model <-
  multinom_reg() %>% 
  set_engine("nnet") %>%
  set_mode("classification")

(mm_output <- fit(multinom_model, species ~ ., data = penguins_train))

vis_dat(mm_output)

#define a random forest model: this model isn't attached to data yet
rf_model <-
  rand_forest() %>% 
  set_engine("ranger") %>% 
  set_mode("classification")

(rf_output <- fit(rf_model, species ~ ., data = penguins_train))

# Set up a workflow_set() to compare the logistic regression 
#model (the winner of lecture here) to the rand_forest 
#model you create. Use accuracy as you primary metric 
# to rank the models.

# fit both models to the 10-fold cross validation
# fit_resamples is a function from the tidymodels ecosystem, specifcally in the workflows package,
# that fits a model to multiple reamples of your data (like in the cross or bootstrapping

penguins_wf_set <- workflow_set(list(species ~ .), list(multinom_model, rf_model)) %>% 
  workflow_map("fit_resamples", resamples = penguins_folds)


# Quick plot function
autoplot(penguins_wf_set) + 
  theme_linedraw()
  
rank <- rank_results(penguins_wf_set, 
             rank_metric = "accuracy", 
             select_best = TRUE)

# As a comment, write a sentence about what 
# model you think is best!
# Submit your R script to Canvas

#With an accuracy of .983, the top ranked model is the multinomial regression model
#because its accuracy is .005 higher than the random forest model. Notably, the
#random_forest does have a higher area under the curve, but since we are ranking
#by accuracy, this is more important.






