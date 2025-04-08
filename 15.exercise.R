#Josh Puyear
#04/07/2025
#Practice making joins and pivots

#accessing Palmer Penguins

library(tidymodels)
library(palmerpenguins)


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

vfold_cv(train_strata, v = 10) |>
  glimpse()

