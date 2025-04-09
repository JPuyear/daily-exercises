
  #you are building on the dataset you should have made for daily assignment 15
 
install.packages("tidymodels")

#load necessary libraries
library(tidymodels)
library(palmerpenguins)
library(ranger)

install.packages("palmerpenguins")


#load dataset and remove na, set seed
penguins <-
na.omit(palmerpenguins::penguins)
set.seed(123)

#split the dataset 70/30, extract each subset
splitdata <-
  initial_split(penguins, prop = .7)
traindata <-
  training(splitdata)
testdata <- testing(splitdata)

#make a 10-fold cross-validation dataset using training data
penguins_folds <-
  vfold_cv(traindata, v - 10, strata = species)

#define multinomial logistic regression model, there are 3 species types
multinom_model <-
  multinom_reg() %>% 
    set_engine("nnet") %>%
  set_mode("classification")

#define a random forest model
rand_forest_model <-
  rand_forest() %>% 
  set_engine("ranger") %>% 
  set_mode("classification")


#make workflow_set to compare both models
penguins_wf_set <- workflow_set(
    preproc = list(species ~ .),
    models = list(multinom = multinom_model, rf = rand_forest_model))

# fit both models to the 10-fold cross validataion
# fit_resamples is a function from the tidymodels ecosystem, specifcally in the workflows package,
# that fits a model to multiple reamples of your data (like in the cross or bootstrapping
penguins_res <- penguins_wf_set %>% 
  workflow_map("fit_resamples", resamples = penguins_folds, control
control_resamples(save_pred = TRUE))

# metrics
penguins_res_metrics <- collect_metrics(penguins_res)

# compare
accuracy_comparison <- penguins_res_metrics
      
  
control_resamples(save_pred = TRUE)
      
      

      
      

  