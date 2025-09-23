library(gamlss)
library(dplyr)
library(tidymodels)

data("diamonds")

diamonds_df <- diamonds %>%
  mutate(clarity = factor(clarity, ordered = FALSE))


diamonds_split <- initial_split(diamonds_df, prop = 0.8, strata = clarity)

training_diamonds <- training(diamonds_split)

testing_diamonds <- testing(diamonds_split)

xgb_recipe_diamonds <- recipe(clarity ~ ., data = training_diamonds) %>%
  step_corr() %>%
  step_normalize(all_numeric_predictors()) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE)


xgb_model_diamonds <- boost_tree(trees = tune(), tree_depth = tune()) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

xgb_workflow <- workflow() %>%
  add_recipe(xgb_recipe_diamonds) %>%
  add_model(xgb_model_diamonds)

#xgb_fitted <- fit(xgb_workflow, data = training_diamonds)

xgb_cv_folds <- vfold_cv(training_diamonds, v = 3)

xgb_grid <- tibble(trees = c(8, 15))  

xgb_grid <- expand.grid(
  trees = c(5, 7),
  tree_depth = c(3, 5)
)


# 1. Wstępne strojenie siatką
initial_results <- tune_grid(
  xgb_workflow,
  resamples = xgb_cv_folds,
  grid = xgb_grid,
  metrics = metric_set(accuracy, roc_auc),
  control = control_grid(verbose = TRUE)
)

# 2. Bayesian tuning z wynikami jako `initial`
set.seed(123)
bayes_results <- tune_bayes(
  xgb_workflow,
  resamples = xgb_cv_folds,
  param_info = parameters(xgb_workflow),
  initial = initial_results,
  iter = 2,  # liczba iteracji
  metrics = metric_set(accuracy, roc_auc),
  control = control_bayes(verbose = TRUE, no_improve = 5)
)



best_xgb_params <- select_best(bayes_results, metric = "roc_auc")


xgb_finalize_workflow <- finalize_workflow(xgb_workflow, best_xgb_params)


final_fit <- fit(xgb_finalize_workflow, data = training_diamonds)

preds_class <- predict(final_fit, testing_diamonds, type = "class")
preds_prob  <- predict(final_fit, testing_diamonds, type = "prob")

preds <- bind_cols(testing_diamonds, preds_class, preds_prob)

metrics(preds, truth = clarity, estimate = .pred_class)   # accuracy, kappa
roc_auc(preds, truth = clarity, starts_with(".pred_"), -.pred_class)


