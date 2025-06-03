library(fs)
library(tidymodels)
library(bestNormalize)
library(bonsai)

# required but not loaded
# library(aorsf)


# ------------------------------------------------------------------------------

tidymodels_prefer()
theme_set(theme_bw())
options(pillar.advice = FALSE, pillar.min_title_chars = Inf)

# ------------------------------------------------------------------------------
# Get data sets

load(path("rdata", "splits.RData"))

# ------------------------------------------------------------------------------
# Set resampling method

set.seed(12)
tox_rs <- vfold_cv(tox_train, repeats = 5)

# ------------------------------------------------------------------------------
# Describe an oblique random forest model

impute_rec <- recipe(Class ~ ., data = tox_train) |> 
  step_impute_knn(all_numeric_predictors(), neighbors = tune()) 

rf_spec <- 
  rand_forest(mtry = tune(), min_n = tune(), trees = 2000) |> 
  set_engine("aorsf") |> 
  set_mode("classification")

rf_wflow <- workflow(impute_rec, rf_spec)
rf_param <- 
  rf_wflow |> 
  extract_parameter_set_dials() |> 
  update(mtry = mtry(c(1, 12)))


# ------------------------------------------------------------------------------
# Tune the model

set.seed(482)
rf_res <- 
  rf_wflow |> 
  tune_grid(
    resamples = tox_rs,
    grid = 25,
    param_info = rf_param,
    control = control_grid(save_pred = TRUE, save_workflow = TRUE)
  )

autoplot(rf_res, metric = "roc_auc")

# ------------------------------------------------------------------------------

rf_best <- select_best(rf_res, metric = "roc_auc")
rf_best_pred <- collect_predictions(rf_res, parameters = rf_best, summarize = FALSE)
rf_best_mean_pred <- collect_predictions(rf_res, parameters = rf_best, summarize = TRUE)

rf_best_mean_pred |> 
  roc_curve(Class, .pred_toxic) |> 
  arrange(specificity, sensitivity) |> 
  ggplot(aes(1 - specificity, sensitivity)) + 
  geom_abline(col = "red", lty = 2) +
  geom_step(direction = "hv") + 
  coord_obs_pred()

# ------------------------------------------------------------------------------
# Fit to training, evaluate on testing

rf_final_wflow <- 
  rf_wflow |> 
  finalize_workflow(rf_best)

set.seed(823)
rf_final_res <- 
  rf_final_wflow |> 
  last_fit(tox_split)

# ------------------------------------------------------------------------------
# Save results

save(rf_res, rf_final_res, tox_test, file = path("rdata", "model.RData"))
