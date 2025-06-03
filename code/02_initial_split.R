library(fs)
library(tidymodels)
library(bestNormalize)

# ------------------------------------------------------------------------------

tidymodels_prefer()
theme_set(theme_bw())
options(pillar.advice = FALSE, pillar.min_title_chars = Inf)

# ------------------------------------------------------------------------------
# Get original data

load(path("rdata", "assay_and_compound_data.RData"))

# ------------------------------------------------------------------------------
# EDA for split

# Counts are about the same
raw_data |> count(Class)

set.seed(301)
tox_split <- initial_split(raw_data |> select(-Compound), prop = 8/10)
tox_train <- training(tox_split)
tox_test  <- testing(tox_split)

# ------------------------------------------------------------------------------
# Are the assay results fairly comparable? 

pca_rec <- recipe(Class ~ ., data = tox_train) |> 
  step_impute_knn(all_numeric_predictors(), neighbors = 5) |> 
  step_orderNorm(all_numeric_predictors()) |> 
  step_pca(all_numeric_predictors(), num_comp = 2) |> 
  prep()

bind_rows(
  bake(pca_rec, tox_train) |> mutate(data = "Training"),
  bake(pca_rec, tox_test) |> mutate(data = "Testing")
) |> 
  ggplot(aes(PC1, PC2, col = data, pch = data)) + 
  geom_point(cex = 2, alpha = 3 / 4) + 
  coord_obs_pred()

# ------------------------------------------------------------------------------
# Save data sets

save(tox_split, tox_train, tox_test, file = path("rdata", "splits.RData"))




