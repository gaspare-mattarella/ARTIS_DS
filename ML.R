#### TASK 2 #####
library(tidymodels)
library(vip)
# Fix the random numbers by setting the seed
# This enables the analysis to be reproducible when random numbers are used
set.seed(222)
# Put 3/4 of the data into the training set
#full_panel$id <-  as.numeric(as.data.frame(full_panel)$id)
full_panel <- as.data.frame(full_panel)
full_panel$`Gross BEL (inc. TPs as whole, pre-TMTP) (£m` <- NULL
full_panel <- full_panel[complete.cases(full_panel),]
full_panel$id <- NULL
full_panel$wave <- NULL

# Mutate the variable 'x' based on your condition
full_panel <- full_panel %>%
  mutate(.keep = 'unused', SCR_ratio = ifelse(`SCR coverage r` < 1, 0, ifelse(`SCR coverage r` >= 1, 1, `SCR coverage r`)))

full_panel$SCR_ratio <- as.factor(full_panel$SCR_ratio)
data_split <- initial_validation_split(full_panel, prop = c(.7,.1))

# Create data frames for the two sets:
train_data <- training(data_split)
test_data  <- testing(data_split)
val_set <- validation(data_split)

cores <- parallel::detectCores()


rf_mod <-
  rand_forest(mtry = tune(), min_n = tune(), trees = 1000) %>%
  set_engine("ranger", num.threads = cores) %>%
  set_mode("classification")

svm_mod <-
  svm_rbf(cost = tune(), rbf_sigma = tune()) %>%
  set_mode("classification") %>%
  set_engine("kernlab")

rf_recipe <-
  recipe(SCR_ratio ~ ., data = train_data) %>%
# remove any zero variance predictors
step_zv(all_predictors()) %>%
  # remove any linear combinations
  step_lincomb(all_numeric())


svm_recipe <-
  recipe(SCR_ratio ~ ., data = train_data)  %>%
  # remove any zero variance predictors
  step_zv(all_predictors()) %>%
  # remove any linear combinations
  step_lincomb(all_numeric())

rf_workflow <-
  workflow() %>%
  add_model(rf_mod) %>%
  add_recipe(rf_recipe)


svm_workflow <-
  workflow() %>%
  add_model(svm_mod) %>%
  add_recipe(svm_recipe)



rf_res <-
  rf_workflow %>%
  tune_grid(
    resamples = vfold_cv(val_set),
            grid = 25,
            control = control_grid(save_pred = TRUE,verbose = T),
            metrics = metric_set(roc_auc))

svm_res <-
  rf_workflow %>%
  tune_grid(
    resamples = vfold_cv(val_set),
    grid = 25,
    control = control_grid(save_pred = TRUE,verbose = T),
    metrics = metric_set(roc_auc))


rf_res %>%
  show_best(metric = "roc_auc")

svm_res %>%
  show_best(metric = "roc_auc")

rf_best <-
  rf_res %>%
  select_best(metric = "roc_auc")

rf_auc <-
  rf_res %>%
  collect_predictions(parameters = rf_best) %>%
  roc_curve(SCR_ratio , .pred_0) %>%
  mutate(model = "Random Forest")


svm_best <-
  svm_res %>%
  select_best(metric = "roc_auc")

svm_auc <-
  svm_res %>%
  collect_predictions(parameters = svm_best) %>%
  roc_curve(SCR_ratio , .pred_0) %>%
  mutate(model = "SVM")

bind_rows(rf_auc, svm_auc) %>%
  ggplot(aes(x = 1 - specificity, y = sensitivity, col = model)) +
  geom_path(lwd = 1.5, alpha = 0.8) +
  geom_abline(lty = 3) +
  coord_equal() +
  scale_color_viridis_d(option = "plasma", end = .6)

# the last model
last_rf_mod <-
  rand_forest(mtry = 5, min_n = 38, trees = 1000) %>%
  set_engine("ranger", num.threads = cores, importance = "impurity") %>%
  set_mode("classification")

# the last workflow
last_rf_workflow <-
  rf_workflow %>%
  update_model(last_rf_mod)

# the last fit
set.seed(345)
last_rf_fit <-
  last_rf_workflow %>%
  last_fit(data_split)

last_rf_fit %>%
  collect_metrics()
last_rf_fit %>%
  extract_fit_parsnip() %>%
  vip::vip(num_features = 20)

