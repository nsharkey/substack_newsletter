

library(tidyverse)
library(arrow)
library(h2o)
library(tictoc)


rm(list = ls())

qbs_by_season_qualified <- 
  read_parquet('000_a_data_science_journey/results/qbs_by_season_qualified.parquet') %>% 
  filter(season_type == 'REG')


training_df <- 
  qbs_by_season_qualified %>% 
  filter(season <= 2020) %>% 
  drop_na(fantasy_points_dk_next) %>% 
  select(
    -season_type, 
    # -season, 
    # -player_id, 
    # -player_name, 
    # -position, 
    # -team, 
    -games, 
    -fantasy_points_espn_next,
    -fantasy_points_dk_above_rplcmnt_next, 
    -fantasy_points_espn_above_rplcmnt_next, 
    -fantasy_teams, 
    -season_last_n)

# test_df <- 
#   qbs_by_season_qualified %>% 
#   filter(season == 2020)

prediction_df <- 
  qbs_by_season_qualified %>% 
  filter(season == 2021)



summary(lm(fantasy_points_dk_next ~ ., data = training_df))



h2o.init()

h2o.removeAll()

training_df_h2o <- as.h2o(training_df)

# test_df_h2o <- as.h2o(test_df)

prediction_df_h2o <- as.h2o(prediction_df)

prediction_df


y <- 'fantasy_points_dk_next'
x <- setdiff(
  colnames(training_df_h2o), 
  c(
    y, 
    'season', 
    'player_id', 
    'player_name', 
    'position', 
    'team'))



tic()

grid_max_models_global <- 10


# # Grid Searches

# GLM

# Hyperparameters
grid_max_models = grid_max_models_global
grid_sort_by = "RMSE"
grid_nfolds = 5
grid_fold_assignment = "Modulo"
grid_stopping_rounds = 2
grid_stopping_metric = grid_sort_by
grid_stopping_tolerance = 1e-3

glm_params <- list(
  alpha = seq(0.001, 0.999, 0.01),
  lambda = seq(0.001, 0.999, 0.01))

glm_search_criteria <- list(
  strategy = "RandomDiscrete",
  max_models = grid_max_models)


h2o_qbs_season_glm_grid <-
  h2o.grid(
    "glm",
    x = x,
    y = y,
    grid_id = "h2o_qbs_season_glm_grid",
    training_frame = training_df_h2o,
    nfolds = grid_nfolds,
    fold_assignment = grid_fold_assignment,
    keep_cross_validation_predictions = TRUE,
    hyper_params = glm_params,
    search_criteria = glm_search_criteria,
    stopping_rounds = grid_stopping_rounds,
    stopping_metric = grid_stopping_metric,
    stopping_tolerance = grid_stopping_tolerance)

h2o_qbs_season_glm_grid_perf <-
  h2o.getGrid(
    grid_id = "h2o_qbs_season_glm_grid",
    sort_by = grid_sort_by,
    decreasing = FALSE)

h2o_qbs_season_glm_grid_perf

h2o_qbs_season_glm_grid_01 <- h2o.getModel(h2o_qbs_season_glm_grid_perf@model_ids[[1]])
h2o_qbs_season_glm_grid_02 <- h2o.getModel(h2o_qbs_season_glm_grid_perf@model_ids[[2]])
h2o_qbs_season_glm_grid_03 <- h2o.getModel(h2o_qbs_season_glm_grid_perf@model_ids[[3]])
h2o_qbs_season_glm_grid_04 <- h2o.getModel(h2o_qbs_season_glm_grid_perf@model_ids[[4]])
h2o_qbs_season_glm_grid_05 <- h2o.getModel(h2o_qbs_season_glm_grid_perf@model_ids[[5]])
h2o_qbs_season_glm_grid_06 <- h2o.getModel(h2o_qbs_season_glm_grid_perf@model_ids[[6]])
h2o_qbs_season_glm_grid_07 <- h2o.getModel(h2o_qbs_season_glm_grid_perf@model_ids[[7]])
h2o_qbs_season_glm_grid_08 <- h2o.getModel(h2o_qbs_season_glm_grid_perf@model_ids[[8]])
h2o_qbs_season_glm_grid_09 <- h2o.getModel(h2o_qbs_season_glm_grid_perf@model_ids[[9]])
h2o_qbs_season_glm_grid_10 <- h2o.getModel(h2o_qbs_season_glm_grid_perf@model_ids[[10]])

# Save the best models
h2o.saveModel(object = h2o_qbs_season_glm_grid_01, path = '000_a_data_science_journey/models/', force = TRUE, export_cross_validation_predictions = TRUE, filename = 'h2o_qbs_season_glm_grid_01.h2o')
h2o.saveModel(object = h2o_qbs_season_glm_grid_02, path = '000_a_data_science_journey/models/', force = TRUE, export_cross_validation_predictions = TRUE, filename = 'h2o_qbs_season_glm_grid_02.h2o')
h2o.saveModel(object = h2o_qbs_season_glm_grid_03, path = '000_a_data_science_journey/models/', force = TRUE, export_cross_validation_predictions = TRUE, filename = 'h2o_qbs_season_glm_grid_03.h2o')
h2o.saveModel(object = h2o_qbs_season_glm_grid_04, path = '000_a_data_science_journey/models/', force = TRUE, export_cross_validation_predictions = TRUE, filename = 'h2o_qbs_season_glm_grid_04.h2o')
h2o.saveModel(object = h2o_qbs_season_glm_grid_05, path = '000_a_data_science_journey/models/', force = TRUE, export_cross_validation_predictions = TRUE, filename = 'h2o_qbs_season_glm_grid_05.h2o')
h2o.saveModel(object = h2o_qbs_season_glm_grid_06, path = '000_a_data_science_journey/models/', force = TRUE, export_cross_validation_predictions = TRUE, filename = 'h2o_qbs_season_glm_grid_06.h2o')
h2o.saveModel(object = h2o_qbs_season_glm_grid_07, path = '000_a_data_science_journey/models/', force = TRUE, export_cross_validation_predictions = TRUE, filename = 'h2o_qbs_season_glm_grid_07.h2o')
h2o.saveModel(object = h2o_qbs_season_glm_grid_08, path = '000_a_data_science_journey/models/', force = TRUE, export_cross_validation_predictions = TRUE, filename = 'h2o_qbs_season_glm_grid_08.h2o')
h2o.saveModel(object = h2o_qbs_season_glm_grid_09, path = '000_a_data_science_journey/models/', force = TRUE, export_cross_validation_predictions = TRUE, filename = 'h2o_qbs_season_glm_grid_09.h2o')
h2o.saveModel(object = h2o_qbs_season_glm_grid_10, path = '000_a_data_science_journey/models/', force = TRUE, export_cross_validation_predictions = TRUE, filename = 'h2o_qbs_season_glm_grid_10.h2o')



# Random Forest

# Hyperparameters
grid_max_models = grid_max_models_global
grid_sort_by = "RMSE"
grid_nfolds = 5
grid_fold_assignment = "Modulo"
grid_stopping_rounds = 2
grid_stopping_metric = grid_sort_by
grid_stopping_tolerance = 1e-3

drf_params = list(
  ntrees = c(500),
  max_depth = seq(1,20),
  min_rows = c(1,5,10),
  sample_rate = seq(0.3,0.5,0.05),
  col_sample_rate_per_tree = seq(0.3,0.5,0.05))

drf_search_criteria <- list(
  strategy = "RandomDiscrete",
  max_models = grid_max_models)


# Train and validate a grid of
h2o_qbs_season_drf_grid <-
  h2o.grid(
    "drf",
    x = x,
    y = y,
    grid_id = "h2o_qbs_season_drf_grid",
    training_frame = training_df_h2o,
    nfolds = grid_nfolds,
    fold_assignment = grid_fold_assignment,
    keep_cross_validation_predictions = TRUE,
    hyper_params = drf_params,
    search_criteria = drf_search_criteria,
    stopping_rounds = grid_stopping_rounds,
    stopping_metric = grid_stopping_metric,
    stopping_tolerance = grid_stopping_tolerance)

h2o_qbs_season_drf_grid_perf <-
  h2o.getGrid(
    grid_id = "h2o_qbs_season_drf_grid",
    sort_by = grid_sort_by,
    decreasing = FALSE)

h2o_qbs_season_drf_grid_perf

h2o_qbs_season_drf_grid_01 <- h2o.getModel(h2o_qbs_season_drf_grid_perf@model_ids[[1]])
h2o_qbs_season_drf_grid_02 <- h2o.getModel(h2o_qbs_season_drf_grid_perf@model_ids[[2]])
h2o_qbs_season_drf_grid_03 <- h2o.getModel(h2o_qbs_season_drf_grid_perf@model_ids[[3]])
h2o_qbs_season_drf_grid_04 <- h2o.getModel(h2o_qbs_season_drf_grid_perf@model_ids[[4]])
h2o_qbs_season_drf_grid_05 <- h2o.getModel(h2o_qbs_season_drf_grid_perf@model_ids[[5]])
h2o_qbs_season_drf_grid_06 <- h2o.getModel(h2o_qbs_season_drf_grid_perf@model_ids[[6]])
h2o_qbs_season_drf_grid_07 <- h2o.getModel(h2o_qbs_season_drf_grid_perf@model_ids[[7]])
h2o_qbs_season_drf_grid_08 <- h2o.getModel(h2o_qbs_season_drf_grid_perf@model_ids[[8]])
h2o_qbs_season_drf_grid_09 <- h2o.getModel(h2o_qbs_season_drf_grid_perf@model_ids[[9]])
h2o_qbs_season_drf_grid_10 <- h2o.getModel(h2o_qbs_season_drf_grid_perf@model_ids[[10]])

# Save the best models
h2o.saveModel(object = h2o_qbs_season_drf_grid_01, path = '000_a_data_science_journey/models/', force = TRUE, export_cross_validation_predictions = TRUE, filename = 'h2o_qbs_season_drf_grid_01.h2o')
h2o.saveModel(object = h2o_qbs_season_drf_grid_02, path = '000_a_data_science_journey/models/', force = TRUE, export_cross_validation_predictions = TRUE, filename = 'h2o_qbs_season_drf_grid_02.h2o')
h2o.saveModel(object = h2o_qbs_season_drf_grid_03, path = '000_a_data_science_journey/models/', force = TRUE, export_cross_validation_predictions = TRUE, filename = 'h2o_qbs_season_drf_grid_03.h2o')
h2o.saveModel(object = h2o_qbs_season_drf_grid_04, path = '000_a_data_science_journey/models/', force = TRUE, export_cross_validation_predictions = TRUE, filename = 'h2o_qbs_season_drf_grid_04.h2o')
h2o.saveModel(object = h2o_qbs_season_drf_grid_05, path = '000_a_data_science_journey/models/', force = TRUE, export_cross_validation_predictions = TRUE, filename = 'h2o_qbs_season_drf_grid_05.h2o')
h2o.saveModel(object = h2o_qbs_season_drf_grid_06, path = '000_a_data_science_journey/models/', force = TRUE, export_cross_validation_predictions = TRUE, filename = 'h2o_qbs_season_drf_grid_06.h2o')
h2o.saveModel(object = h2o_qbs_season_drf_grid_07, path = '000_a_data_science_journey/models/', force = TRUE, export_cross_validation_predictions = TRUE, filename = 'h2o_qbs_season_drf_grid_07.h2o')
h2o.saveModel(object = h2o_qbs_season_drf_grid_08, path = '000_a_data_science_journey/models/', force = TRUE, export_cross_validation_predictions = TRUE, filename = 'h2o_qbs_season_drf_grid_08.h2o')
h2o.saveModel(object = h2o_qbs_season_drf_grid_09, path = '000_a_data_science_journey/models/', force = TRUE, export_cross_validation_predictions = TRUE, filename = 'h2o_qbs_season_drf_grid_09.h2o')
h2o.saveModel(object = h2o_qbs_season_drf_grid_10, path = '000_a_data_science_journey/models/', force = TRUE, export_cross_validation_predictions = TRUE, filename = 'h2o_qbs_season_drf_grid_10.h2o')



# GBM

# Hyperparameters
grid_max_models = grid_max_models_global
grid_sort_by = "RMSE"
grid_nfolds = 5
grid_fold_assignment = "Modulo"
grid_stopping_rounds = 2
grid_stopping_metric = grid_sort_by
grid_stopping_tolerance = 1e-3

gbm_params = list(
  ntrees = c(500),
  max_depth = seq(1,20),
  min_rows = c(1,5,10),
  sample_rate = seq(0.3,0.5,0.05),
  col_sample_rate_per_tree = seq(0.3,0.5,0.05))

gbm_search_criteria <- list(
  strategy = "RandomDiscrete",
  max_models = grid_max_models)


# Train and validate a grid of
h2o_qbs_season_gbm_grid <-
  h2o.grid(
    "gbm",
    x = x,
    y = y,
    grid_id = "h2o_qbs_season_gbm_grid",
    training_frame = training_df_h2o,
    nfolds = grid_nfolds,
    fold_assignment = grid_fold_assignment,
    keep_cross_validation_predictions = TRUE,
    hyper_params = gbm_params,
    search_criteria = gbm_search_criteria,
    stopping_rounds = grid_stopping_rounds,
    stopping_metric = grid_stopping_metric,
    stopping_tolerance = grid_stopping_tolerance)

h2o_qbs_season_gbm_grid_perf <-
  h2o.getGrid(
    grid_id = "h2o_qbs_season_gbm_grid",
    sort_by = grid_sort_by,
    decreasing = FALSE)

h2o_qbs_season_gbm_grid_perf

h2o_qbs_season_gbm_grid_01 <- h2o.getModel(h2o_qbs_season_gbm_grid_perf@model_ids[[1]])
h2o_qbs_season_gbm_grid_02 <- h2o.getModel(h2o_qbs_season_gbm_grid_perf@model_ids[[2]])
h2o_qbs_season_gbm_grid_03 <- h2o.getModel(h2o_qbs_season_gbm_grid_perf@model_ids[[3]])
h2o_qbs_season_gbm_grid_04 <- h2o.getModel(h2o_qbs_season_gbm_grid_perf@model_ids[[4]])
h2o_qbs_season_gbm_grid_05 <- h2o.getModel(h2o_qbs_season_gbm_grid_perf@model_ids[[5]])
h2o_qbs_season_gbm_grid_06 <- h2o.getModel(h2o_qbs_season_gbm_grid_perf@model_ids[[6]])
h2o_qbs_season_gbm_grid_07 <- h2o.getModel(h2o_qbs_season_gbm_grid_perf@model_ids[[7]])
h2o_qbs_season_gbm_grid_08 <- h2o.getModel(h2o_qbs_season_gbm_grid_perf@model_ids[[8]])
h2o_qbs_season_gbm_grid_09 <- h2o.getModel(h2o_qbs_season_gbm_grid_perf@model_ids[[9]])
h2o_qbs_season_gbm_grid_10 <- h2o.getModel(h2o_qbs_season_gbm_grid_perf@model_ids[[10]])

# Save the best models
h2o.saveModel(object = h2o_qbs_season_gbm_grid_01, path = '000_a_data_science_journey/models/', force = TRUE, export_cross_validation_predictions = TRUE, filename = 'h2o_qbs_season_gbm_grid_01.h2o')
h2o.saveModel(object = h2o_qbs_season_gbm_grid_02, path = '000_a_data_science_journey/models/', force = TRUE, export_cross_validation_predictions = TRUE, filename = 'h2o_qbs_season_gbm_grid_02.h2o')
h2o.saveModel(object = h2o_qbs_season_gbm_grid_03, path = '000_a_data_science_journey/models/', force = TRUE, export_cross_validation_predictions = TRUE, filename = 'h2o_qbs_season_gbm_grid_03.h2o')
h2o.saveModel(object = h2o_qbs_season_gbm_grid_04, path = '000_a_data_science_journey/models/', force = TRUE, export_cross_validation_predictions = TRUE, filename = 'h2o_qbs_season_gbm_grid_04.h2o')
h2o.saveModel(object = h2o_qbs_season_gbm_grid_05, path = '000_a_data_science_journey/models/', force = TRUE, export_cross_validation_predictions = TRUE, filename = 'h2o_qbs_season_gbm_grid_05.h2o')
h2o.saveModel(object = h2o_qbs_season_gbm_grid_06, path = '000_a_data_science_journey/models/', force = TRUE, export_cross_validation_predictions = TRUE, filename = 'h2o_qbs_season_gbm_grid_06.h2o')
h2o.saveModel(object = h2o_qbs_season_gbm_grid_07, path = '000_a_data_science_journey/models/', force = TRUE, export_cross_validation_predictions = TRUE, filename = 'h2o_qbs_season_gbm_grid_07.h2o')
h2o.saveModel(object = h2o_qbs_season_gbm_grid_08, path = '000_a_data_science_journey/models/', force = TRUE, export_cross_validation_predictions = TRUE, filename = 'h2o_qbs_season_gbm_grid_08.h2o')
h2o.saveModel(object = h2o_qbs_season_gbm_grid_09, path = '000_a_data_science_journey/models/', force = TRUE, export_cross_validation_predictions = TRUE, filename = 'h2o_qbs_season_gbm_grid_09.h2o')
h2o.saveModel(object = h2o_qbs_season_gbm_grid_10, path = '000_a_data_science_journey/models/', force = TRUE, export_cross_validation_predictions = TRUE, filename = 'h2o_qbs_season_gbm_grid_10.h2o')





# DL 

# Hyperparameters 
grid_max_models = grid_max_models_global
grid_sort_by = "RMSE"
grid_nfolds = 5
grid_fold_assignment = "Modulo"
grid_stopping_rounds = 2
grid_stopping_metric = grid_sort_by
grid_stopping_tolerance = 1e-3

dl_params <- list(
  activation = c(
    "Rectifier", 
    "RectifierWithDropout",
    "Tanh", 
    "TanhWithDropout", 
    "Maxout", 
    "MaxoutWithDropout"),
  hidden = 
    lapply(
      1:grid_max_models, 
      function(x)10 + 
        sample(
          50, sample(4), 
          replace=TRUE)),
  input_dropout_ratio=c(0,0.05),
  l1=seq(0,1e-4,1e-6),
  l2=seq(0,1e-4,1e-6),
  variable_importances = TRUE)

dl_search_criteria <- list(
  strategy = "RandomDiscrete", 
  max_models = grid_max_models)


# Train and validate a grid of  
h2o_qbs_season_dl_grid <- 
  h2o.grid(
    "deeplearning", 
    x = x, 
    y = y,
    grid_id = "h2o_qbs_season_dl_grid",
    training_frame = training_df_h2o,
    nfolds = grid_nfolds,
    fold_assignment = grid_fold_assignment,
    keep_cross_validation_predictions = TRUE,
    hyper_params = dl_params,
    search_criteria = dl_search_criteria, 
    stopping_rounds = grid_stopping_rounds, 
    stopping_metric = grid_stopping_metric, 
    stopping_tolerance = grid_stopping_tolerance) 

h2o_qbs_season_dl_grid_perf <- 
  h2o.getGrid(
    grid_id = "h2o_qbs_season_dl_grid", 
    sort_by = grid_sort_by, 
    decreasing = FALSE)

h2o_qbs_season_dl_grid_perf

h2o_qbs_season_dl_grid_01 <- h2o.getModel(h2o_qbs_season_dl_grid_perf@model_ids[[1]])
h2o_qbs_season_dl_grid_02 <- h2o.getModel(h2o_qbs_season_dl_grid_perf@model_ids[[2]])
h2o_qbs_season_dl_grid_03 <- h2o.getModel(h2o_qbs_season_dl_grid_perf@model_ids[[3]])
h2o_qbs_season_dl_grid_04 <- h2o.getModel(h2o_qbs_season_dl_grid_perf@model_ids[[4]])
h2o_qbs_season_dl_grid_05 <- h2o.getModel(h2o_qbs_season_dl_grid_perf@model_ids[[5]])
h2o_qbs_season_dl_grid_06 <- h2o.getModel(h2o_qbs_season_dl_grid_perf@model_ids[[6]])
h2o_qbs_season_dl_grid_07 <- h2o.getModel(h2o_qbs_season_dl_grid_perf@model_ids[[7]])
h2o_qbs_season_dl_grid_08 <- h2o.getModel(h2o_qbs_season_dl_grid_perf@model_ids[[8]])
h2o_qbs_season_dl_grid_09 <- h2o.getModel(h2o_qbs_season_dl_grid_perf@model_ids[[9]])
h2o_qbs_season_dl_grid_10 <- h2o.getModel(h2o_qbs_season_dl_grid_perf@model_ids[[10]])

# Save the best models
h2o.saveModel(object = h2o_qbs_season_dl_grid_01, path = '000_a_data_science_journey/models/', force = TRUE, export_cross_validation_predictions = TRUE, filename = 'h2o_qbs_season_dl_grid_01.h2o')
h2o.saveModel(object = h2o_qbs_season_dl_grid_02, path = '000_a_data_science_journey/models/', force = TRUE, export_cross_validation_predictions = TRUE, filename = 'h2o_qbs_season_dl_grid_02.h2o')
h2o.saveModel(object = h2o_qbs_season_dl_grid_03, path = '000_a_data_science_journey/models/', force = TRUE, export_cross_validation_predictions = TRUE, filename = 'h2o_qbs_season_dl_grid_03.h2o')
h2o.saveModel(object = h2o_qbs_season_dl_grid_04, path = '000_a_data_science_journey/models/', force = TRUE, export_cross_validation_predictions = TRUE, filename = 'h2o_qbs_season_dl_grid_04.h2o')
h2o.saveModel(object = h2o_qbs_season_dl_grid_05, path = '000_a_data_science_journey/models/', force = TRUE, export_cross_validation_predictions = TRUE, filename = 'h2o_qbs_season_dl_grid_05.h2o')
h2o.saveModel(object = h2o_qbs_season_dl_grid_06, path = '000_a_data_science_journey/models/', force = TRUE, export_cross_validation_predictions = TRUE, filename = 'h2o_qbs_season_dl_grid_06.h2o')
h2o.saveModel(object = h2o_qbs_season_dl_grid_07, path = '000_a_data_science_journey/models/', force = TRUE, export_cross_validation_predictions = TRUE, filename = 'h2o_qbs_season_dl_grid_07.h2o')
h2o.saveModel(object = h2o_qbs_season_dl_grid_08, path = '000_a_data_science_journey/models/', force = TRUE, export_cross_validation_predictions = TRUE, filename = 'h2o_qbs_season_dl_grid_08.h2o')
h2o.saveModel(object = h2o_qbs_season_dl_grid_09, path = '000_a_data_science_journey/models/', force = TRUE, export_cross_validation_predictions = TRUE, filename = 'h2o_qbs_season_dl_grid_09.h2o')
h2o.saveModel(object = h2o_qbs_season_dl_grid_10, path = '000_a_data_science_journey/models/', force = TRUE, export_cross_validation_predictions = TRUE, filename = 'h2o_qbs_season_dl_grid_10.h2o')






#Metalearning 

training_df_h2o$glm01 <- h2o.predict(h2o_qbs_season_glm_grid_01, training_df_h2o)$predict
training_df_h2o$glm02 <- h2o.predict(h2o_qbs_season_glm_grid_02, training_df_h2o)$predict
training_df_h2o$glm03 <- h2o.predict(h2o_qbs_season_glm_grid_03, training_df_h2o)$predict
training_df_h2o$glm04 <- h2o.predict(h2o_qbs_season_glm_grid_04, training_df_h2o)$predict
training_df_h2o$glm05 <- h2o.predict(h2o_qbs_season_glm_grid_05, training_df_h2o)$predict
training_df_h2o$glm06 <- h2o.predict(h2o_qbs_season_glm_grid_06, training_df_h2o)$predict
training_df_h2o$glm07 <- h2o.predict(h2o_qbs_season_glm_grid_07, training_df_h2o)$predict
training_df_h2o$glm08 <- h2o.predict(h2o_qbs_season_glm_grid_08, training_df_h2o)$predict
training_df_h2o$glm09 <- h2o.predict(h2o_qbs_season_glm_grid_09, training_df_h2o)$predict
training_df_h2o$glm10 <- h2o.predict(h2o_qbs_season_glm_grid_10, training_df_h2o)$predict

training_df_h2o$drf01 <- h2o.predict(h2o_qbs_season_drf_grid_01, training_df_h2o)$predict
training_df_h2o$drf02 <- h2o.predict(h2o_qbs_season_drf_grid_02, training_df_h2o)$predict
training_df_h2o$drf03 <- h2o.predict(h2o_qbs_season_drf_grid_03, training_df_h2o)$predict
training_df_h2o$drf04 <- h2o.predict(h2o_qbs_season_drf_grid_04, training_df_h2o)$predict
training_df_h2o$drf05 <- h2o.predict(h2o_qbs_season_drf_grid_05, training_df_h2o)$predict
training_df_h2o$drf06 <- h2o.predict(h2o_qbs_season_drf_grid_06, training_df_h2o)$predict
training_df_h2o$drf07 <- h2o.predict(h2o_qbs_season_drf_grid_07, training_df_h2o)$predict
training_df_h2o$drf08 <- h2o.predict(h2o_qbs_season_drf_grid_08, training_df_h2o)$predict
training_df_h2o$drf09 <- h2o.predict(h2o_qbs_season_drf_grid_09, training_df_h2o)$predict
training_df_h2o$drf10 <- h2o.predict(h2o_qbs_season_drf_grid_10, training_df_h2o)$predict

training_df_h2o$gbm01 <- h2o.predict(h2o_qbs_season_gbm_grid_01, training_df_h2o)$predict
training_df_h2o$gbm02 <- h2o.predict(h2o_qbs_season_gbm_grid_02, training_df_h2o)$predict
training_df_h2o$gbm03 <- h2o.predict(h2o_qbs_season_gbm_grid_03, training_df_h2o)$predict
training_df_h2o$gbm04 <- h2o.predict(h2o_qbs_season_gbm_grid_04, training_df_h2o)$predict
training_df_h2o$gbm05 <- h2o.predict(h2o_qbs_season_gbm_grid_05, training_df_h2o)$predict
training_df_h2o$gbm06 <- h2o.predict(h2o_qbs_season_gbm_grid_06, training_df_h2o)$predict
training_df_h2o$gbm07 <- h2o.predict(h2o_qbs_season_gbm_grid_07, training_df_h2o)$predict
training_df_h2o$gbm08 <- h2o.predict(h2o_qbs_season_gbm_grid_08, training_df_h2o)$predict
training_df_h2o$gbm09 <- h2o.predict(h2o_qbs_season_gbm_grid_09, training_df_h2o)$predict
training_df_h2o$gbm10 <- h2o.predict(h2o_qbs_season_gbm_grid_10, training_df_h2o)$predict

training_df_h2o$dl01 <- h2o.predict(h2o_qbs_season_dl_grid_01, training_df_h2o)$predict
training_df_h2o$dl02 <- h2o.predict(h2o_qbs_season_dl_grid_02, training_df_h2o)$predict
training_df_h2o$dl03 <- h2o.predict(h2o_qbs_season_dl_grid_03, training_df_h2o)$predict
training_df_h2o$dl04 <- h2o.predict(h2o_qbs_season_dl_grid_04, training_df_h2o)$predict
training_df_h2o$dl05 <- h2o.predict(h2o_qbs_season_dl_grid_05, training_df_h2o)$predict
training_df_h2o$dl06 <- h2o.predict(h2o_qbs_season_dl_grid_06, training_df_h2o)$predict
training_df_h2o$dl07 <- h2o.predict(h2o_qbs_season_dl_grid_07, training_df_h2o)$predict
training_df_h2o$dl08 <- h2o.predict(h2o_qbs_season_dl_grid_08, training_df_h2o)$predict
training_df_h2o$dl09 <- h2o.predict(h2o_qbs_season_dl_grid_09, training_df_h2o)$predict
training_df_h2o$dl10 <- h2o.predict(h2o_qbs_season_dl_grid_10, training_df_h2o)$predict




# Train and validate a grid of  
h2o_qbs_season_meta_grid <- 
  h2o.grid(
    "gbm",
    x = c(
      'glm01', 
      'glm02', 
      'glm03', 
      'glm04', 
      'glm05', 
      'glm06', 
      'glm07', 
      'glm08', 
      'glm09', 
      'glm10', 
      'drf01', 
      'drf02', 
      'drf03', 
      'drf04', 
      'drf05', 
      'drf06', 
      'drf07', 
      'drf08', 
      'drf09', 
      'drf10', 
      'gbm01', 
      'gbm02', 
      'gbm03', 
      'gbm04', 
      'gbm05', 
      'gbm06', 
      'gbm07', 
      'gbm08', 
      'gbm09', 
      'gbm10', 
      'dl01', 
      'dl02', 
      'dl03', 
      'dl04', 
      'dl05', 
      'dl06', 
      'dl07', 
      'dl08', 
      'dl09', 
      'dl10'), 
    y = y,
    grid_id = "h2o_qbs_season_meta_grid",
    training_frame = training_df_h2o,
    nfolds = grid_nfolds,
    fold_assignment = grid_fold_assignment,
    keep_cross_validation_predictions = TRUE,
    hyper_params = gbm_params,
    search_criteria = gbm_search_criteria, 
    stopping_rounds = grid_stopping_rounds, 
    stopping_metric = grid_stopping_metric, 
    stopping_tolerance = grid_stopping_tolerance) 

h2o_qbs_season_meta_grid_perf <- 
  h2o.getGrid(
    grid_id = "h2o_qbs_season_meta_grid", 
    sort_by = grid_sort_by, 
    decreasing = FALSE)

h2o_qbs_season_meta_grid_perf

h2o_qbs_season_meta_grid_01 <- h2o.getModel(h2o_qbs_season_meta_grid_perf@model_ids[[1]])
h2o_qbs_season_meta_grid_02 <- h2o.getModel(h2o_qbs_season_meta_grid_perf@model_ids[[2]])
h2o_qbs_season_meta_grid_03 <- h2o.getModel(h2o_qbs_season_meta_grid_perf@model_ids[[3]])
h2o_qbs_season_meta_grid_04 <- h2o.getModel(h2o_qbs_season_meta_grid_perf@model_ids[[4]])
h2o_qbs_season_meta_grid_05 <- h2o.getModel(h2o_qbs_season_meta_grid_perf@model_ids[[5]])
h2o_qbs_season_meta_grid_06 <- h2o.getModel(h2o_qbs_season_meta_grid_perf@model_ids[[6]])
h2o_qbs_season_meta_grid_07 <- h2o.getModel(h2o_qbs_season_meta_grid_perf@model_ids[[7]])
h2o_qbs_season_meta_grid_08 <- h2o.getModel(h2o_qbs_season_meta_grid_perf@model_ids[[8]])
h2o_qbs_season_meta_grid_09 <- h2o.getModel(h2o_qbs_season_meta_grid_perf@model_ids[[9]])
h2o_qbs_season_meta_grid_10 <- h2o.getModel(h2o_qbs_season_meta_grid_perf@model_ids[[10]])


# Get performance of best model 
h2o_qbs_season_meta_grid_01_var_imp <- as.data.frame(h2o.varimp(h2o_qbs_season_meta_grid_01))

# write_csv(h2o_qbs_season_meta_grid_01_var_imp, 'models/h2o_qbs_season_meta_grid_01_var_imp.csv')


# Save the best models
h2o.saveModel(object = h2o_qbs_season_meta_grid_01, path = '000_a_data_science_journey/models/', force = TRUE, export_cross_validation_predictions = TRUE, filename = 'h2o_qbs_season_meta_grid_01.h2o')
h2o.saveModel(object = h2o_qbs_season_meta_grid_02, path = '000_a_data_science_journey/models/', force = TRUE, export_cross_validation_predictions = TRUE, filename = 'h2o_qbs_season_meta_grid_02.h2o')
h2o.saveModel(object = h2o_qbs_season_meta_grid_03, path = '000_a_data_science_journey/models/', force = TRUE, export_cross_validation_predictions = TRUE, filename = 'h2o_qbs_season_meta_grid_03.h2o')
h2o.saveModel(object = h2o_qbs_season_meta_grid_04, path = '000_a_data_science_journey/models/', force = TRUE, export_cross_validation_predictions = TRUE, filename = 'h2o_qbs_season_meta_grid_04.h2o')
h2o.saveModel(object = h2o_qbs_season_meta_grid_05, path = '000_a_data_science_journey/models/', force = TRUE, export_cross_validation_predictions = TRUE, filename = 'h2o_qbs_season_meta_grid_05.h2o')
h2o.saveModel(object = h2o_qbs_season_meta_grid_06, path = '000_a_data_science_journey/models/', force = TRUE, export_cross_validation_predictions = TRUE, filename = 'h2o_qbs_season_meta_grid_06.h2o')
h2o.saveModel(object = h2o_qbs_season_meta_grid_07, path = '000_a_data_science_journey/models/', force = TRUE, export_cross_validation_predictions = TRUE, filename = 'h2o_qbs_season_meta_grid_07.h2o')
h2o.saveModel(object = h2o_qbs_season_meta_grid_08, path = '000_a_data_science_journey/models/', force = TRUE, export_cross_validation_predictions = TRUE, filename = 'h2o_qbs_season_meta_grid_08.h2o')
h2o.saveModel(object = h2o_qbs_season_meta_grid_09, path = '000_a_data_science_journey/models/', force = TRUE, export_cross_validation_predictions = TRUE, filename = 'h2o_qbs_season_meta_grid_09.h2o')
h2o.saveModel(object = h2o_qbs_season_meta_grid_10, path = '000_a_data_science_journey/models/', force = TRUE, export_cross_validation_predictions = TRUE, filename = 'h2o_qbs_season_meta_grid_10.h2o')


# Save the best models' details
h2o.saveModelDetails(object = h2o_qbs_season_meta_grid_01, path = '000_a_data_science_journey/models/', force = TRUE, filename = 'h2o_qbs_season_meta_grid_01_details.json')
h2o.saveModelDetails(object = h2o_qbs_season_meta_grid_02, path = '000_a_data_science_journey/models/', force = TRUE, filename = 'h2o_qbs_season_meta_grid_02_details.json')
h2o.saveModelDetails(object = h2o_qbs_season_meta_grid_03, path = '000_a_data_science_journey/models/', force = TRUE, filename = 'h2o_qbs_season_meta_grid_03_details.json')
h2o.saveModelDetails(object = h2o_qbs_season_meta_grid_04, path = '000_a_data_science_journey/models/', force = TRUE, filename = 'h2o_qbs_season_meta_grid_04_details.json')
h2o.saveModelDetails(object = h2o_qbs_season_meta_grid_05, path = '000_a_data_science_journey/models/', force = TRUE, filename = 'h2o_qbs_season_meta_grid_05_details.json')
h2o.saveModelDetails(object = h2o_qbs_season_meta_grid_06, path = '000_a_data_science_journey/models/', force = TRUE, filename = 'h2o_qbs_season_meta_grid_06_details.json')
h2o.saveModelDetails(object = h2o_qbs_season_meta_grid_07, path = '000_a_data_science_journey/models/', force = TRUE, filename = 'h2o_qbs_season_meta_grid_07_details.json')
h2o.saveModelDetails(object = h2o_qbs_season_meta_grid_08, path = '000_a_data_science_journey/models/', force = TRUE, filename = 'h2o_qbs_season_meta_grid_08_details.json')
h2o.saveModelDetails(object = h2o_qbs_season_meta_grid_09, path = '000_a_data_science_journey/models/', force = TRUE, filename = 'h2o_qbs_season_meta_grid_09_details.json')
h2o.saveModelDetails(object = h2o_qbs_season_meta_grid_10, path = '000_a_data_science_journey/models/', force = TRUE, filename = 'h2o_qbs_season_meta_grid_10_details.json')



training_df_h2o$meta01 <- h2o.predict(h2o_qbs_season_meta_grid_01, training_df_h2o)$predict




prediction_df_h2o$glm01 <- h2o.predict(h2o_qbs_season_glm_grid_01, prediction_df_h2o)$predict
prediction_df_h2o$glm02 <- h2o.predict(h2o_qbs_season_glm_grid_02, prediction_df_h2o)$predict
prediction_df_h2o$glm03 <- h2o.predict(h2o_qbs_season_glm_grid_03, prediction_df_h2o)$predict
prediction_df_h2o$glm04 <- h2o.predict(h2o_qbs_season_glm_grid_04, prediction_df_h2o)$predict
prediction_df_h2o$glm05 <- h2o.predict(h2o_qbs_season_glm_grid_05, prediction_df_h2o)$predict
prediction_df_h2o$glm06 <- h2o.predict(h2o_qbs_season_glm_grid_06, prediction_df_h2o)$predict
prediction_df_h2o$glm07 <- h2o.predict(h2o_qbs_season_glm_grid_07, prediction_df_h2o)$predict
prediction_df_h2o$glm08 <- h2o.predict(h2o_qbs_season_glm_grid_08, prediction_df_h2o)$predict
prediction_df_h2o$glm09 <- h2o.predict(h2o_qbs_season_glm_grid_09, prediction_df_h2o)$predict
prediction_df_h2o$glm10 <- h2o.predict(h2o_qbs_season_glm_grid_10, prediction_df_h2o)$predict

prediction_df_h2o$drf01 <- h2o.predict(h2o_qbs_season_drf_grid_01, prediction_df_h2o)$predict
prediction_df_h2o$drf02 <- h2o.predict(h2o_qbs_season_drf_grid_02, prediction_df_h2o)$predict
prediction_df_h2o$drf03 <- h2o.predict(h2o_qbs_season_drf_grid_03, prediction_df_h2o)$predict
prediction_df_h2o$drf04 <- h2o.predict(h2o_qbs_season_drf_grid_04, prediction_df_h2o)$predict
prediction_df_h2o$drf05 <- h2o.predict(h2o_qbs_season_drf_grid_05, prediction_df_h2o)$predict
prediction_df_h2o$drf06 <- h2o.predict(h2o_qbs_season_drf_grid_06, prediction_df_h2o)$predict
prediction_df_h2o$drf07 <- h2o.predict(h2o_qbs_season_drf_grid_07, prediction_df_h2o)$predict
prediction_df_h2o$drf08 <- h2o.predict(h2o_qbs_season_drf_grid_08, prediction_df_h2o)$predict
prediction_df_h2o$drf09 <- h2o.predict(h2o_qbs_season_drf_grid_09, prediction_df_h2o)$predict
prediction_df_h2o$drf10 <- h2o.predict(h2o_qbs_season_drf_grid_10, prediction_df_h2o)$predict

prediction_df_h2o$gbm01 <- h2o.predict(h2o_qbs_season_gbm_grid_01, prediction_df_h2o)$predict
prediction_df_h2o$gbm02 <- h2o.predict(h2o_qbs_season_gbm_grid_02, prediction_df_h2o)$predict
prediction_df_h2o$gbm03 <- h2o.predict(h2o_qbs_season_gbm_grid_03, prediction_df_h2o)$predict
prediction_df_h2o$gbm04 <- h2o.predict(h2o_qbs_season_gbm_grid_04, prediction_df_h2o)$predict
prediction_df_h2o$gbm05 <- h2o.predict(h2o_qbs_season_gbm_grid_05, prediction_df_h2o)$predict
prediction_df_h2o$gbm06 <- h2o.predict(h2o_qbs_season_gbm_grid_06, prediction_df_h2o)$predict
prediction_df_h2o$gbm07 <- h2o.predict(h2o_qbs_season_gbm_grid_07, prediction_df_h2o)$predict
prediction_df_h2o$gbm08 <- h2o.predict(h2o_qbs_season_gbm_grid_08, prediction_df_h2o)$predict
prediction_df_h2o$gbm09 <- h2o.predict(h2o_qbs_season_gbm_grid_09, prediction_df_h2o)$predict
prediction_df_h2o$gbm10 <- h2o.predict(h2o_qbs_season_gbm_grid_10, prediction_df_h2o)$predict

prediction_df_h2o$dl01 <- h2o.predict(h2o_qbs_season_dl_grid_01, prediction_df_h2o)$predict
prediction_df_h2o$dl02 <- h2o.predict(h2o_qbs_season_dl_grid_02, prediction_df_h2o)$predict
prediction_df_h2o$dl03 <- h2o.predict(h2o_qbs_season_dl_grid_03, prediction_df_h2o)$predict
prediction_df_h2o$dl04 <- h2o.predict(h2o_qbs_season_dl_grid_04, prediction_df_h2o)$predict
prediction_df_h2o$dl05 <- h2o.predict(h2o_qbs_season_dl_grid_05, prediction_df_h2o)$predict
prediction_df_h2o$dl06 <- h2o.predict(h2o_qbs_season_dl_grid_06, prediction_df_h2o)$predict
prediction_df_h2o$dl07 <- h2o.predict(h2o_qbs_season_dl_grid_07, prediction_df_h2o)$predict
prediction_df_h2o$dl08 <- h2o.predict(h2o_qbs_season_dl_grid_08, prediction_df_h2o)$predict
prediction_df_h2o$dl09 <- h2o.predict(h2o_qbs_season_dl_grid_09, prediction_df_h2o)$predict
prediction_df_h2o$dl10 <- h2o.predict(h2o_qbs_season_dl_grid_10, prediction_df_h2o)$predict


run_length <- toc()



prediction_df_h2o %>% 
  as.data.frame() %>% 
  select(
    season, 
    player_id, 
    player_name, 
    position, 
    team, 
    fantasy_points_actual = fantasy_points_dk_next, 
    fantasy_points_predicted_meta01 = meta01, 
    fantasy_points_predicted_gbm01 = gbm01) -> 
  test_df_predictions 


test_df_predictions %>% 
  ggplot(aes(x = fantasy_points_actual, y = fantasy_points_predicted_meta01)) + 
  geom_jitter() + 
  xlim(0,500) + 
  ylim(0,500) 


test_df_predictions %>% 
  ggplot(aes(x = fantasy_points_actual, y = fantasy_points_predicted_gbm01)) + 
  geom_jitter() + 
  xlim(0,500) + 
  ylim(0,500) 


ajksd <- h2o.performance(h2o_qbs_season_meta_grid_01, test_df_h2o)

ajksd <- h2o.performance(h2o_qbs_season_glm_grid_01, test_df_h2o)



# LIME 

# vip(h2o_qbs_season_drf_grid_01)
# h2o.partialPlot(h2o_qbs_season_drf_grid_01, data = training_df_h2o, cols = "Lender_Fee")
# 
# qbs_season_df <- as.data.frame(training_df_h2o)
# 
# h2o_qbs_season_drf_grid_01_explainer <- 
#   lime(
#     qbs_season_df, 
#     h2o_qbs_season_drf_grid_01, 
#     n_bins = 5)
# 
# h2o_qbs_season_drf_grid_01_explanation <- 
#   explain(
#     qbs_season_df, 
#     h2o_qbs_season_drf_grid_01_explainer, 
#     n_features = 5, 
#     labels = "Yes", 
#     kernel_width = .1, 
#     feature_select = "highest_weights")
# 
# plot_features(h2o_qbs_season_drf_grid_01_explanation, ncol = 1) + 
#   ggtitle("drf")


training_df_h2o <- 
  h2o.cbind(
    h2o.predict(h2o_qbs_season_glm_grid_01, training_df_h2o), 
    training_df_h2o)

# Train and validate a grid of  
h2o_qbs_season_lime_drf_grid <- 
  h2o.grid(
    "drf",
    x = x, 
    y = 'predict',
    grid_id = "h2o_qbs_season_lime_drf_grid",
    training_frame = training_df_h2o,
    nfolds = grid_nfolds,
    fold_assignment = grid_fold_assignment,
    keep_cross_validation_predictions = TRUE,
    hyper_params = drf_params,
    search_criteria = drf_search_criteria, 
    stopping_rounds = grid_stopping_rounds, 
    stopping_metric = grid_stopping_metric, 
    stopping_tolerance = grid_stopping_tolerance) 

h2o_qbs_season_lime_drf_grid_perf <- 
  h2o.getGrid(
    grid_id = "h2o_qbs_season_lime_drf_grid", 
    sort_by = grid_sort_by, 
    decreasing = TRUE)

h2o_qbs_season_lime_drf_grid_perf

h2o_qbs_season_lime_drf_grid_01 <- h2o.getModel(h2o_qbs_season_lime_drf_grid_perf@model_ids[[1]])


# # Shutdown the H2O cloud 
# h2o.shutdown(prompt = FALSE)









