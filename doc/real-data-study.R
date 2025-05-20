## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(knitr)
library(FOCI)
library(KPC)
devtools::install()
library(FORD)
library(ggplot2)
library(randomForest)
library(kernlab)
library(dplyr)
library(stringr)
library(purrr)
#install.packages("VIM")
library(VIM)
#install.packages("mice")
library(mice)

## ----load Supperconductivity code, eval=FALSE---------------------------------
# # Libraries ----
# library(FOCI)
# library(KPC)
# library(randomForest)
# library(kernlab)
# library(ford)
# 
# # Data Preprocessing ----
# 
# ## Load training data ----
# filepath_train <- system.file("extdata", "supperconductivity", "suppercoductivity_data.csv", package = "ford")
# suppercoductivity_data <- read.csv(filepath_train, header = TRUE)
# 
# # Train and Test Split ----
# set.seed(7)
# n <- nrow(suppercoductivity_data)
# train_indices <- sample(seq_len(n), size = round(0.7 * n))
# 
# train_suppercoductivity <- suppercoductivity_data[train_indices, ]
# test_suppercoductivity <- suppercoductivity_data[-train_indices, ]
# 
# Y_train <- train_suppercoductivity[[82]]
# X_train <- train_suppercoductivity[, -82]
# 
# Y_test <- test_suppercoductivity[[82]]
# X_test <- test_suppercoductivity[, -82]
# 
# # Methods to apply
# methods <- list(
#   foci = function(Y, X) foci(Y, X, numCores = parallel::detectCores()),
#   ford = function(Y, X) ford(Y, X, numCores = parallel::detectCores()),
#   kfoci = function(Y, X) KFOCI(Y, X, Knn = 1, numCores = parallel::detectCores())
# )
# 
# # Store results
# supperconductivity_results <- list()
# 
# # Pipeline for Feature Selection Methods ----
# for (method_name in names(methods)) {
# 
#   # Feature selection
#   selector <- methods[[method_name]](Y_train, X_train)
#   if (method_name == "kfoci") {
#     selected_idx <- selector
#   } else {
#     selected_idx <- selector$selectedVar$index
#   }
#   selected_vars <- names(X_train)[selected_idx]
# 
#   # Train Random Forest
#   train_selected <- data.frame(Y = Y_train, X_train[, selected_idx, drop = FALSE])
#   set.seed(7)
#   model <- randomForest(Y ~ ., data = train_selected)
# 
#   # Test prediction
#   test_selected <- X_test[, selected_idx, drop = FALSE]
#   preds <- predict(model, newdata = test_selected)
# 
#   # Evaluation
#   mse <- mean((preds - Y_test)^2)
# 
#   # Record the results
#   supperconductivity_results[[method_name]] <- list(
#     selected_variables = selected_vars,
#     MSE = mse
#   )
# 
#   #cat("Done:", method_name, "MSE =", mse, "\n")
# }
# 
# # Random Forest benchmark (no feature selection) ----
# set.seed(7)
# rf_model_full <- randomForest(Y_train ~ ., data = data.frame(Y_train = Y_train, X_train))
# 
# # Predictions and MSE
# rf_preds_full <- predict(rf_model_full, newdata = X_test)
# rf_mse_full <- mean((rf_preds_full - Y_test)^2)
# 
# # Feature importance ranking
# importance_scores <- importance(rf_model_full)
# ordered_vars <- names(sort(importance_scores[, 1], decreasing = TRUE))  # IncMSE column
# 
# # Record benchmark result
# supperconductivity_results[["random_forest_full"]] <- list(
#   selected_variables = ordered_vars,
#   MSE = rf_mse_full
# )
# 
# #cat("Done: Random Forest Full Model, MSE =", rf_mse_full, "\n")
# 
# # Final Results ----
# #save(supperconductivity_results, file = "/results/supperconductivity_results.RData")

## ----load Wave Energy Converter code, eval=FALSE------------------------------
# # Libraries ----
# library(FOCI)
# library(KPC)
# library(randomForest)
# library(kernlab)
# library(ford)
# 
# # Data Preprocessing ----
# 
# ## Load data ----
# city_names <- c("Adelaide", "Perth", "Sydney", "Tasmania")
# wec_datasets <- list()
# 
# for (city in city_names) {
#   filepath <- system.file("extdata", "wave_energy_converters", paste0(city, "_Data.csv"), package = "ford")
#   data <- read.csv(filepath, header = FALSE)
#   wec_datasets[[city]] <- list(train = NULL, test = NULL, data = data)
# }
# 
# # Split datasets into train/test
# set.seed(7)
# for (name in names(wec_datasets)) {
#   n <- nrow(wec_datasets[[name]]$data)
#   train_indices <- sample(seq_len(n), size = round(0.7 * n))
#   wec_datasets[[name]]$train <- wec_datasets[[name]]$data[train_indices, ]
#   wec_datasets[[name]]$test <- wec_datasets[[name]]$data[-train_indices, ]
# }
# 
# # Methods to apply
# methods <- list(
#   foci = function(Y, X) foci(Y, X, numCores = parallel::detectCores()),
#   ford = function(Y, X) ford(Y, X, numCores = parallel::detectCores()),
#   kfoci = function(Y, X) KFOCI(Y, X, Knn = 1, numCores = parallel::detectCores())
# )
# 
# # Store results
# wec_results <- list()
# 
# # Pipeline ----
# for (dataset_name in names(wec_datasets)) {
#   dataset <- wec_datasets[[dataset_name]]
#   train_data <- dataset$train
#   test_data <- dataset$test
# 
#   Y_train <- train_data[[49]]
#   X_train <- train_data[, -49]
# 
#   Y_test <- test_data[[49]]
#   X_test <- test_data[, -49]
# 
#   for (method_name in names(methods)) {
# 
#     # Feature selection
#     selector <- methods[[method_name]](Y_train, X_train)
#     if (method_name == "kfoci") {
#       selected_idx <- selector
#     } else {
#       selected_idx <- selector$selectedVar$index
#     }
#     selected_vars <- names(X_train)[selected_idx]
# 
#     # Train Random Forest
#     train_selected <- data.frame(Y = Y_train, X_train[, selected_idx, drop = FALSE])
#     set.seed(7)
#     model <- randomForest(Y ~ ., data = train_selected)
# 
#     # Test prediction
#     test_selected <- X_test[, selected_idx, drop = FALSE]
#     preds <- predict(model, newdata = test_selected)
# 
#     # Evaluation
#     mse <- mean((preds - Y_test)^2)
# 
#     # Record the results
#     wec_results[[paste(dataset_name, method_name, sep = "_")]] <- list(
#       selected_variables = selected_vars,
#       MSE = mse
#     )
# 
#     #cat("Done:", dataset_name, method_name, "MSE =", mse, "\n")
#   }
# 
#   # Random Forest benchmark (no feature selection) ----
#   set.seed(7)
#   rf_model_full <- randomForest(Y_train ~ ., data = data.frame(Y_train = Y_train, X_train))
# 
#   rf_preds_full <- predict(rf_model_full, newdata = X_test)
#   rf_mse_full <- mean((rf_preds_full - Y_test)^2)
# 
#   # Feature importance
#   importance_scores <- importance(rf_model_full)
#   ordered_vars <- names(sort(importance_scores[, 1], decreasing = TRUE))  # IncMSE
# 
#   wec_results[[paste(dataset_name, "random_forest_full", sep = "_")]] <- list(
#     selected_variables = ordered_vars,
#     MSE = rf_mse_full
#   )
# 
#   #cat("Done:", dataset_name, "Random Forest Full Model, MSE =", rf_mse_full, "\n")
# }
# 
# # Final Results ----
# #save(wec_results, file = "/results/wec_results.RData")
# 

## ----load Lattice Physics result, eval=FALSE----------------------------------
# # Libraries ----
# library(FOCI)
# library(KPC)
# library(randomForest)
# library(kernlab)
# library(ford)
# 
# # Data Preprocessing ----
# 
# ## Load training data ----
# filepath_train <- system.file("extdata", "lattice_physics", "lattice_physics_train.csv", package = "ford")
# lattice_physics_train <- read.table(filepath_train, header = FALSE)
# colnames(lattice_physics_train) <- c("k-inf", "PPPF", paste0("C", 1:39))
# X_train <- lattice_physics_train[, 3:41]
# 
# ## Load test data ----
# filepath_test <- system.file("extdata", "lattice_physics", "lattice_physics_test.csv", package = "ford")
# lattice_physics_test <- read.table(filepath_test, header = FALSE)
# colnames(lattice_physics_test) <- c("k-inf", "PPPF", paste0("C", 1:39))
# X_test <- lattice_physics_test[, 3:41]
# 
# # Responses
# responses <- c("k-inf", "PPPF")
# 
# # Methods to apply
# methods <- list(
#   foci = function(Y, X) foci(Y, X, numCores = 1),
#   ford = function(Y, X) ford(Y, X, numCores = 1),
#   kfoci = function(Y, X) KFOCI(Y, X, Knn = 1, numCores = 1)
# )
# 
# # Store results
# lattice_physics_results <- list()
# 
# # Pipeline ----
# for (resp in responses) {
#   Y_train <- lattice_physics_train[[resp]]
#   Y_test <- lattice_physics_test[[resp]]
# 
#   for (method_name in names(methods)) {
# 
#     # Feature selection
#     selector <- methods[[method_name]](Y_train, X_train)
#     if (method_name == "kfoci") {
#       selected_idx <- selector
#     } else {
#       selected_idx <- selector$selectedVar$index
#     }
#     selected_vars <- names(X_train)[selected_idx]
# 
#     # Train Random Forest
#     train_data <- data.frame(Y = Y_train, X_train[, selected_idx, drop = FALSE])
#     set.seed(1)
#     model <- randomForest(Y ~ ., data = train_data)
# 
#     # Test prediction
#     test_data <- X_test[, selected_idx, drop = FALSE]
#     preds <- predict(model, newdata = test_data)
# 
#     # Evaluation
#     mse <- mean((preds - Y_test)^2)
# 
#     # Record the results
#     lattice_physics_results[[paste(resp, method_name, sep = "_")]] <- list(
#       selected_variables = selected_vars,
#       MSE = mse
#     )
# 
#     #cat("Done:", resp, method_name, "MSE =", mse, "\n")
#   }
# 
#   # Random Forest benchmark (no feature selection) ----
#   set.seed(1)
#   rf_model_full <- randomForest(Y_train ~ ., data = data.frame(Y_train = Y_train, X_train))
# 
#   # Predictions and MSE
#   rf_preds_full <- predict(rf_model_full, newdata = X_test)
#   rf_mse_full <- mean((rf_preds_full - Y_test)^2)
# 
#   # Feature importance ranking
#   importance_scores <- importance(rf_model_full)
#   ordered_vars <- names(sort(importance_scores[, 1], decreasing = TRUE))  # IncMSE column
# 
#   # Record benchmark result
#   lattice_physics_results[[paste(resp, "random_forest_full", sep = "_")]] <- list(
#     selected_variables = ordered_vars,
#     MSE = rf_mse_full
#   )
# 
#   #cat("Done:", resp, "Random Forest Full Model, MSE =", rf_mse_full, "\n")
# }
# 
# # Final Results ----
# #save(lattice_physics_results, file = "/results/lattice_physics_results.RData")

## ----unified-results-table, message=FALSE, warning=FALSE----------------------

# Helper to safely load and extract method info
extract_results <- function(res, foci_key, kfoci_key, ford_key, rf_key) {
  methods <- c(foci_key, kfoci_key, ford_key)
  subset_sizes <- c(length(res[[foci_key]]$selected_variables),
                    length(res[[kfoci_key]]$selected_variables),
                    length(res[[ford_key]]$selected_variables),
                    length(res[[rf_key]]$selected_variables))
  mspe <- c(res[[foci_key]]$MSE,
            res[[kfoci_key]]$MSE,
            res[[ford_key]]$MSE,
            res[[rf_key]]$MSE)
  list(subset_sizes = subset_sizes, mspe = mspe)
}

# Load Superconductivity results
result_path_sc <- system.file("extdata","supperconductivity_results.RData", package = "FORD")
load(result_path_sc)
sc_res <- extract_results(supperconductivity_results, "foci", "kfoci", "ford","random_forest_full")

# Load WEC results
result_path_sc <- system.file("extdata","wec_results.RData", package = "FORD")
load(result_path_sc)
wec_res <- extract_results(wec_results, "Tasmania_foci", "Tasmania_kfoci", "Tasmania_ford","Tasmania_random_forest_full")

# Load Lattice Physics results
result_path_sc <- system.file("extdata","lattice_physics_results.RData", package = "FORD")
load(result_path_sc)
lp_res <- extract_results(lattice_physics_results, "k-inf_foci", "k-inf_kfoci", "k-inf_ford","k-inf_random_forest_full")

# Build full table
methods <- c("FOCI", "KFOCI", "FORD")
final_df_1 <- data.frame(
  Method = methods,
  SC_Subset = sc_res$subset_sizes[1:3],
  SC_MSPE = formatC(sc_res$mspe[1:3], format = "e", digits = 2),
  WEC_Subset = wec_res$subset_sizes[1:3],
  WEC_MSPE = formatC(wec_res$mspe[1:3], format = "e", digits = 2),
  LP_Subset = lp_res$subset_sizes[1:3],
  LP_MSPE = formatC(lp_res$mspe[1:3], format = "e", digits = 2)
)

# Display
kable(final_df_1, caption = "Applications of FOCI, KFOCI, and FORD to Real Datasets", digits = 2)

# Build full table
datasets_names <- c("SC", "WEC", "LP")

ford_subset_sizes <- c(sc_res$subset_sizes[3], wec_res$subset_sizes[3], lp_res$subset_sizes[3])
rf_subset_sizes <- c(sc_res$subset_sizes[4], wec_res$subset_sizes[4], lp_res$subset_sizes[4])

ford_mspe <- c(formatC(sc_res$mspe[3], format = "e", digits = 2),
               formatC(wec_res$mspe[3], format = "e", digits = 2),
               formatC(lp_res$mspe[3], format = "e", digits = 2))

rf_mspe <- c(formatC(sc_res$mspe[4], format = "e", digits = 2),
               formatC(wec_res$mspe[4], format = "e", digits = 2),
               formatC(lp_res$mspe[4], format = "e", digits = 2))

final_df_2 <- data.frame(
  Data = datasets_names,
  Subset_ratio = paste0(ford_subset_sizes,"/",rf_subset_sizes),
  FORD_MSPE = ford_mspe,
  RF_MSPE = rf_mspe
)

# Display
kable(final_df_2, caption = "Applications of FOCI, KFOCI, and FORD to Real Datasets", digits = 2)

