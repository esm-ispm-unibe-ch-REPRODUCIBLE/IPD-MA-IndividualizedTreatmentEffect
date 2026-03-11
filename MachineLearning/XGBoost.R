# XGBoost
library(dplyr)
library(matrixStats)
library(xgboost)


params_df <- expand.grid(
  max_depth = 2:6,
  eta = c(0.1, 0.01, 0.001),
  nrounds = c(100, 500, 1000)
)

xgb_predict_ite <- function(){}

xgb_bootstrap_variance <- function(){}

xgb_get_variance_wrapper <- function()

# Training
xgb_train <- function(data, params_df){}

# Prediction
predict.xgb <- function(){}





  
    # Testing data: covariates and outcome
    X_test <- data_testing[[i]][[j]] %>% select(starts_with("x"))
    x_test_1 <- as.matrix(cbind(treatment = 1, X_test))
    x_test_0 <- as.matrix(cbind(treatment = 0, X_test))   
    xgb_test_1 <- xgb.DMatrix(data = x_test_1)
    xgb_test_0 <- xgb.DMatrix(data = x_test_0)
    
    pred_xgb <- matrix(nrow = 3e3, ncol = nstudies)
    
    xgb_res <- mclapply(1:nstudies, function(l){
      data_l <- data_training[[i]][[j]][[l]]
      x_train <- data_l %>% select(treatment, starts_with("x_")) %>% as.matrix()
      y_train <- data_l$y
      dtrain <- xgb.DMatrix(data  = x_train, label = y_train)
      
      # Cross-validation for hyperparameters selection
      results <- mclapply(seq_len(nrow(params_df)), function(n) {
        
        params <- list(objective = "reg:squarederror", eval_metric = "rmse",
                       max_depth = params_df$max_depth[n], eta = params_df$eta[n],
                       colsample_bytree = 1, min_child_weight = 1, subsample = 1)
        
        cv <- xgb.cv(params = params, data = dtrain, 
                     nrounds = params_df$nrounds[n], nfold = 10,
                     early_stopping_rounds = 20, verbose = 0
        )
        
        min(cv$evaluation_log$test_rmse_mean)
      }, mc.cores = 2)
      
      cv_results <- do.call(rbind, results)
      best_row <- params_df[which.min(cv_results), ]
      
      best_params <- list(objective = "reg:squarederror",
                          eval_metric = "rmse", max_depth = best_row$max_depth,
                          eta = best_row$eta, colsample_bytree = 1,
                          min_child_weight = 1, subsample = 1)
      
      # Training model
      final_model <- xgb.train(data = dtrain, params = best_params,
                               nrounds = best_row$nrounds)
      
      dtest1 <- xgb.DMatrix(x_test_1); dtest0 <- xgb.DMatrix(x_test_0)
      
      pred_xgb <- predict(final_model, dtest1) - predict(final_model, dtest0)
      
      bootstrap_preds <- matrix(NA, nrow = nrow(X_test), ncol = N_sample_bootstrap)
      
      
      for(m in 1:N_sample_bootstrap){
        bs_data <- get_bootstrap_sample(X = data_l)
        x_bs <- bs_data %>% select(treatment, starts_with("x_")) %>% as.matrix()
        y_bs <- bs_data$y
        xgboost.data <- xgb.DMatrix(data = x_bs, label = y_bs)
        xgb_bs <- xgboost(data = xgboost.data, params = best_params,
                          nrounds = best_row$nrounds, verbose = 0)
        bootstrap_preds[, m] <- predict(xgb_bs, newdata = xgb_test_1) -
          predict(xgb_bs, newdata = xgb_test_0)
      }
      
      list(pred_xgb = pred_xgb,
           bootstrap_var = rowVars(bootstrap_preds))
    }, mc.cores = n_cores)
    
    xgb_predictions <- do.call(cbind, lapply(xgb_res, `[[`, "pred_xgb"))
    bootstrap_variances <- do.call(cbind, lapply(xgb_res, `[[`, "bootstrap_var"))
    
    # bootstrap weighting
    weights_bs <- 1/bootstrap_variances
    prediction_bootstrap[, j] <- rowSums(xgb_predictions * weights_bs) / rowSums(weights_bs)
    xgb_bootstrap[j, ] <- cbind(j, assess_performances(ITE_actual = data_testing[[i]][[j]]$true_ITE_cont,
                                                       ITE_predicted = prediction_bootstrap[, j]))
    
    # linear model inverse variance weighting
    inv_var <- 1 / linear_model_variances[[i]][[j]]
    prediction_lm_variance[, j] <- rowSums(xgb_predictions * inv_var) / rowSums(inv_var)
    xgb_lm_variance[j, ] <- cbind(j, assess_performances(ITE_actual = data_testing[[i]][[j]]$true_ITE_cont,
                                                         ITE_predicted = prediction_lm_variance[, j]))

  
  



