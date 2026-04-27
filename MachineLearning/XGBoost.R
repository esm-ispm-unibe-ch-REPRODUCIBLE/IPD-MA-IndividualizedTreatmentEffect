# XGBoost
library(matrixStats)
library(xgboost)

xgb_predict_ite <- function(model, x1_test, x0_test){
	  xgb_test_1 <- xgb.DMatrix(data = x1_test)
    xgb_test_0 <- xgb.DMatrix(data = x0_test)
	return(predict(model, newdata = xgb_test_1) -
	          predict(model, newdata = xgb_test_0))
}

xgb_bootstrap_variance <- function(data_train, # Original training data
                                   N_boot = 100, # Numbers of bootstrap iteration
                                   x1_test,
                                   x0_test,
                                   parameters_list,
                                   outcome_name,
                                   treatment_name,
                                   covariates_names){

  bootstrap_preds <- matrix(nrow = nrow(x1_test), ncol = N_boot)

  for(m in 1:N_boot){
        bs_data <- get_bootstrap_sample(X = data_train)
        x_bs <- as.matrix(bs_data[, c(treatment_name, covariates_names)])
        y_bs <- bs_data[[outcome_name]]
        xgboost.data <- xgb.DMatrix(data = x_bs, label = y_bs)
        xgb_bs <- xgb.train(data = xgboost.data, params = parameters_list$best_params,
                          nrounds = parameters_list$best_nrounds, verbose = 0)
        dm_1 <- xgb.DMatrix(data  = x1_test)
        dm_0 <- xgb.DMatrix(data  = x0_test)
        bootstrap_preds[, m] <- predict(xgb_bs, newdata = dm_1) -
          predict(xgb_bs, newdata = dm_0)
      }

  matrixStats::rowVars(bootstrap_preds)
}

xgb_get_variance_wrapper <- function(second_stage, 
  	                                 newX,
  	                                 N_boot = 100,
  	                                 x1_test,
  	                                 x0_test, 
  	                                 parameters,
                                     outcome_name,
                                     treatment_name,
                                     covariates_names){

      second_stage <- toupper(second_stage)
      model_formula <- as.formula(paste0(outcome_name, " ~ ", treatment_name, " * (",
                              paste(covariates_names, collapse = "+"), ")"))
      
      if(second_stage == "OLS+IV"){
        # returns a function that needs only data_train
        return(function(data_train){
          get_linear_model_variance(
            data_train = data_train,
            newX = newX,
            model_formula = model_formula
          )
        })
        
      } else if(second_stage == "BS"){
        # returns a function that needs only data_train
        return(function(data_train){
          xgb_bootstrap_variance(
            data_train = data_train,
            parameters_list = parameters,
            N_boot = N_boot,
            x1_test = x1_test,
            x0_test = x0_test,
            outcome_name = outcome_name,
            treatment_name = treatment_name,
            covariates_names = covariates_names
          )
        })
        
      } else {
        stop("second_stage must be either 'OLS+IV' or 'BS'")
      }
    } 


xgb_cross_validation <- function(xgb.data, parameters){
	performances <- numeric(length = nrow(parameters))

	# devo aggiugnere un check sui parametri
	for(i in 1:nrow(parameters)){
		params <- list(objective = "reg:squarederror", 
				   eval_metric = "rmse",
                   max_depth = parameters$max_depth[i], 
                   eta = parameters$eta[i],
                   colsample_bytree = 1, 
                   min_child_weight = 1, 
                   subsample = 1)

		cv <- xgb.cv(params = params, data = xgb.data, 
                 nrounds = parameters$nrounds[i], nfold = 10,
                     early_stopping_rounds = 20, verbose = 0
        )
    performances[[i]] <- min(cv$evaluation_log$test_rmse_mean)
	}

	best_row <- parameters[which.min(performances), ]

	to_return <- list(best_nrounds = best_row$nrounds,
		best_params = list(objective = "reg:squarederror",
                        eval_metric = "rmse", max_depth = best_row$max_depth,
                        eta = best_row$eta, colsample_bytree = 1,
                        min_child_weight = 1, subsample = 1))
      
    return(to_return)
}

# Training
xgb_train <- function(data, params_df, covariate_names, 
                        outcome = "y",
                        treatment = "treatment"){
	nstudies <- length(data)
	ncovariates <- length(covariate_names)

	xgb.models <- vector("list", length = nstudies)
	xgb.parameters <- vector("list", length = nstudies)

	for(l in 1:nstudies){
		data_lth_study <- data[[l]]
		x_train <- as.matrix(data[[l]][, c(treatment, covariate_names)])
		y_train <- data[[l]][[outcome]]
		dtrain <- xgb.DMatrix(data  = x_train, label = y_train)

		# Cross-Validation for hyperparameters tuning 
		xgb.parameters[[l]] <- xgb_cross_validation(xgb.data = dtrain, parameters = params_df)

		# Training model
    xgb.models[[l]] <- xgb.train(data = dtrain, params = xgb.parameters[[l]]$best_params,
                               nrounds = xgb.parameters[[l]]$best_nrounds)
	}

	return(list(data = data, 
    final_models = xgb.models,
		final_parameters = xgb.parameters,
    outcome_name = outcome,
    treatment_name = treatment,
    covariates_names = covariate_names))
}

# Prediction
predict.xgb <- function(newX, obj_xgb_train, second_stage, N_boot = 100){
    nstudies <- length(obj_xgb_train$data)
	  x1_test <- as.matrix(cbind(treatment = 1, newX))
    x0_test <- as.matrix(cbind(treatment = 0, newX))  

    predictions <- matrix(nrow = nrow(newX), ncol = nstudies)
  	variance <- matrix(nrow = nrow(newX), ncol = nstudies) 


    variance_method <- xgb_get_variance_wrapper(
      second_stage = second_stage,
      newX = newX,
      parameters = obj_xgb_train$final_parameters[[l]],
      N_boot = N_boot,
      x1_test = x1_test,
      x0_test = x0_test,
      outcome_name = obj_xgb_train$outcome_name,
      treatment_name = obj_xgb_train$treatment_name,
      covariates_names = obj_xgb_train$covariates_names      
    )
  
  for(l in seq_len(nstudies)){
    predictions[, l] <- xgb_predict_ite(model = obj_xgb_train$final_models[[l]], 
                                        x1_test = x1_test, x0_test = x0_test)
    variance[, l] <- variance_method(data_train = obj_xgb_train$data[[l]])
  }

  weights <- 1 / variance
  pooled_predictions <- rowSums(predictions * weights) / rowSums(weights)

  return(pooled_predictions)
}      
  



