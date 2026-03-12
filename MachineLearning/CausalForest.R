# Causal Forest 
library(matrixStats)
library(grf)

cf_predict_ite <- function(model, newX){
	return(predict(model, newX)$predictions)
}

cf_bootstrap_variance <- function(data_train, # Original training data
                                  newX, # New covariates
                                  N_boot = 100, # Numbers of bootstrap iteration
                                  outcome_name,
                                  treatment_name,
                                  covariates_names){
	bootstrap_preds <- matrix(nrow = nrow(newX),
	 						  ncol = N_boot)
      for(m in 1:N_boot){
        bs_data <- get_bootstrap_sample(X = data_train)
        X_bs <- bs_data[, covariates_names]
        Y_bs <- bs_data[[outcome_name]]
        W_bs <- bs_data[[treatment_name]]
        cf_bs <- causal_forest(X_bs, Y_bs, W_bs)
        bootstrap_preds[, m] <- predict(cf_bs, newX)$predictions
      }

     return(rowVars(bootstrap_preds))
}

cf_get_variance_wrapper <- function(){

      
    } 

cf_train_model <- function(data, covariate_names, 
                     	   outcome, treatment){
	X <- data[, covariate_names]
	y <- data[[outcome]]
	W <- data[[treatment]]

	return(causal_forest(X, Y, W))    
}

# Training
cf_train <- function(data, covariate_names, 
                     outcome = "y",
                     treatment = "treatment"){

	nstudies <- length(data)
	cf_models <- vector("list", length = nstudies)

	for(l in 1:nstudies){
		cf_models[[l]] <- cf_train_model(data = data[[l]], 
										 covariate_names = covariate_names, 
                     	   				 outcome = outcome, 
                     	   				 treatment = treatment)


	}

	to_return <- list(data = data,
					  nstudies = nstudies,
					  models = cf_models)
	
}

# Prediction
predict.cf <- function(obj.first.stage, newX){  

    predictions <- matrix(nrow = nrow(newX), ncol = obj.first.stage$nstudies)
  	variance <- matrix(nrow = nrow(newX), ncol = obj.first.stage$nstudies) 

    variance_method <- cf_get_variance_wrapper(
      
    )
  
  for(l in seq_len(obj.first.stage$nstudies)){
    predictions[, l] <- cf_predict_ite(model = obj.first.stage$models[[l]], 
    									newX = newX)
    variance[, l] <- variance_method(obj.first.stage$data[[l]])
  }

  weights <- 1 / variance
  pooled_predictions <- rowSums(predictions * weights) / rowSums(weights)

  return(pooled_predictions)
}      
  



