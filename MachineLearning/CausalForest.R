# Causal Forest 
library(matrixStats)
library(grf)

cf_predict_ite <- function(model, newX){
	return(predict(model, newX)$predictions)
}

cf_bootstrap_variance <- function(data_train, # Original training data
                                  newX, # New covariates
                                  N_boot, # Numbers of bootstrap iteration
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

cf_get_variance_wrapper <- function(second_stage, 
  	                                newX,
  	                                N_boot = 100,
  	                                outcome_name,
  	                                treatment_name,
  	                                covariates_names){

      second_stage <- toupper(second_stage)
      model_formula <- paste0(outcome_name, " ~ ", treatment_name, " * (",
                              paste(covariates_names, collapse = "+"), ")")
      
      if(second_stage == "OLS+IV"){
        # ritorna una funzione che prende solo data_train
        return(function(data_train){
          get_linear_model_variance(
            data_train = data_train,
            newX = newX,
            model_formula = model_formula
          )
        })
        
      } else if(second_stage == "BS"){
        # ritorna una funzione che prende solo data_train
        return(function(data_train){
          cf_bootstrap_variance(
            data_train = data_train,
            N_boot = N_boot,
            newX = newX,
            outcome_name = outcome_name,
            treatment_name = treatment_name,
            covariates_names = covariates_names
          )
        })
        
      } else {
        stop("second_stage must be either 'OLS+IV' or 'BS'")
      }
     
      
    } 

cf_train_model <- function(data, covariate_names, 
                     	   outcome, treatment){
	X <- data[, covariate_names]
	y <- data[[outcome]]
	W <- data[[treatment]]

	return(causal_forest(X, y, W))    
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
					  models = cf_models, 
					  covariate_names = covariate_names, 
 	   				  outcome = outcome, 
 	   				  treatment = treatment)
	
}

# Prediction
predict.cf <- function(obj.first.stage, 
					            newX,
                       second_stage = "BS",
                       N_boot = 100){  

    predictions <- matrix(nrow = nrow(newX), ncol = obj.first.stage$nstudies)
  	variance <- matrix(nrow = nrow(newX), ncol = obj.first.stage$nstudies) 

    variance_method <- cf_get_variance_wrapper(
    								second_stage = second_stage, 
  	                                newX = newX,
  	                                N_boot = N_boot,
	                                outcome_name = obj.first.stage$outcome,
	                                treatment_name = obj.first.stage$treatment,
	                                covariates_names = obj.first.stage$covariate_names
      
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
  



