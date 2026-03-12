# Causal Forest 
library(matrixStats)
library(grf)

cf_predict_ite <- function(model, x1_test, x0_test){
}

cf_bootstrap_variance <- function(){
}

cf_get_variance_wrapper <- function(){

      
    } 

# Training
cf_train <- function(data, params_df, covariate_names, 
                        outcome = "y",
                        treatment = "treatment"){
	
}

# Prediction
predict.cf <- function(){
	x1_test <- as.matrix(cbind(treatment = 1, newX))
    x0_test <- as.matrix(cbind(treatment = 0, newX))  

    predictions <- matrix(nrow = nrow(newX), ncol = object$nstudies)
  	variance <- matrix(nrow = nrow(newX), ncol = object$nstudies) 


    variance_method <- cf_get_variance_wrapper(
      second_stage = second_stage,
      newX = newX,
      parameters = obj_cf_train$final_parameters,
      N_boot = N_boot,
      x1_test = x1_test,
      x0_test = x0_test,
      outcome_name = obj_cf_train$outcome,
      treatment_name = obj_cf_train$treatment,
      covariates_names = obj_cf_train$covariate_names      
    )
  
  for(l in seq_len(object$nstudies)){
    predictions[, l] <- cf_predict_ite(model = obj_cf_train$final_models[[l]], x1_test = x1_test, x0_test = x0_test)
    variance[, l] <- variance_method(object$data[[l]])
  }

  weights <- 1 / variance
  pooled_predictions <- rowSums(predictions * weights) / rowSums(weights)

  return(pooled_predictions)
}      
  



