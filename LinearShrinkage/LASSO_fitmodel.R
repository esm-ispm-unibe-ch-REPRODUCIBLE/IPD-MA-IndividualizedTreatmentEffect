# LASSO 
library(glmnet)
library(matrixStats)

lasso_predict_ite <- function(model, newX){
  x1_test <- cbind(1, newX, newX) 
  x0_test <- cbind(0, newX, 0 * newX) 

  ite <- as.numeric(predict(cv.obj, newx = x1_test, s = "lambda.min") -
                               predict(cv.obj, newx = x0_test, s = "lambda.min"))

  return(ite)
}

lasso_bootstrap_variance <- function(data_train, newX, model_formula,
                               x1_test, x0_test,
                               penalty_factors, lambda_seq,
                               N_boot = 100){

  bootstrap_preds <- matrix(nrow = nrow(newX), ncol = N_boot)

  for(m in 1:N_boot){

      bs_data <- get_bootstrap_sample(X = data_train)

      x_train_bs <- model.matrix(model_formula, data = bs_data)[, -1]

      cv_bs <- cv.glmnet(
          x_train_bs,
          y = bs_data$y,
          nfolds = 5,
          alpha = 1,
          penalty.factor = penalty_factors,
          lambda = lambda_seq
      )

      bootstrap_preds[, m] <- as.numeric(
          predict(cv_bs, newx = x1_test, s = "lambda.min") -
          predict(cv_bs, newx = x0_test, s = "lambda.min")
      )
  }

  matrixStats::rowVars(bootstrap_preds)
}

lasso_build_design_matrix <- function(X, mod.formula){
  return(model.matrix(mod.formula, data = X)[, -1])
}

lasso_ipd_ite <- function(data, newX, covariate_names, second_stage, N_sample_bootstrap = 100,
  covariate_names){
    # y = list of outcomes
    # X = list of datasets with covariates
    # t = list of treatment

    n_covariates <- ncol(data) - 2
    nstudies <- length(data)

    lambda_seq <- 10 ^ seq(2, -3, by = -0.3)
    penalty_factors <- c(rep(0, 1 + n_covariates), rep(1, n_covariates))

    formula_str <- paste("y ~ treatment * (", paste(covariate_names, collapse = " + "), ")", 
                         sep = "")
    model_formula <- as.formula(formula_str)

    predictions <- variance <- matrix(nrow = nrow(newX), ncol = nstudies)
    
    second_stage <- toupper(second_stage)
    if(second_stage == "OLS+IV"){
      variance_method <- get_linear_model_variance
    } else (second_stage == "BS"){
      variance_method <- bootstrap_variance
    } 

    for(l in 1:nstudies){
     # fit cv.glmnet su dataset originale
      cv.obj <- LASSO_fit_model(data = data[[l]], model_formula = model_formula, 
                                lambda_seq = lambda_seq, penalty_factors = penalty_factors,
                                cov_names = covariate_names)
      
      # predictions per ITE
      predictions[, l] <- lasso_predict_ite(model = cv.obj, newX = newX)

      variance[, l] <- variance_method(
      data_train = data_lth_study,
      newX = newX,
      model_formula = model_formula,
      x1_test = x1_test,
      x0_test = x0_test,
      penalty_factors = penalty_factors,
      lambda_seq = lambda_seq
  )

      
    }

    weights <- 1 / variance
    pooled_predictions <- rowSums(predictions * weights) / rowSums(weights)

    return(pooled_predictions)
}

LASSO_fit_model <- function(data, model_formula, lambda_seq, 
                            penalty_factors, cov_names){

      x_train <- lasso_build_design_matrix(X = data[, cov_names], mod.formula = model_formula)
      
      # fit cv.glmnet su dataset originale
      cv.obj <- cv.glmnet(x_train, y = y[[l]], nfolds = 10, alpha = 1,
                            penalty.factor = penalty_factors, lambda = lambda_seq)

      return(cv.obj)
    
}
