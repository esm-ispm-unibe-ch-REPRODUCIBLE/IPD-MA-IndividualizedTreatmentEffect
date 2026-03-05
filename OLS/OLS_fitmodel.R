# Ordinary Least Square Regression - Inverse Variance Multivariate meta-analysis
# 1. Linear Model + IV: meta-analyse coefficients
n_cores <- 3
library(parallel)
n.parameters_to_meta_analyze <- n_covariates + 1
# Linear models
linear_model_performances_fixed_effects <- mclapply(1:nscenarios, function(i){
  # We want to create an array that contains for each dataset the parameters
  # estimated in each study
  # These regression parameters will be then used to make the final predictions
  
  # How many parameters do we need to calculate ITE? 
  # Number of effect modifiers (gamma) + treatment (delta)
  
  # Matrix to save the performances from every simulation
  performance_matrix <- matrix(ncol = 5, nrow = nsim)
  predictions <- matrix(ncol = nsim, nrow = 3e3)

  for(j in 1:nsim){
    # Covariates of testing dataset
    test_dataset <- data_testing[[i]][[j]] %>% select(starts_with("x_")) %>% 
      as.matrix()
    # True outcome to compare with the estimate
    true_test_ITE <- data_testing[[i]][[j]]$true_ITE_cont
    # Let's create the matrix that will contain the regression parameters
    # to meta-analyze for each study
    coefficients_to_meta_analyze <- matrix(nrow = nstudies, 
                                           ncol = n.parameters_to_meta_analyze)
    
    # Let's create a list that will contain the variance/covariance matrix of the coefficients
    vcov_parameters_to_meta_analyze <- list()
    
    for(l in 1:nstudies){
      # Fit linear model
      mod <- lm(model_formula,  data = data_training[[i]][[j]][[l]])
      
      # Save the punctual estimates
      coefficients_to_meta_analyze[l, ] <- mod$coefficients[startsWith(names(mod$coefficients), 
                                                                       "treatment")]
      
      # Save the variance/cov matrix
      vcov_parameters_to_meta_analyze[[l]] <- vcov(mod)[startsWith(rownames(vcov(mod)), "treatment"), 
                                                        startsWith(colnames(vcov(mod)), "treatment")]
    }
    
    # Meta-analysis of the parameters: 
    # It's a multivariate meta-analysis
    # Perform meta-analysis and save pooled summary measures
    # meta_analysis.method <- ifelse(nstudies > 3, "reml", "fixed")
    meta_obj <- mvmeta::mvmeta(coefficients_to_meta_analyze,
                               vcov_parameters_to_meta_analyze, 
                               method = "fixed") 
    parameters_meta_analyzed <- coef(meta_obj)
    
    # Predict ITE on new data
    predictions[, j] <- cbind(1, test_dataset) %*% parameters_meta_analyzed
    
    # Assess performances
    performance_matrix[j, ] <- cbind(j, assess_performances(ITE_actual = true_test_ITE,
                                                            ITE_predicted = predictions[, j]))
    

  }
  
 list(performance_matrix, predictions)
}, mc.cores = n_cores)
 

