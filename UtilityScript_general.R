# Utility script

# Bootstrap


get_bootstrap_sample <- function(X){
  n <- nrow(X)
  index <- sample(1:n, size = n, replace = T)
  return(X[index, ])
}

get_linear_model_variance <- function(data_train,
                                      newX,
                                      model_formula) {
  
  # Model fit
  ausiliar_model <- lm(model_formula, data = data_train)
  
  # Cov matrixes
  Vbeta <- vcov(ausiliar_model)
  
  # New patients data; under treatment and control
  data_1 <- cbind(newX, treatment = 1)
  data_0 <- cbind(newX, treatment = 0)
  
  x1_test <- model.matrix(model_formula, data = data_1)[, -1]
  x0_test <- model.matrix(model_formula, data = data_0)[, -1]
  
  diff_x <- x1_test - x0_test
  
  # Linear Variances
  variances <- rowSums((diff_x %*% Vbeta) * diff_x)
  
  return(as.numeric(variances))
}
