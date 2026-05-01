# Bayesian linear model
library(rjags)
covariate_names <- paste0("x_", 1:8)
Bayes_linear_model <- Bayesian_unpenalized_linear_model(data = sample_train.df, 
                                                        covariate_names = covariate_names, 
                                                        outcome = "y",
                                                        treatment = "treatment")

predictions.MVMA <- predict.Bayesian_linear_regression(obj.Bayesian_UP = Bayes_linear_model, 
                                                       newX = sample_test.df[, covariate_names], 
                                                       second_stage = "Bayesian-MVMA")


predictions.mixing <- predict.Bayesian_linear_regression(obj.Bayesian_UP = Bayes_linear_model, 
                                                       newX = sample_test.df[, covariate_names], 
                                                       second_stage = "MCMC")
