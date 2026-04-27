# create vectors with covariate names
covariates_names <- paste0("x_", 1:8)

# fit model that returns the meta-analyzed coefficients
ols_train_model <- OLS_ipd_train(data = sample_train.df,
                                 second_stage = "reml",
                                 covariate_names = covariates_names,
                                 outcome = "y",
                                 treatment = "treatment")

# make predictions
ols_predictions <- predict.OLS_MVMA(obj.OLS = ols_train_model,
                                    newX = sample_test.df[, covariates_names])
