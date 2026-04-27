covariate_names <- paste0("x_", 1:8)

# LASSO
lasso_prepare_data <- lasso_ipd_spec(covariate_names = covariate_names,
                                     outcome = "y", treatment = "treatment",
                                     lambda_seq = NULL)
lasso.fit <- lasso_ipd_train(data = sample_train.df, spec = lasso_prepare_data)

lasso.predictions <- predict.ipd_lasso(object = lasso.fit,
                                       newX = sample_test.df[, covariate_names],
                                       second_stage = "OLS+IV")

lasso.predictions.BS <- predict.ipd_lasso(object = lasso.fit,
                                          newX = sample_test.df[, covariate_names],
                                          second_stage = "BS", N_boot = 5)
# RIDGE
ridge_prepare_data <- ridge_ipd_spec(covariate_names = covariate_names,
                                     outcome = "y", treatment = "treatment",
                                     lambda_seq = NULL)
ridge.fit <- ridge_ipd_train(data = sample_train.df, spec = lasso_prepare_data)

ridge.predictions <- predict.ipd_ridge(object = lasso.fit,
                                       newX = sample_test.df[, covariate_names],
                                       second_stage = "OLS+IV")

ridge.predictions.BS <- predict.ipd_ridge(object = lasso.fit,
                                          newX = sample_test.df[, covariate_names],
                                          second_stage = "BS", N_boot = 5)
