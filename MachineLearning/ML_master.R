covariate_names <- paste0("x_", 1:8)

# Causal Forest
cf.fit <- cf_train(data = sample_train.df, 
                   covariate_names = covariate_names,
                   outcome = "y", treatment = "treatment")

cf.predictions.ols.iv <- predict.cf(obj.first.stage = cf.fit, 
                                    newX = sample_test.df[, covariate_names],
                                    second_stage = "OLS+IV")

cf.predictions.BS <- predict.cf(obj.first.stage = cf.fit,
                                newX = sample_test.df[, covariate_names],
                                second_stage = "BS", N_boot = 5)
# XGBoost
params_df <- expand.grid(
  max_depth = 2:6,
  eta = c(0.1, 0.01, 0.001),
  nrounds = c(100, 500, 1000)
)
xgb.fit <- xgb_train(data = sample_train.df, params_df = params_df, 
                     covariate_names = covariate_names, outcome = "y",
                     treatment = "treatment")

xgb.predictions.ols.iv <- predict.xgb(newX = sample_test.df[, covariate_names], 
                                      obj_xgb_train = xgb.fit,
                                       second_stage = "OLS+IV")

xgb.predictions.BS <- predict.xgb(obj_xgb_train = xgb.fit,
                                  newX = sample_test.df[, covariate_names],
                                  second_stage = "BS", N_boot = 5)
