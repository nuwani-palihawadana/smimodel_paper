# Loading required packages ----------------------------------------------------
library(cgaim)
library(dplyr)
library(fabletools)
library(readr)
library(ROI)
library(ROI.plugin.gurobi)
library(smimodel)
library(tibble)
library(tidyr)
library(tsibble)

conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("lag", "dplyr")
conflicted::conflict_prefer("unpack", "tidyr")

# Data -------------------------------------------------------------------------
solarData <- readRDS("./data/solar_daily_large.rds")

# Training set
solar_train <- solarData %>%
  filter(Date <= "2012-10-31") %>%
  as_tsibble(index = Date)

# Index variables
index.vars <- colnames(solarData)[5:27]

# greedy
# L0 penalty
lambda0 = seq(1, 12, by = 1)
# L2 penalty
lambda2 = seq(0, 12, by = 1)
# Full grid
grid1 <- expand.grid(lambda0, lambda2)

# Starting point options
starting <- grid1[c(1, 6, 12, 73, 78, 84, 145, 150, 156), ]
# L0 penalty
lambda0_start = as.numeric(unique(unlist(starting[1])))
# L2 penalty
lambda2_start = as.numeric(unique(unlist(starting[2])))

## SMI Model - ppr #######################################################
## Penalty = (1, 12)
## Num_ind = 5
# smimodel_solar_ppr <- greedy_smimodel(data = solar_train, 
#                                       yvar = "Solar_lag_000",
#                                       index.vars = index.vars,
#                                       initialise = "ppr",
#                                       linear.vars = "Month",
#                                       lambda0_seq = lambda0, 
#                                       lambda2_seq = lambda2, 
#                                       lambda_step = 1,
#                                       lambda0_start_seq = lambda0_start, 
#                                       lambda2_start_seq = lambda2_start)

# Save output
# saveRDS(smimodel_solar_ppr, file = paste0('./results/smimodel_solar_ppr_newStructure.rds'))

# Read output
smimodel_solar_ppr <- readRDS("./results/smimodel_solar_ppr_newStructure.rds")

# Test set
solar_test <- solarData %>%
  filter((Date >= "2013-01-01") & (Date <= "2013-02-28"))
# Adjusting the test set data to remove future solar intensity lags
for(i in 5:7){
  solar_test[(i - 3):NROW(solar_test), i] <- NA
}
# Convert to a tsibble
solar_test <- solar_test %>%
  as_tsibble(index = Date)

# Obtain predictions
solar_preds_ppr <- predict(object = smimodel_solar_ppr, newdata = solar_test,
                           recursive = TRUE, recursive_colRange = 5:7)

# Test set errors
MSE_SMI_ppr <- MSE(.resid = (solar_preds_ppr$Solar_lag_000 - solar_preds_ppr$.predict))
MSE_SMI_ppr
MAE_SMI_ppr <- MAE(.resid = (solar_preds_ppr$Solar_lag_000 - solar_preds_ppr$.predict))
MAE_SMI_ppr

## SMI Model - additive ###################################################
## Penalty = (1, 0)
## Num_ind = 23
# smimodel_solar_additive <- greedy_smimodel(data = solar_train, 
#                                            yvar = "Solar_lag_000",
#                                            index.vars = index.vars,
#                                            initialise = "additive",
#                                            linear.vars = "Month",
#                                            lambda0_seq = lambda0, 
#                                            lambda2_seq = lambda2, 
#                                            lambda_step = 1,
#                                            lambda0_start_seq = lambda0_start, 
#                                            lambda2_start_seq = lambda2_start)

# Save output
# saveRDS(smimodel_solar_additive, file = paste0('./results/smimodel_solar_additive_newStructure.rds'))

# Read output
smimodel_solar_additive <- readRDS("./results/smimodel_solar_additive_newStructure.rds")

# Test set
solar_test <- solarData %>%
  filter((Date >= "2013-01-01") & (Date <= "2013-02-28"))
## Adjusting the test set data to remove future demand lags
for(i in 5:7){
  solar_test[(i - 3):NROW(solar_test), i] <- NA
}
# Convert to a tsibble
solar_test <- solar_test %>%
  as_tsibble(index = Date) 

# Obtain predictions
solar_preds_additive <- predict(object = smimodel_solar_additive, newdata = solar_test,
                                recursive = TRUE, recursive_colRange = 5:7)

# Test set errors
MSE_SMI_additive <- MSE(.resid = (solar_preds_additive$Solar_lag_000 - solar_preds_additive$.predict))
MSE_SMI_additive
MAE_SMI_additive <- MAE(.resid = (solar_preds_additive$Solar_lag_000 - solar_preds_additive$.predict))
MAE_SMI_additive

## SMI Model - linear #####################################################
## Penalty = (1, 0)
## Num_ind = 0
# smimodel_solar_linear <- greedy_smimodel(data = solar_train, 
#                                          yvar = "Solar_lag_000",
#                                          index.vars = index.vars,
#                                          initialise = "linear",
#                                          linear.vars = "Month",
#                                          lambda0_seq = lambda0, 
#                                          lambda2_seq = lambda2, 
#                                          lambda_step = 1,
#                                          lambda0_start_seq = lambda0_start, 
#                                          lambda2_start_seq = lambda2_start)

# Save output
# saveRDS(smimodel_solar_linear, file = paste0('./results/smimodel_solar_linear_newStructure.rds'))

# Read output
smimodel_solar_linear <- readRDS("./results/smimodel_solar_linear_newStructure.rds")

# Test set
solar_test <- solarData %>%
  filter((Date >= "2013-01-01") & (Date <= "2013-02-28"))
## Adjusting the test set data to remove future demand lags
for(i in 5:7){
  solar_test[(i - 3):NROW(solar_test), i] <- NA
}
# Convert to a tsibble
solar_test <- solar_test %>%
  as_tsibble(index = Date) 

# Obtain predictions
solar_preds_linear <- predict(object = smimodel_solar_linear, newdata = solar_test,
                              recursive = TRUE, recursive_colRange = 5:7)

# Test set errors
MSE_SMI_linear <- MSE(.resid = (solar_preds_linear$Solar_lag_000 - solar_preds_linear$.predict))
MSE_SMI_linear
MAE_SMI_linear <- MAE(.resid = (solar_preds_linear$Solar_lag_000 - solar_preds_linear$.predict))
MAE_SMI_linear


## Backward Elimination ########################################################

# Validation set
solar_val <- solarData %>%
  filter((Date >= "2012-11-01") & (Date <= "2012-12-31")) %>%
  as_tsibble(index = Date)

# Model fitting
# solar_backward <- model_backward(data = solar_train,
#                                  val.data = solar_val,
#                                  yvar = "Solar_lag_000", 
#                                  s.vars = index.vars,
#                                  linear.vars = "Month") 

# Save output
# saveRDS(solar_backward, file = paste0('./results/solar_backward.rds'))

# Read output
solar_backward <- readRDS("./results/solar_backward.rds")

# Test set
solar_test <- solarData %>%
  filter((Date >= "2013-01-01") & (Date <= "2013-02-28"))
## Adjusting the test set data to remove future demand lags
for(i in 5:7){
  solar_test[(i - 3):NROW(solar_test), i] <- NA
}
# Convert to a tsibble
solar_test <- solar_test %>%
  as_tsibble(index = Date) 

# Obtain predictions
backward_preds <- predict(object = solar_backward, newdata = solar_test,
                          recursive = TRUE, recursive_colRange = 5:7)
MSE_backward <- MSE(.resid = (backward_preds$Solar_lag_000 - backward_preds$.predict))
MSE_backward
MAE_backward <- MAE(.resid = (backward_preds$Solar_lag_000 - backward_preds$.predict))
MAE_backward


## GAIM ########################################################################

# Model fitting
# index.ind = c(rep(1, 3), rep(2, 4), rep(3, 4), rep(4, 4), rep(5, 4), rep(6, 4))
# solar_gaim <- model_gaim(data = solar_train, yvar = "Solar_lag_000", 
#                          index.vars = index.vars, 
#                          index.ind = index.ind,
#                          linear.vars = "Month") 

# Save output
# saveRDS(solar_gaim, file = paste0('./results/solar_gaim.rds'))

# Read output
solar_gaim <- readRDS("./results/solar_gaim.rds")

# Test set
solar_test <- solarData %>%
  filter((Date >= "2013-01-01") & (Date <= "2013-02-28"))
## Adjusting the test set data to remove future demand lags
for(i in 5:7){
  solar_test[(i - 3):NROW(solar_test), i] <- NA
}
# Convert to a tsibble
solar_test <- solar_test %>%
  as_tsibble(index = Date) 

# Obtain predictions
gaim_preds <- predict(object = solar_gaim, newdata = solar_test,
                      recursive = TRUE, recursive_colRange = 5:7)
MSE_gaim <- MSE(.resid = (gaim_preds$Solar_lag_000 - gaim_preds$.predict))
MSE_gaim
MAE_gaim <- MAE(.resid = (gaim_preds$Solar_lag_000 - gaim_preds$.predict))
MAE_gaim


## PPR #########################################################################

# Model fitting
# solar_ppr <- model_ppr(data = solar_train, yvar = "Solar_lag_000", 
#                        index.vars = index.vars, num_ind = 6) 
# num_ind = number of indices in GAIM 

# Save output
# saveRDS(solar_ppr, file = paste0('./results/solar_ppr.rds'))

# Read output
solar_ppr <- readRDS("./results/solar_ppr.rds")

# Test set
solar_test <- solarData %>%
  filter((Date >= "2013-01-01") & (Date <= "2013-02-28"))
## Adjusting the test set data to remove future demand lags
for(i in 5:7){
  solar_test[(i - 3):NROW(solar_test), i] <- NA
}
# Convert to a tsibble
solar_test <- solar_test %>%
  as_tsibble(index = Date) 

# Obtain predictions
ppr_preds <- predict(object = solar_ppr, newdata = solar_test,
                     recursive = TRUE, recursive_colRange = 5:7)
MSE_ppr <- MSE(.resid = (ppr_preds$Solar_lag_000 - ppr_preds$.predict))
MSE_ppr
MAE_ppr <- MAE(.resid = (ppr_preds$Solar_lag_000 - ppr_preds$.predict))
MAE_ppr


# Creating a data frame of test set errors and writing to a csv
results_solar <- tibble(
  Model = c("SMI Model (1, 12) - PPR", "SMI Model (1, 0) - Additive", 
            "SMI Model (1, 0) - Linear", "Backward Elimination", "GAIM", "PPR"), 
  Predictors = c("24", "24", "1", "15", "24", "23"),
  Indices = c("5", "23", "0", "NA", "6", "6"),
  MSE = c(MSE_SMI_ppr, MSE_SMI_additive, MSE_SMI_linear, MSE_backward, MSE_gaim, MSE_ppr),
  MAE = c(MAE_SMI_ppr, MAE_SMI_additive, MAE_SMI_linear, MAE_backward, MAE_gaim, MAE_ppr)
)
write_csv(results_solar, "./results/solar_results.csv")

# Predictions for test set
solar_test <- solarData %>%
  filter((Date >= "2013-01-01") & (Date <= "2013-02-28")) %>%
  as_tsibble(index = Date)
SolarPreds <- tibble(
  Date = solar_test$Date,
  Actual = solar_test$Solar_lag_000,
  SMImodel_PPR = as.numeric(solar_preds_ppr$.predict),
  SMImodel_Additive = as.numeric(solar_preds_additive$.predict),
  SMImodel_Linear = as.numeric(solar_preds_linear$.predict),
  PPR = as.numeric(ppr_preds$.predict),
  Backward = as.numeric(backward_preds$.predict),
  GAIM = as.numeric(gaim_preds$.predict)
)
write_csv(SolarPreds, "./results/solar_predictions.csv")

# Graph
SolarPreds <- readr::read_csv("results/solar_predictions.csv") |> 
  rename(
    `SMI Model (1,0) - Additive` = SMImodel_Additive,
    `Backward Elimination` = Backward
  ) |> 
  select(-SMImodel_Linear, -SMImodel_PPR) |> 
  tidyr::pivot_longer(Actual:GAIM, names_to = "Series", values_to = "Intensity")
SolarPreds |> 
  ggplot(aes(x = Date, y = Intensity, colour = Series)) +
  geom_line() +
  scale_x_date(date_breaks = "7 days") +
  labs(
    x = "Date", y = "Solar Intensity (Watts per square metre)",
    title = "Actual vs. Predicted Solar Intensity"
  ) +
  scale_colour_manual(name = "Series", values = c(
    "Actual" = "grey",
    "SMI Model (1,0) - Additive" = "#D55E00",
    "PPR" = "#0072B2",
    "Backward Elimination" = "#009E73",
    "GAIM" = "#CC79A7"
  )) +
  theme(
    legend.position = "bottom",
    legend.box = "vertical"
  )
