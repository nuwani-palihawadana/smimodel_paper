# Loading required packages ----------------------------------------------------
library(cgaim)
library(dplyr)
library(fabletools)
library(ggplot2)
library(readr)
library(ROI)
library(smimodel)
library(tibble)
library(tidyr)
library(tsibble)

conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("lag", "dplyr")
conflicted::conflict_prefer("unpack", "tidyr")
conflicted::conflict_prefer("MSE", "fabletools")
conflicted::conflict_prefer("MAE", "fabletools")

# Data -------------------------------------------------------------------------
heatData <- readRDS("./data/Heat_Corrected.rds")

# Training set
heat_train <- heatData %>%
  as_tsibble(index = Date) %>%
  filter(Date <= "2012-08-31") %>%
  drop_na()

# heat_train <- heat_train %>%
#   tsibble::fill_gaps()
# feasts::ACF(heat_train, y = heat_train$Death_lag_000, lag_max = 200) %>% autoplot()

# Validation set
heat_val <- heatData %>%
  as_tsibble(index = Date) %>%
  filter((Date >= "2013-06-01") & (Date <= "2013-08-31")) %>%
  drop_na()

# Test Set 1
test1 <- heatData %>%
  filter(Date >= "2014-06-01")
## Adjusting the test set data to remove future death lags
for(i in 5:18){
  test1[(i - 3):NROW(test1), i] <- NA
}
# Convert to a tsibble
test1 <- test1 %>%
  as_tsibble(index = Date)

# Test Set 2
test2 <- heatData %>%
  filter((Date >= "2014-06-01") & (Date <= "2014-06-30"))
## Adjusting the test set data to remove future death lags
for(i in 5:18){
  test2[(i - 3):NROW(test2), i] <- NA
}
# Convert to a tsibble
test2 <- test2 %>%
  as_tsibble(index = Date)

# Index variables
index.vars <- colnames(heatData)[5:63]

# # greedy
# # L0 penalty
# lambda0 = seq(1, 12, by = 1)
# # L2 penalty
# lambda2 = seq(0, 12, by = 1)
# # Full grid
# grid1 <- expand.grid(lambda0, lambda2)
# 
# # Starting point options
# starting <- grid1[c(1, 6, 12, 73, 78, 84, 145, 150, 156), ]
# # L0 penalty
# lambda0_start = as.numeric(unique(unlist(starting[1])))
# # L2 penalty
# lambda2_start = as.numeric(unique(unlist(starting[2])))


## SMI Model - ppr #############################################################
## Penalty = (5, 12)
## Num_ind = 7
## Num_var = 59+2 = 61
# smimodel_heat_ppr <- greedy_smimodel(data = heat_train, 
#                                      val.data = heat_val,
#                                      yvar = "Death_lag_000",
#                                      index.vars = index.vars,
#                                      initialise = "ppr",
#                                      s.vars = c("DOS", "Year"),
#                                      lambda0_seq = lambda0, 
#                                      lambda2_seq = lambda2, 
#                                      lambda_step = 1,
#                                      lambda0_start_seq = lambda0_start, 
#                                      lambda2_start_seq = lambda2_start,
#                                      MIPGap = 5e-2,
#                                      TimeLimit = 600,
#                                      recursive = TRUE,
#                                      recursive_colRange = 5:18)

# Saving output
# saveRDS(smimodel_heat_ppr, file = paste0('./results/smimodel_heat_greedy_ppr_response.rds'))

# Read output
smimodel_heat_ppr <- readRDS("./results/smimodel_heat_greedy_ppr_response.rds")

# Obtain predictions
heat_preds_ppr1 <- predict(object = smimodel_heat_ppr, newdata = test1,
                           recursive = TRUE, recursive_colRange = 5:18)

# Test set errors
MSE_SMI_ppr1 <- MSE(.resid = (heat_preds_ppr1$Death_lag_000 - heat_preds_ppr1$.predict))
MSE_SMI_ppr1
MAE_SMI_ppr1 <- MAE(.resid = (heat_preds_ppr1$Death_lag_000 - heat_preds_ppr1$.predict))
MAE_SMI_ppr1

# Obtain predictions
heat_preds_ppr2 <- predict(object = smimodel_heat_ppr, newdata = test2,
                           recursive = TRUE, recursive_colRange = 5:18)

# Test set errors
MSE_SMI_ppr2 <- MSE(.resid = (heat_preds_ppr2$Death_lag_000 - heat_preds_ppr2$.predict))
MSE_SMI_ppr2
MAE_SMI_ppr2 <- MAE(.resid = (heat_preds_ppr2$Death_lag_000 - heat_preds_ppr2$.predict))
MAE_SMI_ppr2

## SMI Model - additive ########################################################
## Penalty = (1, 0)
## Num_ind = 45
# smimodel_heat_additive <- greedy_smimodel(data = heat_train, 
#                                      val.data = heat_val,
#                                      yvar = "Death_lag_000",
#                                      index.vars = index.vars,
#                                      initialise = "additive",
#                                      s.vars = c("DOS", "Year"),
#                                      lambda0_seq = lambda0, 
#                                      lambda2_seq = lambda2, 
#                                      lambda_step = 1,
#                                      lambda0_start_seq = lambda0_start, 
#                                      lambda2_start_seq = lambda2_start,
#                                      MIPGap = 5e-2,
#                                      TimeLimit = 600,
#                                      recursive = TRUE,
#                                      recursive_colRange = 5:18)

# Saving output
# saveRDS(smimodel_heat_additive, file = paste0('./results/smimodel_heat_greedy_additive_response.rds'))

# Read output
smimodel_heat_additive <- readRDS("./results/smimodel_heat_greedy_additive_response.rds")

# Obtain predictions
heat_preds_additive1 <- predict(object = smimodel_heat_additive, newdata = test1,
                                recursive = TRUE, recursive_colRange = 5:18)

# Test set errors
MSE_SMI_additive1 <- MSE(.resid = (heat_preds_additive1$Death_lag_000 - heat_preds_additive1$.predict))
MSE_SMI_additive1
MAE_SMI_additive1 <- MAE(.resid = (heat_preds_additive1$Death_lag_000 - heat_preds_additive1$.predict))
MAE_SMI_additive1

# Obtain predictions
heat_preds_additive2 <- predict(object = smimodel_heat_additive, newdata = test2,
                                recursive = TRUE, recursive_colRange = 5:18)

# Test set errors
MSE_SMI_additive2 <- MSE(.resid = (heat_preds_additive2$Death_lag_000 - heat_preds_additive2$.predict))
MSE_SMI_additive2
MAE_SMI_additive2 <- MAE(.resid = (heat_preds_additive2$Death_lag_000 - heat_preds_additive2$.predict))
MAE_SMI_additive2

## SMI Model - linear ##########################################################
## Penalty = (6, 11)
## Num_ind = 2
## Num_var = 59 + 2 = 61
# smimodel_heat_linear <- greedy_smimodel(data = heat_train, 
#                                      val.data = heat_val,
#                                      yvar = "Death_lag_000",
#                                      index.vars = index.vars,
#                                      initialise = "linear",
#                                      s.vars = c("DOS", "Year"),
#                                      lambda0_seq = lambda0, 
#                                      lambda2_seq = lambda2, 
#                                      lambda_step = 1,
#                                      lambda0_start_seq = lambda0_start, 
#                                      lambda2_start_seq = lambda2_start,
#                                      MIPGap = 5e-2,
#                                      TimeLimit = 600,
#                                      recursive = TRUE,
#                                      recursive_colRange = 5:18)

# Saving output
# saveRDS(smimodel_heat_linear, file = paste0('./results/smimodel_heat_greedy_linear_response.rds'))

# Read output
smimodel_heat_linear <- readRDS("./results/smimodel_heat_greedy_linear_response.rds")

# Obtain predictions
heat_preds_linear1 <- predict(object = smimodel_heat_linear, newdata = test1,
                              recursive = TRUE, recursive_colRange = 5:18)

# Test set errors
MSE_SMI_linear1 <- MSE(.resid = (heat_preds_linear1$Death_lag_000 - heat_preds_linear1$.predict))
MSE_SMI_linear1
MAE_SMI_linear1 <- MAE(.resid = (heat_preds_linear1$Death_lag_000 - heat_preds_linear1$.predict))
MAE_SMI_linear1

# Obtain predictions
heat_preds_linear2 <- predict(object = smimodel_heat_linear, newdata = test2,
                              recursive = TRUE, recursive_colRange = 5:18)

# Test set errors
MSE_SMI_linear2 <- MSE(.resid = (heat_preds_linear2$Death_lag_000 - heat_preds_linear2$.predict))
MSE_SMI_linear2
MAE_SMI_linear2 <- MAE(.resid = (heat_preds_linear2$Death_lag_000 - heat_preds_linear2$.predict))
MAE_SMI_linear2


## Backward Elimination ########################################################

# spline_var <- c(colnames(train)[5:63], colnames(train)[3], colnames(train)[2])
# 
# heat_backward <- model_backward(data = heat_train,
#                          val.data = heat_val,
#                          yvar = "Death_lag_000",
#                          s.vars = spline_var,
#                          recursive = TRUE,
#                          recursive_colRange = 5:18)

# Saving output
# saveRDS(heat_backward, file = paste0('./results/heat_backward_response.rds'))

# Read output
heat_backward <- readRDS("./results/heat_backward_response.rds")

# Obtain predictions
backward_preds1 <- predict(object = heat_backward, newdata = test1,
                           recursive = TRUE, recursive_colRange = 5:18)

# Test set errors
MSE_backward1 <- MSE(.resid = (backward_preds1$Death_lag_000 - backward_preds1$.predict))
MSE_backward1
MAE_backward1 <- MAE(.resid = (backward_preds1$Death_lag_000 - backward_preds1$.predict))
MAE_backward1

# Obtain predictions
backward_preds2 <- predict(object = heat_backward, newdata = test2,
                           recursive = TRUE, recursive_colRange = 5:18)

# Test set errors
MSE_backward2 <- MSE(.resid = (backward_preds2$Death_lag_000 - backward_preds2$.predict))
MSE_backward2
MAE_backward2 <- MAE(.resid = (backward_preds2$Death_lag_000 - backward_preds2$.predict))
MAE_backward2


## GAIM ########################################################################

combinedData <- dplyr::bind_rows(heat_train, heat_val)
# index.ind = c(rep(1, 14), rep(2, 15), rep(3, 15), rep(4, 15))
# heat_gaim <- model_gaim(data = combinedData,
#                         yvar = "Death_lag_000",
#                         index.vars = index.vars,
#                         index.ind = index.ind,
#                         s.vars = c("DOS", "Year"))
# 
# # Saving output
# saveRDS(heat_gaim, file = paste0('./results/heat_gaim_response.rds'))

# Read output
heat_gaim <- readRDS("./results/heat_gaim_response.rds")

# Obtain predictions
gaim_preds1 <- predict(object = heat_gaim, newdata = test1, 
                       recursive = TRUE, recursive_colRange = 5:18)

# Test set errors
MSE_gaim1 <- MSE(.resid = (gaim_preds1$Death_lag_000 - gaim_preds1$.predict))
MSE_gaim1
MAE_gaim1 <- MAE(.resid = (gaim_preds1$Death_lag_000 - gaim_preds1$.predict))
MAE_gaim1

# Obtain predictions
gaim_preds2 <- predict(object = heat_gaim, newdata = test2,
                       recursive = TRUE, recursive_colRange = 5:18)

# Test set errors
MSE_gaim2 <- MSE(.resid = (gaim_preds2$Death_lag_000 - gaim_preds2$.predict))
MSE_gaim2
MAE_gaim2 <- MAE(.resid = (gaim_preds2$Death_lag_000 - gaim_preds2$.predict))
MAE_gaim2


## PPR #########################################################################

# heat_ppr <- model_ppr(data = combinedData,
#                       yvar = "Death_lag_000",
#                       index.vars = c(index.vars, "DOS", "Year"),
#                       num_ind = 4)
# 
# # Saving output
# saveRDS(heat_ppr, file = paste0('./results/heat_ppr_response.rds'))

# Read output
heat_ppr <- readRDS("./results/heat_ppr_response.rds")

# Obtain predictions
ppr_preds1 <- predict(object = heat_ppr, newdata = test1,
                      recursive = TRUE, recursive_colRange = 5:18)

# Test set errors
MSE_ppr1 <- MSE(.resid = (ppr_preds1$Death_lag_000 - ppr_preds1$.predict))
MSE_ppr1
MAE_ppr1 <- MAE(.resid = (ppr_preds1$Death_lag_000 - ppr_preds1$.predict))
MAE_ppr1

# Obtain predictions
ppr_preds2 <- predict(object = heat_ppr, newdata = test2,
                      recursive = TRUE, recursive_colRange = 5:18)

# Test set errors
MSE_ppr2 <- MSE(.resid = (ppr_preds2$Death_lag_000 - ppr_preds2$.predict))
MSE_ppr2
MAE_ppr2 <- MAE(.resid = (ppr_preds2$Death_lag_000 - ppr_preds2$.predict))
MAE_ppr2

# Creating a data frame of test set errors and writing to a csv
results_heat <- tibble(
  Model = c("SMI Model (5, 12) - PPR", "SMI Model (1, 0) - Additive", 
            "SMI Model (6, 11) - Linear", "Backward Elimination", "GAIM", "PPR"), 
  Predictors = c("61", "61", "61", "40", "61", "61"),
  Indices = c("7", "59", "2", "NA", "4", "4"),
  MSE1 = c(MSE_SMI_ppr1, MSE_SMI_additive1, MSE_SMI_linear1, MSE_backward1, MSE_gaim1, MSE_ppr1),
  MAE1 = c(MAE_SMI_ppr1, MAE_SMI_additive1, MAE_SMI_linear1, MAE_backward1, MAE_gaim1, MAE_ppr1),
  MSE2 = c(MSE_SMI_ppr2, MSE_SMI_additive2, MSE_SMI_linear2, MSE_backward2, MSE_gaim2, MSE_ppr2),
  MAE2 = c(MAE_SMI_ppr2, MAE_SMI_additive2, MAE_SMI_linear2, MAE_backward2, MAE_gaim2, MAE_ppr2)
)
write_csv(results_heat, "./results/heat_results_correct.csv")

# Predictions for test set 2
HeatPreds <- tibble(
  Date = test2$Date,
  Actual = test2$Death_lag_000,
  SMImodel_PPR = as.numeric(heat_preds_ppr2$.predict),
  SMImodel_Additive = as.numeric(heat_preds_additive2$.predict),
  SMImodel_Linear = as.numeric(heat_preds_linear2$.predict),
  PPR = as.numeric(ppr_preds2$.predict),
  Backward = as.numeric(backward_preds2$.predict),
  GAIM = as.numeric(gaim_preds2$.predict)
)
write_csv(HeatPreds, "./results/heat_predictions_correct.csv")


# Graph
readr::read_csv(here::here("results/heat_predictions_correct.csv")) |> 
  select(-SMImodel_Additive, -SMImodel_Linear) |> 
  rename(
    `SMI Model (5, 12) - PPR` = SMImodel_PPR,
    `Backward Elimination` = Backward
  ) |> 
  tidyr::pivot_longer(Actual:GAIM, names_to = "Series", values_to = "Deaths") |> 
  ggplot(aes(x = Date, y = Deaths, colour = Series)) +
  geom_line() +
  scale_x_date(date_breaks = "7 days") +
  labs(x = "Date", y = "Number of Deaths") +
  ggtitle("Actual vs. Predicted Number of Deaths") +
  scale_colour_manual(name = "Series", values = c(
    "Actual" = "grey",
    "SMI Model (5, 12) - PPR" = "#D55E00",
    "PPR" = "#0072B2",
    "Backward Elimination" = "#009E73",
    "GAIM" = "#CC79A7"
  )) +
  theme(
    legend.position = "bottom",
    legend.box = "vertical"
  )
