# Loading required packages ----------------------------------------------------
library(cgaim)
library(dplyr)
library(fabletools)
library(ggplot2)
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
heatData <- readRDS("./data/heat_data_corrected.rds")

# Training set
heat_train <- heatData %>%
  as_tsibble(index = Date) %>%
  filter(Date <= "2012-08-31") %>%
  drop_na()

# Validation set
heat_val <- heatData %>%
  as_tsibble(index = Date) %>%
  filter((Date >= "2013-06-01") & (Date <= "2013-08-31")) %>%
  drop_na()

# Test Set 1
test1 <- heatData %>%
  as_tsibble(index = Date) %>%
  filter(Date >= "2014-06-01")

# Test Set 2
test2 <- heatData %>%
  as_tsibble(index = Date) %>%
  filter((Date >= "2014-06-01") & (Date <= "2014-06-30"))

# Index variables
index.vars <- colnames(heatData)[5:49]

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


## SMI Model - ppr #############################################################
## Penalty = (12, 0)
## Num_ind = 5
# smimodel_heat_ppr <- greedy_smimodel(data = data, 
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
#                                      TimeLimit = 600)

# Saving output
# saveRDS(smimodel_heat_ppr, file = paste0('./results/smimodel_heat_greedy_ppr_newStructure.rds'))

# Read output
smimodel_heat_ppr <- readRDS("./results/smimodel_heat_greedy_ppr_newStructure.rds")

# Obtain predictions
heat_preds_ppr1 <- predict(object = smimodel_heat_ppr, newdata = test1)

# Test set errors
MSE_SMI_ppr1 <- MSE(.resid = (heat_preds_ppr1$Death_lag_000 - heat_preds_ppr1$.predict))
MSE_SMI_ppr1
MAE_SMI_ppr1 <- MAE(.resid = (heat_preds_ppr1$Death_lag_000 - heat_preds_ppr1$.predict))
MAE_SMI_ppr1

# Obtain predictions
heat_preds_ppr2 <- predict(object = smimodel_heat_ppr, newdata = test2)

# Test set errors
MSE_SMI_ppr2 <- MSE(.resid = (heat_preds_ppr2$Death_lag_000 - heat_preds_ppr2$.predict))
MSE_SMI_ppr2
MAE_SMI_ppr2 <- MAE(.resid = (heat_preds_ppr2$Death_lag_000 - heat_preds_ppr2$.predict))
MAE_SMI_ppr2

## SMI Model - additive ########################################################
## Penalty = (1, 0)
## Num_ind = 45
# smimodel_heat_additive <- greedy_smimodel(data = data,
#                                           yvar = "Death_lag_000",
#                                           index.vars = index.vars,
#                                           initialise = "additive",
#                                           s.vars = c("DOS", "Year"),
#                                           lambda0_seq = lambda0,
#                                           lambda2_seq = lambda2,
#                                           lambda_step = 1,
#                                           lambda0_start_seq = lambda0_start,
#                                           lambda2_start_seq = lambda2_start,
#                                           MIPGap = 5e-2,
#                                           TimeLimit = 600)

# Saving output
# saveRDS(smimodel_heat_additive, file = paste0('./results/smimodel_heat_greedy_additive_newStructure.rds'))

# Read output
smimodel_heat_additive <- readRDS("./results/smimodel_heat_greedy_additive_newStructure.rds")

# Obtain predictions
heat_preds_additive1 <- predict(object = smimodel_heat_additive, newdata = test1)

# Test set errors
MSE_SMI_additive1 <- MSE(.resid = (heat_preds_additive1$Death_lag_000 - heat_preds_additive1$.predict))
MSE_SMI_additive1
MAE_SMI_additive1 <- MAE(.resid = (heat_preds_additive1$Death_lag_000 - heat_preds_additive1$.predict))
MAE_SMI_additive1

# Obtain predictions
heat_preds_additive2 <- predict(object = smimodel_heat_additive, newdata = test2)

# Test set errors
MSE_SMI_additive2 <- MSE(.resid = (heat_preds_additive2$Death_lag_000 - heat_preds_additive2$.predict))
MSE_SMI_additive2
MAE_SMI_additive2 <- MAE(.resid = (heat_preds_additive2$Death_lag_000 - heat_preds_additive2$.predict))
MAE_SMI_additive2

## SMI Model - linear ##########################################################
## Penalty = (12, 5)
## Num_ind = 2
# smimodel_heat_linear <- greedy_smimodel(data = data,
#                                         yvar = "Death_lag_000",
#                                         index.vars = index.vars,
#                                         initialise = "linear",
#                                         s.vars = c("DOS", "Year"),
#                                         lambda0_seq = lambda0,
#                                         lambda2_seq = lambda2,
#                                         lambda_step = 1,
#                                         lambda0_start_seq = lambda0_start,
#                                         lambda2_start_seq = lambda2_start,
#                                         MIPGap = 5e-2,
#                                         TimeLimit = 600)

# Saving output
# saveRDS(smimodel_heat_linear, file = paste0('./results/smimodel_heat_greedy_linear_newStructure.rds'))

# Read output
smimodel_heat_linear <- readRDS("./results/smimodel_heat_greedy_linear_newStructure.rds")

# Obtain predictions
heat_preds_linear1 <- predict(object = smimodel_heat_linear, newdata = test1)

# Test set errors
MSE_SMI_linear1 <- MSE(.resid = (heat_preds_linear1$Death_lag_000 - heat_preds_linear1$.predict))
MSE_SMI_linear1
MAE_SMI_linear1 <- MAE(.resid = (heat_preds_linear1$Death_lag_000 - heat_preds_linear1$.predict))
MAE_SMI_linear1

# Obtain predictions
heat_preds_linear2 <- predict(object = smimodel_heat_linear, newdata = test2)

# Test set errors
MSE_SMI_linear2 <- MSE(.resid = (heat_preds_linear2$Death_lag_000 - heat_preds_linear2$.predict))
MSE_SMI_linear2
MAE_SMI_linear2 <- MAE(.resid = (heat_preds_linear2$Death_lag_000 - heat_preds_linear2$.predict))
MAE_SMI_linear2


## Backward Elimination ########################################################

# spline_var <- c(colnames(heatData)[5:49], colnames(heatData)[2:3])
# 
# heat_backward <- model_backward(data = heat_train,
#                                 val.data = heat_val,
#                                 yvar = "Death_lag_000",
#                                 s.vars = spline_var)

# Saving output
# saveRDS(heat_backward, file = paste0('./results/heat_backward.rds'))

# Read output
heat_backward <- readRDS("./results/heat_backward.rds")

# Obtain predictions
backward_preds1 <- predict(object = heat_backward, newdata = test1)

# Test set errors
MSE_backward1 <- MSE(.resid = (backward_preds1$Death_lag_000 - backward_preds1$.predict))
MSE_backward1
MAE_backward1 <- MAE(.resid = (backward_preds1$Death_lag_000 - backward_preds1$.predict))
MAE_backward1

# Obtain predictions
backward_preds2 <- predict(object = heat_backward, newdata = test2)

# Test set errors
MSE_backward2 <- MSE(.resid = (backward_preds2$Death_lag_000 - backward_preds2$.predict))
MSE_backward2
MAE_backward2 <- MAE(.resid = (backward_preds2$Death_lag_000 - backward_preds2$.predict))
MAE_backward2


## GAIM ########################################################################

# index.ind = c(rep(1, 15), rep(2, 15), rep(3, 15))
# heat_gaim <- model_gaim(data = heat_train,
#                         yvar = "Death_lag_000",
#                         index.vars = index.vars,
#                         index.ind = index.ind,
#                         s.vars = c("DOS", "Year"))

# Saving output
# saveRDS(heat_gaim, file = paste0('./results/heat_gaim.rds'))

# Read output
heat_gaim <- readRDS("./results/heat_gaim.rds")

# Obtain predictions
gaim_preds1 <- predict(object = heat_gaim, newdata = test1)

# Test set errors
MSE_gaim1 <- MSE(.resid = (gaim_preds1$Death_lag_000 - gaim_preds1$.predict))
MSE_gaim1
MAE_gaim1 <- MAE(.resid = (gaim_preds1$Death_lag_000 - gaim_preds1$.predict))
MAE_gaim1

# Obtain predictions
gaim_preds2 <- predict(object = heat_gaim, newdata = test2)

# Test set errors
MSE_gaim2 <- MSE(.resid = (gaim_preds2$Death_lag_000 - gaim_preds2$.predict))
MSE_gaim2
MAE_gaim2 <- MAE(.resid = (gaim_preds2$Death_lag_000 - gaim_preds2$.predict))
MAE_gaim2


## PPR #########################################################################

# heat_ppr <- model_ppr(data = heat_train,
#                       yvar = "Death_lag_000",
#                       index.vars = c(index.vars, "DOS", "Year"),
#                       num_ind = 3)

# Saving output
# saveRDS(heat_ppr, file = paste0('./results/heat_ppr.rds'))

# Read output
heat_ppr <- readRDS("./results/heat_ppr.rds")

# Obtain predictions
ppr_preds1 <- predict(object = heat_ppr, newdata = test1)

# Test set errors
MSE_ppr1 <- MSE(.resid = (ppr_preds1$Death_lag_000 - ppr_preds1$.predict))
MSE_ppr1
MAE_ppr1 <- MAE(.resid = (ppr_preds1$Death_lag_000 - ppr_preds1$.predict))
MAE_ppr1

# Obtain predictions
ppr_preds2 <- predict(object = heat_ppr, newdata = test2)

# Test set errors
MSE_ppr2 <- MSE(.resid = (ppr_preds2$Death_lag_000 - ppr_preds2$.predict))
MSE_ppr2
MAE_ppr2 <- MAE(.resid = (ppr_preds2$Death_lag_000 - ppr_preds2$.predict))
MAE_ppr2


# Creating a data frame of test set errors and writing to a csv
results_heat <- tibble(
  Model = c("SMI Model (12, 0) - PPR", "SMI Model (1, 0) - Additive", 
            "SMI Model (12, 5) - Linear", "Backward Elimination", "GAIM", "PPR"), 
  Predictors = c("47", "47", "47", "36", "47", "47"),
  Indices = c("5", "45", "2", "NA", "3", "3"),
  MSE1 = c(MSE_SMI_ppr1, MSE_SMI_additive1, MSE_SMI_linear1, MSE_backward1, MSE_gaim1, MSE_ppr1),
  MAE1 = c(MAE_SMI_ppr1, MAE_SMI_additive1, MAE_SMI_linear1, MAE_backward1, MAE_gaim1, MAE_ppr1),
  MSE2 = c(MSE_SMI_ppr2, MSE_SMI_additive2, MSE_SMI_linear2, MSE_backward2, MSE_gaim2, MSE_ppr2),
  MAE2 = c(MAE_SMI_ppr2, MAE_SMI_additive2, MAE_SMI_linear2, MAE_backward2, MAE_gaim2, MAE_ppr2)
)
write_csv(results_heat, "./results/heat_results.csv")

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
write_csv(HeatPreds, "./results/heat_predictions.csv")


# Graph
readr::read_csv(here::here("results/heat_predictions.csv")) |> 
  select(-SMImodel_Additive, -SMImodel_Linear) |> 
  rename(
    `SMI Model (12, 0) - PPR` = SMImodel_PPR,
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
    "SMI Model (12, 0) - PPR" = "#D55E00",
    "PPR" = "#0072B2",
    "Backward Elimination" = "#009E73",
    "GAIM" = "#CC79A7"
  )) +
  theme(
    legend.position = "bottom",
    legend.box = "vertical"
  )
