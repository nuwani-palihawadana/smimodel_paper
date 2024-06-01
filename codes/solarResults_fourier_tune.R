# Note: The estimation of SMI Models were conducted in MonARCH (Monash Advanced
# Research Computing Hybrid) HPC Cluster. The codes used to fit (in HPC)
# multiple SMI models with linear initialisation to select fourier terms (in
# the folder "codes/solar_HPC_fourier_tune"), comparing the models fitted using
# different number of fourier terms to select the best model (file
# codes/solar_fourier_tune_outputCombine.R"), and fit final SMI models (in HPC)
# with the selected number of fourier terms (in the folder "codes/solar_HPC")are
# provided separately.

# Loading required packages ----------------------------------------------------
library(cgaim)
library(dplyr)
library(fable)
library(fabletools)
library(ggplot2)
library(lubridate)
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
solarData <- readRDS("./data/solar_data_withFourier.rds")

# Training set
solar_train <- solarData %>%
  filter(Date <= "2012-10-31") %>%
  as_tsibble(index = Date) %>%
  drop_na()

# Validation set
solar_val <- solarData %>%
  filter((Date >= "2012-11-01") & (Date <= "2012-12-31")) %>%
  as_tsibble(index = Date)

# Test set
solar_test <- solarData %>%
  filter((Date >= "2013-01-01") & (Date <= "2013-02-28"))
## Adjusting the test set data to remove future demand lags
for(i in 23:25){
  solar_test[(i - 21):NROW(solar_test), i] <- NA
}
# Convert to a tsibble
solar_test <- solar_test %>%
  as_tsibble(index = Date)

# Number of fourier terms (pairs)
numFourier <- 1:10

# Index variables
index.vars <- colnames(solarData)[23:45]
# Linear variables
linear.vars <- colnames(solarData)[2:17] # Using 8 pairs of fourier terms.
# Number of pairs to use was determined based on the number of pairs selected 
# by the SMI Model with linear initialisation.

## SMI Model - ppr #############################################################
## Penalty = (1, 0)
## Num_ind = 4
## Num_var = 23 + 16 = 39

# Read output
smimodel_solar_ppr <- readRDS("./results/smimodel_solar_ppr_fourier8.rds")

# Obtain predictions
solar_preds_ppr <- predict(object = smimodel_solar_ppr, newdata = solar_test,
                           recursive = TRUE, recursive_colRange = 23:25)

# Test set errors
MSE_SMI_ppr <- MSE(.resid = (solar_preds_ppr$Solar_lag_000 - solar_preds_ppr$.predict))
MSE_SMI_ppr
MAE_SMI_ppr <- MAE(.resid = (solar_preds_ppr$Solar_lag_000 - solar_preds_ppr$.predict))
MAE_SMI_ppr

## SMI Model - additive ########################################################
## Penalty = (6, 0)
## Num_ind = 23
## Num_var = 23 + 16 = 39

# Read output
smimodel_solar_additive <- readRDS("./results/smimodel_solar_additive_fourier8.rds")

# Obtain predictions
solar_preds_additive <- predict(object = smimodel_solar_additive, newdata = solar_test,
                                recursive = TRUE, recursive_colRange = 23:25)

# Test set errors
MSE_SMI_additive <- MSE(.resid = (solar_preds_additive$Solar_lag_000 - solar_preds_additive$.predict))
MSE_SMI_additive
MAE_SMI_additive <- MAE(.resid = (solar_preds_additive$Solar_lag_000 - solar_preds_additive$.predict))
MAE_SMI_additive

## SMI Model - linear ##########################################################
## Penalty = (1, 0)
## Num_ind = 0
## Num_var = 0 + 16 = 16

# Read output
smimodel_solar_linear <- readRDS("./results/smimodel_solar_linear_fourier8.rds")

# Obtain predictions
solar_preds_linear <- predict(object = smimodel_solar_linear, newdata = solar_test,
                              recursive = TRUE, recursive_colRange = 23:25)

# Test set errors
MSE_SMI_linear <- MSE(.resid = (solar_preds_linear$Solar_lag_000 - solar_preds_linear$.predict))
MSE_SMI_linear
MAE_SMI_linear <- MAE(.resid = (solar_preds_linear$Solar_lag_000 - solar_preds_linear$.predict))
MAE_SMI_linear


## Backward Elimination ########################################################

# Read output
solar_backward <- readRDS("./results/solar_backward_fourier8.rds")

# Obtain predictions
backward_preds <- predict(object = solar_backward, newdata = solar_test,
                          recursive = TRUE, recursive_colRange = 23:25)
MSE_backward <- MSE(.resid = (backward_preds$Solar_lag_000 - backward_preds$.predict))
MSE_backward
MAE_backward <- MAE(.resid = (backward_preds$Solar_lag_000 - backward_preds$.predict))
MAE_backward


## GAIM ########################################################################

combinedData <- dplyr::bind_rows(solar_train, solar_val)
# # Model fitting
# index.ind = c(rep(1, 3), rep(2, 4), rep(3, 4), rep(4, 4), rep(5, 4), rep(6, 4))
# solar_gaim <- model_gaim(data = combinedData,
#                          yvar = "Solar_lag_000",
#                          index.vars = index.vars,
#                          index.ind = index.ind,
#                          linear.vars = linear.vars)
# 
# # Save output
# saveRDS(solar_gaim, file = paste0('./results/solar_gaim_fourier8.rds'))

# Read output
solar_gaim <- readRDS("./results/solar_gaim_fourier8.rds")

# Obtain predictions
gaim_preds <- predict(object = solar_gaim, newdata = solar_test,
                      recursive = TRUE, recursive_colRange = 23:25)
MSE_gaim <- MSE(.resid = (gaim_preds$Solar_lag_000 - gaim_preds$.predict))
MSE_gaim
MAE_gaim <- MAE(.resid = (gaim_preds$Solar_lag_000 - gaim_preds$.predict))
MAE_gaim


## PPR #########################################################################

# # Model fitting
# solar_ppr <- model_ppr(data = combinedData, yvar = "Solar_lag_000",
#                        index.vars = index.vars, num_ind = 6)
# # num_ind = number of indices in GAIM
# 
# # Save output
# saveRDS(solar_ppr, file = paste0('./results/solar_ppr_correct.rds'))

# Read output
solar_ppr <- readRDS("./results/solar_ppr_correct.rds")

# Obtain predictions
ppr_preds <- predict(object = solar_ppr, newdata = solar_test,
                     recursive = TRUE, recursive_colRange = 23:25)
MSE_ppr <- MSE(.resid = (ppr_preds$Solar_lag_000 - ppr_preds$.predict))
MSE_ppr
MAE_ppr <- MAE(.resid = (ppr_preds$Solar_lag_000 - ppr_preds$.predict))
MAE_ppr


# Creating a data frame of test set errors and writing to a csv
results_solar <- tibble(
  Model = c("SMI Model (1, 0) - PPR", "SMI Model (6, 0) - Additive", 
            "SMI Model (1, 0) - Linear", "Backward Elimination", "GAIM", "PPR"), 
  Predictors = c("39", "39", "16", "36", "39", "23"),
  Indices = c("4", "23", "0", "NA", "6", "6"),
  MSE = c(MSE_SMI_ppr, MSE_SMI_additive, MSE_SMI_linear, MSE_backward, MSE_gaim, MSE_ppr),
  MAE = c(MAE_SMI_ppr, MAE_SMI_additive, MAE_SMI_linear, MAE_backward, MAE_gaim, MAE_ppr)
)
write_csv(results_solar, "./results/solar_results_fourier8.csv")

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
write_csv(SolarPreds, "./results/solar_predictions_fourier8.csv")

# Graph
SolarPreds <- readr::read_csv("results/solar_predictions_fourier8.csv") |> 
  rename(
    `SMI Model (1, 0) - PPR` = SMImodel_PPR,
    `Backward Elimination` = Backward
  ) |> 
  select(-SMImodel_Linear, -SMImodel_Additive) |> 
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
    "SMI Model (1, 0) - PPR" = "#D55E00",
    "PPR" = "#0072B2",
    "Backward Elimination" = "#009E73",
    "GAIM" = "#CC79A7"
  )) +
  theme(
    legend.position = "bottom",
    legend.box = "vertical"
  )
