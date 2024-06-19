# Loading required packages ----------------------------------------------------
library(dplyr)
library(fabletools)
library(ROI)
library(smimodel)
library(tibble)
library(tidyr)
library(tsibble)

conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("lag", "dplyr")
conflicted::conflict_prefer("unpack", "tidyr")

## Single SMI Model
# Data -------------------------------------------------------------------------
# The below data set is generated for low noise level N(mu = 0, sigma = 0.1). 
# For high noise level, change to N(mu = 0, sigma = 0.5), and use the same code.
n = 1205
set.seed(123)
sim_data1 <- tibble(x_lag_000 = runif(n),
                    z_lag_000 = rnorm(n, mean = 5, sd = 2)) %>%
  mutate(
    # Add x_lags
    x_lag = lag_matrix(x_lag_000, 5)
  ) %>%
  unpack(x_lag, names_sep = "_") %>%
  mutate(
    # Add z_lags
    z_lag = lag_matrix(z_lag_000, 5)
  ) %>%
  unpack(z_lag, names_sep = "_") %>%
  mutate(
    # Different response variables for experiments
    y1 = (0.9*x_lag_000 + 0.6*x_lag_001 + 0.45*x_lag_003)^3 + rnorm(n, sd = 0.1),
    y2 = (0.9*x_lag_000 + 0.6*x_lag_001 + 0.45*x_lag_003)^3 + (0.35*x_lag_002 + 0.7*x_lag_005)^2 + rnorm(n, sd = 0.1),
    inddd = seq(1, n)
  ) %>%
  drop_na() %>%
  select(inddd, starts_with("y"), starts_with("x_lag"), starts_with("z_lag")) %>%
  as_tsibble(index = inddd)

# Training set
sim_train1 <- sim_data1[1:1000, ]
# Test set
sim_test1 <- sim_data1[1001:1200, ]


# Index variables ("all x") ----------------------------------------------------
index.vars1 <- colnames(sim_data1)[4:9]

## Actual model: y1 (Single index)
# PPR
smimodel1_ppr <- model_smimodel(data = sim_train1, 
                                yvar = "y1",
                                index.vars = index.vars1,
                                initialise = "ppr")
smimodel1_ppr$fit[[1]]$initial
smimodel1_ppr$fit[[1]]$best

# Obtaining predictions on test set
smimodel1_ppr_preds <- predict(object = smimodel1_ppr, newdata = sim_test1)

# Test MSE
test_mse1_ppr <- MSE(.resid = (smimodel1_ppr_preds$y1 - smimodel1_ppr_preds$.predict))
test_mse1_ppr

# Additive
smimodel1_additive <- model_smimodel(data = sim_train1, 
                                     yvar = "y1",
                                     index.vars = index.vars1,
                                     initialise = "additive")
smimodel1_additive$fit[[1]]$initial
smimodel1_additive$fit[[1]]$best

# Obtaining predictions on test set
smimodel1_additive_preds <- predict(object = smimodel1_additive, newdata = sim_test1)

# Test MSE
test_mse1_additive <- MSE(.resid = (smimodel1_additive_preds$y1 - smimodel1_additive_preds$.predict))
test_mse1_additive

# Linear
smimodel1_linear <- model_smimodel(data = sim_train1, 
                                   yvar = "y1",
                                   index.vars = index.vars1,
                                   initialise = "linear")
smimodel1_linear$fit[[1]]$initial
smimodel1_linear$fit[[1]]$best

# Obtaining predictions on test set
smimodel1_linear_preds <- predict(object = smimodel1_linear, newdata = sim_test1)

# Test MSE
test_mse1_linear <- MSE(.resid = (smimodel1_linear_preds$y1 - smimodel1_linear_preds$.predict))
test_mse1_linear

# Multiple
smimodel1_multiple <- model_smimodel(data = sim_train1, 
                                     yvar = "y1",
                                     index.vars = index.vars1,
                                     initialise = "multiple")
smimodel1_multiple$fit[[1]]$initial
smimodel1_multiple$fit[[1]]$best

# Obtaining predictions on test set
smimodel1_multiple_preds <- predict(object = smimodel1_multiple, newdata = sim_test1)

# Test MSE
test_mse1_multiple <- MSE(.resid = (smimodel1_multiple_preds$y1 - smimodel1_multiple_preds$.predict))
test_mse1_multiple


## Actual model: y2 (2-index)
# PPR
smimodel2_ppr <- model_smimodel(data = sim_train1, 
                                yvar = "y2",
                                index.vars = index.vars1,
                                initialise = "ppr")
smimodel2_ppr$fit[[1]]$initial
smimodel2_ppr$fit[[1]]$best

# Obtaining predictions on test set
smimodel2_ppr_preds <- predict(object = smimodel2_ppr, newdata = sim_test1)

# Test MSE
test_mse2_ppr <- MSE(.resid = (smimodel2_ppr_preds$y1 - smimodel2_ppr_preds$.predict))
test_mse2_ppr

# Additive
smimodel2_additive <- model_smimodel(data = sim_train1, 
                                     yvar = "y2",
                                     index.vars = index.vars1,
                                     initialise = "additive")
smimodel2_additive$fit[[1]]$initial
smimodel2_additive$fit[[1]]$best

# Obtaining predictions on test set
smimodel2_additive_preds <- predict(object = smimodel2_additive, newdata = sim_test1)

# Test MSE
test_mse2_additive <- MSE(.resid = (smimodel2_additive_preds$y1 - smimodel2_additive_preds$.predict))
test_mse2_additive

# Linear
smimodel2_linear <- model_smimodel(data = sim_train1, 
                                   yvar = "y2",
                                   index.vars = index.vars1,
                                   initialise = "linear")
smimodel2_linear$fit[[1]]$initial
smimodel2_linear$fit[[1]]$best #Does not give the correct structure of the actual model

# Obtaining predictions on test set
smimodel2_linear_preds <- predict(object = smimodel2_linear, newdata = sim_test1)

# Test MSE
test_mse2_linear <- MSE(.resid = (smimodel2_linear_preds$y1 - smimodel2_linear_preds$.predict))
test_mse2_linear

# Multiple
smimodel2_multiple <- model_smimodel(data = sim_train1, 
                                     yvar = "y2",
                                     index.vars = index.vars1,
                                     initialise = "multiple")
smimodel2_multiple$fit[[1]]$initial
smimodel2_multiple$fit[[1]]$best

# Obtaining predictions on test set
smimodel2_multiple_preds <- predict(object = smimodel2_multiple, newdata = sim_test1)

# Test MSE
test_mse2_multiple <- MSE(.resid = (smimodel2_multiple_preds$y1 - smimodel2_multiple_preds$.predict))
test_mse2_multiple


# Index variables ("all x + all z") --------------------------------------------
index.vars2 <- colnames(sim_data1)[4:15]

## Actual model: y1 (Single index)
# PPR
smimodel11_ppr <- model_smimodel(data = sim_train1, 
                                 yvar = "y1",
                                 index.vars = index.vars2,
                                 initialise = "ppr")
smimodel11_ppr$fit[[1]]$initial
smimodel11_ppr$fit[[1]]$best

# Obtaining predictions on test set
smimodel11_ppr_preds <- predict(object = smimodel11_ppr, newdata = sim_test1)

# Test MSE
test_mse11_ppr <- MSE(.resid = (smimodel11_ppr_preds$y1 - smimodel11_ppr_preds$.predict))
test_mse11_ppr

# Additive
smimodel11_additive <- model_smimodel(data = sim_train1, 
                                      yvar = "y1",
                                      index.vars = index.vars2,
                                      initialise = "additive")
smimodel11_additive$fit[[1]]$initial
smimodel11_additive$fit[[1]]$best

# Obtaining predictions on test set
smimodel11_additive_preds <- predict(object = smimodel11_additive, newdata = sim_test1)

# Test MSE
test_mse11_additive <- MSE(.resid = (smimodel11_additive_preds$y1 - smimodel11_additive_preds$.predict))
test_mse11_additive

# Linear
smimodel11_linear <- model_smimodel(data = sim_train1, 
                                    yvar = "y1",
                                    index.vars = index.vars2,
                                    initialise = "linear")
smimodel11_linear$fit[[1]]$initial
smimodel11_linear$fit[[1]]$best

# Obtaining predictions on test set
smimodel11_linear_preds <- predict(object = smimodel11_linear, newdata = sim_test1)

# Test MSE
test_mse11_linear <- MSE(.resid = (smimodel11_linear_preds$y1 - smimodel11_linear_preds$.predict))
test_mse11_linear

# Multiple
smimodel11_multiple <- model_smimodel(data = sim_train1, 
                                      yvar = "y1",
                                      index.vars = index.vars2,
                                      initialise = "multiple")
smimodel11_multiple$fit[[1]]$initial
smimodel11_multiple$fit[[1]]$best

# Obtaining predictions on test set
smimodel11_multiple_preds <- predict(object = smimodel11_multiple, newdata = sim_test1)

# Test MSE
test_mse11_multiple <- MSE(.resid = (smimodel11_multiple_preds$y1 - smimodel11_multiple_preds$.predict))
test_mse11_multiple


## Actual model: y2 (2-index)
# PPR
smimodel22_ppr <- model_smimodel(data = sim_train1, 
                                 yvar = "y2",
                                 index.vars = index.vars2,
                                 initialise = "ppr")
smimodel22_ppr$fit[[1]]$initial
smimodel22_ppr$fit[[1]]$best

# Obtaining predictions on test set
smimodel22_ppr_preds <- predict(object = smimodel22_ppr, newdata = sim_test1)

# Test MSE
test_mse22_ppr <- MSE(.resid = (smimodel22_ppr_preds$y1 - smimodel22_ppr_preds$.predict))
test_mse22_ppr

# Additive
smimodel22_additive <- model_smimodel(data = sim_train1, 
                                      yvar = "y2",
                                      index.vars = index.vars2,
                                      initialise = "additive")
smimodel22_additive$fit[[1]]$initial
smimodel22_additive$fit[[1]]$best

# Obtaining predictions on test set
smimodel22_additive_preds <- predict(object = smimodel22_additive, newdata = sim_test1)

# Test MSE
test_mse22_additive <- MSE(.resid = (smimodel22_additive_preds$y1 - smimodel22_additive_preds$.predict))
test_mse22_additive

# Linear
smimodel22_linear <- model_smimodel(data = sim_train1, 
                                    yvar = "y2",
                                    index.vars = index.vars2,
                                    initialise = "linear")
smimodel22_linear$fit[[1]]$initial
smimodel22_linear$fit[[1]]$best #Does not give the correct structure of the actual model

# Obtaining predictions on test set
smimodel22_linear_preds <- predict(object = smimodel22_linear, newdata = sim_test1)

# Test MSE
test_mse22_linear <- MSE(.resid = (smimodel22_linear_preds$y1 - smimodel22_linear_preds$.predict))
test_mse22_linear

# Multiple
smimodel22_multiple <- model_smimodel(data = sim_train1, 
                                      yvar = "y2",
                                      index.vars = index.vars2,
                                      initialise = "multiple")
smimodel22_multiple$fit[[1]]$initial
smimodel22_multiple$fit[[1]]$best

# Obtaining predictions on test set
smimodel22_multiple_preds <- predict(object = smimodel22_multiple, newdata = sim_test1)

# Test MSE
test_mse22_multiple <- MSE(.resid = (smimodel22_multiple_preds$y1 - smimodel22_multiple_preds$.predict))
test_mse22_multiple


# Index variables ("some x + all z") --------------------------------
index.vars3 <- colnames(sim_data1)[c(4:6, 10:15)]

## Actual model: y1 (Single index)
# PPR
smimodel111_ppr <- model_smimodel(data = sim_train1, 
                                  yvar = "y1",
                                  index.vars = index.vars3,
                                  initialise = "ppr")
smimodel111_ppr$fit[[1]]$initial
smimodel111_ppr$fit[[1]]$best

# Obtaining predictions on test set
smimodel111_ppr_preds <- predict(object = smimodel111_ppr, newdata = sim_test1)

# Test MSE
test_mse111_ppr <- MSE(.resid = (smimodel111_ppr_preds$y1 - smimodel111_ppr_preds$.predict))
test_mse111_ppr

# Additive
smimodel111_additive <- model_smimodel(data = sim_train1, 
                                       yvar = "y1",
                                       index.vars = index.vars3,
                                       initialise = "additive")
smimodel111_additive$fit[[1]]$initial
smimodel111_additive$fit[[1]]$best

# Obtaining predictions on test set
smimodel111_additive_preds <- predict(object = smimodel111_additive, newdata = sim_test1)

# Test MSE
test_mse111_additive <- MSE(.resid = (smimodel111_additive_preds$y1 - smimodel111_additive_preds$.predict))
test_mse111_additive

# Linear
smimodel111_linear <- model_smimodel(data = sim_train1, 
                                     yvar = "y1",
                                     index.vars = index.vars3,
                                     initialise = "linear")
smimodel111_linear$fit[[1]]$initial
smimodel111_linear$fit[[1]]$best

# Obtaining predictions on test set
smimodel111_linear_preds <- predict(object = smimodel111_linear, newdata = sim_test1)

# Test MSE
test_mse111_linear <- MSE(.resid = (smimodel111_linear_preds$y1 - smimodel111_linear_preds$.predict))
test_mse111_linear

# Multiple
smimodel111_multiple <- model_smimodel(data = sim_train1, 
                                       yvar = "y1",
                                       index.vars = index.vars3,
                                       initialise = "multiple")
smimodel111_multiple$fit[[1]]$initial
smimodel111_multiple$fit[[1]]$best

# Obtaining predictions on test set
smimodel111_multiple_preds <- predict(object = smimodel111_multiple, newdata = sim_test1)

# Test MSE
test_mse111_multiple <- MSE(.resid = (smimodel111_multiple_preds$y1 - smimodel111_multiple_preds$.predict))
test_mse111_multiple


## Actual model: y2 (2-index)
# PPR
smimodel222_ppr <- model_smimodel(data = sim_train1, 
                                  yvar = "y2",
                                  index.vars = index.vars3,
                                  initialise = "ppr")
smimodel222_ppr$fit[[1]]$initial
smimodel222_ppr$fit[[1]]$best

# Obtaining predictions on test set
smimodel222_ppr_preds <- predict(object = smimodel222_ppr, newdata = sim_test1)

# Test MSE
test_mse222_ppr <- MSE(.resid = (smimodel222_ppr_preds$y1 - smimodel222_ppr_preds$.predict))
test_mse222_ppr

# Additive
smimodel222_additive <- model_smimodel(data = sim_train1, 
                                       yvar = "y2",
                                       index.vars = index.vars3,
                                       initialise = "additive")
smimodel222_additive$fit[[1]]$initial
smimodel222_additive$fit[[1]]$best

# Obtaining predictions on test set
smimodel222_additive_preds <- predict(object = smimodel222_additive, newdata = sim_test1)

# Test MSE
test_mse222_additive <- MSE(.resid = (smimodel222_additive_preds$y1 - smimodel222_additive_preds$.predict))
test_mse222_additive

# Linear
smimodel222_linear <- model_smimodel(data = sim_train1, 
                                     yvar = "y2",
                                     index.vars = index.vars3,
                                     initialise = "linear")
smimodel222_linear$fit[[1]]$initial
smimodel222_linear$fit[[1]]$best #Does not give the correct structure of the actual model

# Obtaining predictions on test set
smimodel222_linear_preds <- predict(object = smimodel222_linear, newdata = sim_test1)

# Test MSE
test_mse222_linear <- MSE(.resid = (smimodel222_linear_preds$y1 - smimodel222_linear_preds$.predict))
test_mse222_linear

# Multiple
smimodel222_multiple <- model_smimodel(data = sim_train1, 
                                       yvar = "y2",
                                       index.vars = index.vars3,
                                       initialise = "multiple")
smimodel222_multiple$fit[[1]]$initial
smimodel222_multiple$fit[[1]]$best

# Obtaining predictions on test set
smimodel222_multiple_preds <- predict(object = smimodel222_multiple, newdata = sim_test1)

# Test MSE
test_mse222_multiple <- MSE(.resid = (smimodel222_multiple_preds$y1 - smimodel222_multiple_preds$.predict))
test_mse222_multiple