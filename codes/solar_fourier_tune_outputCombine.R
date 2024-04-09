# Loading required packages ----------------------------------------------------
library(dplyr)
library(fabletools)
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

# Additional functions required ------------------------------------------------
# Function to combine outputs into a list
combRDS_fourier <- function(pathList, numFourier){
  ll <- length(pathList)
  smimodel1 <- vector(mode = "list", length = length(numFourier))
  num_fourier1 <- vector(mode = "list", length = length(numFourier))
  for(i in 1:ll){
    bb <- readRDS(pathList[i])
    hh <- bb$num_fourier
    hh_ind <- which(numFourier == hh)
    smimodel1[[hh_ind]] <- bb$fitted_model
    num_fourier1[[hh_ind]] <- bb$num_fourier
  }
  result <- list("num_fourier" = num_fourier1, "smimodel" = smimodel1)
  return(result)
}

# Function to calculate and compare test MSEs
min_testMSE <- function(models.list, newdata, yvar, recursive_colRange){
  testMSE <- vector(mode = "list", length = length(models.list$num_fourier))
  for(j in 1:length(models.list$num_fourier)){
    # Obtain predictions
    preds <- predict(object = models.list$smimodel[[j]], newdata = newdata,
                     recursive = TRUE, recursive_colRange = recursive_colRange)
    # Test MSE
    testMSE[[j]] <- MSE(.resid = (preds[ , {{yvar}}][[1]] - preds$.predict))
  }
  min_ind <- which.min(testMSE)
  min_num_fourier <- models.list$num_fourier[[min_ind]]
  min_MSE <- testMSE[[min_ind]]
  min_smimodel <- models.list$smimodel[[min_ind]]
  output <- list("min_num_fourier" = min_num_fourier,
                 "min_MSE" = min_MSE,
                 "min_smimodel" = min_smimodel,
                 "testMSE_list" = testMSE)
  return(output)
}

# PPR starting point -----------------------------------------------------------
# Reading and combining outputs
pathList_ppr <- list.files(path="./smimodelSolar_fourier_tune_ppr_results/results",
                           pattern = ".rds$", full.names = TRUE)
# Replace "path" appropriately in the above line of code.
combModels_ppr <- combRDS_fourier(pathList_ppr, numFourier)

# Select number of fourier terms
minimum_error_ppr <- min_testMSE(models.list = combModels_ppr,
                                 newdata = solar_test,
                                 yvar = "Solar_lag_000",
                                 recursive_colRange = 23:25)
minimum_error_ppr$min_num_fourier
minimum_error_ppr$min_MSE

# Linear starting point --------------------------------------------------------
# Reading and combining outputs
pathList_linear <- list.files(path="./smimodelSolar_fourier_tune_linear_results/results",
                              pattern = ".rds$", full.names = TRUE)
# Replace "path" appropriately in the above line of code.
combModels_linear <- combRDS_fourier(pathList_linear, numFourier)

# Select number of fourier terms
minimum_error_linear <- min_testMSE(models.list = combModels_linear,
                                    newdata = solar_test,
                                    yvar = "Solar_lag_000",
                                    recursive_colRange = 23:25)
minimum_error_linear$min_num_fourier
minimum_error_linear$min_MSE

# Additive starting point ------------------------------------------------------
# The optimisation was run with MIPGap = 5e-2, TimeLimit = 600 due to 
# computational time being longer for some models.

# Reading and combining outputs
pathList_additive <- list.files(path="./smimodelSolar_fourier_tune_additive_limit_results/results",
                                pattern = ".rds$", full.names = TRUE)
# Replace "path" appropriately in the above line of code.
combModels_additive <- combRDS_fourier(pathList_additive, numFourier)

# Select number of fourier terms
minimum_error_additive <- min_testMSE(models.list = combModels_additive,
                                      newdata = solar_test,
                                      yvar = "Solar_lag_000",
                                      recursive_colRange = 23:25)
minimum_error_additive$min_num_fourier
minimum_error_additive$min_MSE

