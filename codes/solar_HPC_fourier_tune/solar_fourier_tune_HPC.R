# Loading required packages ----------------------------------------------------
library(dplyr)
library(fabletools)
library(forecast)
library(lubridate)
library(ROI)
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

# greedy -----------------------------------------------------------------------
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

# Index variables
index.vars <- colnames(solarData)[23:45]

# Number of fourier terms (pairs)
num_fourier <- 1:10

#mm <- 1 #If running within R uncomment this.  This will fit the model with 
#number of fourier terms as specfied in the first element of the vector "num_fourier".
mm <- as.numeric(commandArgs()[[6]]) # If running array jobs in HPC uncomment this.

# Linear variables
linear.vars <- colnames(solarData)[2:(2 + (mm * 2) - 1)]

output_mm <- greedy_smimodel(data = solar_train,
                             val.data = solar_val,
                             yvar = "Solar_lag_000",
                             index.vars = index.vars,
                             initialise = "linear", 
                             linear.vars = linear.vars,
                             lambda0_seq = lambda0, 
                             lambda2_seq = lambda2, 
                             lambda_step = 1,
                             lambda0_start_seq = lambda0_start, 
                             lambda2_start_seq = lambda2_start,
                             refit = FALSE, # As we want to compare validation 
                             # set MSEs for Fourier term selection
                             recursive = TRUE,
                             recursive_colRange = 23:25)

output_final <- list("fitted_model" = output_mm, "num_fourier" = mm)

# Saving output
saveRDS(output_final, file = paste0('./results/modelLinear_',mm,'.rds'))