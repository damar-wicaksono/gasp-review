#
# title     : commonDataPiston.R
# purpose   : R Script to construct a dataframe for making plots on piston
#           : model
# author    : WD41, LRS/EPFL/PSI
# date      : Feb. 2016
#
# Set Random Seed Number ------------------------------------------------------
set.seed(2345890)

# Load Required Library -------------------------------------------------------
source("../simulation/r_test_functions/piston.R") # The piston model function

# Create Dataframe ------------------------------------------------------------
n_samples <- 200
input_names <- c("M", "S", "V[0]", "k", "P[0]", "T[amb]", "T[0]")

# Create random matrix from uniform distribution (0, 1)
xx <- replicate(7, runif(n_samples, 0, 1))

# Rescale the input
xx_rescaled <- rescaleInputPiston(xx)

# Evaluate the cycle time
cycle_time <- evalOutputPiston(xx_rescaled)

# Create Dataframe for tidy plotting, don't use rescaled input
piston_df <- data.frame(x = xx[,1], y = cycle_time, input = input_names[1])
for (i in 2:7)
{
    piston_df <- rbind(piston_df, 
                       data.frame(x = xx[,i], 
                                  y = cycle_time, 
                                  input = input_names[i]))
}
