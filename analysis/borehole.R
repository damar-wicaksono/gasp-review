#
# title     : borehole.R
# purpose   : Analysis of Borehole function using Gaussian Process
# author    : WD41
# date      : 13.04.2017
#
# Load Required Libraries -----------------------------------------------------
library(DiceKriging)
library(ggplot2)
source("../simulation/r_test_functions/borehole.R")

# Load Required Data ----------------------------------------------------------
input_names <- c()
for (i in 1:8) input_names <- c(input_names, paste("x", i, sep=""))

xx_tests <- read.csv("../simulation/design/8params/lhs_5000_8.csv", header = F)
names(xx_tests) <- input_names
xx_tests_resc <- data.frame(rescaleInputBorehole(xx_tests))
y_tests <- evalOutputBorehole(rescaleInputBorehole(xx_tests))

# The modeling choices --------------------------------------------------------
transf_names <- c("original", "log", "sqrt")
num_smpls <- c(16, 32, 64, 128)
reg_forms <- c("~0", "~.", "~.^2")
reg_names <- c("constant", "full_linear", "full_linear+int")
cov_types <- c("exp", "gauss", "powexp", "matern3_2", "matern5_2")
doe_names <- c("srs", "lhs", "lhs-opt", "sobol")
n_reps <- 25

# Initialize dataframe --------------------------------------------------------
err_borehole <- data.frame(rmse = c(),
                           q2 = c(),
                           cov_type = c(),
                           doe = c(),
                           reg = c(),
                           n = c(),
                           transf = c())

# Loop over modeling choices to construct metamodel ---------------------------
# Loop over type of output transformation, number of training samples, 
# type of design, regression term, covariance types, and replications. 
# Note that for the regression term, the full interaction model is fitted only
# for the last two sample sizes, 64 and 128.
# Fewer number of samples are known to fail
for (transf_name in transf_names)
{
    # Loop over several possible output transformations
    if (transf_name == "log")
    {
        transf <- log
        inv_transf <- exp
    } else if (transf_name == "sqrt")
    {
        trasnf <- sqrt
        inv_transf <- function(x) x^2
    } else
    {
        transf <- identity
        inv_transf <- identity
    }
    
    for (num_smpl in num_smpls)
    {
        # Loop over several training sample size
        if (num_smpl < 64)
        {
            reg_forms <- c("~0", "~.")
            reg_names <- c("constant", "full_linear")
        } else
        {
            reg_forms <- c("~0", "~.", "~.^2")
            reg_names <- c("constant", "full_linear", "full_linear+int")
        }
        for (doe_name in doe_names)
        {
            # Loop over regression term
            for (i in 1:length(reg_forms))
            {
                # Loop over covariance type
                for (cov_type in cov_types)
                {
                    # Loop over replications
                    for (j in 1:n_reps)
                    {
                        # Read input
                        train_filename <- paste(doe_name, "_", num_smpl, 
                                                "_8_", j, ".csv", 
                                                sep = "")
                        train_fullname <- paste("../simulation/design/8params/", 
                                                train_filename, 
                                                sep = "")
                        xx_train <- read.csv(train_fullname, header = F)
                        # Evaluate output with rescaled input
                        y_train <- evalOutputBorehole(rescaleInputBorehole(xx_train))
                        # Give names to the input dataset
                        names(xx_train) <- input_names
                        
                        # Train the model
                        m <- km(as.formula(reg_forms[i]),
                                design = xx_train, response = transf(y_train), 
                                covtype = cov_type,
                                control = list(pop.size = 100, trace = T))
                        
                        # Make prediction using metamodel
                        y_preds <- predict(m, newdata=xx_tests, type="UK")$mean
                        
                        # Compute Predictivity Coefficient
                        err_borehole <- rbind(err_borehole,
                                              data.frame(
                                                  rmse = evalErmse(y_tests, 
                                                                   inv_transf(y_preds)),
                                                  q2 = evalQ2(y_tests, 
                                                              inv_transf(y_preds)),
                                                  cov_type = cov_type,
                                                  doe = doe_name,
                                                  reg = reg_names[i],
                                                  n = num_smpl,
                                                  transf = transf_name)) 
                        
                    }
                }
            }
        }
    }
}

saveRDS(err_borehole, "err_borehole.rds")
