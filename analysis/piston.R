#
# title     :
# purpose   : Analysis of Piston Model using Gaussian Process
# author    : WD41
# date      : 16.04.2017
#

# Load Required Libraries -----------------------------------------------------
library(DiceKriging)
library(ggplot2)

source("../simulation/r_test_functions/piston.R")
source("./r-scripts/evalRMSE.R")  # to compute relative root mean squared error
source("./r-scripts/evalQ2.R")    # to compute coefficient of determination

# Load the required data ------------------------------------------------------
input_names <- c()
for (i in 1:7) input_names <- c(input_names, paste("x", i, sep = ""))

xx_tests <- read.csv("../simulation/design/7params/lhs_5000_7.csv", header = F)
names(xx_tests) <- input_names
y_tests <- evalOutputPiston(rescaleInputPiston(xx_tests))

# Exploratory -------
hist(y_tests)
hist(1/y_tests)
hist(log(y_tests))
hist(sqrt(y_tests))

# The modeling choices --------------------------------------------------------
transf_names <- c("original", "log", "sqrt", "inv")
num_smpls <- c(16L, 32L, 64L, 128L)
cov_types <- c("exp", "gauss", "powexp", "matern3_2", "matern5_2")
doe_names <- c("srs", "lhs", "lhs-opt", "sobol")
n_reps <- 25L

# Initialize the dataframe ----------------------------------------------------
err_piston <- data.frame(q2 = c(), 
                         cov_type = c(),
                         doe = c(),
                         reg = c(),
                         n = c())

# Loop over modeling choices to construct metamodel ---------------------------
# Loop over type of output transformation, number of training samples, 
# type of design, regression term, covariance types, and replications. 
# Note that for the regression term, the full interaction model is fitted only
# for the last two sample sizes, 64 and 128.
# Fewer number of samples are known to fail

# Loop over several possible output transformations
for (transf_name in transf_names)
{
    # Decide which tranformation and its inverse based on the name
    if (transf_name == "log")
    {
        # Log transformation
        transf <- log
        inv_transf <- exp
    } else if (transf_name == "sqrt")
    {
        # Square root transformation
        transf <- sqrt
        inv_transf <- function(x) x^2
    } else if (transf_name == "inv")
    {
        # Inverse (aka reciprocal) transformation
        transf <- function(x) 1/x
        inv_transf <- transf
    } else
    {
        # Identity (aka original) transformation
        transf <- identity
        inv_transf <- identity
    }
    
    # Loop over several training sample size
    for (num_smpl in num_smpls)
    {
        # Decide which regression terms are available
        if (num_smpl < 64)
        {
            reg_forms <- c("~0", "~.")
            reg_names <- c("constant", "full_linear")
        } else
        {
            reg_forms <- c("~0", "~.", "~.^2")
            reg_names <- c("constant", "full_linear", "full_linear+int")
        }
        
        # Loop over several designs of experiment
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
                                                "_7_", j, ".csv", 
                                                sep = "")
                        train_fullname <- paste("../simulation/design/7params/", 
                                                train_filename, 
                                                sep = "")
                        xx_train <- read.csv(train_fullname, header = F)
                        # Evaluate output with rescaled input
                        y_train <- evalOutputPiston(rescaleInputPiston(xx_train))
                        # Give names to the input dataset
                        names(xx_train) <- input_names
                        
                        # Train the model
                        m <- km(as.formula(reg_forms[i]),
                                design = xx_train, response = transf(y_train), 
                                covtype = cov_type,
                                control = list(pop.size = 100, trace = F))
                        
                        # Make prediction using metamodel and transform back
                        y_preds <- inv_transf(
                            predict(m, newdata=xx_tests, type="UK")$mean)
                        
                        # Compute Predictivity Coefficient
                        err_piston <- rbind(err_piston,
                                            data.frame(
                                                rmse = evalRMSE(y_tests, 
                                                                y_preds),
                                                q2 = evalQ2(y_tests, 
                                                            y_preds),
                                                cov_type = cov_type,
                                                doe = doe_name,
                                                reg = reg_names[i],
                                                n = num_smpl,
                                                transf = transf_name)) 
                        
                        print(paste(transf_name, num_smpl, doe_name, reg_names[i], cov_type, j))
                    }
                }
            }
        }
    }
}

saveRDS(err_piston, "err_piston.rds")

# Do some cleanup -------------------------------------------------------------

