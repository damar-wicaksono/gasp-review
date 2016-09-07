#
# title     : run-damped_cosine.R
# purpose   : Evaluate the damped cosine function according to the design 
#           : matrix
# author    : WD41, LRS/EPFL/PSI
# date      : September 2016
#

# Load the Required Packages --------------------------------------------------
source("./r-scripts/damped_cosine.R")

# Initialize the parameter of the simulation experiment -----------------------
dm_methods <- c("srs", "lhs", "sobol")  # Design of Experiment method
n_samples <- c(4, 8, 16, 32)            # Number of Samples
n_params <- 1                           # Number of Parameters
n_reps <- 25                            # Number of Replications
dm_folder <- "./dmfiles"
y_folder <- "./outputs"

x_testset_fullname <- "./dmfiles/lhs_300_1-test.csv"
y_testset_fullname <- "./outputs/lhs_300_1-test_output.csv"

# Loop over all the design matrices  ------------------------------------------

for (dm_method in dm_methods) {
    # Loop over the method of generations
    for (n_sample in n_samples) {
        # Loop over all the sample size
        for (rep in 1:n_reps) {
            # Loop over all the replications
        
            # Create fullname of the design matrix
            dm_filename <- paste(dm_method, n_sample, n_params, "rep-", 
                                 sep = "_")
            dm_filename <- paste(dm_filename, rep, sep = "")
            dm_filename <- paste(dm_filename, "csv", sep = ".")
            dm_fullname <- paste(dm_folder, dm_method, dm_filename, sep = "/")
            
            # Read the design matrix
            x <- as.matrix(read.csv(dm_fullname, header = F, col.names = "x"))
            
            # Compute the damped cosine function value
            y <- damped_cosine(x)
            
            # Prepare the output file filename
            y_filename <- paste(dm_method, n_sample, n_params, "rep-", 
                                sep = "_")
            y_filename <- paste(y_filename, rep, sep = "")
            y_filename <- paste(y_filename, "_output", sep = "")
            y_filename <- paste(y_filename, "csv", sep = ".")
            # Create folder if it does not exist
            dir.create(paste(y_folder, dm_method, sep = "/"), showWarnings = F)
            y_fullname <- paste(y_folder, dm_method, y_filename, sep = "/")
            # Write the results into file
            write.table(y, y_fullname, sep = ",", col.names = F, row.names = F)
        }
    }
}

# Compute validation set ------------------------------------------------------
x_valid <- as.matrix(read.csv(x_testset_fullname, header = F, col.names = "x"))
y_valid <- damped_cosine(x)
write.table(y_valid, y_testset_fullname, sep = ",", 
            col.names = F, row.names = F)
