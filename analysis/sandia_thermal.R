#
# title     : sandia_thermal.R
# purpose   : Analysis of Sandia Lab. Thermal Problem using Gaussian Process
# author    : WD41
# date      : 18.04.2017
#
# Load Required Libraries -----------------------------------------------------
library(DiceKriging)
library(foreach)
library(doParallel)

source("../simulation/r_test_functions/sandiatherm.R")
source("./r-scripts/evalQ2.R")
source("./r-scripts/evalRMSE.R")
source("./r-scripts/evalErrorSandia.R")
source("./r-scripts/selectRegression.R")

# Prepare parallel processing
cores <- detectCores()
cl <- makeCluster(cores[1] - 3) # not to overload your computer
registerDoParallel(cl)

# Global variables ------------------------------------------------------------
# Input names as string
input_names <- c()
for (i in 1:2) input_names <- c(input_names, paste("x", i, sep = ""))

# Read test data set
xx_tests <- read.csv("../simulation/design/2params/lhs_5000_2.csv", header = F)
names(xx_tests) <- input_names

# Sandia Thermal Challenge Problem common variables
t_s <- seq(0, 1000.0, by = 100)     # Time grid
x0  <- 0.0                          # Surface ([m])
T0 <- 25                            # Initial temperature ([degC])
q <- c(1000, 2000, 3000, 3500)      # Selection of surface heat flux ([W.m-2])
L   <- c(1.27, 1.90, 2.54) * 1e-2   # Selection for the slab length ([m])

# Gaussian Process metamodel design choices
doe_names <- c("srs", "lhs", "lhs-opt", "sobol")
cov_types <- c("exp", "gauss", "powexp", "matern3_2", "matern5_2")
reg_names <- c("constant", "full_linear", "full_linear+int")
transf_names <- c("original", "sqrt", "log")
num_smpls <- c(4L, 8L, 16L, 32L)
num_reps <- 25

# Experimental Configuration --------------------------------------------------
exp_confs <- list() 
exp_confs[[1]] <- c(q[1], L[1]) # Ensemble experiment 1
exp_confs[[2]] <- c(q[1], L[3]) # Ensemble experiment 2
exp_confs[[3]] <- c(q[3], L[1]) # Ensemble experiment 3
exp_confs[[4]] <- c(q[2], L[3]) # Ensemble experiment 4
exp_confs[[5]] <- c(q[3], L[2]) # Accreditation experiment
exp_confs[[6]] <- c(q[4], L[2]) # Regulatory compliance

# Create a long list of all combinations of possible design choices -----------
input_lists <- list()
i <- 1
for (transf_name in transf_names)
{
    for (num_smpl in num_smpls)
    {
        # Decide which regression terms are available
        if (num_smpl < 8)
        {
            reg_names <- c("constant")
        } else if (num_smpl < 16)
        {
            reg_names <- c("constant", "full_linear")
        } else
        {
            reg_names <- c("constant", "full_linear", "full_linear+int")
        }
        
        for (doe_name in doe_names)
        {
            for (reg_name in reg_names)
            {
                for (cov_type in cov_types)
                {
                    for (num_rep in 1:num_reps)
                    {
                        input_lists[[i]] <- list()
                        input_lists[[i]][[1]] <- t_s
                        input_lists[[i]][[2]] <- x0
                        input_lists[[i]][[3]] <- T0
                        input_lists[[i]][[4]] <- exp_confs
                        input_lists[[i]][[5]] <- num_smpl
                        input_lists[[i]][[6]] <- num_rep
                        input_lists[[i]][[7]] <- cov_type
                        input_lists[[i]][[8]] <- reg_name
                        input_lists[[i]][[9]] <- doe_name
                        input_lists[[i]][[10]] <- "../simulation/design/2params/"
                        input_lists[[i]][[11]] <- transf_name
                        i <- i + 1
                    }
                }    
            }
        }
    }
}

# Create a flat wrapper function ----------------------------------------------
evalErrorSandiaFlat <- function(input_list)
{
    return(evalErrorSandia(input_list[[1]], input_list[[2]], input_list[[3]],
                           input_list[[4]], input_list[[5]], input_list[[6]],
                           input_list[[7]], input_list[[8]], input_list[[9]],
                           input_list[[10]], input_list[[11]]))
}

# Parallel Processing ---------------------------------------------------------
err_sandia <- foreach(i = 1:length(input_lists),
                      .combine='rbind',
                      .packages="DiceKriging") %dopar%
{
    temp_df <- evalErrorSandiaFlat(input_lists[[i]])
    temp_df
}

# Stop Cluster ----------------------------------------------------------------
stopCluster(cl)

# Save the result into R object -----------------------------------------------
saveRDS(err_sandia, "err_sandia.rds")

# Exploratory Plot ------------------------------------------------------------
p <- ggplot(data = subset(err_sandia, reg == "constant"))
p1 <- p + geom_boxplot(mapping = aes(x=cov_type, y = pc1_rmse)) + facet_grid(. ~ doe)
p1
p1 <- p + geom_jitter(mapping = aes(x=cov_type, y = rmse), width=0.10) + facet_grid(reg ~ transf)
p1 <- p + geom_point(mapping = aes(x=cov_type, y = rmse)) + facet_grid(. ~ reg)
