#
# title     :
# purpose   : Analysis of Sandia Lab. Thermal Problem using Gaussian Process
# author    : WD41
# date      : 18.04.2017
#
# Load Required Libraries -----------------------------------------------------
library(DiceKriging)

source("../simulation/r_test_functions/sandiatherm.R")

# Global variables ------------------------------------------------------------
# Input names as string
input_names <- c()
for (i in 1:2) input_names <- c(input_names, paste("x", i, sep = ""))

# Test data set
xx_tests <- read.csv("../simulation/design/2params/lhs_5000_2.csv", header = F)
names(xx_tests) <- input_names

# Sandia Thermal Challenge Problem common variables
t_s <- seq(0, 1000.0, by = 100)     # Time grid
x0  <- 0.0                          # Surface ([m])
T0 <- 25                            # Initial temperature ([K])
q <- c(1000, 2000, 3000, 3500)      # Selection of surface heat flux ([W.m-2])
L   <- c(1.27, 1.90, 2.54) * 1e-2   # Selection for the slab length ([m])

# Gaussian Process metamodel design choices
cov_types <- c("exp", "gauss", "powexp", "matern3_2", "matern5_2")
reg_form <- "~0"
doe_name <- "srs"
num_smpl <- 4
num_reps <- 25
reg_name <- "constant"
transf_name <- "original"
transf <- identity
inv_transf <- identity

# Experimental Configuration --------------------------------------------------
exp_confs <- list() 
exp_confs[[1]] <- c(q[1], L[1]) # Ensemble experiment 1
exp_confs[[2]] <- c(q[1], L[3]) # Ensemble experiment 2
exp_confs[[3]] <- c(q[3], L[1]) # Ensemble experiment 3
exp_confs[[4]] <- c(q[2], L[3]) # Ensemble experiment 4
exp_confs[[5]] <- c(q[3], L[2]) # Accreditation experiment
exp_confs[[6]] <- c(q[4], L[2]) # Regulatory compliance

# Initialize the dataframe ----------------------------------------------------
err_sandia <- data.frame(pc1_q2 = c(), pc1_rmse = c(),
                         pc2_q2 = c(), pc2_rmse = c(),
                         pc3_q2 = c(), pc3_rmse = c(),
                         rec_err = c(),
                         cov_type = c(),
                         doe = c(),
                         reg = c(),
                         n = c(),
                         transf_name = c())

# Loop over covariance types -----
for (cov_type in cov_types)
{
    # Loop over replications -----
    for (num_rep in 1:num_reps) 
    {
        # Read input
        train_filename <- paste(doe_name, "_", num_smpl, 
                                "_2_", num_rep, ".csv", 
                                sep = "")
        train_fullname <- paste("../simulation/design/2params/", 
                                train_filename, 
                                sep = "")
        xx_train <- read.csv(train_fullname, header = F)
        # Give names to the input dataset
        names(xx_train) <- input_names
        # Evaluate output with rescaled input
        y_train <- evalOutputFranke(xx_train)
        
        # Temporary placeholders
        rec_error <- c()    # reconstruction error
        pc1_preds <- c()    # predicted PC1 score
        pc1_tests <- c()    # test data PC1 score
        pc2_preds <- c()    # predicted PC2 score
        pc2_tests <- c()    # test data PC2 score
        pc3_preds <- c()    # predicted PC3 score
        pc3_tests <- c()    # test data PC3 score
        
        # Loop Over Experimental Configurations -----
        for (exp_conf in exp_confs)
        {
            # Compute the raw test and training output
            yy_tests <- evalOutputSandiaThermal(rescaleInputSandiaThermal(xx_tests), 
                                                t_s, x0, T0, exp_conf[1], exp_conf[2])
            
            yy_train <- evalOutputSandiaThermal(rescaleInputSandiaThermal(xx_train), 
                                                t_s, x0, T0, exp_conf[1], exp_conf[2])
            
            # Carry out SVD for Principal Component Analysis
            # Centered and scaled the raw output (standardized)
            yy_train_std <- scale(yy_train[,2:11], scale = T)
            yy_tests_centered <- sweep(yy_tests[,2:11], MARGIN = 2, 
                                       attr(yy_train_std, "scaled:center"), 
                                       `-`)
            yy_tests_std <- sweep(yy_tests_centered,
                                  MARGIN = 2,
                                  attr(yy_train_std, "scaled:scale"),
                                  `/`)# Remove the first column
            # Carry Out SVD
            yy_tests_svd <- svd(yy_tests_std)
            yy_train_svd <- svd(yy_train_std)
            # Construct Singular Values Matrix
            D <- diag(yy_train_svd$d)
            # Compute the Principal Component Scores
            pcs_train <- yy_train_svd$u %*% D
            # Project test data using the PC from training
            pcs_tests <- yy_tests_std %*% yy_train_svd$v
            
            km_list <- list()
            pcs_preds <- matrix(0, nrow = dim(xx_tests)[1], ncol = 3)
            
            # Train the Kriging metamodel (Retain 3 based on a preli. analysis)
            # Loop Over Principal Component metamodel -----
            for (i in 1:3) 
            {
                # Train the metamodel
                km_list[[i]] <- km(as.formula(reg_form),
                                   design = xx_train, 
                                   response = transf(pcs_train[,i]), 
                                   covtype = cov_type,
                                   control = list(pop.size = 100, trace = F),
                                   nugget = 1e-8 * var(pcs_train[,i]))
                # Compute the prediction on test data
                pcs_preds[,i] <- inv_transf(predict(km_list[[i]], 
                                                    newdata=xx_tests, 
                                                    type="UK")$mean)
            }
            
            # Reconstruct time-dependent temperature
            dev_matrix <- sweep(pcs_preds[,1:3] %*% t(yy_train_svd$v[,1:3]),
                                MARGIN = 2,
                                attr(yy_train_std, "scaled:scale"),
                                `*`)
            ave_matrix <- matrix(replicate(dim(xx_tests)[1], 
                                           attr(yy_train_std, "scaled:center")),
                                 ncol = dim(yy_tests_std)[2], byrow = T)
            rec_matrix <- ave_matrix + dev_matrix
            
            # Compute the reconstruction error
            rec_error <- c(rec_error, yy_tests[,2:11] - rec_matrix)
            
            # Compile results from all experimental configurations
            pc1_tests <- c(pc1_tests, pcs_tests[,1])
            pc2_tests <- c(pc2_tests, pcs_tests[,2])
            pc3_tests <- c(pc3_tests, pcs_tests[,3])
            pc1_preds <- c(pc1_preds, pcs_preds[,1])
            pc2_preds <- c(pc2_preds, pcs_preds[,2])
            pc3_preds <- c(pc3_preds, pcs_preds[,3])
        }
        
        # Compute the Q2 and RMSE for the PC score prediction and the Recon. Error
        err_sandia <- rbind(err_sandia,
                            data.frame(
                                pc1_q2 = evalQ2(pc1_tests, pc1_preds),
                                pc2_q2 = evalQ2(pc2_tests, pc2_preds),
                                pc3_q2 = evalQ2(pc3_tests, pc3_preds),
                                pc1_rmse = evalRMSE(pc1_tests, pc1_preds),
                                pc2_rmse = evalRMSE(pc2_tests, pc2_preds),
                                pc3_rmse = evalRMSE(pc3_tests, pc3_preds),
                                rec_err = sqrt(mean(rec_error^2)),
                                cov_type = cov_type,
                                doe = doe_name,
                                reg = reg_name,
                                n = num_smpl,
                                transf = transf_name
                            ))
        print(paste(transf_name, num_smpl, doe_name, reg_name, cov_type, num_rep))
    }
}

# Exploratory Plot ------------------------------------------------------------
plot(pcs_tests[,1], pcs_preds[,1])
abline(0,1)
sqrt(mean(rec_error^2))
# Compute the Reconstruction Error
evalRMSE(y_test = pcs_tests[,1], pcs_preds[,1])
evalRMSE(y_test = pcs_tests[,2], pcs_preds[,2])
evalRMSE(y_test = pcs_tests[,3], pcs_preds[,3])

evalQ2(y_tests, y_preds)

plot(t_s, yy_tests[300,])
lines(t_s[2:11], rec_matrix[300,])
rec_matrix[1,]
yy_tests[,2:11] - rec_matrix
#(yy_train_svd$d / sqrt(7))^2 / sum((yy_train_svd$d / sqrt(7))^2)

p <- ggplot(data = err_sandia) #subset(err_sandia, n == c(32)  & doe == "lhs-opt" & transf == "sqrt"))
p1 <- p + geom_point(mapping = aes(x=cov_type, y = pc1_rmse))
p1
p1 <- p + geom_jitter(mapping = aes(x=cov_type, y = rmse), width=0.10) + facet_grid(reg ~ transf)
p1 <- p + geom_point(mapping = aes(x=cov_type, y = rmse)) + facet_grid(. ~ reg)
