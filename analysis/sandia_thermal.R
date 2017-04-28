#
# title     :
# purpose   : Analysis of Sandia Lab. Thermal Problem using Gaussian Process
# author    : WD41
# date      : 18.04.2017
#
# Load Required Libraries -----------------------------------------------------
library(DiceKriging)

source("../simulation/r_test_functions/sandiatherm.R")

# Load the required data ------------------------------------------------------
input_names <- c()
for (i in 1:2) input_names <- c(input_names, paste("x", i, sep = ""))

xx_tests <- read.csv("../simulation/design/2params/lhs_5000_2.csv", header = F)
names(xx_tests) <- input_names



# Training --------------
t_s <- seq(0, 1000.0, by = 100)
x0  <- 0.0
T0 <- 25 # 
q <- c(1000, 2000, 3000, 3500)
L   <- c(1.27, 1.90, 2.54) * 1e-2

yy_train <- evalOutputSandiaThermal(rescaleInputSandiaThermal(xx_train), 
                                    t_s, x0, 25, q[1], L[1])
plot(t_s, yy_train[1,], ylim = c(25, 1200), type = "l")
for (i in 2:8) lines(t_s, yy_train[i,])

yy_tests <- evalOutputSandiaThermal(rescaleInputSandiaThermal(xx_tests), 
                                    t_s, x0, 25, q[1], L[1])

cov_type <- "gauss"
reg_form <- "~0"


# Experimental Configuration
exp_confs <- list() 
exp_confs[[1]] <- c(q[1], L[1]) 
exp_confs[[2]] <- c(q[1], L[3])
exp_confs[[3]] <- c(q[3], L[1])
exp_confs[[4]] <- c(q[2], L[3])
exp_confs[[5]] <- c(q[4], L[2])

rec_rmse <- c()
doe_name <- "srs"
num_smpl <- 8
num_reps <- 25
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

    rec_error <- c()
    # Loop Over Configuration -----
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
                               design = xx_train, response = pcs_train[,i], 
                               covtype = cov_type,
                               control = list(pop.size = 100, trace = F),
                               nugget = 1e-8 * var(pcs_train[,i]))
            # Compute the prediction on test data
            pcs_preds[,i] <- predict(km_list[[i]], 
                                     newdata=xx_tests, 
                                     type="UK")$mean
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
    }
    rec_rmse <- c(rec_rmse, sqrt(mean(rec_error^2)))
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

plot(t_s, yy_tests[3,])
lines(t_s[2:11], rec_matrix[3,])
rec_matrix[1,]
yy_tests[,2:11] - rec_matrix
#(yy_train_svd$d / sqrt(7))^2 / sum((yy_train_svd$d / sqrt(7))^2)


