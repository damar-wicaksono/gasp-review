#' Compute the statistics of error for Sandia thermal challenge metamodel
#'    
#' Three error statistics are included: Q2 and RMSE (for each of the 3 
#' principal component scores) and Reconstruction Error (back in physical
#' space, i.e., temperature)
#' The results are aggregated for all experimental configuration (i.e.,
#' ensemble, accreditation, and regulatory)
#'
#' @param t_s vector of time grid at which the temperature is evaluated
#' @param x0 the location at which the temperature is evaluated ([m])
#' @param T0 the initial surface temperature in [degC]
#' @param exp_confs the list of experimental configurations (length and flux)
#' @param num_smpl the number of training samples (integer)
#' @param num_rep  the number of which replication (integer)
#' @param cov_type the covariance function type (character)
#' @param reg_name the name of the regression formula (character)
#' @param doe_name the name of the design of experiment type (character)
#' @param doe_dir  the directory location of the DOE (character)
#' @param transf_name the name of the transformation (character)
#' @return a single-element dataframe with the error statistics
evalErrorSandia <- function(t_s, x0, T0, exp_confs, num_smpl, num_rep, 
                            cov_type, reg_name, doe_name, doe_dir, transf_name)
{
    # Read input
    train_filename <- paste(doe_name, "_", num_smpl, 
                            "_2_", num_rep, ".csv", 
                            sep = "")
    train_fullname <- paste(doe_dir, 
                            train_filename, 
                            sep = "")
    xx_train <- read.csv(train_fullname, header = F)
    # Give names to the input dataset
    input_names <- c()
    for (i in 1:dim(xx_train)[2]) input_names <- c(input_names, 
                                                   paste("x", i, sep = ""))
    names(xx_train) <- input_names

    # Temporary placeholders
    rec_error <- c()    # reconstruction error
    pc1_preds <- c()    # predicted PC1 score
    pc1_tests <- c()    # test data PC1 score
    pc2_preds <- c()    # predicted PC2 score
    pc2_tests <- c()    # test data PC2 score
    pc3_preds <- c()    # predicted PC3 score
    pc3_tests <- c()    # test data PC3 score
    
    # Select Transformation
    transf <- selectTransf(transf_name) 
    inv_transf <- selectInvTransf(transf_name)
    
    # Select Regression
    reg_form <- selectRegression(reg_name)
    
    # Loop Over Experimental Configurations -----
    for (exp_conf in exp_confs)
    {
        # Compute the raw test and training output
        yy_tests <- evalOutputSandiaThermal(rescaleInputSandiaThermal(xx_tests), 
                                            t_s, x0, T0, 
                                            exp_conf[1], exp_conf[2])
        
        yy_train <- evalOutputSandiaThermal(rescaleInputSandiaThermal(xx_train), 
                                            t_s, x0, T0, 
                                            exp_conf[1], exp_conf[2])
        
        # Carry out SVD for Principal Component Analysis
        # Centered and scaled the raw output (standardized)
        # Remove the first column because there's no variance at t = 0 [s]
        yy_train_std <- scale(yy_train[,2:11], scale = T)
        yy_tests_centered <- sweep(yy_tests[,2:11], MARGIN = 2, 
                                   attr(yy_train_std, "scaled:center"), 
                                   `-`)
        yy_tests_std <- sweep(yy_tests_centered,
                              MARGIN = 2,
                              attr(yy_train_std, "scaled:scale"),
                              `/`)
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
                               control = list(pop.size = 1000, trace = F),
                               nugget = 1e-8 * var(pcs_train[,i]))
            # Compute the prediction on test data
            pcs_preds[,i] <- inv_transf(predict(km_list[[i]], 
                                                newdata=xx_tests, 
                                                type="UK")$mean,
                                        min(pcs_train[,i]))
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
    error_sandia <- data.frame(pc1_q2 = evalQ2(pc1_tests, pc1_preds),
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
                               transf = transf_name)
    
    return(error_sandia)
}

#' Select the raw output transformation before fitting the metamodel
#'
#' Three possible selections: log, sqrt, and identity
#' Before doing the log and sqrt transformations, the raw output is shifted 
#' such that the minimum is 1
#'
#' @param transf_name the name of the transformation (character)
#' @return the transformation function
selectTransf <- function(transf_name) 
{
    if (transf_name == "log")
    {
        # Log transformation
        transf <- function(x) log(x - min(x) + 1)
    } else if (transf_name == "sqrt")
    {
        # Square root transformation
        transf <- function(x) sqrt(x - min(x) + 1)
    } else
    {
        # Identity (aka original) transformation
        transf <- function(x) x
    }
    
    return(transf)
}

#' Select the inverse transformation after metamodel prediction
#'
#' Three possible selections: log, sqrt, and identity
#' Before doing the log and sqrt transformations, the prediction is shifted 
#'
#' @param transf_name the name of the original transformation (character)
#' @return the inverse transformation function
selectInvTransf <- function(transf_name) 
{
    if (transf_name == "log")
    {
        # Log transformation
        inv_transf <- function(x, y) exp(x) + (y - 1) 
    } else if (transf_name == "sqrt")
    {
        # Square root transformation
        inv_transf <- function(x, y) (x)^2 + (y - 1)
    } else
    {
        # Identity (aka original) transformation
        inv_transf <- function(x, y) x
    }
    
    return(inv_transf)
}
