#
# title     :
# purpose   : Analysis of Piston Model using Gaussian Process
# author    :
# date      :
#

# ----
library(DiceKriging)
library(ggplot2)
source("../simulation/r_test_functions/piston.R")
input_names <- c("M", "S", "V0", "k", "P0", "Tamb", "T0")

# Q2 function -----------
evalQ2 <- function(y_test, y_pred)
{
    q2 <- 1 - sum((y_test - y_pred)^2) / sum((y_test - mean(y_test))^2)
    return(q2)
}

# Ermse function -----------
evalErmse <- function(y_test, y_pred)
{
    q2 <- sqrt(sum((y_test - y_pred)^2) / sum((y_test - mean(y_test))^2))
    return(q2)
}

# ------
input_names <- c()
for (i in 1:7) input_names <- c(input_names, paste("x",i,sep=""))
xx_init <- read.csv("../simulation/design/7params/srs_64_7.csv", header = F)
xx_pool  <- read.csv("../simulation/design/7params/lhs_1000_7.csv", header = F)

set.seed(3733)
cov_types <- c("exp", "gauss", "powexp", "matern3_2", "matern5_2")
reg_forms <- c("~0", "~.", "~.^2")
reg_names <- c("constant", "full_linear", "full_linear+int")
num_smpls <- c(100, 200, 400, 800)
doe_names <- c("srs", "lhs", "lhs-opt", "sobol")

# Read the Data ---------
q2_df <- data.frame(q2 = c(), 
                    cov_type = c(),
                    doe = c(),
                    reg = c(),
                    n = c())
for (doe_name in doe_names)
{
    # Read input and a pool test data
    init_fname <- paste("../simulation/design/7params/", doe_name, "_64_7.csv", sep = "")
    xx_init <- read.csv(init_fname, header = F)
    for (i in 1:length(reg_forms))
    {
        for (cov_type in cov_types)
        {
            for (j in 1:25)
            {
                # Sample from test data to complete test input
                xx_train <- rbind(xx_init, xx_pool[sample(1:1000, 0.36*100),])
                
                # Sample from the pool test data
                xx_tests <- xx_pool[sample(1:1000, 700),]
                
                # Rescale the input
                xx_train_resc <- data.frame(rescaleInputPiston(xx_train))
                xx_tests_resc <- data.frame(rescaleInputPiston(xx_tests))
                names(xx_train_resc) <- input_names
                names(xx_tests_resc) <- input_names
                
                # Evaluate the output based on training and test data
                y_train <- evalOutputPiston(xx_train_resc)
                y_tests <- evalOutputPiston(xx_tests_resc)
                
                # Train the model
                m <- km(as.formula(reg_forms[i]), 
                        design = xx_train_resc, response = y_train, 
                        covtype = cov_type,
                        control = list(pop.size = 100, trace = F))
                
                # Make prediction using metamodel
                y_preds <- predict(m, newdata=xx_tests_resc, type="UK")$mean
                
                # Compute Predictivity Coefficient
                #q2 <- c(q2, evalQ2(y_tests, y_preds))
                q2_df <- rbind(q2_df, data.frame(q2 = evalQ2(y_tests, y_preds), 
                                                 cov_type = cov_type,
                                                 doe = doe_name,
                                                 reg = reg_names[i],
                                                 n = 100))
            }
        }
    }
}

# Make the plot
p <- ggplot(data = q2_df)
p <- p + geom_jitter(mapping = aes(x=cov_type, y = q2), width=0.10) + facet_grid(doe ~ reg)
p
p <- p + geom_point(mapping = aes(x=cov_type, y = q2)) + facet_grid(doe ~ reg)
p <- p + geom_jitter(mapping = aes(x=reg, y = q2), width=0.10)


names(xx) <- input_names
names(xx_test) <- input_names
names(xx_rescaled) <- input_names
names(xx_test_rescaled) <- input_names

xx_rescaled <- data.frame(rescaleInputPiston(xx))
xx_test_rescaled <- data.frame(rescaleInputPiston(xx_test))
xx_rescaled
y <- evalOutputPiston(xx_rescaled)
y_test <- evalOutputPiston(xx_test_rescaled)

plot(xx_rescaled[,1], y)
plot(xx_rescaled[,2], y)

m <- km(as.formula("~."), design = xx_rescaled, response = y, covtype = "exp")
m
plot(m)

y_pred <- predict(m, newdata=xx_test_rescaled, type="UK")$mean
plot(y_pred, y_test)
