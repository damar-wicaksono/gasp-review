#
# title     :
# purpose   : Analysis of Piston Model using Gaussian Process
# author    :
# date      :
#

# Load Required Libraries -----
library(DiceKriging)
library(ggplot2)

source("../simulation/r_test_functions/piston.R")
input_names <- c("M", "S", "V0", "k", "P0", "Tamb", "T0")



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


set.seed(3733)
cov_types <- c("exp", "gauss", "powexp", "matern3_2", "matern5_2")
reg_forms <- c("~0", "~.", "~.^2")
reg_names <- c("constant", "full_linear", "full_linear+int")
doe_names <- c("srs", "lhs", "lhs-opt", "sobol")
n_reps <- 25

# Read pool of test data
xx_tests  <- read.csv("../simulation/design/7params/lhs_1000_7.csv", header = F)
y_tests <- evalOutputPiston(rescaleInputPiston(xx_pool))
names(xx_tests) <- input_names

reg_forms <- c("~0", "~.") 
reg_names <- c("constant", "full_linear") 
num_smpls <- c(16, 32) # 64, 128)
doe_names <- c("srs", "lhs", "lhs-opt", "sobol")


# Read the Data ---------
q2_piston <- data.frame(q2 = c(), 
                        cov_type = c(),
                        doe = c(),
                        reg = c(),
                        n = c())

for (num_smpl in num_smpls)
{
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
                            design = xx_train, response = log(y_train), 
                            covtype = cov_type,
                            control = list(pop.size = 100, trace = T))
                    
                    # Make prediction using metamodel
                    y_preds <- predict(m, newdata=xx_tests, type="UK")$mean
                    
                    # Compute Predictivity Coefficient
                    q2_piston <- rbind(q2_piston,
                                        data.frame(q2 = evalErmse(y_tests, exp(y_preds)), 
                                                   cov_type = cov_type,
                                                   doe = doe_name,
                                                   reg = reg_names[i],
                                                   n = num_smpl)) 
                    
                }
            }
        }
        
    }
}

p <- ggplot(data = subset(q2_piston, n == 32))
p <- p + geom_point(mapping = aes(x=cov_type, y = q2)) + facet_grid(reg ~ doe)

p <- p + geom_jitter(mapping = aes(x=cov_type, y = q2), width=0.10) + facet_grid(reg ~ doe)
p

reg <- "constant"
doe <- "srs"
plot(ecdf(rnorm(10)))
plot(ecdf(subset(q2_piston, cov_type == "gauss" & reg == "constant" & doe == "srs")$q2), do.p=F)
lines(ecdf(subset(q2_piston, cov_type == "gauss" & reg == "constant" & doe == "lhs")$q2), col = "red", do.p=F)
lines(ecdf(subset(q2_piston, cov_type == "gauss" & reg == "constant" & doe == "lhs-opt")$q2), col = "red", do.p=F)
lines(ecdf(subset(q2_piston, cov_type == "gauss" & reg == "constant" & doe == "sobol")$q2), col = "green", do.p=F)
lines(ecdf(subset(q2_piston, cov_type == "powexp" & reg == "constant" & doe == "srs")$q2), do.p=F)
lines(ecdf(subset(q2_piston, cov_type == "powexp" & reg == "constant" & doe == "lhs")$q2), col = "red", do.p=F)
lines(ecdf(subset(q2_piston, cov_type == "powexp" & reg == "constant" & doe == "sobol")$q2), col = "green", do.p=F)
lines(ecdf(subset(q2_piston, cov_type == "matern5_2" & reg == "constant" & doe == "srs")$q2), do.p=F)
lines(ecdf(subset(q2_piston, cov_type == "matern5_2" & reg == "constant" & doe == "lhs")$q2), col = "red", do.p=F)
lines(ecdf(subset(q2_piston, cov_type == "matern5_2" & reg == "constant" & doe == "sobol")$q2), col = "green", do.p=F)



subset(q2_piston, cov_type == "gauss" & reg == "constant" & doe == "lhs")

lines(ecdf(subset(q2_piston, cov_type == "exp" & reg == reg)$q2), col = col2rgb("#7fc97f"), do.p=F)
lines(ecdf(subset(q2_piston, cov_type == "powexp" & reg == reg)$q2), col = "red", do.p=F)
lines(ecdf(subset(q2_piston, cov_type == "matern3_2" & reg == reg)$q2), col = "green", do.p=F)
lines(ecdf(subset(q2_piston, cov_type == "matern5_2" & reg == reg)$q2), col = "violet", do.p=F)

plot(ecdf(subset(q2_piston, cov_type == "gauss" & reg == reg)$q2), do.p=F)

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
