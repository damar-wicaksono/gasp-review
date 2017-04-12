#
# title     :
# purpose   : Analysis of Borehole function using Gaussian Process
# author    :
# date      :
#

source("../simulation/r_test_functions/borehole.R")
xx_init <- read.csv("../simulation/design/8params/srs_16_8.csv", header = F)
xx_pool  <- read.csv("../simulation/design/8params/lhs_500_8.csv", header = F)

set.seed(3733)
cov_types <- c("exp", "gauss", "powexp", "matern3_2", "matern5_2")
reg_forms <- c("~0", "~.", "~.^2")
reg_names <- c("constant", "full_linear", "full_linear+int")
num_smpls <- c(100, 200, 400, 800)
doe_names <- c("srs", "lhs", "lhs-opt", "sobol")

n_reps <- 25
input_names <- c()
for (i in 1:8) input_names <- c(input_names, paste("x",i,sep=""))
q2_borehole <- data.frame(q2 = c(), 
                          cov_type = c(),
                          doe = c(),
                          reg = c(),
                          n = c())

for (doe_name in doe_names)
{
    # Read input and a pool test data
    init_fname <- paste("../simulation/design/8params/", doe_name, "_64_8.csv", sep = "")
    xx_init <- read.csv(init_fname, header = F)
    for (i in 1:length(reg_forms))
    {
        for (cov_type in cov_types)
        {
            for (j in 1:n_reps)
            {
                # Sample from test data to complete test input
                xx_train <- rbind(xx_init, xx_pool[sample(1:500, 0.32*64),])
                
                # Sample from the pool test data
                xx_tests <- xx_pool[sample(1:500, 450),]
                
                # Rescale the input
                xx_train_resc <- data.frame(rescaleInputBorehole(xx_train))
                xx_tests_resc <- data.frame(rescaleInputBorehole(xx_tests))
                names(xx_train_resc) <- input_names
                names(xx_tests_resc) <- input_names
                
                # Evaluate the output based on training and test data
                y_train <- evalOutputPiston(xx_train_resc)
                y_tests <- evalOutputPiston(xx_tests_resc)
                
                # Train the model
                m <- km(as.formula(reg_forms[1]), 
                        design = xx_train, response = y_train, 
                        covtype = "exp",
                        control = list(pop.size = 100, trace = F))
                
                # Make prediction using metamodel
                y_preds <- predict(m, newdata=xx_tests, type="UK")$mean
                
                # Compute Predictivity Coefficient
                q2_borehole <- rbind(q2_borehole,
                                     data.frame(q2 = evalErmse(y_tests, y_preds), 
                                                cov_type = cov_type,
                                                doe = doe_name,
                                                reg = reg_names[i],
                                                n = 32))    
            }
        }
    }
}

p <- ggplot(data = q2_borehole)
p <- p + geom_jitter(mapping = aes(x=cov_type, y = q2), width=0.10) + facet_grid(doe ~ reg)
p <- p + geom_point(mapping = aes(x=cov_type, y = q2)) + facet_grid(reg ~ doe)
p


init_fname <- paste("../simulation/design/8params/", "lhs-opt", "_64_8.csv", sep = "")
xx_init2 <- read.csv(init_fname, header = F)
plot(xx_init1[,1], xx_init1[,2])
plot(xx_init2[,1], xx_init2[,2])
