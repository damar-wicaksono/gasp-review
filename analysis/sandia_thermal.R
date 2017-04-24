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
#y_tests <- evalOutputFranke(xx_tests)

xx_train <- read.csv("../simulation/design/2params/srs_8_2_1.csv", header = F)
names(xx_train) <- input_names

# Training --------------
t_s <- seq(0, 1000.0, by = 100)
x0  <- 0.0
q <- c(1000, 2000, 3000, 3500)
L   <- c(1.27, 1.90, 2.54) * 1e-2

yy_train <- evalOutputSandiaThermal(rescaleInputSandiaThermal(xx_train), 
                                    t_s, x0, 25, q[1], L[1])
plot(t_s, yy_train[1,], ylim = c(25, 1200), type = "l")
for (i in 2:16) lines(t_s, yy_train[i,])

yy_tests <- evalOutputSandiaThermal(rescaleInputSandiaThermal(xx_tests), 
                                    t_s, x0, 25, q[1], L[1])

# Carry out dimension reduction -----------------------------------------------

# SVD of temperature
yy_scaled <- sweep(yy_train, 2, colMeans(yy_train)) / sd(yy_train)
yy_svd <- svd(yy_scaled, nv = ncol(yy_scaled))
(yy_svd$d / sqrt(15))^2 / sum((yy_svd$d / sqrt(15))^2)
D <- diag(yy_svd$d)
pc_scores <- yy_svd$u %*% D

# Project test data using training principal component
yy_tests_scaled <- sweep(yy_tests, 2, colMeans(yy_train)) / sd(yy_train)
pcs_tests <- yy_tests_scaled %*% yy_svd$v

km_lists <- list()
pc_scores_preds <- matrix(0, nrow = dim(xx_tests)[1], ncol = 3)
# Training Kriging metamodel
for (i in 1:3) 
{
    km_lists[[i]] <- km(as.formula("~0"),
                        design = xx_train, response = pc_scores[,i], 
                        covtype = "gauss",
                        control = list(pop.size = 100, trace = F),
                        nugget = 1e-8 * var(transf(y_train)))
    pc_scores_preds[,i] <- predict(km_lists[[i]], newdata=xx_tests, type="UK")$mean
}

# Compute Reconstruction Error both from PC and Kriging -----
hot_matrix <- pc_scores_preds[,1:2] %*% t(yy_svd$v[,1:2]) * sd(yy_train)
mean_vector <- apply(yy_train, 2, mean)
rec_matrix <- matrix(replicate(dim(xx_tests)[1], mean_vector), ncol=11, byrow = T) + hot_matrix
plot(t_s, yy_tests[1,])
points(t_s, rec_matrix[1,], pch = 6)

sqrt(mean(apply((yy_tests - rec_matrix)^2, 1, sum))) # Mean of Frobenius Norm


evalRMSE(y_test = pcs_tests[,3], pc_scores_preds)
abline(0,1)
plot(m)
matrix(c(pcs_tests[1:10,1], pc_scores_preds[1:10]), nrow=10)
yy_pca <- prcomp(yy)
yy_svd$v
yy_pca$sdev^2/sum(yy_pca$sdev^2)
yy_svd$d^2 / sum(yy_svd$d^2)

(yy_svd$d / sqrt(31))^2 / sum((yy_svd$d / sqrt(31))^2)

yy_svd$d[1]^2 / 31
pc_scores[,1] * yy_svd$v[,1]

matrix(apply(yy, 2, mean), ncol=11) + matrix(pc_scores[,1], nrow=32) %*% matrix(yy_svd$v[,1], ncol = 11)
mean_vector <- apply(yy, 2, mean)
devi_vector <- (matrix(pc_scores[,1], nrow=32) %*% matrix(yy_svd$v[,1], ncol = 11) + 
    matrix(pc_scores[,2], nrow=32) %*% matrix(yy_svd$v[,2], ncol = 11) + 
    matrix(pc_scores[,3], nrow=32) %*% matrix(yy_svd$v[,3], ncol = 11)) * sd(yy)
rec_matrix <- matrix(replicate(32, mean_vector), ncol=11,byrow = T) + devi_vector

mean(sqrt(apply((yy - rec_matrix)^2,1, mean)))

plot(t_s, yy[31,]) #, type = "l", ylim = c(-0.5, 0.5))
points(t_s, rec_matrix[31,], pch=6)
for (i in 2:32) lines(t_s, yy[i,])
for (i in 2:32) lines(t_s, rec_matrix[i,],col="red")

matrix(yy_svd$v[,1], nrow = 11)

mean((yy[,] - apply(yy, 2, mean) - pc_scores[,1] * yy_svd$v[,1] * sd(yy))^2)
(yy[1,] - apply(yy, 2, mean))
sqrt(mean(apply(sweep(yy, 2, apply(yy, 2, mean))^2, 1, mean)))
mean(sqrt(apply(sweep(yy, 2, apply(yy, 2, mean))^2, 1, mean)))

plot(t_s, apply(yy, 2, mean) + pc_scores[1,1] * yy_svd$v[,1] * sd(yy)
     + pc_scores[1,2] * yy_svd$v[,1] * sd(yy)
     + pc_scores[1,3] * yy_svd$v[,1] * sd(yy))
lines(t_s, yy[1,])

plot(t_s, yy_scaled[1,], type = "l", ylim = c(-0.5, 0.5))
for (i in 2:32) lines(t_s, yy_scaled[i,])

yy_svd$d^2 / 31) / sum(yy_svd$d^2 / 31))[1:3])
yy_svd <- svd(yy_scaled, nv = ncol(yy_scaled))
sd2 <- (yy_svd$d / sqrt(31))^2 / sum(sd2)
sd2
pc_scores[,1]
yy_scaled %*% yy_pca$rotation

hist(pc_scores[,3])
?pca
?svd
yy_centered
yy[32,] -  apply(yy, 2, mean)
yy_centered[32,]
length(apply(yy, 2, mean))


plot(t_s, yy[1,])
for (i in 2:100) lines(t_s, yy[i,])
series_sum <- 0
for (j in 1:6) {
    series_sum <- series_sum + 1/j**2 * 
        exp(-1 * j**2 * pi**2 * xx[,1] / xx[,2] * t_s[1] / L[1]**2) * 
        cos(j * pi * x0 / L[1])
}
series_sum <- series_sum * 2 / pi**2
series_sum
y<- replicate(100, 25) + q[1] * L[1] / xx[,1] * (xx[,1] / xx[,2] * t_s[1] / L[1]**2 + 1.0/3.0 - x0 / L[1] + 
         0.5 * (x0/L[1])**2 - series_sum)

y
(xx[,1] / xx[,2] * t_s[1] / L[1]**2 + 1.0 / 3.0 - 0.5 * series_sum)


exp(-1 * j**2 * pi**2 * xx[,1] / xx[,2] * t_s[1] / L[1]**2)


series_sum <- 0
series_sum <- series_sum + 1/1**2 * exp(-1 * 1**2 * pi**2 * xx[,1] / xx[,2] * t_s[10] / L[1]**2) * 
    cos(1 * pi * x0 / L[1])
series_sum <- series_sum * 2 / pi**2
series_sum


-1 * 1**2 * pi**2 * xx[,1] / xx[,2] * t_s[10] / L[1]**2
q[1] * L[1] / xx[,1] * (xx[,1] / xx[,2] * t_s[1] / L[1]**2 + 1.0/3.0 - x0 / L[1] + 0.5 * (x0/L[1])**2 - series_sum)
xx
q[1] * L[1] / xx[,1]

xx[,1] / xx[,2] * t_s[1] / L[1]**2

series_sum
q[1] * L[1] * (xx[,1]/xx[,2] * t_s[10] / L[1]**2 + 1.0/3.0 - x0/L[1] + 0.5 * (x0/L[1])**2 - series_sum)
q[1] * L[1] / xx[,1] * (xx[,1] / xx[,2] * t_s[i] / L[1]**2 + 1.0/3.0 - x0 / L[1] + 
                            0.5 * (x0/L[1])**2 - series_sum)
series_sum
xx[,1]/xx[,2] * t_s[10] / L[1]**2 + 1.0/3.0 - x0/L[1] + 0.5 * (x0/L[1])**2 - series_sum

