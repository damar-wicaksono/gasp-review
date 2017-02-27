#
# title     : plot_franke_3d.R
# purpose   : R Script to create 3D surface plot of Franke's function in its 
#           : domain
# author    : WD41, LRS/EPFL/PSI
# date      : Feb. 2016
#
# Load Required Library -------------------------------------------------------
library("plot3D")

source("../simulation/r_test_functions/franke.R")

# Create Data -----------------------------------------------------------------
xx <- matrix(c(seq(0, 1, length.out = 100), seq(0, 1, length.out = 100)), 
             ncol=2)
# Create a uniform grid of cartesian product, 100-by-100
xx_grid <- expand.grid(xx[,1], xx[,2])

# Evaluate at each value
zz <- evalFranke(xx_grid)

# Make the Plot ---------------------------------------------------------------
png(otpfullname, width=20, height=15.0, units="cm", res=600)
par(mfrow=c(1, 1), 
    oma = c(1.5, 1.5, 1.5, 1.5) + 0.1,
    mar = c(0, 0, 0, 0) + 0.1)
persp3D(x = xx[,1], y = xx[,2], z = matrix(zz, 100, 100), 
        col = ramp.col(c("white", "black")), zlim=c(-0.5,1.5),
        xlab = "x1", ylab = "x2", zlab = "y",
        border = "black", ticktype = "detailed", expand = 0.75, phi = 30)
dev.off()
