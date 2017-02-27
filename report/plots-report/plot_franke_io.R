#
# title     : plot_franke_io.R
# purpose   : R Script to create series of plots depicting the Input/Output of
#           : Franke's function using 200 random samples of the inputs. 
#           :   1. The histogram of the output
#           :   2. Scatter plot output vs. x1
#           :   3. Scatter plot output vs. x2
# author    : WD41, LRS/EPFL/PSI
# date      : Feb. 2016
#
# Set seed number -------------------------------------------------------------
set.seed(345890)

# Load Required Library -------------------------------------------------------
library(ggplot2)
source("./plots-report/Rscripts/multiplot.R")
source("../simulation/r_test_functions/franke.R")

# Create Data -----------------------------------------------------------------
# Sample random number, 200
xx <- matrix(c(runif(200, 0, 1), runif(200, 0, 1)), ncol = 2)

# Evaluate Franke's function
y <- evalFranke(xx)

# Create dataframe for tidy data
franke <- data.frame(x1 = xx[,1], x2 = xx[,2], y = y)

# Create the plot -------------------------------------------------------------
p <- ggplot(data = franke)

# Histogram of output
p1 <- p + geom_histogram(mapping = aes(x = y))
p1 <- p1 + theme_bw(base_size = 14)

# Scatter plot, output vs. x1
p2 <- p + geom_point(mapping = aes(x = x1, y = y))
p2 <- p2 + theme_bw(base_size = 14)
p2 <- p2 + xlab(expression(x[1]))

# Scatter plot, output vs. x2
p3 <- p + geom_point(mapping = aes(x = x2, y = y))
p3 <- p3 + theme_bw(base_size = 14)
p3 <- p3 + xlab(expression(x[2]))

# Save the plot
png(otpfullname, width = 270, height = 90, units = "mm", res = 600)
print(multiplot(p1, p2, p3, cols = 3))
dev.off()
