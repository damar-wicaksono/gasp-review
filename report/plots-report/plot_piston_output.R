#
# title     : plot_piston_output.R
# purpose   : R Script to create output histogram of piston model
# author    : WD41, LRS/EPFL/PSI
# date      : Feb. 2016
#
# Load Required Library -------------------------------------------------------
source("./plots-report/Rscripts/commonDataPiston.R")

# Create the plot -------------------------------------------------------------
p <- ggplot(data = subset(piston_df, input = "M"))

p <- p + geom_histogram(mapping = aes(x = y))

# Tweak the plotting canvas
p <- p + xlab("Cycle Time [s]")
p <- p + theme_bw(base_size = 12)

# Save the plot
png(otpfullname, width = 100, height = 100, units = "mm", res = 600)
print(p)
dev.off()