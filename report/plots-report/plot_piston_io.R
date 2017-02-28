#
# title     : plot_piston_io.R
# purpose   : R Script to create IO plot of piston model using ggplot facet
# author    : WD41, LRS/EPFL/PSI
# date      : Feb. 2016
#
# Load Required Library -------------------------------------------------------
source("./plots-report/Rscripts/commonDataPiston.R")

# Create the plot -------------------------------------------------------------
p <- ggplot(data = piston_df)

p <- p + geom_point(mapping = aes(x = x, y = y))
p <- p + facet_wrap(~input, labeller = label_parsed)

# Tweak the plotting canvas
p <- p + ylab("Cycle Time [s]")
p <- p + xlab("Normalized Input [-]")
p <- p + theme_bw(base_size = 12)

# Save the plot
png(otpfullname, width = 200, height = 200, units = "mm", res = 600)
print(p)
dev.off()
