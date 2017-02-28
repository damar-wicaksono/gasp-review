#
# title     : plots_report.R
# purpose   : The main driver script with command line argument to create plots
#           : appeared in the technical report
# author    : WD41, LRS/EPFL/PSI
# date      : Dec. 2016
#

# `p` is the canonical variable name for the plot
# Set the working directory ---------------------------------------------------
setwd(paste(getwd()))

# Initialize (or Activate) the Packrat package --------------------------------
#packrat::init(getwd())
packrat::on()

# Install Required Packages, do it only once with packrat ---------------------
# `packrat::snapshot()` seems to fail if the fda package is installed first
#install.packages("ggplot2")
#packrat::snapshot()
#install.packages("optparse")
#packrat::snapshot()
#install.packages("DiceKriging")
#packrat::snapshot()
#install.packages("plot3D")
#packrat::snapshot()

# Load Required Packages ------------------------------------------------------
library("ggplot2")
library("optparse")

# Parse command line argument -------------------------------------------------
option_list <- list(make_option(c("-f", "--figure"), type = "character",
                                default = NULL,
                                help = "Figure to create",
                                metavar = "integer"),
                    make_option(c("-o", "--output"), type = "character",
                                default=NULL,
                                help="Output fullname",
                                metavar="character"))

opt_parser <- OptionParser(option_list = option_list)
opt <- parse_args(opt_parser)

# Check if the required argument is given
if (is.null(opt$figure)) {
    print_help(opt_parser)
    stop("The argument must be supplied", call. = FALSE)
} else if (!is.null(opt$figure)) {
#    if(!is.element(opt$figure, seq(1,30))) {
#        print_help(opt_parser)
#        stop("Only figure 1 to 30", call. = FALSE)
#    }
}

otpfullname <- opt$output   # Plot output filename

if (opt$figure == 1)
{
    source("./plots-report/plot_design_illustration.R")
    
}  else if (opt$figure == 2)
{
    source("./plots-report/plot_bivariate.R")
    
} else if(opt$figure == 3)
{
    source("./plots-report/plot_bivariate_samples.R")
    
} else if (opt$figure == 4) 
{
    source("./plots-report/plot_mvn_2vars.R")
    
} else if (opt$figure == 5) 
{
    source("./plots-report/plot_mvn_15vars.R")
    
} else if (opt$figure == 6) 
{
    source("./plots-report/plot_random_function.R")
    
} else if (opt$figure == 7)
{
    source("./plots-report/plot_corrfun_gauss.R")
    
} else if (opt$figure == 8)
{
    source("./plots-report/plot_corrfun_gauss_realization.R")
    
} else if (opt$figure == 9)
{
    source("./plots-report/plot_corrfun_exp.R")
    
} else if (opt$figure == 10)
{
    source("./plots-report/plot_corrfun_exp_realization.R")
    
} else if (opt$figure == 11)
{
    source("./plots-report/plot_corrfun_powexp.R")
    
} else if (opt$figure == 12)
{
    source("./plots-report/plot_corrfun_powexp_realization.R")
} else if (opt$figure == 13)
{
    source("./plots-report/plot_corrfun_matern.R")
    
} else if (opt$figure == 14)
{
    source("./plots-report/plot_corrfun_matern_realization.R")
    
} else if (opt$figure == 15)
{
    source("./plots-report/plot_random_surface.R")
    
} else if (opt$figure == 16)
{
    source("./plots-report/plot_process_variance.R")
    
} else if (opt$figure == 17)
{
    source("./plots-report/plot_mean_function_unconditional.R")
    
} else if (opt$figure == 18)
{
    source("./plots-report/plot_mean_function_conditional.R")
    
} else if (opt$figure == 19)
{
    source("./plots-report/plot_bayesian_perspective.R")
    
} else if (opt$figure == "franke_3d")
{
    source("./plots-report/plot_franke_3d.R")
    
} else if (opt$figure == "franke_io")
{
    source("./plots-report/plot_franke_io.R")
    
} else if (opt$figure == "piston_io")
{
    source("./plots-report/plot_piston_io.R")
}

