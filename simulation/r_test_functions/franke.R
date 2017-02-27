#' Calculate the Franke's Function, a 2-D analytical function
#'
#'  Franke's function [1] and x1,x2 are defined in (0,1).
#'
#' [1] E. N. Ben-Ari & D. Steinberg, "Modeling Data from Computer Experiments:
#'     An Empirical Comparison of Kriging with MARS and Projection Pursuit 
#'     Regression," Quality Engineering, vol. 19, pp. 327 - 338, 2007, 
#'     doi: 10.1080/08982110701580930
#'
#' @param xx a matrix of n-by-2 input parameters, in (0,1)
#' @return the value of Franke's function
evalFranke <- function(xx) {
    # Assign input arguments to local variables
    x1 <- xx[,1] 
    x2 <- xx[,2]

    # Calculate the function value
    y <- 0.75 * exp(-1/4 * ((9*x1 - 2)^2 + (9*x2 - 2)^2)) +
         0.75 * exp(-1/49 * (9*x1 + 1)^2 - 1/10 * (9*x2 + 1)^2) +
         0.50 * exp(-1/4 * ((9*x1 - 7)^2 + (9*x2 - 3)^2)) -
         0.20 * exp(-1 * ((9*x1 - 4)^2 + (9*x2 - 7)^2))

    return(y)
}