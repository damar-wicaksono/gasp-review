#' Calculate the Franke's Function, a 2-D analytical function
#'
#'  Franke's function [1] and x1,x2 are defined in (0,1).
#'
#' References:
#' [1] Ben Haaland and Peter Z. G. Qian, "Accurate Emulators for Large-Scale
#'     Computer Experiments," The Annals of Statistics, vol. 39, No. 6, 
#'     pp. 2974 - 3002, doi: 10.1214/11-AOS929
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