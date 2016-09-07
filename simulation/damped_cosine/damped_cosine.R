#' Calculate the Damped Cosine Test Function
#'
#' Reference:
#'   T. Santner, B. Williams, and W. Notz, "The Design and Analysis of 
#    Computer Experiments," pp. 64, Springer-Verlag, New York, 2003.
#' 
#' @param x A vector of float [0, 1]. The argument.
#' @return the function values evaluated at x
damped_cosine <- function(x) {
    if (missing(x)) {
        stop("Function needs argument with values [0,1]")
    } else if (any(x > 1.0)) {
        stop("Function argument greater than 1.0, the domain is [0,1]")
    } else if (any(x < 1.0)) {
        stop("Function argument smaller than 1.0, the domain is [0,1]")
    }

    y <- exp(1)^(-1.4 * x) * cos(3.5 * pi * x)
    return(y)
}
