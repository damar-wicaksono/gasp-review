#' Rescale the normalized input according to the piston cycle input range
#'
#'  The range of input parameter of parameter 7 are taken from [1]. 
#'  The parameters, their desription, and range of values are:
#'
#'   1. M, piston weight, [kg], [30, 60]
#'   2. S, piston surface, [m^2], [0.005, 0.020]
#'   3. V0, initial gas volume, [m^3], [0.002, 0.010]
#'   4. k, spring coefficient, [N/m], [1'000, 5'000]
#'   5. P0, atmpspheric pressure, [N/m^2], [9x10^4, 11x10^4]
#'   6. Tamb, ambient temperature, [K], [290, 296]
#'   7. T0, filling gas temperature, [K], [340, 346]
#'
#' References:
#' [1] E. N. Ben-Ari & D. Steinberg, "Modeling Data from Computer Experiments:
#'     An Empirical Comparison of Kriging with MARS and Projection Pursuit 
#'     Regression," Quality Engineering, vol. 19, pp. 327 - 338, 2007, 
#'     doi: 10.1080/08982110701580930
#'
#' @param xx A matrix of n-by-7 normalized input parameters, i.e., in (0,1)
#' @return an n-by-7 matrix of rescaled input parameters
rescaleInputPiston <- function(xx) {
    # Rescale the inputs according to the inputs respective ranges
    M  <-  30.0 + (60.0 - 30.0) * xx[,1] 
    S  <- 0.005 + (0.020 - 0.005) * xx[,2]
    V0 <- 0.002 + (0.010 - 0.002) * xx[,3]
    k  <- 1000. + (5000. - 1000.) * xx[,4]
    P0 <-   9e4 + (11e4 - 9e4) * xx[,5]
    Tamb <- 290. + (296. - 290.) * xx[,6]
    T0 <-  340. + (360. - 340.) * xx[,7]

    # Create a matrix of rescaled inputs
    yy <- matrix(c(M, S, V0, k, P0, Tamb, T0), nrow = length(M))

    return(yy)
}
