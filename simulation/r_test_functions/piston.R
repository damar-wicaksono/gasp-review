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

#' Calculate the flow rate through a borehole using the Borehole function
#'
#'  The piston performance model is a 7-dimensional scalar function. 
#'  The input is the  following and listed as the column of the input matrix:
#'   1. M, piston weight, [kg]
#'   2. S, piston surface, [m^2]
#'   3. V0, initial gas volume, [m^3]
#'   4. k, spring coefficient, [N/m]
#'   5. P0, atmpspheric pressure, [N/m^2]
#'   6. Tamb, ambient temperature, [K]
#'   7. T0, filling gas temperature, [K]
#'
#'  The output of the function is the cycle time, or the time required for the 
#'  piston to finish a cycle, or the frequency of the piston, in [s].
#'  Cycle time is often used as a measure of piston performance.
#'
#' Reference:
#' [1] E. N. Ben-Ari & D. Steinberg, "Modeling Data from Computer Experiments:
#'     An Empirical Comparison of Kriging with MARS and Projection Pursuit 
#'     Regression," Quality Engineering, vol. 19, pp. 327 - 338, 2007, 
#'     doi: 10.1080/08982110701580930
#'
#' @param xx A matrix of n-by-7 input parameters, rescaled
#' @return cycle time, the time required for the piston to finish a cycle, [s]
evalOutputPiston <- function(xx) {
    # Assign input arguments to local variables
    M <- xx[,1] 
    S  <- xx[,2]
    V0 <- xx[,3]
    k <- xx[,4]
    P0 <- xx[,5]
    Tamb <- xx[,6]
    T0  <- xx[,7]

    # Calculate the current force
    A <- P0 * S + 19.62 * M - k * V0 / S

    # Calculate the cylinder volume
    V <- S / (2*k) * (sqrt(A^2 + 4 * k * P0 * V0 * Tamb / T0) - A)

    # Calculate the cycle time
    y <- 2 * pi * sqrt(M / (k + S^2 * P0 * V0 / T0 * Tamb / V^2))

    return(y)
}
