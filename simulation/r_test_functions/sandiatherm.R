#' Rescale the normalized input according to actual input ranges
#'    
#' The thermal problem formulation contains 4 input parameters, 2 of which are 
#' the control variables (q and L) and the other two are "model" variables 
#' (k and rho_cp) The range of parameters is as follows:
#'
#'  1. q, heat flux at the slab surface, [W.m^-2], [1'000, 3'500]
#'  2. L, length of the slab, [m], [1.27E-2, 2.54E-2]
#'  3. k, thermal conductivity, [W.m^-1.K^-1], [0.0455, 0.0811]
#'  4. rho_cp, volumetric heat capacity, [J.m^-3.K^-1], [3.38E5, 4.69E5]
#'
#'  **Reference:**
#'  (1) Kevin J. Dowding, Martin Pilch, and Richard G. Hills, "Formulation of 
#'      the Thermal Problem," Computer Methods in Applied Mechanics and 
#'      Engineering, vol. 197, 2008, pp. 2385 - 2389
#'
#' @param xx an n-by-4 normalized input parameters matrix, where n is the 
#'      of number independent realizations
#' @return an n-by-4 matrix of rescaled input parameters
sandiatherm.rescale_input <- function(xx) {
    # Rescale the inputs
    q <- 1000. + (3500. - 1000.) * xx[,1]
    L <- 1.27E-2 + (2.54E-2 - 1.27E-2) * xx[,2]
    k <- 0.0455 + (0.0811 - 0.0455) * xx[,3]
    rho_cp <- 3.38E5 + (4.69E5 - 3.38E5) * xx[,4]

    # Create a matrix of rescaled inputs
    yy <- matrix(c(q, L, k, rho_cp), nrow = length(q))

    return(yy)
}


#' Evaluate the time-dependent temperature at selected time and position
#'    
#' The Sandia Lab. thermal problem is based on a truncated infinite series 
#' solution of heat conduction problem. It contains in total 7 free parameters 
#' described below. The first 4 are considered input parameters.
#'
#' **Reference:**
#' (1) Kevin J. Dowding, Martin Pilch, and Richard G. Hills, "Formulation of 
#'     the Thermal Problem," Computer Methods in Applied Mechanics and 
#'     Engineering, vol. 197, 2008, pp. 2385 - 2389
#'
#' @param xx an n-by-4 input parameters matrix containing in sequential 
#'      column: q, L, k, and rho_cp. n is the number of independent realization
#' @param t vector of time-points (t) in [s] where the temperature is evaluated
#' @param x lateral location (x) in [m] where the temperature is evaluated
#' @param temp_init the initial temperature (temp_init) in [K]
#' @return an n-by-length(t) of temperature in [K] at each select 
#'      time-points for each input parameters xx
sandiatherm.eval <- function(xx, t, x, temp_init) {
    # Assign the input parameters into local variables
    q <- xx[,1]
    L <- xx[,2]
    k <- xx[,3]
    rho_cp <- xx[,4]

    # Create a matrix for the complete output
    n_s <- dim(xx)[1]   # number of samples
    n_t <- length(t)    # number of time points 
    yy <- matrix(vector(,n_s), vector(,n_t), nrow = n_s, ncol = n_t) 

    # Loop over time-points and evaluate the temperature
    for (i in 1:length(t)) {
        if (t[i] < 0.) {
            stop("Not valid for negative time")
        } else if (t[i] < 1E-6) {
            yy[,i] = temp_init
        } else {
            # Compute the sum of the series
            series_sum <- 0
            for (j in 1:7) {
                series_sum <- series_sum + 1/j**2 * 
                    exp(-1 * j**2 * pi**2 * k / rho_cp * t[i] / L**2) * 
                    cos(j * pi * x / L)
            }
            series_sum <- series_sum * 2 / pi**2

            # Compute the temperature
            yy[,i] = replicate(n_s, temp_init) + q * L / k * 
                (k / rho_cp * t[i] / L**2 + 1.0/3.0 - x / L + 
                0.5 * (x/L)**2 - series_sum)
        }
    }
    return(yy)
}
