#' Rescale the normalized input according to the borehole function inputs range
#'
#'  The range of input parameter of parameter 8 are taken from [1] where it 
#'  was modified from the original range in [2] to induce more non-linearities
#'  and non-additivity to the output. The range of the parameters is as follow:
#'
#'   1. rw, radius of the borehole, [m], [0.05, 0.15]
#'   2. r, radius of influence, [m], [100, 50'000]
#'   3. Tu, transmissivity of upper aquifer, [m^2.year^-1], [63'070, 115'600]
#'   4. Hu, potentiometric head of upper aquifer, [m], [990, 1'110]
#'   5. Tl, transmissivity of lower aquifer, [m^2.year^-1], [63.1, 116]
#'   6. Hl, potentiometric head of lower aquifer, [m], [700, 820]
#'   7. L, length of the borehole, [m], [1'120, 1'680]
#'   8. Kw, hydraulic conductivity of the borehole, [m.year^-1],[1'500, 15'000]
#'
#' References:
#' (1) Max D. Morris, Toby J. Mitchell, and Donald Ylvisaker, "Bayesian Design
#'     and Analysis of Computer Experiments: Use of Derivatives in Surface 
#'     Prediction," Technometrics, vol. 35, no. 3, 1993, pp. 243-255
#' (2) Brian A. Worley, "Deterministic Uncertainty Analysis," in Proc. of 
#'     the American Nuclear Society Winter Meeting, Los Angeles, USA, 1987
#'
#' @param xx A matrix of n-by-8 normalized input parameters
#' @return an n-by-8 matrix of rescaled input parameters
borehole.rescale_input <- function(xx) {
    # Rescale the inputs
    rw <- 0.05 + (0.15 - 0.05) * xx[,1] 
    r  <- 100. + (50000. - 100.) * xx[,2]
    Tu <- 63070. + (115600. - 63070.) * xx[,3]
    Hu <- 990. + (1110. - 990.) * xx[,4]
    Tl <- 63.1 + (116. - 63.1) * xx[,5]
    Hl <- 700. + (820. - 700.) * xx[,6]
    L  <- 1120. + (1680. - 1120.) * xx[,7]
    Kw <- 1500. + (15000. - 1500.) * xx[,8]

    # Create a matrix of rescaled inputs
    yy <- matrix(c(rw, r, Tu, Hu, Tl, Hl, L, Kw), nrow = length(rw))

    return(yy)
}


#' Calculate the flow rate through a borehole using the Borehole function
#'
#'  The borehole function is 8-dimensional scalar function. The input is the 
#'  following and listed as the column of the input matrix:
#'   1. rw, radius of the borehole, [m]
#'   2. r, radius of influence, [m]
#'   3. Tu, transmissivity of upper aquifer, [m^2.year^-1]
#'   4. Hu, potentiometric head of upper aquifer, [m]
#'   5. Tl, transmissivity of lower aquifer, [m^2.year^-1]
#'   6. Hl, potentiometric head of lower aquifer, [m]
#'   7. L, length of the borehole, [m]
#'   8. Kw, hydraulic conductivity of the borehole, [m.year^-1]

#'  The output of the function is the flow rate through the borehole in 
#'  [m^3.year^-1]
#'
#' Reference:
#' (1) Max D. Morris, Toby J. Mitchell, and Donald Ylvisaker, "Bayesian Design
#'     and Analysis of Computer Experiments: Use of Derivatives in Surface 
#'     Prediction," Technometrics, vol. 35, no. 3, 1993, pp. 243-255
#'
#' @param xx A matrix of n-by-8 input parameters
#' @return the flow rate through the borehole evaluated at xx, in [m^3.year^-1]
borehole.eval <- function(xx) {
    # Assign input arguments to local variables
    rw <- xx[,1] 
    r  <- xx[,2]
    Tu <- xx[,3]
    Hu <- xx[,4]
    Tl <- xx[,5]
    Hl <- xx[,6]
    L  <- xx[,7]
    Kw <- xx[,8]

    # Calculate the flow rate
    y <- (2 * pi * Tu * (Hu - Hl) / log(r/rw) / 
         (1 + 2 * L * Tu / log(r/rw) / rw**2 / Kw + Tu/Tl))

    return(y)
}