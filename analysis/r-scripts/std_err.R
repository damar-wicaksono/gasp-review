#' Compute the Standardized Individual Prediction Error of metamodel prediction
#' 
#' Given a km object, test input dataframe, test output vector, compute the 
#' Standardized Invidual Prediction Error of the metamodel.
#' 
#' Reference:
#'     Leonardo Bastos and Anthony O'Hagan, "Diagnostics for Gaussian Process
#'     Emulators," Technometrics, vol. 51, no. 4, pp. 425-438, 2009
#'
#' @param km_object the kriging model produced by DiceKriging package
#' @param x_test the test inputs (a dataframe)
#' @param y_test the test output (a vector)
#' @return a vector of standardized individual prediction error for each of the
#'     meta-model prediction
std_err <- function(km_object, x_test, y_test) {
    y_predict <- predict(km_object, newdata = x_test, "UK")
    y_mean <- y_predict$mean    # The mean
    y_sd <- y_predict$sd        # The standard deviation
    
    return((y_test - y_mean) / y_sd)
}