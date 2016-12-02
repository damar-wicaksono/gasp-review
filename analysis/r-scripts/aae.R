#' Calculate the Average Absolute Error (AAE) of a meta-model prediction
#' 
#' Given the km object, test input data, test output data compute the average
#' absolute error (AAE)
#'
#' Make sure that the test input dataframe has the same column names as the 
#' one used for training
#' 
#' Reference:
#'   Husam Hammad, "Validation of metamodels in simulation: a new metric,"
#'   Engineering with Computers, vol. 27, pp. 309 - 317, 2011
#' 
#' @param km_object the Gaussian process metamodel object from DiceKriging
#' @param x_test the test inputs (a dataframe)
#' @param y_test the test output data set (a vector)
#' @return the average absolute error of the meta-model prediction against 
#'     validation/test data
aae <- function(km_object, x_test, y_test) {
    y_predict <- predict(km_object, newdata = x_test, "UK")$mean
    
    return (mean(abs(y_predict - y_test)))
}