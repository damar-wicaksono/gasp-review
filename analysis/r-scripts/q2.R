#' Calculate the Coefficient of Determination (Q2) of a meta-model prediction
#' 
#' Given the km object, test input data, test output data compute the 
#' coefficient of determination
#'
#' Reference:
#'   B. Iooss, L. Boussouf, V. Feuillard, and A. Marrel, "Numerical Studies of 
#'   the metamodel fitting and validation processes," International Journal of 
#'   Advances in Systems and Measurements, vol. 3, pp. 11-21, 2010.
#' 
#' @param km_object the Gaussian process metamodel object from DiceKriging
#' @param x_test the test input dataframe
#' @param y_test the test output data set (a vector)
#' @return the coefficient of determination
q2 <- function(km_object, x_test, y_test ) {
   y_predict <- predict(km_object, newdata = x_test, "UK")$mean
   ss_res <- sum((y_test - y_predict)^2)            # residual sum of squares
   ss_tot <- sum((y_test - mean(y_test))^2) # total sum of squares
   
   return(1 - (ss_res/ss_tot))
}
