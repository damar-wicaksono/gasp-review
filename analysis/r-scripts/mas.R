#' Calculate the Model Acceptability Score (MAS) of a meta-model prediction
#' 
#' Given the km object, test input data, test output data compute the model
#' acceptability score (MAS) of a meta-model.
#'
#' MAS is based on counting the number of acceptable model performance measured
#' by the deviation of the ratio (y_pred/y_obs) from 1.
#' The user is required to specify the level of deviation for acceptable 
#' solution
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
#' @param y_test the test output data set
#' @param dev_level the admissible deviation level from ratio of 1, between 0-1
#' @return the model acceptability score, in percentage
mas <- function(km_object, x_test, y_test, dev_level = 0.2) {
    y_predict <- predict(km_object, newdata = x_test, "UK")$mean
    
    pred_to_val_ratio <- y_predict / y_test
    
    lower_criterion <- pred_to_val_ratio > (1 - dev_level)
    upper_criterion <- pred_to_val_ratio < (1 + dev_level)

    accept_score <- sum(lower_criterion & upper_criterion)
    
    return(accept_score / length(y_test) * 100)
}