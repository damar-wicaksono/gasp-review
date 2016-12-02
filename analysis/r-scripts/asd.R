#' Compute the Acceptance Score Distribution (ASD) of a meta-model prediction
#' 
#' Given the km object, test input data, test output data compute the 
#' Acceptance Score Distribution (ASD). 
#' 
#' The output is a dataframe with the the input parameters, 
#' the meta-model prediction, the validation data, and boolean
#' value of accept/reject. TRUE refers to ACCEPT.
#' 
#' Acceptability of a prediction is measured based on the deviation of the 
#' ratio (y_pred/y_obs) from 1. 
#' The user is required to specify the level of deviation for an acceptable 
#' solution.
#' 
#' Make sure that the test input dataframe has the same column names as the 
#' one used for training
#'
#' Reference:
#'   Husam Hammad, "Space partitioning in engineering design via metamodel
#'   acceptance score distribution," Engineering with Computers, vol. 23, 
#'   pp. 175 - 185, 2007
#' 
#' @param km_object the Gaussian process metamodel object from DiceKriging
#' @param x_test the test inputs (a dataframe)
#' @param y_test the test output (a vector)
#' @param dev_level the level of deviation from a ratio of 1, between 0-1
#' @return a dataframe with the inputs, output prediction, validation output, 
#'    and accept/reject
asd <- function(km_object, x_test, y_test, dev_level = 0.2) {
    y_predict <- predict(km_object, newdata = x_test, "UK")$mean
    
    pred_to_val_ratio <- y_predict / y_test
    
    lower_criterion <- pred_to_val_ratio > (1 - dev_level)
    upper_criterion <- pred_to_val_ratio < (1 + dev_level)
    accept_score <- lower_criterion & upper_criterion
    
    accept_score_dist <- data.frame(x_test, 
                                    y_test = y_test, 
                                    y_predict = y_predict,
                                    accept = accept_score)
    
    return(accept_score_dist)
}
