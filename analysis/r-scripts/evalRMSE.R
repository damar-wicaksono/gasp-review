evalRMSE <- function(y_test, y_pred)
{
    rmse <- sqrt(sum((y_test - y_pred)^2) / sum((y_test - mean(y_test))^2))
    return(rmse)
}
