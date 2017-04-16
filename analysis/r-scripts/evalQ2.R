# Q2 function -----------
evalQ2 <- function(y_test, y_pred)
{
    q2 <- 1 - sum((y_test - y_pred)^2) / sum((y_test - mean(y_test))^2)
    return(q2)
}