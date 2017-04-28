

selectRegression <- function(reg_name)
{
    # Decide which regression terms are available
    if (reg_name == "constant")
    {
        reg_form <- c("~0")
    } else if (reg_name == "full_linear")
    {
        reg_form <- c("~.")
    } else if (reg_name == "full_linear+int")
    {
        reg_form <- c("~.^2")
    }
    
    return(reg_form)
}
