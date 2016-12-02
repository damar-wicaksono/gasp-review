#' Construct an Acceptance Score Distribution (ASD) Plot
#' 
#' Given an ASD dataframe, construct the plot of accept/reject for each of 
#' the input dimensions
#' 
#' Requires the ggplot2 library
#' 
#' Reference:
#'    Husam Hammad, "Space partitioning in engineering design via metamodel
#'   acceptance score distribution," Engineering with Computers, vol. 23, 
#'   pp. 175 - 185, 2007
#'
#' @param asd_df a dataframe containing the results of Acceptance Score 
#'     Distribution
#' @return a ggplot2 object
plot_asd <- function(asd_df) {
    # Create a tidy dataframe for ggplot plotting
    k <- ncol(asd_df) - 3   # the number of dimension / input parameters
    d <- data.frame(x = subset(asd_df, accept == T)[,"x1"],
                    accept="accept",
                    inp="x1")
    d <- rbind(d, data.frame(x=subset(a, accept == F)[,"x1"],
                             accept="reject",
                             inp="x1"))
    if (k > 1) {
        for (i in 2:k) {
            param_name = paste("x", i, sep = "")
            d <- rbind(d, data.frame(
                x = subset(asd_df, accept == T)[,param_name],
                accept = "accept",
                inp = param_name))
            d <- rbind(d, data.frame(
                x = subset(asd_df, accept == F)[,param_name],
                accept = "reject",
                inp = param_name))
        }    
    }
    
    # Create the plot
    p <- ggplot(d, aes(x = accept, y = x, shape = accept)) + 
        geom_jitter(position = position_jitter(0,0), size = 4) + 
        coord_flip() +
        facet_grid(inp ~.) 
    
    # Adjust the plot
    p <- p + scale_shape_manual(values = c(16, 4))          # change shape
    p <- p + scale_x_discrete(limits=c("reject", "accept")) # re-order y-axis
    
    p <- p + theme_bw(base_size = 14)
    p <- p + theme(legend.position = "none")
    p <- p + theme(strip.text.y=element_text(angle=0))
    p <- p + theme(strip.background = element_rect(fill = "#ffffb3",
                                                   colour = "black",
                                                   size = 0.45))
    p <- p + theme(strip.text = element_text(face = "bold"))
    
    p <- p + xlab("Accept/Reject")       
    p <- p + ylab("Normalized Inputs")
    p <- p + ylim(0, 1.0)
    
    return(p)
}
