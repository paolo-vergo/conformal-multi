#' Plot Confidence Regions obtained from Full Conformal
#'
#' @param full It's the output of the multivariate full conformal prediction function
#' @return A list of ggplots (output[[i]] is the i-th observation confidence region).
#' @details It exploits the package \code{\link{ggplot2}} and \code{\link{hrbrthemes}}
#' to better visualize the results.
#'
#' @example inst/examples/ex.full.R
#'
#' @export plot_multidim_full



plot_multidim_full=function(full){

  #Get Data
  valid_points = full$valid_points
  pred = full$pred


 n0 = length(valid_points)

 plots<- lapply(1:n0, function(k){

   df=valid_points[[k]]
   g_plot = ggplot2::ggplot(data=df, ggplot2::aes(Var1,Var2)) + ggplot2::geom_raster(ggplot2::aes(fill = -pval)) + ggplot2::scale_fill_distiller(palette = "RdPu") + hrbrthemes::theme_ipsum() + ggplot2::xlab("y1") + ggplot2::ylab("y2") + ggplot2::ggtitle(paste("Test Observation",k) ) + ggplot2::geom_point(data=pred[k,],ggplot2::aes(x=X1,y=X2),shape=8, size=10)

   return(g_plot )

 })

 return(plots)
}


utils::globalVariables(c("Var1", "Var2", "X1", "X2"))
