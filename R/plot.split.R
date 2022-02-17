#' Plot Confidence Regions obtained with Split Conformal
#'
#' @param split The output of a split multivariate conformal
#' prediction function.
#' @param same.scale Should I force the same scale for all the y-axis ? Default
#' is FALSE.
#' @return g_list A list of ggplots
#' @details It exploits the package \code{\link{ggplot2}} and \code{\link{gridExtra}}
#' to better visualize the results. It outputs n0=length(x0) plots.
#' @example inst/examples/ex.split.R
#' @export plot_multidim_split



plot_multidim_split=function(split, same.scale = FALSE){


  #Get Data
  x0 = split$x0
  lo = split$lo
  up = split$up
  pred = split$pred

  # Find bounds for the plots

  if(same.scale){

    y_up = max(up) +0.01 * sd(up)
    y_lo = min(lo) -0.01 * sd(lo)

  }

  # Define dimensions
  p<-ncol(x0)
  q<-ncol(lo)
  g_list<-vector("list",p*q)



  gl <- lapply(1:p,function(ii) lapply(1:q, function(jj){

    df=data.frame(xd=x0[,ii],yg=pred[,jj],y_min=lo[,jj], y_max=up[,jj])

    ggg<- ggplot2::ggplot(df, ggplot2::aes(x=xd,y = yg)) + ggplot2::geom_pointrange(ggplot2::aes(ymin = y_min, ymax = y_max), color ="red") + ggplot2::xlab(paste("x ",ii)) + ggplot2::ylab(paste("y ",jj))


    if(same.scale)
      ggg = ggg + ggplot2::ylim(y_up,y_lo)

    return(ggg)

  }))



  glist <- do.call(c, gl)
  do.call(gridExtra::"grid.arrange", c(glist, ncol=q,top="Confidence Intervals"))
  return(gl)

}

utils::globalVariables(c( "xd", "y_max", "y_min", "yg"))

