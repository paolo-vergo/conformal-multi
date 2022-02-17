#'@noRd


depth.max=function(inp){
  inp=scale(inp)

  d=future.apply::future_sapply(1:nrow(inp), function(i) 1/max(abs(inp[i,])))

  return(d)
}
