#' BUILDING THE MULTISPLIT INTERVAL FOR EACH X0, BY COMBINING MULTIPLE
#' SPLIT WITH MULTISPLIT ALGORITHM
#'
#'
#' @param yyy column vector of B lower bounds and B upper bounds
#' @param B number of replications
#' @param tr truncation threshold for the algorithm
#' @importFrom utils flush.console
#' @noRd


interval.build=function(yyy,B,tr){



  h=rep(1:0,each=B)

  o = order(yyy,2-h)

  ys <- yyy[o]
  hs <- h[o]

  count <- 0
  leftend <- 0
  lo<-up<-0


  for (j in 1:(2*B) ){
    if ( hs[j]==1 ) {
      count <- count + 1

      if ( count > tr && (count - 1) <= tr) {
        leftend <- ys[j]
      }

    }

    else {
      if ( count > tr && (count - 1) <= tr) {
        rightend <- ys[j]
        lo <- leftend
        up <- rightend
      }

      count <- count - 1
    }
  }




  return(c(lo,up))
}
