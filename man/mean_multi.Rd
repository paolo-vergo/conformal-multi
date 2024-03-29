% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mean.R
\name{mean_multi}
\alias{mean_multi}
\title{Mean of Multivariate Response}
\usage{
mean_multi()
}
\value{
A list with the training function and the prediction function.
}
\description{
This model is fed to conformal prediction functions.
It outputs a training function and a prediction function.
}
\details{
The training function takes as input:

x The feature matrix  (dim n x p)
y The response matrix (dim n x q)

The predict function, instead, takes as input:

out The output of a previous call to train.fun
newx The new features to evaluate (i.e. an n0 x p matrix)
}
\seealso{
\code{\link{conformal.multidim.split}}
}
