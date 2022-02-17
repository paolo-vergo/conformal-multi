## One instance

n=75
p=4
q=2
mu=rep(0,p)
x = mvtnorm::rmvnorm(n, mu)
beta<-sapply(1:q, function(k) c(mvtnorm::rmvnorm(1,mu)))
y = x%*%beta + t(mvtnorm::rmvnorm(q,1:n))
x0=x[ceiling(0.9*n):n,]
y0=y[ceiling(0.9*n):n,]
n0<-nrow(y0)
funs=lm_multi()

sol<-conformal.multidim.jackplus(x,y,x0,train.fun = funs$train.fun,
                                      predict.fun = funs$predict.fun,alpha=0.05)


valid.point<-mean(sapply(1:n0,function(u) prod(sapply(1:q, function(j)
  as.numeric(sol[[u]][1,j] <= y0[u,j] && sol[[u]][2,j] >= y0[u,j]  )))))


valid.point




