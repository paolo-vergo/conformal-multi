
n = 33
n0 = 2
p = 2
mu = rep(0,p)
x = mvtnorm::rmvnorm(n, mu)
my_grid <- seq(from=0,to=1,length.out=2)
y = t(apply(x,1,function(u) u[1] + u[2]*cos(6*pi*my_grid)))
x0 = mvtnorm::rmvnorm(n0, mu)
fun=mean_multi()
#fun=lm_multi()

B=3

final.multi=conformal.multidim.msplit(x=x,y=y, x0=x0,
                                      fun$train.fun, fun$predict.fun,
                       alpha=0.1,
                       split=NULL, seed=FALSE, randomized=FALSE,seed_beta=FALSE,
                       verbose=FALSE, training_size=NULL,s_type="st-dev",B=B,lambda=0,
                       score="l2")



