
n = 4
n0 = 2
p = 2
mu = rep(0,p)
x = mvtnorm::rmvnorm(n, mu)
my_grid <- seq(from=0,to=1,length.out=2)
y = t(apply(x,1,function(u) u[1] + u[2]*cos(6*pi*my_grid)))
x0 = mvtnorm::rmvnorm(n0, mu)
fun=mean_multi()
#fun=lm_multi()

#################################### FULL CONFORMAL

final.full=conformal.multidim.full(x, y, x0, fun$train.fun,
                                fun$predict.fun, score="l2",
                                num.grid.pts.dim=5, grid.factor=1.25,
                                verbose=FALSE)

ppp<-plot_multidim_full(final.full)

