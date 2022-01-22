
sample_size=98

my_grid <- seq(from=0,to=1,length.out=5)
mu <- c(0,0,0)
sigma <- rbind(c(1,0.6,0.6), c(0.6,1,0.6), c(0.6,0.6,1))
mltvnorm3 <- mvtnorm::rmvnorm(sample_size, mu, sigma)
y=t(apply(mltvnorm3,1,function(x) x[1] + x[2]*cos(6*pi*my_grid) + x[3]*sin(6*pi*my_grid)))
x=mltvnorm3 + mvtnorm::rmvt(sample_size, diag(length(mu)))## add noise

n0=10
x0 = mvtnorm::rmvt(n0, diag(length(mu)))

fun=mean_multi()
fun=lm_multi()
fun=elastic.funs()


############################## SPLIT CONFORMAL


final.point = conformal.multidim.split(x,y[,1:2],x0[1:10,], fun$train.fun, fun$predict.fun,
                             alpha=0.1,
                                split=NULL, seed=FALSE, randomized=FALSE,seed_tau=FALSE,
                                verbose=FALSE, training_size=0.5,score ="l2",s_type="st-dev")

ppp2<-plot_multidim(final.point)
