library(data.table)
library(animint2)
f <- function(V1, V2){
  (V1^2 + V2 - 11)^2 + (V1+V2^2-7)^2
}
xfun <- function(fun){
  force(fun)
  function(x)do.call(fun, as.list(x))
}
xf <- xfun(f)
Vec <- function(to, from=c(0,0))data.table(to=t(to), from=t(from))
grad <- function(x,y)c(
  2*(x^2+y-11)*2*x+2*(x+y^2-7),
  2*(x^2+y-11)+2*(x+y^2-7)*2*y)
xg <- xfun(grad)
hess <- function(x,y)rbind(
  c(4*(x^2+y-11)+8*x^2+2, 4*x+4*y),
  c(4*x+4*y, 4*(x+y^2-7)+8*y^2+2))
xh <- xfun(hess)
grad_at_d <- function(dir.vec)Q.mat %*% dir.vec + c.vec
qd <- function(V1, V2){
  V.mat <- cbind(V1, V2)
  0.5*rowSums((V.mat %*% Q.mat) * V.mat) + V.mat %*% c.vec
}
xqd <- xfun(qd)
L2norm <- function(x)sqrt(sum(x^2))

gseq <- seq(-5, 5, by=0.1)
(grid.dt <- CJ(V1=gseq, V2=gseq)[
, log10.f := log10(f(V1,V2)+1)
][])
ggplot()+
  coord_equal()+
  geom_tile(aes(
    V1, V2, fill=log10.f),
    color=NA,
    data=grid.dt)+
  scale_fill_gradient(low="red", high="white")

iseq <- seq(-4, 4, by=1)
iseq <- seq(-4, 4, by=4)#for testing
opt.vec.dt.list <- list()
step.dt.list <- list()
(initial.dt <- CJ(V1=iseq, V2=iseq, tau=seq(0.1, 0.4, by=0.1))[
, x.start := sprintf("%.1f, %.1f", V1, V2)
][])
armijo.point.dt.list <- list()
armijo.fun.dt.list <- list()
for(param.i in 1:nrow(initial.dt)){
  initial.row <- initial.dt[param.i]
  x.start <- initial.row$x.start
  tau <- initial.row$tau
  #current.x <- c(-4.0, 0)
  current.x <- initial.row[, c(V1, V2)]
  cat(sprintf("%d / %d params %s tau=%.1f\n", param.i, nrow(initial.dt), x.start, tau))
  step.num <- 0
  grad.thresh <- 1e-8
  armijo.steps <- 0
  while({
    norm.grad <- L2norm(c.vec <- xg(current.x))
    step.dt.list[[paste(x.start, tau, step.num)]] <- data.table(
      x.start, tau, step.num,  t(current.x), norm.grad, f=xf(current.x),
      armijo.steps)
    norm.grad > grad.thresh
  }){
    dir.vec <- -c.vec
    min.log2.step <- -20
    (step.size.vec <- 2^seq(0, min.log2.step))
    armijo.slope <- c(tau*t(c.vec) %*% dir.vec)
    armijo.intercept <- xf(current.x)
    fstep <- function(step.vec){
      do.call(f, as.data.table(t(matrix(dir.vec,2,length(step.vec))*matrix(step.vec, 2, length(step.vec), byrow=TRUE)+current.x)))
    }
    armijo.grid <- unique(sort(c(
      step.size.vec,
      2^seq(0, min.log2.step, l=200),
      seq(0, 1, by=0.01))))
    armijo.fun.list <- list(
      objective=fstep,
      bound=function(step.vec)armijo.intercept+step.vec*armijo.slope)
    armijo.it.dt.list <- list()
    for(Function in names(armijo.fun.list)){
      fun <- armijo.fun.list[[Function]]
      armijo.it.dt.list[[Function]] <- data.table(
        Function, step.size=armijo.grid, f=fun(armijo.grid)
      )[, f.thresh := ifelse(f<0, 0, f)][, let(
        log2.step=log2(step.size),
        log10.f=log10(f.thresh))][]
    }
    (armijo.it.dt <- rbindlist(armijo.it.dt.list))
    armijo.only <- armijo.it.dt[step.size %in% step.size.vec]
    armijo.wide <- dcast(
      armijo.only,
      step.size + log2.step ~ Function,
      value.var=c("f", "log10.f")
    )[.N:1][, admissible := f_objective<f_bound][1:which(admissible)[1]]
    armijo.steps <- nrow(armijo.wide)
    ggplot()+geom_line(aes(step.size, f, color=Function), data=armijo.it.dt)
    ggplot()+
      geom_line(aes(log2.step, log10.f, color=Function), data=armijo.it.dt)+
      geom_point(aes(log2.step, log10.f_objective, fill=admissible), data=armijo.wide, shape=21)
    armijo.fun.dt.list[[paste(x.start, tau, step.num)]] <- data.table(
      x.start, tau, step.num, armijo.it.dt)
    armijo.point.dt.list[[paste(x.start, tau, step.num)]] <- data.table(
      x.start, tau, step.num, armijo.wide)
    armijo.step <- armijo.wide[.N, step.size]
    step.vec <- dir.vec*armijo.step
    new.x <- current.x+step.vec
    opt.vec.list <- list(
      Armijo.step=step.vec)
    for(name in names(opt.vec.list)){
      vec <- opt.vec.list[[name]]
      opt.vec.dt.list[[paste(x.start, tau, step.num, name)]] <- data.table(
        x.start, tau, step.num,  name, Vec(current.x+vec, current.x))
    }
    current.x <- new.x
    step.num <- step.num+1
  }
}
(opt.vec.dt <- rbindlist(opt.vec.dt.list))
(step.dt <- rbindlist(step.dt.list))
table(step.dt$armijo.steps)
(armijo.fun.dt <- rbindlist(armijo.fun.dt.list))
(armijo.point.dt <- rbindlist(armijo.point.dt.list))

save(opt.vec.dt, step.dt, armijo.fun.dt, armijo.point.dt, grid.dt, file="figure-armijo-2D-data.RData")
