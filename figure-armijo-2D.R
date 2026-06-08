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
vector.dt.list <- list()
opt.vec.dt.list <- list()
step.dt.list <- list()
(initial.dt <- CJ(V1=iseq, V2=iseq)[
, x.start := sprintf("%.1f, %.1f", V1, V2)
][])

for(param.i in 1:nrow(initial.dt)){
  initial.row <- initial.dt[param.i]
  x.start <- initial.row$x.start
  #current.x <- c(-4.0, 0)
  current.x <- initial.row[, c(V1, V2)]
  Delta.start <- Delta <- initial.row$radius
  cat(sprintf("%d / %d params %s\n", param.i, nrow(initial.dt), x.start))
  step.num <- 0
  grad.thresh <- 1e-8
  while({
    norm.grad <- L2norm(c.vec <- xg(current.x))
    step.dt.list[[paste(x.start, step.num)]] <- data.table(
      x.start, step.num,  t(current.x), norm.grad, f=xf(current.x))
    norm.grad > grad.thresh
  }){
    dir.vec <- -c.vec
    (step.size.vec <- 2^seq(0, -20))
    tau <- 0.2
    armijo.slope <- tau*t(c.vec) %*% dir.vec
    armijo.intercept <- xf(current.x)
    fstep <- function(step.vec){
      do.call(f, as.data.table(t(matrix(dir.vec,2,length(step.vec))*matrix(step.vec, 2, length(step.vec), byrow=TRUE)+current.x)))
    }
    curve(fstep(x), 0, 1)
    abline(armijo.intercept, armijo.slope)
    armijo.vec <- armijo.intercept+step.size*armijo.slope
    points(step.size, armijo.vec)
    armijo.ok <- fx.at.step<armijo.vec
    if(all(armijo.ok==FALSE))stop("no armijo steps")
    first.ok <- which(armijo.ok)[1]
    armijo.show <- 1:first.ok
    armijo.class <- rep(NA, length(armijo.ok))
    armijo.class[armijo.show] <- FALSE
    armijo.class[first.ok] <- TRUE
    points(step.size, fx.at.step, col=ifelse(armijo.class, "black", "red"))
    ls.dt.list[[paste(initial.i, iteration)]] <- data.table(
      initial.i, initial.x=first.x, iteration, 
      step=step.size, armijo=armijo.vec, fx=fx.at.step,
      frame(length(armijo.show))
    )[armijo.show]
    armijo.step <- step.size[first.ok]
    new.x <- x0+x0.dir*armijo.step
    if(new.x==x0)done <- TRUE
    armijo.dt.list[[paste(initial.i, iteration)]] <- data.table(
      initial.i, initial.x=first.x, iteration, new.x,
      armijo.slope, armijo.intercept)
    x0 <- new.x

    opt.vec.list <- list(
      neg.grad=-c.vec/L2norm(c.vec)*Delta,
      cgrad=cgrad.dir)
    for(name in names(opt.vec.list)){
      vec <- opt.vec.list[[name]]
      opt.vec.dt.list[[paste(x.start, step.num, name)]] <- data.table(
        x.start, step.num,  name, Vec(current.x+vec, current.x))
    }
    step.num <- step.num+1
    current.x <- current.x+cgrad.dir
  }
}
(vector.dt <- rbindlist(vector.dt.list))
(ball.f.dt <- rbindlist(ball.f.dt.list))
(ball.q.dt <- rbindlist(ball.q.dt.list))
(opt.vec.dt <- rbindlist(opt.vec.dt.list))
(q.grid.dt <- rbindlist(q.grid.dt.list))
(step.dt <- rbindlist(step.dt.list)[
, next.action := c(action[-1], "end")
, by=.(x.start, Delta.start)][])
Delta.dt <- unique(step.dt[, .(Delta.start)])
save(vector.dt, ball.f.dt, ball.q.dt, opt.vec.dt, q.grid.dt, step.dt, Delta.dt, grid.dt, initial.dt, ball.path.dt, file="figure-trust-region-2D-data.RData")
