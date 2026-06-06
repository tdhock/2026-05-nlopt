# https://optimization.cbe.cornell.edu/index.php?title=Trust-region_methods
library(data.table)
library(animint2)
dir.vec <- c(1, 3)/10
p.vec <- c(-3, -1)/10
Delta <- 1
max_step_in_ball <- function(){
  vA <- c(t(p.vec)%*%p.vec)
  vB <- c(2*t(p.vec)%*%dir.vec)
  vC <- c(t(dir.vec)%*%dir.vec-Delta^2)
  (-vB + sqrt(vB^2-4*vA*vC))/(2*vA)
}
sol.vec <- dir.vec+max_step_in_ball()*p.vec
vector.list <- list(
  dir=dir.vec,
  p=p.vec,
  solution=sol.vec)
vector.dt.list <- list()
for(name in names(vector.list)){
  vec <- vector.list[[name]]
  vector.dt.list[[name]] <- data.table(t(vec), name)
}
(vector.dt <- rbindlist(vector.dt.list))
radians <- seq(0, 2*pi, length.out = 200)
ball.path.dt <- data.table(
  radians,
  x=cos(radians)*Delta,
  y=sin(radians)*Delta)
ggplot()+
  coord_equal()+
  geom_segment(aes(
    0, 0, xend=V1, yend=V2, color=name),
    data=vector.dt)+
  geom_path(aes(
    x, y),
    data=ball.path.dt)

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
cgrad.action <- function(){
  max.dstep <<- max_step_in_ball()
  pQp <<- t(p.vec)%*%Q.mat%*%p.vec
  if(pQp<0)return("concave")
  gvec <- grad_at_d(dir.vec)
  if(norm(gvec)<grad.thresh)return("Newton")
  dstep <<- c(-t(gvec) %*% p.vec / pQp)
  if(dstep>max.dstep)return("outside")
  return("update")
}
cgrad.it <- function(){
  action <<- cgrad.action()
  if(action=="Newton"){
    dir.vec
  }else if(action=="update"){
    dir.vec <<- dir.vec+dstep*p.vec
    p.step <- c(t(grad_at_d(dir.vec)) %*% Q.mat %*% p.vec / pQp)
    p.vec <<- p.step*p.vec-grad_at_d(dir.vec)
    NULL
  }else{
    dstep <<- max.dstep
    dir.vec+dstep*p.vec
  }
}
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
ball.q.dt.list <- list()
ball.f.dt.list <- list()
step.dt.list <- list()
q.grid.dt.list <- list()
(initial.dt <- CJ(V1=iseq, V2=iseq, radius=2^seq(-2, 6))[
, x.start := sprintf("%.1f, %.1f", V1, V2)
][])

for(param.i in 1:nrow(initial.dt)){
  initial.row <- initial.dt[param.i]
  x.start <- initial.row$x.start
  #current.x <- c(-4.0, 0)
  current.x <- initial.row[, c(V1, V2)]
  Delta.start <- Delta <- initial.row$radius
  cat(sprintf("%d / %d params %s r=%.3f\n", param.i, nrow(initial.dt), x.start, Delta.start))
  step.num <- 0
  grad.thresh <- 1e-10
  action <- "initial"
  while({
    norm.grad <- L2norm(c.vec <- xg(current.x))
    step.dt.list[[paste(x.start, Delta.start, step.num)]] <- data.table(
      x.start, Delta.start, step.num,  t(current.x), norm.grad, f=xf(current.x), action, Delta)
    norm.grad > grad.thresh
  }){
    Q.mat <- xh(current.x)
    (ball.q.dt.list[[paste(x.start, Delta.start, step.num)]] <- data.table(
      x.start, Delta.start, step.num,  radians,
      Delta,
      x=cos(radians)*Delta,
      y=sin(radians)*Delta
    )[, qfun := qd(x,y)][])
    qseq <- seq(-Delta, Delta, length.out = 21)
    q.grid.dt.list[[paste(x.start, Delta.start, step.num)]] <- data.table(x.start, Delta.start, step.num,  CJ(V1=qseq, V2=qseq))[
    , qfun := qd(V1, V2)
    ][, let(
      qrel = (qfun-min(qfun))/(max(qfun)-min(qfun)),
      V1rel=V1/Delta,
      V2rel=V2/Delta
    )][]
    dir.vec <- c(0,0)
    p.vec <- -grad_at_d(dir.vec)
    action <- "init"
    done <- FALSE
    iteration <- 1
    while(!done){
      p.before <- p.vec
      dir.before <- dir.vec
      cgrad.dir <- cgrad.it()
      vector.list <- list(
        dir=Vec(dir.before),
        p=Vec(dir.before+dstep*p.before, dir.before))
      if(is.numeric(cgrad.dir)){
        done <- TRUE
        vector.list$result <- Vec(cgrad.dir)
      }
      for(name in names(vector.list)){
        vdt <- vector.list[[name]]
        vector.dt.list[[paste(x.start, Delta.start, step.num, iteration, name)]] <- data.table(
          x.start, Delta.start, step.num,  iteration, name, Delta, vdt, action)
      }
      iteration <- iteration+1
    }
    opt.vec.list <- list(
      neg.grad=-c.vec/L2norm(c.vec)*Delta,
      cgrad=cgrad.dir)
    for(name in names(opt.vec.list)){
      vec <- opt.vec.list[[name]]
      opt.vec.dt.list[[paste(x.start, Delta.start, step.num, name)]] <- data.table(
        x.start, Delta.start, step.num,  name, Vec(current.x+vec, current.x))
    }
    ball.f.dt.list[[paste(x.start, Delta.start, step.num)]] <- data.table(
      x.start, Delta.start, step.num,  radians,
      x=cos(radians)*Delta+current.x[1],
      y=sin(radians)*Delta+current.x[2])
    f.diff <- xf(current.x)-xf(cgrad.dir+current.x)
    q.diff <- -xqd(cgrad.dir)
    ratio <- f.diff/q.diff
    Delta.mult <- fcase(
      ratio>0.75 && dstep==max.dstep, 2,
      ratio<0.25, 0.5,
      default=1)
    Delta <- Delta*Delta.mult
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
