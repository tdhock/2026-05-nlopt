# https://optimization.cbe.cornell.edu/index.php?title=Trust-region_methods
library(data.table)
library(animint2)
dir.vec <- c(1, 3)
p.vec <- c(-3, -1)
Delta <- 5
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

initial.x <- current.x <- c(-4.0, 0)
#initial.x <- current.x <- c(0, -5)
initial.radius <- Delta <- 1
step.num <- 0
grad.thresh <- 1e-10
vector.dt.list <- list()
opt.vec.dt.list <- list()
ball.q.dt.list <- list()
ball.f.dt.list <- list()
step.dt.list <- list()
q.grid.dt.list <- list()
action <- "initial"
while({
  norm.grad <- L2norm(c.vec <- xg(current.x))
  print(step.dt.list[[paste(step.num)]] <- data.table(
    step.num, t(current.x), norm.grad, f=xf(current.x), action, Delta))
  norm.grad > grad.thresh
}){
  Q.mat <- xh(current.x)
  (ball.q.dt.list[[paste(step.num)]] <- data.table(
    step.num, radians,
    Delta,
    x=cos(radians),
    y=sin(radians)
  )[, qfun := qd(x,y)][])
  qseq <- seq(-Delta, Delta, length.out = 21)
  q.grid.dt.list[[paste(step.num)]] <- data.table(step.num, CJ(V1=qseq, V2=qseq))[
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
      vector.dt.list[[paste(step.num, iteration, name)]] <- data.table(
        step.num, iteration, name, Delta, vdt, action)
    }
    iteration <- iteration+1
  }
  opt.vec.list <- list(
    neg.grad=-c.vec/L2norm(c.vec)*Delta,
    cgrad=cgrad.dir)
  for(name in names(opt.vec.list)){
    vec <- opt.vec.list[[name]]
    opt.vec.dt.list[[paste(step.num, name)]] <- data.table(
      step.num, name, Vec(current.x+vec, current.x))
  }
  ball.f.dt.list[[paste(step.num)]] <- data.table(
    step.num, radians,
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
(vector.dt <- rbindlist(vector.dt.list))
(ball.f.dt <- rbindlist(ball.f.dt.list))
(ball.q.dt <- rbindlist(ball.q.dt.list))
(opt.vec.dt <- rbindlist(opt.vec.dt.list))
(q.grid.dt <- rbindlist(q.grid.dt.list))
(step.dt <- rbindlist(step.dt.list)[, next.action := c(action[-1], "end")][])

step.long <- melt(step.dt[, let(
  "log10(|∇f|)"=log10(norm.grad),
  "log10(f)"=log10(f),
  "log2(Δ)"=log2(Delta)
)], measure.vars = c("log10(|∇f|)", "log10(f)", "log2(Δ)"))
(minball <- ball.q.dt[, .SD[which.min(qfun)], by=step.num])
animint(
  out.dir="figure-trust-region-2D",
  duration=list(step.num=500),
  steps=ggplot()+
    geom_line(aes(
      step.num, value),
      data=step.long)+
    make_tallrect(step.long, "step.num")+
    facet_grid(variable ~ ., scales="free"),
  variables=ggplot()+
    coord_equal()+
    geom_tile(aes(
      V1, V2, fill=log10.f),
      color=NA,
      data=grid.dt)+
    scale_fill_gradient(low="blue", high="white")+
    geom_path(aes(
      x, y, group=1, key=1),
      showSelected="step.num",
      data=ball.f.dt)+
    geom_segment(aes(
      from.V1, from.V2, xend=to.V1, yend=to.V2,
      key=name, color=name),
      showSelected="step.num",
      data=opt.vec.dt)+
    geom_point(aes(
      to.V1, to.V2, color=name, key=name),
      showSelected="step.num",
      data=opt.vec.dt)+
    geom_point(aes(
      V1, V2, color=name, key=1),
      showSelected="step.num",
      data=data.table(name="iterate", step.dt))+
    geom_text(aes(
      V1, V2, label=next.action, key=1),
      showSelected="step.num",
      data=data.table(name="iterate", step.dt)),
  q=ggplot()+
    coord_equal(xlim=c(-1,1), ylim=c(-1,1))+
    geom_tile(aes(
      V1rel, V2rel, fill=qrel, key=paste(V1rel, V2rel)),
      showSelected="step.num",
      color=NA,
      data=q.grid.dt)+
    geom_path(aes(
      x, y, group=1, key=1),
      showSelected="step.num",
      data=ball.q.dt)+
    geom_segment(aes(
      from.V1/Delta, from.V2/Delta,
      xend=to.V1/Delta, yend=to.V2/Delta,
      key=name, color=name),
      showSelected="step.num",
      data=vector.dt)+
    geom_point(aes(
      to.V1/Delta, to.V2/Delta, color=name, key=name),
      showSelected="step.num",
      data=vector.dt)+
    geom_point(aes(
      x/Delta, y/Delta, color=name, key=1),
      showSelected="step.num",
      data=data.table(name="min", minball))+
    scale_fill_gradient(low="white", high="black")
)


