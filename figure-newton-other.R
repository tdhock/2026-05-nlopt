library(data.table)
library(animint2)
x.min <- 0
x.max <- 5*pi
thresh.x <- function(xbar)fcase(
  xbar < x.min, -Inf,
  xbar > x.max, Inf,
  default=xbar)
y.min <- -2
y.max <- 2
thresh.f <- function(y)fcase(
  y < y.min, NA_real_,
  y > y.max, NA_real_,
  default=y)
inf.f <- function(y)fcase(
  y < y.min, -Inf,
  y > y.max, Inf,
  default=y)
grid.x <- seq(x.min, x.max, l=200)
grid.dt <- data.table(grid.x, fx=sin(grid.x), what="objective")
grid.by <- 0.1
grid.by <- 2
initial.x <- seq(x.min, x.max, by=grid.by)[-1]
approx.dt.list <- list()
newton.dt.list <- list()
initial.dt.list <- list()
grad.dt.list <- list()
for(initial.i in seq_along(initial.x)){
  x0 <- first.x <- initial.x[initial.i]
  done <- FALSE
  iteration <- 0
  while(!done){
    iteration <- iteration+1
    initial.dt.list[[paste(initial.i, iteration)]] <- data.table(
      initial.i, initial.x=first.x, iteration, x0, fx=sin(x0), abs.grad=abs(cos(x0)), what="initial")
    Taylor <- function(x)sin(x0)+cos(x0)*(x-x0)-0.5*sin(x0)*(x-x0)^2
    approx.dt.list[[paste(initial.i, iteration)]] <- data.table(
      initial.i, initial.x=first.x, iteration, grid.x, qx=Taylor(grid.x), what="approx")
    new.x <- x0+cos(x0)/sin(x0)
    if(new.x==x0)done <- TRUE
    newton.dt.list[[paste(initial.i, iteration)]] <- data.table(
      initial.i, initial.x=first.x, iteration, new.x,
      rbind(
        data.table(value=sin(new.x), what="new objective"),
        data.table(value=Taylor(new.x), what="q'(x)=0")))
    x0 <- new.x
  }
}
approx.dt <- rbindlist(approx.dt.list)
newton.dt <- rbindlist(newton.dt.list)
initial.dt <- rbindlist(initial.dt.list)
frames <- function(frame.vals, dt){
  data.table(frame=frame.vals)[, data.table(dt), by=frame][, Frame := iteration+(frame-1)/3][]
}   
quad.line.dt <- frames(2:3, approx.dt)
init.point.dt <- rbind(
  frames(1:2, initial.dt[, .(initial.i, initial.x, iteration, x0, fx, what="f(x*) guess")]),
  frames(3, newton.dt[what=="q'(x)=0", .(initial.i, initial.x, iteration, x0=new.x, fx=value, what)]))
max.dt <- initial.dt[, .SD[iteration==max(iteration)], by=initial.i]
max.dt[order(abs.grad)]
max.dt[order(iteration)]
err.dt <- max.dt[
, .(initial.i, xbar=x0)
][
  initial.dt, on="initial.i"
][
, err := x0-xbar
][
, abs.err := abs(err)
][
, is.end := c(rev(cumsum(diff(rev(abs.err))<=0)==0),TRUE)
, by=initial.i
][]
for(conv.order in 1:2){
  err.dt[, paste0(
    "ratio", conv.order
  ) := c(abs.err[-1]/abs.err[-length(err)]^conv.order,NA), by=initial.i][]
}
first.dt <- err.dt[iteration==1]
first.show <- data.table(first.dt)[, xthresh := thresh.x(xbar)][]

ggplot()+
  geom_line(aes(
    iteration, log10(abs(err)), group=initial.i),
    data=err.dt[is.end==TRUE])

ggplot()+
  geom_line(aes(
    iteration, ratio2, group=initial.i),
    data=err.dt[is.end==TRUE])


