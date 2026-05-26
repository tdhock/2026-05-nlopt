library(data.table)
library(animint2)
x.min <- 0
x.max <- 4*pi
grid.x <- seq(x.min, x.max, l=200)
grid.dt <- data.table(grid.x, fx=sin(grid.x), what="objective")
grid.by <- 0.1
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
        data.table(value=Taylor(new.x), what="critical point")))
    x0 <- new.x
  }
}
approx.dt <- rbindlist(approx.dt.list)[qx %between% c(-2, 4)]
newton.dt <- rbindlist(newton.dt.list)
initial.dt <- rbindlist(initial.dt.list)

ggplot()+
  geom_line(aes(
    grid.x, fx, color=what),
    data=grid.dt)+
  geom_point(aes(
    x0, fx, color=what),
    data=initial.dt)+
  geom_line(aes(
    grid.x, qx, color=what),
    data=approx.dt)+
  geom_point(aes(
    new.x, value, color=what),
    data=newton.dt)+
  facet_grid(initial.x ~ iteration, labeller=label_both)

  

ggplot()+
  geom_line(aes(
    iteration, log10(abs.grad)),
    data=initial.dt)+
  geom_point(aes(
    iteration, log10(abs.grad)),
    data=initial.dt)+
  facet_grid(initial.x ~ ., labeller=label_both)

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
plot(log10(abs(err)) ~ iteration, err.dt[initial.i==82], col=ifelse(is.end, "red", "black"))

ggplot()+
  geom_line(aes(
    iteration, log10(abs(err)), group=initial.i),
    data=err.dt[is.end==TRUE])

ggplot()+
  geom_line(aes(
    iteration, ratio2, group=initial.i),
    data=err.dt[is.end==TRUE])

first.dt <- err.dt[iteration==1]
ggplot()+
  geom_point(aes(
    x0, xbar),
    data=first.dt)

first.show <- data.table(first.dt)[, xthresh := fcase(
  xbar < x.min, -Inf,
  xbar > x.max, Inf,
  default=xbar)][]

selected.color <- "red"
animint(
  ggplot()+
    geom_segment(aes(
      x0, xthresh,
      xend=x0, yend=-Inf),
      showSelected="initial.x",
      color=selected.color,
      data=data.table(xvar="start", yvar="end", first.show))+
    geom_segment(aes(
      x0, xthresh,
      xend=-Inf, yend=xthresh),
      showSelected="initial.x",
      color=selected.color,
      data=data.table(xvar="start", yvar="end", first.show))+
    geom_point(aes(
      x0, xthresh),
      clickSelects="initial.x",
      color=selected.color,
      alpha_off=0.2,
      alpha=1,
      color_off="black",
      data=data.table(xvar="start", yvar="end", first.show))+
    facet_grid(yvar ~ xvar, scales="free")+
    geom_line(aes(
      grid.x, fx),
      data=data.table(xvar="start", yvar="f(x)", grid.dt))+
    geom_point(aes(
      x0, fx),
      color=selected.color,
      showSelected="initial.x",
      data=data.table(xvar="start", yvar="f(x)", first.dt))+
    geom_tallrect(aes(
      xmin=x0-grid.by/2, xmax=x0+grid.by/2),
      clickSelects="initial.x",
      alpha=0.5,
      color=selected.color,
      data=data.table(xvar="start", yvar="f(x)", first.dt))+
    geom_path(aes(
      fx, grid.x),
      data=data.table(xvar="f(x)", yvar="end", grid.dt))+
    geom_label_aligned(aes(
      sin(xbar), xthresh,
      label=sprintf(
        "%s at x=%.1f f(x)=%.3f",
        ifelse(sin(xbar)>0, "max", "min"),
        xbar, sin(xbar)),
      hjust=ifelse(sin(xbar)>0, 1, 0)),
      alignment="vertical",
      showSelected="initial.x",
      alpha=0.5,
      color=selected.color,
      data=data.table(xvar="f(x)", yvar="end", first.show))+
    geom_point(aes(
      sin(xbar), xthresh),
      color=selected.color,
      showSelected="initial.x",
      data=data.table(xvar="f(x)", yvar="end", first.show))
)
