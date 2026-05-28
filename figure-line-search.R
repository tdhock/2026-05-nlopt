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
armijo.dt.list <- list()
initial.dt.list <- list()
grad.dt.list <- list()
initial.x=5
for(initial.i in seq_along(initial.x)){
  x0 <- first.x <- initial.x[initial.i]
  done <- FALSE
  last.frame <- 0
  frame <- function(n=1){
    out <- data.table(frame=seq(last.frame+1, last.frame+n))
    last.frame <<- last.frame+n
    out
  }
  iteration <- 0
  while(!done){
    iteration <- iteration+1
    x0.grad <- cos(x0)
    x0.dir <- -x0.grad
    initial.dt.list[[paste(initial.i, iteration)]] <- data.table(
      initial.i, initial.x=first.x, iteration, x0, fx=sin(x0), abs.grad=abs(x0.grad), what="initial")
    grid.dt[, plot(grid.x, fx, type="l")]
    points(x0, sin(x0))
    arrows(x0, sin(x0), x0+x0.dir, sin(x0), length=0.1)
    step.size <- 2^seq(0, -20)
    fx.at.step <- sin(x0+x0.dir*step.size)
    tau <- 0.2
    armijo.slope <- tau*x0.grad*x0.dir
    armijo.intercept <- sin(x0)
    curve(sin(x0+x0.dir*x), 0, 1)
    abline(armijo.intercept, armijo.slope)
    armijo.vec <- armijo.intercept+step.size*armijo.slope
    points(step.size, armijo.vec)
    points(step.size, fx.at.step)
    armijo.ok <- fx.at.step<armijo.vec
    if(all(armijo.ok==FALSE))stop("no admissible steps")
    first.ok <- which(armijo.ok)[1]
    armijo.show <- 1:first.ok
    approx.dt.list[[paste(initial.i, iteration)]] <- data.table(
      initial.i, initial.x=first.x, iteration, grid.x, qx=Taylor(grid.x), what="approx")
    new.x <- x0+cos(x0)/sin(x0)
    if(new.x==x0)done <- TRUE
    armijo.dt.list[[paste(initial.i, iteration)]] <- data.table(
      initial.i, initial.x=first.x, iteration, new.x,
      rbind(
        data.table(value=sin(new.x), what="new objective"),
        data.table(value=Taylor(new.x), what="q'(x)=0")))
    x0 <- new.x
  }
}
approx.dt <- rbindlist(approx.dt.list)
armijo.dt <- rbindlist(armijo.dt.list)
  initial.dt <- rbindlist(initial.dt.list)
  
frames <- function(frame.vals, dt){
  data.table(frame=frame.vals)[, data.table(dt), by=frame][, Frame := iteration+(frame-1)/3][]
}   
quad.line.dt <- frames(2:3, approx.dt)
init.point.dt <- rbind(
  frames(1:2, initial.dt[, .(initial.i, initial.x, iteration, x0, fx, what="f(x*) guess")]),
  frames(3, armijo.dt[what=="q'(x)=0", .(initial.i, initial.x, iteration, x0=new.x, fx=value, what)]))
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
first.dt <- err.dt[iteration==1]
first.show <- data.table(first.dt)[, xthresh := thresh.x(xbar)][]

(prev.dt <- initial.dt[
  initial.dt,
  on=.(initial.x, iteration <= iteration),
  data.table(offset=(0:2)/3)[, data.table(
    initial.x, iteration=x.iteration, Frame=i.iteration+offset,
    x0, fx, point="f(x) prev"
  ), by=offset],
  nomatch=0L])
selected.color <- "red"
history.color <- "grey"
viz <- animint(
  title="Armijo’s method for stationary point finding",
  source="https://github.com/tdhock/2026-05-nlopt/blob/main/figure-armijo.R",
  approx=ggplot()+
    ggtitle("Objective and iterations for selection")+
    scale_x_continuous("x = optimization variable")+
    scale_y_continuous("f(x) = optimization objective")+
    scale_color_manual(values=c(
      "f(x) objective"="black",
      "q(x) approx"="violet"))+
    scale_fill_manual(values=c(
      "f(x) prev"=history.color,
      "f(x*) guess"="grey50",
      "q'(x)=0"="violet"))+
    theme_bw()+
    theme_animint(width=800, colspan=2, last_in_row=TRUE)+
    coord_cartesian(ylim=c(y.min, y.max))+
    geom_line(aes(
      grid.x, fx, color=Function),
      data=grid.dt[, Function := "f(x) objective"])+
    geom_path(aes(
      thresh.x(x0), fx,
      key=1),
      color=history.color,
      showSelected=c("initial.x","Frame"),
      data=prev.dt)+
    geom_point(aes(
      thresh.x(x0), fx,
      key=iteration,
      fill=point),
      showSelected=c("initial.x","Frame"),
      size=3,
      data=prev.dt)+
    geom_point(aes(
      thresh.x(x0), fx,
      key=1,
      fill=point),
      showSelected=c("initial.x","Frame"),
      size=5,
      data=init.point.dt[, point := what])+
    geom_path(aes(
      grid.x, qx,
      key=1,
      color=Function),
      showSelected=c("initial.x","Frame"),
      data=quad.line.dt[, Function := "q(x) approx"]),
  select=ggplot()+
    ggtitle("Select initial point")+
    scale_x_continuous("")+
    scale_y_continuous("")+
    geom_segment(aes(
      x0, xthresh,
      xend=x0, yend=-Inf),
      clickSelects="initial.x",
      alpha=0.5,
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
      color="black",
      fill=selected.color,
      alpha_off=0.2,
      alpha=1,
      color_off="black",
      data=data.table(xvar="start", yvar="end", first.show))+
    geom_point(aes(
      initial.x, iteration),
      clickSelects="initial.x",
      color="black",
      fill=selected.color,
      alpha_off=0.2,
      alpha=1,
      color_off="black",
      data=data.table(xvar="start", yvar="iterations", max.dt))+
    facet_grid(yvar ~ xvar, scales="free")+
    geom_line(aes(
      grid.x, fx),
      data=data.table(xvar="start", yvar="f(x)", grid.dt))+
    geom_point(aes(
      x0, fx),
      color="black",
      fill=selected.color,
      showSelected="initial.x",
      data=data.table(xvar="start", yvar="f(x)", first.dt))+
    geom_tallrect(aes(
      xmin=x0-grid.by/2, xmax=x0+grid.by/2),
      clickSelects="initial.x",
      alpha=0.5,
      color=selected.color,
      fill=selected.color,
      data=data.table(xvar="start", yvar="f(x)", first.dt))+
    geom_tallrect(aes(
      xmin=x0-grid.by/2, xmax=x0+grid.by/2),
      clickSelects="initial.x",
      alpha=0.5,
      color=selected.color,
      fill=selected.color,
      data=data.table(xvar="start", yvar="iterations", first.dt))+
    geom_path(aes(
      fx, grid.x),
      data=data.table(xvar="f(x)", yvar="end", grid.dt))+
    geom_label_aligned(aes(
      sin(xbar)*0.8, xthresh,
      label=sprintf(
        " %s at x=%.1f f(x)=%.0f ",
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
      color="black",
      fill=selected.color,
      showSelected="initial.x",
      data=data.table(xvar="f(x)", yvar="end", first.show)),
  steps=ggplot()+
    ggtitle("Select optimization iteration")+
    scale_x_continuous("Iteration", 1:10)+
    scale_y_continuous("")+
    facet_grid(yvar ~ .,scales="free")+
    geom_vline(aes(
      xintercept=Frame),
      color="grey50",
      showSelected="initial.x",
      data=init.point.dt[frame==1])+
    geom_path(aes(
      Frame, log10(abs.grad)),
      showSelected="initial.x",
      data=data.table(yvar="log10[abs(grad)]", initial.dt[, Frame := iteration]))+
    geom_point(aes(
      Frame, log10(abs.grad)),
      showSelected="initial.x",
      data=data.table(yvar="log10[abs(grad)]", initial.dt))+
    geom_path(aes(
      Frame, inf.f(fx)),
      showSelected="initial.x",
      data=data.table(yvar="f(x)", init.point.dt[order(Frame)]))+
    geom_point(aes(
      Frame, inf.f(fx)),
      showSelected="initial.x",
      data=data.table(yvar="f(x)", init.point.dt))+
    make_tallrect(init.point.dt, "Frame"),
  duration=list(Frame=500),
  time=list(ms=1000, variable="Frame"),
  out.dir="figure-armijo",
  first=list(initial.x=12.2)
)
viz

if(FALSE){
  animint2pages(viz, "2026-05-25-Armijo-stationary-point", chromote_sleep_seconds=3)
}
