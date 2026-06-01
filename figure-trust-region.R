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
n.grid <- 400
grid.x <- seq(x.min, x.max, length.out = n.grid)
f <- sin
f.deriv <- cos
f.second <- function(x){
  out <- -sin(x)
  ifelse(out==0, 1e-4, out)
}
grid.dt <- data.table(grid.x, fx=f(grid.x), what="objective")
grid.by <- 2
grid.by <- 0.5
(initial.param.dt <- CJ(
  x=seq(x.min, x.max, by=grid.by)[-1],
  radius=2^seq(-5, 5)))
approx.dt.list <- list()
q.dt.list <- list()
candidate.dt.list <- list()
initial.dt.list <- list()
for(initial.i in 1:nrow(initial.param.dt)){
  init <- initial.param.dt[initial.i][, params := sprintf(
    "x=%.1f radius=%.3f", x, radius)][]
  x0 <- init$x
  delta <- init$radius
  done <- FALSE
  iteration <- 0
  while(!done){
    iteration <- iteration+1
    Taylor <- function(x)f(x0)+f.deriv(x0)*(x-x0)+0.5*f.second(x0)*(x-x0)^2
    qfun <- function(d)f(x0)+f.deriv(x0)*d+0.5*f.second(x0)*d^2
    approx.dt.list[[paste(initial.i, iteration)]] <- data.table(
      initial.i, initial=init, iteration, grid.x, px=Taylor(grid.x))
    grid.d <- seq(-1, 1, length.out = n.grid)
    q.dt.list[[paste(initial.i, iteration)]] <- data.table(
      initial.i, initial=init, iteration, grid.d, qd=qfun(grid.d*delta), fd=f(x0+grid.d*delta))
    newton.d <- -f.deriv(x0)/f.second(x0)
    dir.dt <- rowwiseDT(
      name=, offset=, feasible=,
      "min", -delta, TRUE,
      "Max", delta, TRUE,
      "Newton", newton.d, abs(newton.d)<delta
    )[, x := x0+offset][, let(
      px = Taylor(x),
      fx = f(x),
      norm.offset=offset/delta
    )][]
    candidate.dt.list[[paste(initial.i, iteration)]] <- data.table(
      initial.i, initial=init, iteration, dir.dt)
    best <- dir.dt[feasible==TRUE][which.min(px)]
    it.row <- data.table(
      delta, log2.delta=log2(delta), x0, fx0=f(x0),
      log10.abs.grad=log10(abs(f.deriv(x0))), best
    )[, ratio := ifelse(px==fx, 1, (fx0-fx)/(fx0-px))][]
    if(it.row$ratio<0.25){
      action <- "halve delta"
      delta <- delta/2
    }else{
      action <- "step"
      if(it.row$x==x0)done <- TRUE
      x0 <- it.row$x
    }
    if(it.row$ratio>0.75){
      action <- if(action=="step")"double delta, step" else "double delta"
      delta <- delta*2
    }
    initial.dt.list[[paste(initial.i, iteration)]] <- data.table(
      initial.i, initial=init, iteration, it.row, action)
  }
}
approx.dt <- rbindlist(approx.dt.list)
(initial.dt <- rbindlist(initial.dt.list))
(candidate.dt <- rbindlist(candidate.dt.list))
(q.dt <- rbindlist(q.dt.list))

first.it <- setkey(initial.dt[iteration==1], initial.i)
last.it <- initial.dt[, .(
  max.it=max(iteration),
  x.at.min=x,
  f.at.min=fx
), keyby=initial.i][first.it]

(init.long <- melt(
  initial.dt[, "f(x) obj." := fx0][, "log10[abs(grad)]" := log10.abs.grad][, "log2[delta]" := log2.delta],
  measure.vars=c("f(x) obj.", "log10[abs(grad)]", "log2[delta]"),
  id.vars=c("initial.params", "iteration")))
(ratio.dt <- melt(
  initial.dt[, "f(x+d) objective" := fx][, "q(d) approx." := px],
  measure.vars=c("f(x+d) objective", "q(d) approx."),
  variable.name="Function",
  id.vars=c("initial.params", "iteration", "x0", "fx0", "norm.offset", "ratio")))

Point_scale <- scale_fill_manual(values=c(
  prev="grey",
  initial="grey",
  step="grey50",
  Newton="red",
  min="deepskyblue",
  Max="blue"))
point.dt <- rbind(
  candidate.dt[, .(x, fx=px, step=norm.offset, Point = name, initial.params, iteration)],
  initial.dt[, .(x, fx, step=norm.offset, Point="step", initial.params, iteration)],
  initial.dt[, .(x=x0, fx=fx0, step=0, Point="initial", initial.params, iteration)])
zoom.dt <- melt(
  q.dt[, "f(x+d) objective" := fd][, "q(d) approx." := qd],
  variable.name="Function",
  measure.vars=c("f(x+d) objective", "q(d) approx."))
(prev.dt <- initial.dt[
  initial.dt,
  on=.(initial.params, iteration <= iteration),
  .(initial.params, iteration=i.iteration, x0, fx0, Point="prev"),
  nomatch=0L])
fsize <- 4
qsize <- 2
fun.sizes <- c(
  "f(x+d) objective"=fsize,
  "q(d) approx."=qsize)
trust.color <- "grey90"
FAC <- function(dt, facet, ...)data.table(dt, facet, ...)
OBJ <- function(dt, ...)FAC(dt, facet="f(x) objective", ...)
RAD <- function(dt, ...)FAC(dt, facet="log2(initial radius)", ...)
viz <- animint(
  title="Trust region with Newton optimization algorithm in 1D",
  source="https://github.com/tdhock/2026-05-nlopt/blob/main/figure-trust-region.R",
  ## init=ggplot()+
  ##   ggtitle("Select initial parameters")+
  ##   scale_fill_gradient(low="white", high="red")+
  ##   facet_grid(facet ~ ., scales="free")+
  ##   geom_tile(aes(
  ##     initial.x, log2(initial.radius),
  ##     fill=max.it),
  ##     data=OBJ(last.it),
  ##     clickSelects="initial.params")+
  ##   geom_text(aes(
  ##     initial.x, log2(initial.radius),
  ##     label=max.it),
  ##     data=OBJ(last.it),
  ##     clickSelects="initial.params")+
  ##   theme_bw()+
  ##   geom_tallrect(aes(
  ##     xmin=x0-delta, xmax=x0+delta, key=1),
  ##     showSelected=c("initial.params","iteration"),
  ##     fill=trust.color,
  ##     color=NA,
  ##     data=RAD(initial.dt))+
  ##   geom_path(aes(
  ##     x0, fx0, key=1),
  ##     showSelected=c("initial.params","iteration"),
  ##     size=6,
  ##     data=RAD(prev.dt))+
  ##   coord_cartesian(
  ##     ylim=c(y.min, y.max),
  ##     xlim=c(x.min, x.max))+
  ##   Point_scale+
  ##   geom_line(aes(
  ##     grid.x, fx, color=Function),
  ##     size=fsize,
  ##     data=RAD(grid.dt, Function="f(x) objective"))+
  ##   geom_path(aes(
  ##     grid.x, px, color=Function, key=1),
  ##     showSelected=c("initial.params","iteration"),
  ##     size=qsize,
  ##     data=RAD(approx.dt, Function="Taylor approx."))+
  ##   geom_point(aes(
  ##     x, fx, key=Point, fill=Point),
  ##     showSelected=c("initial.params","iteration"),
  ##     size=6,
  ##     data=RAD(point.dt))+
  ##   geom_text(aes(
  ##     x, fx-0.05, key=Point, label=substr(Point, 1, 1)),
  ##     showSelected=c("initial.params","iteration","Point"),
  ##     size=10,
  ##     data=RAD(point.dt)),
  init=ggplot()+
    ggtitle("Select initial parameters")+
    theme_animint(width=800)+
    scale_fill_gradient(low="white", high="red")+
    geom_tile(aes(
      initial.x, log2(initial.radius),
      fill=max.it),
      data=last.it,
      clickSelects="initial.params")+
    geom_text(aes(
      initial.x, log2(initial.radius),
      label=max.it),
      data=last.it,
      clickSelects="initial.params"),
  iterations=ggplot()+
    ggtitle("Select iteration")+
    scale_x_continuous(breaks=1:12)+
    theme_animint(last_in_row=TRUE)+
    geom_line(aes(
      iteration, value),
      showSelected="initial.params",
      data=init.long)+
    geom_point(aes(
      iteration, value),
      showSelected="initial.params",
      data=init.long)+
    geom_text(aes(
      iteration, -Inf, label=substr(name, 1, 1)),
      showSelected="initial.params",
      data=data.table(variable="log10[abs(grad)]", initial.dt))+
    make_tallrect(init.long, "iteration")+
    facet_grid(variable ~ ., scales="free"),
  funs=ggplot()+
    theme_bw()+
    theme_animint(width=800)+
    ggtitle("Objective and approximation for selected parameters and iteration")+
    ylab("f(x) = objective")+
    xlab("x = optimization variable")+
    geom_tallrect(aes(
      xmin=x0-delta, xmax=x0+delta, key=1),
      showSelected=c("initial.params","iteration"),
      fill=trust.color,
      color=NA,
      initial.dt)+
    geom_path(aes(
      x0, fx0, key=1),
      showSelected=c("initial.params","iteration"),
      size=6,
      data=prev.dt)+
    coord_cartesian(
      ylim=c(y.min, y.max),
      xlim=c(x.min, x.max))+
    Point_scale+
    geom_line(aes(
      grid.x, fx, color=Function),
      size=fsize,
      data=data.table(Function="f(x) objective", grid.dt))+
    geom_path(aes(
      grid.x, px, color=Function, key=1),
      showSelected=c("initial.params","iteration"),
      chunk_vars="initial.params",
      size=qsize,
      data=data.table(Function="Taylor approx.", approx.dt))+
    geom_point(aes(
      x, fx, key=Point, fill=Point),
      showSelected=c("initial.params","iteration"),
      size=6,
      data=point.dt)+
    geom_text(aes(
      x, fx-0.05, key=Point, label=substr(Point, 1, 1)),
      showSelected=c("initial.params","iteration","Point"),
      size=10,
      data=point.dt),
  zoom=ggplot()+
    ggtitle("Zoom to trust region")+
    theme_bw()+
    geom_tallrect(aes(
      xmin=xmin, xmax=xmax),
      data=data.table(xmin=-1, xmax=1),
      color=NA,
      fill=trust.color)+
    coord_cartesian(
      ylim=c(-1, 1),
      xlim=c(-1, 1))+
    Point_scale+
    scale_x_continuous(
      "Step size",
      breaks=c(-1.1, 0, 1),
      labels=c("-delta", "0", "delta"))+
    scale_size_manual(values=fun.sizes)+
    geom_path(aes(
      grid.d, value, color=Function, group=Function,
      size=Function,
      key=Function),
      chunk_vars="initial.params",
      showSelected=c("initial.params","iteration"),
      data=zoom.dt)+
    geom_rect(aes(
      xmax=pmax(0, norm.offset), 
      xmin=pmin(0, norm.offset),
      ymax=fx0,
      ymin=value,
      key=Function,
      color=Function,
      size=Function),
      alpha=0.5,
      fill=NA,
      linetype="dotted",
      showSelected=c("initial.params","iteration"),
      data=ratio.dt)+
    geom_text(aes(
      0, -Inf, key=1,
      label=sprintf("ratio=%.2f %s", ratio, action)),
      showSelected=c("initial.params","iteration"),
      data=initial.dt)+
    geom_point(aes(
      step, fx, key=Point, fill=Point),
      showSelected=c("initial.params","iteration"),
      size=6,
      data=point.dt)+
    geom_text(aes(
      step, fx-0.05, key=Point, label=substr(Point, 1, 1)),
      showSelected=c("initial.params","iteration","Point"),
      size=10,
      data=point.dt),
  duration=list(iteration=500),
  out.dir="figure-trust-region"
)
viz
if(FALSE){
  animint2pages(viz, "2026-06-01-trust-region-sin", chromote_sleep_seconds=3)
}
