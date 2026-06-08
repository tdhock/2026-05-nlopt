library(data.table)
library(animint2)
load("figure-armijo-2D-data.RData")
(initial.x.dt <- unique(initial.dt[, .(V1, V2, x.start)]))
step.long <- melt(step.dt[, let(
  "log10(|∇f|)"=log10(norm.grad),
  "log10(f)"=log10(f),
  "Armijo steps"=armijo.steps
)], measure.vars = c("log10(|∇f|)", "log10(f)", "Armijo steps"))
point.size <- 4
scfill <- scale_fill_gradient(low="blue", high="white")
(prev.dt <- step.dt[
  step.dt,
  .(x.start, tau, step.num=i.step.num, prev.step=x.step.num, V1, V2),
  on=.(x.start, tau, step.num<=step.num)])
end.dt <- step.dt[, .SD[.N], by=.(x.start, tau)]
range.dt <- dcast(
  end.dt,
  x.start ~ .,
  list(min, max),
  value.var="step.num"
)[
, step.range := sprintf("%d–%d", step.num_min, step.num_max)
][initial.x.dt, on="x.start"]
ad.colors <- c(
  "FALSE"="red",
  "TRUE"="transparent")
viz <- animint(
  out.dir="figure-armijo-2D",
  duration=list(step.num=500),
  title="2D gradient descent with Armijo line search",
  source="https://github.com/tdhock/2026-05-nlopt/blob/main/figure-armijo-2D.R",
  steps=ggplot()+
    ggtitle("Select step")+
    theme_animint(width=500, height=500)+
    scale_y_continuous("")+
    scale_x_continuous(
      "Step number",
      breaks=seq(0, 100, by=10))+
    make_tallrect(step.long, "step.num")+
    geom_line(aes(
      step.num, value, group=tau),
      showSelected=c("x.start"),
      clickSelects="tau",
      size=3,
      alpha_off=0.2,
      alpha=1,
      data=step.long)+
    facet_grid(variable ~ ., scales="free"),
  variables=ggplot()+
    ggtitle("Cost f(x), select x start")+
    scale_color_discrete("Vector")+
    theme_animint(last_in_row=TRUE)+
    theme_animint(width=500, height=500)+
    grid.dt[, coord_cartesian(xlim=range(V1), ylim=range(V2))]+
    geom_tile(aes(
      V1, V2, fill=log10.f),
      color=NA,
      data=grid.dt)+
    scfill+
    scale_x_continuous("x1 = first optimization variable")+
    scale_y_continuous("x2 = second optimization variable")+
    geom_segment(aes(
      from.V1, from.V2, xend=to.V1, yend=to.V2,
      key=name, color=name),
      showSelected=c("x.start","tau","step.num"),
      data=opt.vec.dt)+
    geom_point(aes(
      to.V1, to.V2, color=name, key=name),
      showSelected=c("x.start","tau","step.num"),
      size=point.size,
      data=opt.vec.dt)+
    geom_point(aes(
      V1, V2, color=name, key=1),
      showSelected=c("x.start","tau","step.num"),
      size=point.size,
      data=data.table(name="iterate", step.dt))+
    geom_text(aes(
      V1, V2, label=armijo.steps, key=1),
      showSelected=c("x.start","tau","step.num"),
      data=data.table(name="iterate", step.dt))+
    geom_path(aes(
      V1, V2, key=1),
      data=prev.dt,
      showSelected=c("x.start", "tau", "step.num"))+
    geom_point(aes(
      V1, V2),
      data=initial.x.dt,
      size=7,
      fill=NA,
      clickSelects="x.start"),
  ## armijo=ggplot()+
  ##   geom_line(aes(
  ##     step.size, ifelse(over, Inf, f), color=Function, group=Function, key=Function),
  ##     showSelected=c("x.start","tau","step.num"),
  ##     data=armijo.fun.dt)+
  ##   scale_fill_manual(values=ad.colors)+
  ##   geom_point(aes(
  ##     step.size, ifelse(over_objective, Inf, f_objective), fill=admissible,
  ##     key=step.size),
  ##     showSelected=c("x.start","tau","step.num"),
  ##     size=4,
  ##     data=armijo.point.dt),
  armijoLog=ggplot()+
    ggtitle("Armijo line search for selected step")+
    theme_animint(width=500, height=500)+
    geom_line(aes(
      log2.step, thresh.log10, color=Function, group=tau, key=tau),
      showSelected=c("x.start","step.num"),
      clickSelects="tau",
      size=3,
      data=armijo.fun.dt[Function=="bound"])+
    geom_line(aes(
      log2.step, thresh.log10, color=Function, group=Function, key=Function),
      showSelected=c("x.start","tau","step.num"),
      data=armijo.fun.dt)+
    scale_fill_manual(values=ad.colors)+
    scale_y_continuous("Log10(objective), normalized to [0,1]")+
    geom_point(aes(
      log2.step, thresh.log10_objective, fill=admissible, key=step.size),
      showSelected=c("x.start","tau","step.num"),
      size=4,
      data=armijo.point.dt),
  initx=ggplot()+
    ggtitle("Select initial values")+
    theme_animint(width=500, height=500)+
    grid.dt[, coord_cartesian(xlim=range(V1), ylim=range(V2))]+
    geom_tile(aes(
      V1, V2, fill=log10.f),
      color=NA,
      data=grid.dt)+
    scfill+
    geom_path(aes(
      V1, V2,
      group=paste(x.start, tau)),
      alpha=0.2,
      size=1,
      alpha_off=0.2,
      clickSelects="x.start",
      data=step.dt)+
    geom_path(aes(
      V1, V2,
      key=tau,
      group=tau),
      data=step.dt,
      size=4,
      alpha=1,
      alpha_off=0.2,
      showSelected="x.start",
      clickSelects="tau")+
    geom_point(aes(
      V1, V2,
      key=tau,
      group=tau),
      data=end.dt,
      size=4,
      alpha=1,
      alpha_off=0.5,
      fill="violet",
      showSelected="x.start",
      clickSelects="tau")+
    geom_point(aes(
      V1, V2),
      data=initial.x.dt,
      size=7,
      fill=NA,
      clickSelects="x.start")+
    geom_text(aes(
      V1, V2, label=step.range),
      data=range.dt,
      color="red",
      clickSelects="x.start"),
  first=list(
    x.start="-4.0, 0.0",
    tau="0.1")
)

#viz

if(FALSE){
  animint2pages(viz, "2026-06-08-armijo-2D", chromote_sleep_seconds=3)
}
