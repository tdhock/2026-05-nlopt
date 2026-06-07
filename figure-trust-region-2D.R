# https://optimization.cbe.cornell.edu/index.php?title=Trust-region_methods
library(data.table)
library(animint2)
load("figure-trust-region-2D-data.RData")
(initial.x.dt <- unique(initial.dt[, .(V1, V2, x.start)]))
step.long <- melt(step.dt[, let(
  "log10(|∇f|)"=log10(norm.grad),
  "log10(f)"=log10(f),
  "log2(Δ)"=log2(Delta)
)], measure.vars = c("log10(|∇f|)", "log10(f)", "log2(Δ)"))
(minball <- ball.q.dt[, .SD[which.min(qfun)], by=step.num])
point.size <- 4
scfill <- scale_fill_gradient(low="blue", high="white")
vector.dirs <- vector.dt[name=="dir"]
(prev.dt <- step.dt[
  step.dt,
  .(x.start, Delta.start, step.num=i.step.num, prev.step=x.step.num, V1, V2),
  on=.(x.start, Delta.start, step.num<=step.num)])
end.dt <- step.dt[next.action=="end"]
range.dt <- dcast(
  end.dt,
  x.start ~ .,
  list(min, max),
  value.var="step.num"
)[
, step.range := sprintf("%d–%d", step.num_min, step.num_max)
][initial.x.dt, on="x.start"]
viz <- animint(
  out.dir="figure-trust-region-2D",
  duration=list(step.num=500),
  title="2D Trust region and conjugate gradient",
  steps=ggplot()+
    ggtitle("Select step and initial region size Δ")+
    theme_animint(width=500, height=500)+
    geom_line(aes(
      step.num, value),
      showSelected=c("x.start", "Delta.start"),
      data=step.long)+
    scale_y_continuous("")+
    scale_x_continuous(
      "Step number",
      breaks=seq(0, 100))+
    geom_text(aes(
      step.num, -Inf, label=substr(next.action, 1, 1)),
      showSelected=c("x.start", "Delta.start"),
      data=data.table(name="iterate", variable="log10(f)", step.dt))+
    make_tallrect(step.long, "step.num")+
    geom_point(aes(
      0, log2(Delta.start)),
      data=data.table(variable="log2(Δ)", Delta.dt),
      size=7,
      fill="white",
      color="black",
      clickSelects="Delta.start")+
    geom_text(aes(
      0, log2(Delta.start)-0.5, label=step.num),
      clickSelects="Delta.start",
      showSelected="x.start",
      color="red",
      data=data.table(variable="log2(Δ)", end.dt))+
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
    geom_path(aes(
      x, y, group=1, key=1),
      showSelected=c("x.start","Delta.start","step.num"),
      data=ball.f.dt)+
    geom_segment(aes(
      from.V1, from.V2, xend=to.V1, yend=to.V2,
      key=name, color=name),
      showSelected=c("x.start","Delta.start","step.num"),
      data=opt.vec.dt)+
    geom_point(aes(
      to.V1, to.V2, color=name, key=name),
      showSelected=c("x.start","Delta.start","step.num"),
      size=point.size,
      data=opt.vec.dt)+
    geom_point(aes(
      V1, V2, color=name, key=1),
      showSelected=c("x.start","Delta.start","step.num"),
      size=point.size,
      data=data.table(name="iterate", step.dt))+
    geom_text(aes(
      V1, V2, label=next.action, key=1),
      showSelected=c("x.start","Delta.start","step.num"),
      data=data.table(name="iterate", step.dt))+
    geom_path(aes(
      V1, V2, key=1),
      data=prev.dt,
      showSelected=c("x.start", "Delta.start", "step.num"))+
    geom_point(aes(
      V1, V2),
      data=initial.x.dt,
      size=7,
      fill=NA,
      clickSelects="x.start"),
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
      group=paste(x.start, Delta.start)),
      alpha=0.2,
      size=1,
      alpha_off=0.2,
      clickSelects="x.start",
      data=step.dt)+
    geom_path(aes(
      V1, V2,
      key=Delta.start,
      group=Delta.start),
      data=step.dt,
      size=4,
      alpha=1,
      alpha_off=0.2,
      showSelected="x.start",
      clickSelects="Delta.start")+
    geom_point(aes(
      V1, V2,
      key=Delta.start,
      group=Delta.start),
      data=end.dt,
      size=4,
      alpha=1,
      alpha_off=0.5,
      fill="violet",
      showSelected="x.start",
      clickSelects="Delta.start")+
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
  q=ggplot()+
    ggtitle("q(d), select conj. grad. iteration")+
    scale_color_discrete("Vector")+
    theme_animint(width=500, height=500)+
    coord_equal(xlim=c(-1,1), ylim=c(-1,1))+
    scale_fill_gradient(
      "Relative q(d)",
      low="blue", high="white")+
    geom_tile(aes(
      V1rel, V2rel, fill=qrel, key=paste(V1rel, V2rel)),
      showSelected=c("x.start","Delta.start","step.num"),
      color=NA,
      data=q.grid.dt)+
    geom_path(aes(
      x, y, group=1, key=1),
      data=ball.path.dt)+
    geom_segment(aes(
      from.V1/Delta, from.V2/Delta,
      xend=to.V1/Delta, yend=to.V2/Delta,
      key=name, color=name),
      showSelected=c("x.start","Delta.start","step.num","iteration"),
      data=vector.dt)+
    geom_point(aes(
      to.V1/Delta, to.V2/Delta, color=name, key=name),
      showSelected=c("x.start","Delta.start","step.num","iteration"),
      size=point.size,
      data=vector.dt)+
    geom_point(aes(
      x/Delta, y/Delta, color=name, key=1),
      showSelected=c("x.start","Delta.start","step.num"),
      size=point.size,
      data=data.table(name="min", minball))+
    geom_point(aes(
      iteration/max(iteration), 1.04,
      key=iteration),
      showSelected=c("x.start","Delta.start","step.num"),
      clickSelects="iteration",
      size=8,
      fill="white",
      color="black",
      data=vector.dirs)+
    geom_text(aes(
      iteration/max(iteration), 1,
      key=iteration,
      label=iteration),
      showSelected=c("x.start","Delta.start","step.num"),
      clickSelects="iteration",
      data=vector.dirs)+
    scale_x_continuous(
      "d1 relative to trust region",
      breaks=c(-1, 0, 1),
      labels=c("-Δ", "0", "Δ"))+
    scale_y_continuous(
      "d2 relative to trust region",
      breaks=c(-1, 0, 1),
      labels=c("-Δ", "0", "Δ"))
)
viz

if(FALSE){
  animint2pages(viz, "2026-06-05-trust-region-2D", chromote_sleep_seconds)
}
