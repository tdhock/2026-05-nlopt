true.slope <- -0.6
true.intercept <- 1.3
set.seed(1)
N <- 100
y <- rep(c(1,-1), length.out=N)
x1 <- runif(N, -1, 2)
x2 <- x1*true.slope+true.intercept+y*runif(N, 0.5, 2)
library(data.table)
library(animint2)
sim.dt <- data.table(
  i=seq_along(y),
  x1, x2, y, label=factor(y)
)[order(label)]
grid.by <- 0.05
param.dt <- CJ(
  slope=seq(-1.2, 0.2, by=grid.by),
  intercept=seq(0.5, 2.1, by=grid.by)
)[, slope_intercept := sprintf("%.2f, %.2f", slope, intercept)]
pred.dt.list <- list()
for(param.i in 1:nrow(param.dt)){
  param.row <- param.dt[param.i]
  pred.dt.list[[param.i]] <- data.table(param.i, param.row, sim.dt)[
  , f := param.row[, x2-x1*slope-intercept]
  ][, let(
    pred = factor(sign(f)),
    yf = y*f
  )][]
}
pred.dt <- rbindlist(pred.dt.list)
stats.dt <- pred.dt[, .(
  errors=sum(pred!=label),
  min.yf=min(yf)
), by=names(param.dt)][
, margin := ifelse(min.yf>0, min.yf, NA)
][]
abline.dt <- data.table(
  offset=c(0,1,-1),
  line=c("separator","margin","margin")
)[
, data.table(stats.dt)[, intercept := intercept+ifelse(is.na(margin), 0, margin)*offset]
, by=.(offset,line)]
(best.param <- stats.dt[which.max(margin), .(slope_intercept)])
best.abline <- best.param[abline.dt, on=.NATURAL, nomatch=0L]
best.pred <- pred.dt[best.param, on=.NATURAL]
ggplot()+
  geom_point(aes(
    x1, x2, color=label, fill=pred),
    shape=21,
    data=best.pred)+
  geom_abline(aes(
    slope=slope,
    intercept=intercept,
    linetype=line),
    data=best.abline)+
  scale_linetype_manual(values=c(
    margin="dashed",
    separator="solid"))

ggplot()+
  geom_tile(aes(
    slope, intercept, fill=margin),
    data=stats.dt)+
  scale_fill_gradient(
    low="white",
    high="red")

viz <- animint(
  out.dir="figure-svm",
  duration=list(slope_intercept=500),
  data=ggplot()+
    ggtitle("Data and selected classifier")+
    geom_text(aes(
      2, 3.5,
      key=1,
      label=sprintf(
        "slope=%.2f intercept=%.2f",
        slope, intercept)),
      hjust=1,
      showSelected="slope_intercept",
      data=stats.dt)+
    geom_text(aes(
      2, 3.1,
      key=1,
      label=sprintf(
        "errors=%d margin=%.4f",
        errors, margin)),
      hjust=1,
      showSelected="slope_intercept",
      data=stats.dt)+
    geom_abline(aes(
      slope=slope,
      intercept=intercept,
      key=offset,
      linetype=line),
      showSelected="slope_intercept",
      data=abline.dt)+
    geom_point(aes(
      x1, x2,
      key=i,
      color=label,
      fill=pred),
      showSelected="slope_intercept",
      data=pred.dt)+
    xlab("Feature 1")+
    ylab("Feature 2")+
    scale_linetype_manual(values=c(
      margin="dashed",
      separator="solid")),
  margin=ggplot()+
    ggtitle("Click to select classifier")+
    geom_tile(aes(
      slope, intercept, fill=margin),
      clickSelects="slope_intercept",
      data=stats.dt)+
    geom_point(aes(
      slope, intercept, color=errors),
      clickSelects="slope_intercept",
      data=stats.dt[errors>0 | margin==max(margin,na.rm=TRUE)])+
    scale_color_gradient(
      low="white",
      high="blue",
      breaks=c(1,5,10,15,20))+
    scale_fill_gradient(
      low="white",
      high="red"))
viz
if(FALSE){
  animint2pages(viz, "2026-06-19-svm-grid", chromote_sleep_seconds=3)
}
