library(data.table)
library(animint2)
data(WorldBank)
wb <- data.table(WorldBank)
top <- wb[year==2010][is.finite(life.expectancy)][order(-life.expectancy)][100:110]
top[, .(country)][wb[year==1960], on=.NATURAL, nomatch=0L][order(life.expectancy)]
cvec <- c("Iceland","Singapore")
wb.show <- wb[country %in% c("Canada", "Japan")]
wb.show <- wb[country %in% c("Honduras", "Morocco", "Algeria")]
wb.show <- wb[country %in% cvec]

rect.up <- 2
rect.down <- 0.8
rect.w.per.char <- 1.2
set_hjust <- function(dt)dt[, let(
  hjust = ifelse(year==1960, 1, 0),
  rect.w = rect.w.per.char*nchar(country)
)][]
two.years <- c(1960, 2011)
rect.dt <- set_hjust(wb.show[year %in% two.years])
animint(
  ggplot()+
    theme_animint(width=800)+
    scale_y_continuous(limits=c(60, 90))+
    geom_rect(aes(
      xmin=year-hjust*rect.w, xmax=year+(1-hjust)*rect.w,
      ymax=life.expectancy+rect.up,
      ymin=life.expectancy-rect.down),
      fill="white",
      data=rect.dt)+
    geom_line(aes(
      year, life.expectancy, color=country, group=country),
      data=wb.show)+
    geom_text(aes(
      year, life.expectancy,
      hjust=hjust,
      label=country),
      size=20,
      data=rect.dt)
)

offset <- seq(-5, 5, by=0.5)
CJ.args <- structure(list(offset, offset), names=cvec)
(offset.dt <- do.call(CJ, CJ.args))
offset.mat <- as.matrix(offset.dt)
rmse <- sqrt(rowMeans(offset.mat^2))
(rect.wide <- dcast(rect.dt, year ~ country, value.var="life.expectancy"))
Offset <- apply(offset.mat, 1, paste, collapse=", ")
grid.dt <- rect.wide[, {
  life.expectancy <- offset.mat+matrix(
    unlist(.SD),
    nrow(offset.mat),
    ncol(offset.mat),
    byrow=TRUE)
  rect.top <- life.expectancy+rect.up
  rect.bottom <- life.expectancy-rect.down
  feasible <- rect.top[, "Singapore"] < rect.bottom[, "Iceland"]
  RMSE <- ifelse(feasible, rmse, NA)
  data.table(
    life.expectancy,
    offset=offset.mat,
    Offset,
    RMSE)
}, by=year][
, selector := paste0("offset", year)
][]
grid.long <- set_hjust(melt(
  grid.dt,
  measure.vars=cvec,
  id.vars=c("year","selector","Offset"),
  variable.name="country",
  variable.factor = FALSE,
  value.name="life.expectancy"
))

viz <- animint(
  title="Linear constraints for label position optimization",
  source="https://github.com/tdhock/2026-05-nlopt/blob/main/figure-directlabels-grid.R",  
  labels=ggplot()+
    ggtitle("Selected label positions")+
    theme_animint(width=800, last_in_row=TRUE, colspan=2)+
    scale_x_continuous(breaks=seq(1960, 2010, by=10))+
    geom_line(aes(
      year, life.expectancy, color=country, group=country),
      data=wb.show)+
    geom_point(aes(
      year, life.expectancy,
      key=paste(year, country)),
      showSelected=c(selector="Offset", "country"),
      size=5,
      data=grid.long)+
    geom_rect(aes(
      xmin=year-hjust*rect.w, xmax=year+(1-hjust)*rect.w,
      ymax=life.expectancy+rect.up,
      color=country,
      key=paste(year, country),
      ymin=life.expectancy-rect.down),
      fill="white",
      showSelected=c(selector="Offset"),
      data=grid.long)+
    geom_text(aes(
      year, life.expectancy,
      hjust=hjust,
      key=paste(year, country),
      label=country),
      showSelected=c(selector="Offset", "country"),
      size=20,
      data=grid.long),
  selectize=structure(list(TRUE,TRUE),names=unique(grid.long$selector)),
  first=structure(list("0, 0", "0, 0"),names=unique(grid.long$selector)),
  duration=structure(list(500,500),names=unique(grid.long$selector))
)
hide <- TRUE
for(y in two.years){
  year.dt <- grid.dt[year==y]
  min.dt <- year.dt[RMSE==min(RMSE,na.rm=TRUE)][, point := "min"]
  gg <- ggplot()+
    ggtitle(paste("Select label positions for", y))+
    geom_tile(aes(
      Singapore, Iceland, fill=RMSE),
      data=year.dt,
      clickSelects=c(selector="Offset"))+
    geom_point(aes(
      Singapore, Iceland, color=point),
      data=min.dt,
      fill=NA,
      showSelected="point",
      clickSelects=c(selector="Offset"))+
    scale_color_manual(values=c(min="black"))+
    scale_fill_gradient(
      low="red",
      high="white",
      limits=c(0, max(offset)))
  if(hide){
    gg <- gg+theme(legend.position="none")
    hide <- FALSE
  }
  viz[[paste0("heat",y)]] <- gg
}
viz
if(FALSE){
  animint2pages(viz, "2026-06-24-directlabels-grid", chromote_sleep_seconds=3)
}
