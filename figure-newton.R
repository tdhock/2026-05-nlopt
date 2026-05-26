library(data.table)
library(animint2)
grid.x <- seq(-2*pi, 4*pi, l=200)
grid.dt <- data.table(grid.x, fx=sin(grid.x), what="objective")
initial.x <- seq(1, 4*pi, by=2)
approx.dt.list <- list()
newton.dt.list <- list()
initial.dt.list <- list()
for(initial.i in seq_along(initial.x)){
  x0 <- first.x <- initial.x[initial.i]
  for(iteration in 1:4){
    initial.dt.list[[paste(initial.i, iteration)]] <- data.table(
      initial.i, initial.x=first.x, iteration, x0, fx=sin(x0), what="initial")
    Taylor <- function(x)sin(x0)+cos(x0)*(x-x0)-0.5*sin(x0)*(x-x0)^2
    approx.dt.list[[paste(initial.i, iteration)]] <- data.table(
      initial.i, initial.x=first.x, iteration, grid.x, qx=Taylor(grid.x), what="approx")
    new.x <- x0+cos(x0)/sin(x0)
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

  

