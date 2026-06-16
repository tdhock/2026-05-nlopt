library(data.table)
data.list <- list()
(objs <- load("figure-trust-region-2D-data.RData"))
data.list$trust <- step.dt[x.start %in% c("-1.0, 1.0", "1.0, 2.0", "0.0, 0.0") & Delta.start==0.25]
(objs <- load("figure-armijo-2D-data.RData"))
data.list$armijo <- step.dt[x.start %in% c("-3.0, -1.0", "-2.0, 2.0", "1.0, -1.0") & tau==0.1]

cost.dt.list <- list()
var.dt.list <- list()
for(data.name in names(data.list)){
  wide.dt <- data.list[[data.name]][, .(
    x.initial=x.start,
    itération = step.num,
    norm.grad, f, V1, V2)]
  var.dt.list[[data.name]] <- wide.dt#[, .(x.initial, itération, V1, V2)]
  cost.dt.list[[data.name]] <- melt(wide.dt, measure.vars=c("norm.grad", "f"))
}
(cost.dt <- rbindlist(cost.dt.list))
(var.dt <- rbindlist(var.dt.list))

gg <- ggplot()+
  geom_line(aes(
    itération, value),
    data=cost.dt)+
  geom_point(aes(
    itération, value),
    data=cost.dt)+
  facet_grid(variable ~ x.initial, labeller=label_both, scales="free_y")+
  scale_x_continuous(breaks=seq(0,100,by=10))+
  scale_y_log10("")
png("figure-trust-region-2D-static-cost.png", width=10, height=4, units="in", res=200)
print(gg)
dev.off()

gg <- ggplot()+
  geom_tile(aes(
    V1, V2, fill=log10.f),
    color=NA,
    data=grid.dt)+
  scale_fill_gradient(low="white", high="red")+
  scale_x_continuous("x1 = premier variable d’optimisation")+
  scale_y_continuous("x2 = deuxième variable d’optimisation")+
  coord_equal()+
  geom_path(aes(
    V1, V2),
    data=var.dt)+
  geom_point(aes(
    V1, V2),
    data=var.dt)+
  facet_grid(. ~ x.initial, labeller=label_both)
png("figure-trust-region-2D-static-vars.png", width=14, height=3, units="in", res=200)
print(gg)
dev.off()
