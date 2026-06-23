library(data.table)
library(ggplot2)
notes.dt <- fread("notes-ROP631.csv")[
, programme := gsub(" ", "\n", Programme)
][is.finite(exam)]
show.list <- list("min","mean","median","max",personnes=length)
fun.list <- list()
for(fun.i in seq_along(show.list)){
  x <- show.list[[fun.i]]
  if(is.character(x)){
    fun <- get(x)
    name <- x
  }else{
    fun <- x
    name <- names(show.list)[[fun.i]]
  }
  fun.list[[name]] <- fun
}
offset.dt <- data.table(variable=names(fun.list), offset=seq_along(fun.list))
stats.dt <- dcast(
  notes.dt,
  programme ~ .,
  fun.list,
  value.var="exam")
stats.long <- melt(
  stats.dt,
  measure.vars=measure(variable, pattern="exam_(.*)")
)[offset.dt, on="variable"][, chr := paste(value)][]
gg <- ggplot()+
  geom_histogram(aes(
    exam),
    data=notes.dt,
    color="black",
    fill="white",
    binwidth=5,
    center=2.5)+
  geom_text(aes(
    90, 5-offset/2,
    label=sprintf("%s=%s", variable, as.integer(value))),
    hjust=1,
    data=stats.long)+
  facet_grid(programme ~ ., labeller=label_both)+
  scale_x_continuous(
    "note de l’intra, ROP631, été 2026",
    breaks=seq(0,200,by=5))+
  scale_y_continuous(
    "personnes étudiantes")
png("notes-ROP631.png", width=5, height=3, units="in", res=200)
print(gg)
dev.off()

stats.dt <- dcast(
  notes.dt,
  . ~ .,
  fun.list,
  value.var="exam")
stats.long <- melt(
  stats.dt,
  measure.vars=measure(variable, pattern="exam_(.*)")
)[offset.dt, on="variable"][, chr := paste(value)][]
gg <- ggplot()+
  geom_histogram(aes(
    exam),
    data=notes.dt,
    color="black",
    fill="white",
    binwidth=5,
    center=2.5)+
  geom_text(aes(
    90, 5-offset/4,
    label=sprintf("%s=%s", variable, as.integer(value))),
    hjust=1,
    data=stats.long)+
  scale_x_continuous(
    "note de l’intra, ROP631, été 2026",
    breaks=seq(0,200,by=5))+
  scale_y_continuous(
    "personnes étudiantes")
png("notes-ROP631-all.png", width=5, height=4, units="in", res=200)
print(gg)
dev.off()
