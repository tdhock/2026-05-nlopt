library(data.table)
library(ggplot2)
(notes.dt <- fread("notes-ROP631-final.csv")[
, programme := gsub(" ", "\n", Programme)
][is.finite(final)][, mean.examen := (intra+final)/2][])
minitests <- grep("[0-9] ", names(notes.dt), value=TRUE)
minitest.dt <- melt(notes.dt, measure.vars=minitests)
byname <- c("Prénom","Nom de famille")
minitest.dt[, {
  scores <- sort(value)
  best <- sort(value)[-(1:4)]
  possible <- length(best)*2
  total <- sum(best)
  data.table(
    total, possible, percent=100*total/possible,
    minitests=length(scores)
  )
}, by=byname][order(`Nom de famille`)]
notes.dt[order(-final), .(Prénom, final)]
notes.dt[order(-mean.examen), .(`Nom de famille`, Prénom, `Adresse de courriel`, mean.examen, intra, final)]
notes.dt[order(-intra), .(`Nom de famille`, Prénom, `Adresse de courriel`, mean.examen, intra, final)]
notes.dt[order(`Nom de famille`), .(`Nom de famille`, Prénom, intra, final)]
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
offset.dt <- data.table(stat=names(fun.list), offset=seq_along(fun.list))
exam.levs <- c("intra", "final")
(notes.long <- melt(
  notes.dt,
  measure.vars=exam.levs,
)[, examen := factor(variable, exam.levs)][])
(stats.dt <- dcast(
  notes.long,
  examen + programme ~ .,
  fun.list))
(stats.long <- melt(
  stats.dt,
  measure.vars=measure(
    stat,
    pattern="_(.*)")
)[offset.dt, on="stat"])
gg <- ggplot()+
  geom_histogram(aes(
    value),
    data=notes.long,
    color="black",
    fill="white",
    binwidth=5,
    center=2.5)+
  geom_text(aes(
    55, 5-offset/2,
    label=sprintf("%s=%s", stat, as.integer(value))),
    hjust=1,
    data=stats.long)+
  facet_grid(programme ~ examen, labeller=label_both)+
  scale_x_continuous(
    "note, ROP631, été 2026",
    breaks=seq(0,200,by=5))+
  scale_y_continuous(
    "personnes étudiantes")
png("notes-ROP631-final.png", width=12, height=4, units="in", res=200)
print(gg)
dev.off()

(stats.dt <- dcast(
  notes.long,
  examen ~ .,
  fun.list))
(stats.long <- melt(
  stats.dt,
  measure.vars=measure(
    stat,
    pattern="_(.*)")
)[offset.dt, on="stat"])
gg <- ggplot()+
  geom_histogram(aes(
    value),
    data=notes.long,
    color="black",
    fill="white",
    binwidth=5,
    center=2.5)+
  geom_text(aes(
    55, 5-offset/2,
    label=sprintf("%s=%s", stat, as.integer(value))),
    hjust=1,
    data=stats.long)+
  facet_grid(examen ~ ., labeller=label_both)+
  scale_x_continuous(
    "note, ROP631, été 2026",
    breaks=seq(0,200,by=5))+
  scale_y_continuous(
    "personnes étudiantes")
png("notes-ROP631-final-all.png", width=6, height=4, units="in", res=200)
print(gg)
dev.off()

