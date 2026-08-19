library(data.table)
library(ggplot2)
final.dt <- fread("notes-ROP631-final.csv")
meta.dt <- final.dt[, .(
  `Nom d’utilisateur`,
  Programme,
  programme = gsub(" ", "\n", Programme)
)]
(notes.dt <- fread("notes-ROP631-all.csv", na.strings = "-")[
  meta.dt, on="Nom d’utilisateur"
][is.finite(final)])
tp.cols <- grep("tp", names(notes.dt), value=TRUE)
weights <- c(
  minitest=0.2,
  TP=0.3,
  intra=0.2,
  final=0.3)
tp.mat <- notes.dt[, tp.cols, with=FALSE]
notes.dt[, let(
  TP = rowMeans(tp.mat),
  minitest = 100*présence/28
)][]
w.mat <- matrix(weights, nrow(notes.dt), length(weights), byrow=TRUE)
percents <- notes.dt[, names(weights), with=FALSE]
notes.dt[, note := rowSums(w.mat*percents)][]

out.dt <- notes.dt[, data.table(
  Prénom, Nom=`Nom de famille`, Programme,
  round(percents), note=round(note)
)][order(note)]
fwrite(out.dt, "notes-ROP631-all-calculé.csv")

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

(stats.dt <- dcast(
  notes.dt,
  programme ~ .,
  fun.list))
(stats.long <- melt(
  stats.dt,
  measure.vars=measure(
    stat,
    pattern="_(.*)")
)[offset.dt, on="stat"])
gg <- ggplot()+
  geom_histogram(aes(
    note),
    data=notes.dt,
    color="black",
    fill="white",
    binwidth=5,
    center=2.5)+
  geom_text(aes(
    84, 5-offset/2,
    label=sprintf("%s=%s", stat, as.integer(value))),
    hjust=1,
    data=stats.long)+
  facet_grid(programme ~ ., labeller=label_both)+
  scale_x_continuous(
    "note, ROP631, été 2026",
    breaks=seq(0,200,by=5))+
  scale_y_continuous(
    "personnes étudiantes")
png("notes-ROP631-all.png", width=6, height=4, units="in", res=200)
print(gg)
dev.off()

