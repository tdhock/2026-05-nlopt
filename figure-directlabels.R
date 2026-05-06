# source: https://github.com/rdatatable-community/data-table-raft/pull/1
Archive <- "https://cloud.r-project.org/src/contrib/Archive/"
get_Archive <- function(Package, releases.dir="~/releases"){
  dir.create(releases.dir, showWarnings = FALSE)
  pkg.html <- file.path(releases.dir, paste0(Package, ".html"))
  if(!file.exists(pkg.html)){
    u <- paste0(Archive, Package)
    download.file(u, pkg.html)
  }
  readLines(pkg.html)
}
(Archive.data.table <- get_Archive("data.table"))
file.pattern <- list(
  '(?<=>)',
  package=".*?",
  "_",
  version="[0-9.-]+",
  "[.]tar[.]gz")
options(datatable.print.nrows=20) # instead of default 100.
nc::capture_all_str(Archive.data.table, file.pattern)
library(data.table)
Archive.pattern <- list(
  file=file.pattern,
  "</a>",
  "\\s+",
  IDate=".*?", as.IDate,
  "\\s")
(Archive.dt <- nc::capture_all_str(Archive.data.table, Archive.pattern))
setkey(Archive.dt, IDate)
every.year.since.2016 <- seq(
  as.IDate("2016-04-14"),
  Sys.time(),
  by="year")
(grid.dt <- setkey(data.table(
  grid.IDate=c(
    as.IDate("2006-04-14"), # first release.
    as.IDate("2011-04-14"), # fifth anniversary.
    every.year.since.2016))))
(nearest.dt <- unique(Archive.dt[grid.dt, .(
  file, version, package, release=x.IDate
), roll="nearest"]))
desc.dt <- nearest.dt[, {
  cache.dir <- "~/Archive"
  dir.create(cache.dir, showWarnings = FALSE)
  dt.tar.gz <- file.path(cache.dir, file)
  if(!file.exists(dt.tar.gz)){
    url.tar.gz <- paste0(Archive, package, "/", file)
    download.file(url.tar.gz, dt.tar.gz)
  }
  conn <- gzfile(dt.tar.gz, "b")
  DESCRIPTION <- file.path(package, "DESCRIPTION")
  untar(conn, files=DESCRIPTION)
  close(conn)
  as.data.table(read.dcf(DESCRIPTION)[,"Author",drop=FALSE])
}, by=.(version, release)]
options(
  datatable.prettyprint.char=30, # print ... after this many characters.
  width=100) # max characters before wrapping columns to next line.
desc.dt[, no.newlines := gsub("\n", " ", Author)][]
author.pattern <- list(
  name=".+?",
  nc::quantifier(
    " \\[",
    roles=".+?",
    "\\]",
    "?"),
  nc::quantifier(
    " \\(", 
    paren=".+?",
    "\\)",
    "?"),
  ## each author ends with one of these (\z means end of string).
  nc::alternatives(" with (?:many )?contributions from ", ", ", "\\z"))
(author.dt <- desc.dt[, nc::capture_all_str(
  no.newlines, author.pattern
), by=.(version, release)])
author.dt[roles==""]
linewidth.values <- c(
  ctb=2,
  aut=1)
author.dt[
, Role := factor(fcase(
  roles=="aut, cre" | grepl("Dowle|Srinivasan", name), "aut",
  roles=="", "ctb",
  default=roles), names(linewidth.values))
][
, table(roles, Role, useNA="always")
]
(count.dt <- author.dt[, .(people=.N), by=.(release, version, Role)])
library(ggplot2)
(gg <- ggplot(count.dt, aes(
  release, people, color=Role))+
   ggtitle("data.table contributor and author counts for selected releases")+
   theme(
     panel.grid.minor=element_blank(),
     text=element_text(size=20),
     axis.text.x=element_text(hjust=1, angle=40))+
   geom_line(aes(linewidth=Role))+
   geom_point(shape=21, fill="white")+
   scale_x_date(breaks=grid.dt$grid.IDate)+
   scale_linewidth_manual(values=linewidth.values)+
   scale_y_log10(limits=c(0.2, 500)))

geom_dl_poly <- function(role, position, direction)directlabels::geom_dl(aes(
  label=sprintf("%s\n%s %s", version, people, Role)),
  data=count.dt[Role==role],
  method=list(
    cex=1.1, # text size of direct labels.
    directlabels::dl.add(y=direction*0.2),# cm between polygon point and data point.
    directlabels::polygon.method(
      position, offset.cm=0.5))) #space between polygon point and text.
(dl <- gg+
  geom_dl_poly("ctb", "top", 1)+
  geom_dl_poly("aut", "bottom", -1))
