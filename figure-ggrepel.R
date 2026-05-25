library(ggplot2)
data(WorldBank,package="animint2")
wb <- subset(WorldBank,year==2000)
ggplot(wb,aes(
  life.expectancy, fertility.rate, color=region))+
  geom_point()+
  ggrepel::geom_label_repel(aes(label=country))
