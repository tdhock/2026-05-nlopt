library(data.table)
plan.dt <- fread("plan.csv")
sum(plan.dt$heures)
