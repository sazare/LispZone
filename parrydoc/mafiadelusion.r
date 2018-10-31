# mafiadelusion.r

library(igraph)

gd = read.table("mafia.txt")

gg = graph.data.frame(gd,directed=T)
#E(gg)$label <- as.character(gd[[3]])
V(gg)$color <- "lightgray" 
V(gg)$border <- "lightgray"
tkplot(gg)

