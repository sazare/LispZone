library(igraph)

gd = read.table("pdatb.graph")
gg = graph.data.frame(gd[1:2],directed=T)
E(gg)$label <- gd[[3]]
V(gg)$color <- "lightgray" 
tkplot(gg)

gg2 = graph.data.frame(gd[1:30,1:2],directed=T)
E(gg2)$label <- as.character(gd[1:30,3])
V(gg2)$color <- "lightgray" 
tkplot(gg2)