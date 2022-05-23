library(igraph)
G <- graph.tree(n=12,children=2)

co <- layout.reingold.tilford(G, params=list(root=1)) 
plot(G, layout=co)
