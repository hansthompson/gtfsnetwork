library(igraph)

# edgelist with two nodes with outdegree > 1. 
edgelist <- data.frame(source = c("Z","A", "B", "C", "D", "E", "F", "F", "A"),
                       target = c("A","B", "C", "D", "E", "F", "G", "H", "I"),
                       edge_sequence = c(0,1, 2, 3, 4, 5, 6, NA , NA),
                       source_node_out_degree = c(1,1, 1, 1, 1, 1, 2, 2, 2),
                       group = factor(c(1,1,1,1,1,1,1,2,2)),
                       map = c(1,2,3,3,3,3,3,8,9,10))
plot(graph.data.frame(edgelist), edge.arrow.size = 0.3)
g <- graph.data.frame(edgelist)
h <- contract.vertices(g,mapping = c(1,2,3,3,3,3,3,8,9,10), 
                       vertex.attr.comb = toString)
new_edgelist <- as_edgelist(simplify(h))
plot(graph.data.frame(new_edgelist), edge.arrow.size = 0.3)



       