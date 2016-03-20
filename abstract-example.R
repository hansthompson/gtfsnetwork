library(dplyr)
library(igraph)

el <- data.frame(group =    c("A", "A", "A", "B", "B", "B", "B", "C"),
           sequence = c(1,2,3,1,2,3,4,1),
           source =   c("a", "b", "c", "a", "e", "f", "g", "f"),
           target =   c("b", "c", "d", "e", "f", "g", "d", "h"),
           weight =   c(1,2,1,2,1,2,1,2))

edges_new <- edges %>% select(source, target, trip_id) %>%
  group_by(source, target) %>%
  mutate(outdegree_source = length(source)) %>%
  group_by(trip_id) %>%
  mutate(is_changed = ifelse(outdegree_source == 1, 0, 1)) %>%
  ungroup() %>% 
  mutate(mapping_vector = cumsum(is_changed))

#par(mar=c(0,0,0,0))
g <- graph_from_data_frame(edges_new[,1:2], directed = TRUE)
g <- contract.vertices(g, mapping = c(1,edges_new$mapping_vector), vertex.attr.comb=toString)
g <- simplify(g)
V(g)$label <- ""

plot(g, vertex.size = 2,
     edge.arrow.size=0.01,
     layout=layout_on_sphere)

#layout=layout_in_circle
#layout=layout_randomly
#layout=layout_on_sphere
#layout=layout_with_fr
