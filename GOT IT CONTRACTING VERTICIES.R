library(dplyr)
library(stringr)
library(devtools)
#install_github("hansthompson/gtfsnetwork")
library(gtfsnetwork)

stops <- read.csv("stops.txt", stringsAsFactors = TRUE)
trips <- read.csv("trips.txt", stringsAsFactors = TRUE)
stop_times <- read.csv("stop_times.txt", stringsAsFactors = TRUE)

starting_time <- hms("09:00:00")
ending_time   <- hms("10:00:00")
srv_id <- "1" # weekdays Monday-Friday for People Mover

gtfs_el <- gtfs2edgelist(stop_times)
filtered_gtfs_edge_list <- filter_srv_id(gtfs_edgelist = gtfs_el, trips, srv_id  = "1")
filtered_gtfs_edge_list <- filter_gtfs_edgelist(gtfs_edgelist = filtered_gtfs_edge_list, 
                                                start_time = starting_time, end_time = ending_time)

filtered_gtfs_edge_list <- remove_duplicate_edges(filtered_gtfs_edge_list, trips)

filtered_gtfs_edge_list <- filtered_gtfs_edge_list %>% group_by(source, target) %>% mutate(n = length(source)) %>% 
  group_by(trip_id)

edges <- filtered_gtfs_edge_list %>% select(source, target, size = transit_time, 
                                            edge_sequence, trip_id, n)

graph_object <- edges %>%
  mutate(n = n + ifelse(row_number(n) == 1,  1, 0)) %>%
  mutate(n = n + ifelse(row_number() == n(), 1, 0)) %>%
  mutate(n = n - min(n)) %>%
  ungroup() %>%
  mutate(foo = create_mapping(n, source, 1))

vertex_names <- V(graph.data.frame(graph_object[,1:2]))$name

for(i in seq(vertex_names)) {
  if(any(vertex_names[i] == graph_object$source)){
      vertex_names[i] <- graph_object[vertex_names[i] == graph_object$source,]$foo[1]
  }
  #print(sum(vertex_names[i] == graph_object$source))
}

hootie <- graph_object %>% select(source, target)

h3 <- graph.data.frame(hootie)

plot(h3, vertex.size = 2,
     edge.arrow.size=0.01, vertex.label = NA)

h4 <- simplify(graph.data.frame(as_edgelist(contract.vertices(h3, 
                                                              mapping = vertex_names, 
                                                              vertex.attr.comb = toString)),
                                directed = T))

V(h4)$name[str_detect(V(h4)$name, ",")] <- paste0(countCharOccurrences(",", V(h4)$name[str_detect(V(h4)$name, ",")]), "-nodes")
V(h4)$name[!str_detect(V(h4)$name, "-")] <- tolower(as.character(inner_join(data.frame(stop_id = as.character(V(h4)$name[!str_detect(V(h4)$name, "-")])), stops, by = "stop_id")$stop_name))
V(h4)$label.cex = 0.75
plot(h4, vertex.size = 2,
     edge.arrow.size=0.01)
