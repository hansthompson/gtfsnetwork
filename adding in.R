library(gtfsnetwork)

stops <- read.csv("stops.txt", stringsAsFactors = TRUE)
trips <- read.csv("trips.txt", stringsAsFactors = TRUE)
stop_times <- read.csv("stop_times.txt", stringsAsFactors = TRUE)

starting_time <- hms("09:00:00")
ending_time   <- hms("09:015:00")
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

# this is the outdegree expected for one node to one node
min_value <- 1

edges %>% group_by(trip_id) %>%
  mutate(edge_sequence = edge_sequence - min(edge_sequence) + 1)

graph_object <-  edges %>% mutate(mapping = cumsum(ifelse(n - min_value == 0, 0, 1))) %>%
  filter(trip_id == "1-640-I-1")

gtfs_g <- simplify(contract.vertices(graph.data.frame(graph_object[,1:2]), 
                  mapping = c(1,graph_object$mapping),
                  vertex.attr.comb=toString))

gtfs_g$name <- "TEST"
gtfs_g <- set.vertex.attribute(gtfs_g, "color", value=c("red", "green"))

plot(gtfs_g)

plot(simplify(contract.vertices(graph.data.frame(edges[,1:2]), 
                       mapping = c(1, mapping_object),
                       vertex.attr.comb=toString)))

lo <- layout.kamada.kawai(iedges,dim=2)
plot(iedges,  vertex.size=1, vertex.label = NA, edge.arrow.size = 0.3,
     layout = lo)

colnames(filtered_gtfs_edge_list)[colnames(filtered_gtfs_edge_list) == "target"] <- "stop_id"
temp_edgelist <- inner_join(stops, filtered_gtfs_edge_list, by = "stop_id") %>% 
  select(trip_id, source, target = stop_id, target_lat = stop_lat, target_lon = stop_lon, 
         transit_time)
colnames(temp_edgelist)[colnames(temp_edgelist) == "source"] <- "stop_id"
inner_join(stops, temp_edgelist, by = "stop_id") %>% 
  select(trip_id, source = stop_id, target, source_lat = stop_lat, source_lon = stop_lon,
         target_lat, target_lon, transit_time)

plot(stops$stop_lon, stops$stop_lat)

library(sp)
library(rgeos)
library(mapproj)


stops_sp <- SpatialPointsDataFrame(coords = stops %>% select( stop_lat,  stop_lon), 
                                   data = stops %>% select(-stop_lat, -stop_lon),
                                   proj4string =  CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))

spTransform(stops_sp, CRSobj = CRS("+proj=laea +lon_0=-149.9 +lat_0=61.1 +ellps=sphere") )


any(!stops$stop_id %in% gtfs_edgelist$source)
any(!gtfs_edgelist$source %in% stops$stop_id)
