library(dplyr)
library(lubridate)

gtfs2edgelist <- function(stop_times) {
  arrivals <- stop_times %>% 
    group_by(trip_id) %>% filter(stop_sequence != 1) %>% select(-arrival_time) %>%
    mutate(target = stop_id)
  departures <- stop_times %>% group_by(trip_id) %>% filter(stop_sequence != max(stop_sequence)) %>%
    select(trip_id, arrival_time, stop_sequence, stop_id) %>% 
    mutate(source = stop_id, stop_sequence = stop_sequence + 1)
   
  inner_join(arrivals, departures, by = c("trip_id", "stop_sequence"))  %>% 
    group_by(trip_id) %>%
    mutate(transit_time = abs((hour(hms(departure_time) - hms(arrival_time)) * -60) - 
                               minute(hms(departure_time) - hms(arrival_time)))) %>%
    mutate(departs_source = arrival_time, target_arrival = departure_time) %>%
    select(src, target, transit_time, departs_source, target_arrival) 
}

filter_gtfs_edgelist <- function(gtfs_edgelist, start_time, end_time) {
    if(class(start_time) != "Period") {start_time <- hms(start_time)}
    if(class(end_time) != "Period") {end_time <- hms(end_time)}
  
    gtfs_edgelist %>% ungroup() %>% 
      filter(hms(departs_source) > start_time &
             hms(target_arrival) < end_time)
}

filter_srv_id <- function(gtfs_edgelist, trips, srv_id) {
  inner_join(gtfs_edgelist, trips, by = "trip_id") %>% 
    filter(service_id == srv_id) %>% 
  select(trip_id, source, target, transit_time, departs_source, target_arrival)
}

remove_duplicate_edges <- function(gtfs_edgelist, trips) {
  inner_join(gtfs_edgelist, trips, by = "trip_id") %>% 
    group_by(route_id) %>% distinct(source, target) %>% 
    select(trip_id, source, target, transit_time)
}                             

bin_edges <- function(gtfs_edgelist, trips) {
  inner_join(gtfs_edgelist, trips, by = "trip_id") %>%
    group_by(route_id, source, target) %>% summarize(n = length(route_id))
}