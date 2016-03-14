# gtfsnetwork
### author: "Hans Thompson"
### date: "March 13, 2016"

## Installation
```{r , include=FALSE}
library(devtools)
install_github("hansthompson/gtfsnetwork")
library(gtfsnetwork)
```

## Example

```{r}
library(lubridate)
download.file("http://transitfeeds.com/p/people-mover/370/latest/download", 
              destfile = "gtfs.zip")
unzip("gtfs.zip")

trips <- read.csv("trips.txt", stringsAsFactors = TRUE)
stop_times <- read.csv("stop_times.txt", stringsAsFactors = TRUE)

gtfs_el <- gtfs2edgelist(stop_times)
starting_time <- hms("09:00:00")
ending_time   <- hms("11:30:00")
srv_id <- "1" # weekdays Monday-Friday for People Mover

filtered_gtfs_edge_list <- filter_srv_id(gtfs_edgelist = gtfs_el, trips, srv_id  = "1")
filtered_gtfs_edge_list <- filter_gtfs_edgelist(gtfs_edgelist = filtered_gtfs_edge_list, 
                                                start_time = starting_time, end_time = ending_time)

filtered_gtfs_edge_list <- remove_duplicate_edges(filtered_gtfs_edge_list, trips)

edges <- filtered_gtfs_edge_list %>% select(source, target, size = transit_time)

library(networkD3)
library(igraph)

gtfs_igraph <- graph.data.frame(edges, directed = TRUE)
plot(gtfs_igraph)
```




