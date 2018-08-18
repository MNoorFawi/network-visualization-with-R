customerActor <- read.csv('customer_actor.csv', stringsAsFactors = FALSE)
head(customerActor)

top10actors <- unique(customerActor$Actor_Name) 
### data is already sorted descreasingly
topCustomerPerActor <- 
  customerActor[!duplicated(customerActor$Actor_Name), ]
names(topCustomerPerActor)[1] <- "Top_Fan"
topFans <- topCustomerPerActor$Top_Fan
head(topCustomerPerActor)

library(igraph)
#first we trim the data to show only customers who rented more than twice 
cust <- customerActor[customerActor$Number_of_Rentals > 2, ]
# creating the nodes of the graph from data.frame 
graph <- graph_from_data_frame(cust[, c(1, 3)])
# edge weights according to number of rentals
E(graph)$weight <- cust[, 2]
# assigning node type
V(graph)$who <- ifelse(V(graph)$name %in% top10actors, 'superstar',
                       ifelse(V(graph)$name %in% topFans, 'top_fan', 
                              'fan'))
# node colors, size and label
V(graph)$color <- c('pink', 'skyblue', 'gold')[factor(V(graph)$who)]
V(graph)$size <- c(5, 10, 7.5)[factor(V(graph)$who)]
V(graph)$label.color <- "black"
V(graph)$label <- NA
# edge color based on weights
E(graph)$color <- ifelse(E(graph)$weight >= 4, 'black', 'grey80')
E(graph)$arrow.size = 0.1
#plotting the graph
plot(graph)

library(visNetwork)
data <- toVisNetworkData(graph)
# visNetwork(nodes = data$nodes, edges = data$edges, 
#     height = "500px") %>%
#   visIgraphLayout()

# or visualizing it directly:

# visIgraph(graph)

cus <- spread(cust, Actor_Name, Number_of_Rentals)
row.names(cus) <- cus$Customer_Name
cus <- cus[, -1]
cus[is.na(cus)] <- 0

library(GGally)
library(sna)
library(network)
library(ggplot2)
library(tidyr)
library(dplyr)

net <- network(cus ,
               matrix.type = "bipartite",
               ignore.eval = FALSE,
               names.eval = "weights")

## node type
net %v% "superstar" = ifelse(
  network.vertex.names(net) %in% top10actors, "actor",
  ifelse(
    network.vertex.names(net) %in% topFans, "topFan", "fan")
)
## node color
net %v% "color" = ifelse(
  net %v% "superstar" == "actor", "cornflowerblue",
  ifelse(
    net %v% "superstar" == "topFan", "firebrick1", "forestgreen")
)
## edge colors
net %e% "coloring" = ifelse(
  net %e% "weights" > 4, "black", "grey77")

ggnet2(net, label = top10actors, color = "color", 
       label.color = "black", label.size = 5,
       alpha = "superstar", alpha.palette = c(
         "actor" = 1, "fan" = 0.5, "topFan" = 0.7),
       size = "superstar", size.palette = c(
         "actor" = 5, "fan" = 1, "topFan" = 2.5),
       edge.color = "coloring", edge.alpha = 0.7) +
  guides(alpha = FALSE, size = FALSE)

