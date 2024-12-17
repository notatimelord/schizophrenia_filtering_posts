library(igraph)
library(dplyr)
library(tidyr)

#for the keywords without onset
keywords <- c("schizophrenia", "paranoia", "hallucinations", "addiction", 
              "emptiness", "depressed", "alcohol", "genes", 
              "sadness", "abuse", "homicidal_tendencies", "suicide", 
              "anxiety", "sleep issues", "social_withdrawal", "self_harm", 
              "guilt", "anger", "fear", "bipolar", 
              "psychosis", "meds", "trauma", "poor academics")

values <- c(20, 22, 23, 22, 16, 20, 13, 12, 13, 10, 18, 13, 16, 11, 17, 14, 9, 20, 17, 11, 15, 17, 16, 7)

edges <- unlist(lapply(keywords, function(keyword) c("Degrees", keyword)))
g <- graph_from_edgelist(matrix(edges, ncol = 2, byrow = TRUE), directed = FALSE)

E(g)$weight <- values #edge weight

# for circular structure
num_nodes <- length(keywords)
theta <- seq(0, 2*pi, length.out = num_nodes + 1)[-1]  # Angles for the nodes
layout <- cbind(cos(theta), sin(theta))  # Coordinates for the nodes in a circle


layout <- rbind(c(0, 0), layout)  

plot(g, 
     layout = layout, 
     vertex.size = 10,  
     vertex.label.cex = 0.8,  
     vertex.label.dist = 2, 
     vertex.label.color = "black", 
     vertex.color = "lightblue", 
     edge.width = E(g)$weight / 10,  
     edge.label = E(g)$weight, 
     edge.label.cex = 0.8, 
     edge.label.color = "blue", 
     main = "Degrees - Circular Graph")
