library(igraph)
library(dplyr)
library(tidyr)

#for the keywords without onset
keyword_group <- c("hallucinations", "paranoia", "schizophrenia", "addiction", 
                   "bipolar", "depressed", "guilt", "anger", 
                   "trauma", "psychosis", "suicide", "emptiness", 
                   "alcohol", "genes", "sadness", "homicidal_tendencies", 
                   "fear", "social_withdrawal", "sleep issues", "poor academics",
                   "self_harm", "poor academics")

values <- c(23, 22, 20, 22, 11, 20, 9, 20, 16, 17, 13, 16, 13, 12, 13, 18, 17, 14, 11, 7)


# for the keywords with onset
keywords <- c("social_withdrawal", "paranoia", "hallucinations", "addiction", 
              "emptiness", "depressed", "alcohol", "anger", 
              "sadness", "abuse", "homicidal_tendencies", "suicide", 
              "anxiety", "sleep_issues", "genes", "self_harm", 
              "guilt", "Kid", "Teenager", "Young Adult", "trauma", "meds",
              "schizophrenia", "poor_academic_perf", "Adult")

values <- c(15, 25, 27, 25, 17, 24, 15, 21, 16, 10, 19, 15, 19, 13, 19, 4, 9, 10, 12, 11, 20, 15, 24, 9, 15)



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

