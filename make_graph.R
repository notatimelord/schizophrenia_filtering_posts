library(igraph)
 
 # Define the data
 keywords <- c("anger", "paranoia", "hallucinations", "addiction", 
               "emptiness", "depressed", "alcohol", "social_withdrawal", 
               "sadness", "abuse", "homicidal_tendencies", "suicide", 
               "anxiety", "sleep issues", "genes", "self_harm", 
               "guilt", "schizophrenia", "poor academics")
 
 values <- c(16, 17, 18, 17, 9, 15, 6, 10, 9, 7, 12, 11, 12, 9, 9, 4, 8, 14, 7)
 
 # Create an edge list
 edges <- unlist(lapply(keywords, function(keyword) c("Degrees", keyword)))
 
 # Create the graph
 g <- graph_from_edgelist(matrix(edges, ncol = 2, byrow = TRUE), directed = FALSE)
 
 # Set edge weights (values)
 E(g)$weight <- values
 
 # Create a custom layout for the circular structure
 # Calculate positions manually for even distribution
 num_nodes <- length(keywords)
 theta <- seq(0, 2*pi, length.out = num_nodes + 1)[-1]  # Angles for the nodes
 layout <- cbind(cos(theta), sin(theta))  # Coordinates for the nodes in a circle
 
 # Adjust layout so that "Degrees" stays at the center
 layout <- rbind(c(0, 0), layout)  # Add "Degrees" at the center
 
 # Plot the graph with light blue nodes and names inside the nodes
 plot(g, layout = layout, vertex.size = 35, vertex.label.cex = 0.8, 
      vertex.label.dist = 0,  # Place the labels inside the nodes
      vertex.label.color = "black", 
      vertex.color = "lightblue",  # Set node color to light blue
      edge.label = E(g)$weight, edge.label.cex = 0.8, edge.label.color = "blue", 
      main = "Degrees - Circular Graph")
 
