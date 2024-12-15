library(igraph)
library(dplyr)
library(tidyr)

# Load the data
results <- read.csv("keyword_pairs_count.csv")

# Prepare the data
keyword_pairs <- results %>%
  separate(Pair, into = c("Keyword1", "Keyword2"), sep = ", ") %>%
  mutate(Count = as.integer(Count))

# Create the graph
edges <- keyword_pairs %>%
  select(Keyword1, Keyword2, Count) %>%
  filter(Keyword1 != Keyword2)

g <- graph_from_data_frame(edges, directed = FALSE)

# Create layout for plotting
layout <- layout_with_fr(g)

# Plot the graph
plot(g, 
     vertex.size = 15,
     vertex.label.cex = 0.8,
     vertex.label.dist = 2.5,
     vertex.color = "skyblue",
     edge.width = E(g)$Count / 10,
     edge.color = "gray",
     layout = layout,
     main = "Schizophrenia Co-occurrence Network with Weights being the Edge thickness")

# Calculate closeness centrality
closeness_centrality <- closeness(g)
print(closeness_centrality)

# Run Louvain community detection
community <- cluster_louvain(g)

# Plot the community detection
plot(community, g)

# Print the symptoms assigned to each community
community_members <- communities(community)
for (i in 1:length(community_members)) {
  cat("\nCommunity", i, "includes symptoms: \n")
  cat(community_members[[i]], "\n\n")
}




 global_clustering <- transitivity(g, type = "global")
 print(global_clustering)

 local_clustering <- transitivity(g, type = "localaverage")
 print(local_clustering)

 # Calculate degree for each node
 node_degrees <- degree(g, mode = "all")  # Use "all" for an undirected graph
 
 # Find the node with the highest degree
 most_significant_node <- names(which.max(node_degrees))
 
 # Print the most significant node and its degree
 cat("The most significant node is:", most_significant_node, "with a degree of", max(node_degrees), "\n")
 
 # Optional: View all nodes and their degrees
 print(node_degrees)
 # Load required libraries
 # Load required libraries
 # Load required libraries
 library(igraph)
 # Define the updated data
keywords <- c("anger", "paranoia", "hallucinations", "addiction", 
              "emptiness", "depressed", "alcohol", "social_withdrawal", 
              "sadness", "abuse", "homicidal_tendencies", "suicide", 
              "anxiety", "sleep issues", "genes", "self_harm", 
              "guilt", "schizophrenia", "poor academics", "bipolar", 
              "psychosis", "meds", "trauma")

values <- c(19, 21, 22, 21, 14, 19, 11, 13, 12, 9, 17, 13, 14, 10, 11, 5, 8, 19, 7, 12, 16, 15, 10)

 
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
 
 
 
