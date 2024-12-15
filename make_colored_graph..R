library(igraph)
library(dplyr)
library(tidyr)

# Load the results
results <- read.csv("keyword_pairs_count.csv")

# Prepare the keyword pairs and counts
keyword_pairs <- results %>%
  separate(Pair, into = c("Keyword1", "Keyword2"), sep = ", ") %>%
  mutate(Count = as.integer(Count))

# Create edges data frame
edges <- keyword_pairs %>%
  select(Keyword1, Keyword2, Count) %>%
  filter(Keyword1 != Keyword2)

# Create graph from the data frame
g <- graph_from_data_frame(edges, directed = FALSE)

# Compute the layout
layout <- layout_with_fr(g)

# Define color ranges for low, moderate, and high weights
low_threshold <- quantile(E(g)$Count, 0.33)  # Lower third
high_threshold <- quantile(E(g)$Count, 0.67)  # Upper third

# Assign colors based on edge weights
edge_colors <- ifelse(E(g)$Count <= low_threshold, "gray", 
                      ifelse(E(g)$Count <= high_threshold, "green", "red"))

# Plot the graph with labels inside nodes
plot(g, 
     vertex.size = 15,
     vertex.label.cex = 0.8,
     vertex.label.dist = 0,  # Move labels inside the nodes
     vertex.color = "white",
     edge.width = E(g)$Count / 10,
     edge.color = edge_colors,  # Apply color based on edge weight
     layout = layout,
     main = "Schizophrenia Co-occurrence Network with Weights Colored by Edge Weight")

# Calculate global clustering coefficient
global_clustering <- transitivity(g, type = "global")
print(global_clustering)

# Calculate local clustering coefficient
local_clustering <- transitivity(g, type = "localaverage")
print(local_clustering)

# Calculate degree for each node
node_degrees <- degree(g, mode = "all")

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
