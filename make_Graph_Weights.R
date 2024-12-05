# Load necessary libraries
library(igraph)
library(dplyr)
library(tidyr)

# Step 1: Load the keyword pairs count CSV file
results <- read.csv("keyword_pairs_count.csv")

# Step 2: Prepare the data
# Ensure that the Pair column is split into two keywords
keyword_pairs <- results %>%
  separate(Pair, into = c("Keyword1", "Keyword2"), sep = ", ") %>%
  mutate(Count = as.integer(Count))  # Ensure that Count is an integer

# Step 3: Create an edge list with weights (the 'Count' represents the edge weight)
edges <- keyword_pairs %>%
  select(Keyword1, Keyword2, Count) %>%
  filter(Keyword1 != Keyword2)  # Avoid self-loops

# Step 4: Create the graph with weighted edges
# We will use the 'Count' as the weight for the edges
g <- graph_from_data_frame(edges, directed = FALSE)

# Step 5: Plot the graph
# Use a force-directed layout (Fruchterman-Reingold algorithm)
layout <- layout_with_fr(g)

# Plot the graph
plot(g, 
     vertex.size = 15,  # Adjust size of nodes
     vertex.label.cex = 0.8,  # Adjust the size of node labels
     vertex.label.dist = 1.5,  # Distance of label from the node
     vertex.color = "skyblue",  # Color of nodes
     edge.width = E(g)$Count / 10,  # Edge width based on the count (weight), adjust divisor to change thickness
     edge.color = "gray",  # Color of edges
     layout = layout,  # Use force-directed layout
     main = "Schizofrenia Co-occurrence Network with
     Weights being the Edge thickness")  # Title of the graph

