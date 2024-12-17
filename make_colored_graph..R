library(igraph)
library(dplyr)
library(tidyr)

results <- read.csv("keyword_pairs_count.csv")
results <- read.csv("final_merged_keyword_pair_counts.csv")
keyword_pairs <- results %>%
  separate(Pair, into = c("Keyword1", "Keyword2"), sep = ", ") %>%
  mutate(Count = as.integer(Count))

edges <- keyword_pairs %>%
  select(Keyword1, Keyword2, Count) %>%
  filter(Keyword1 != Keyword2)

g <- graph_from_data_frame(edges, directed = FALSE)

layout <- layout_with_fr(g)

# Define color ranges for low, moderate, and high weights
low_threshold <- quantile(E(g)$Count, 0.33)  # Lower third
high_threshold <- quantile(E(g)$Count, 0.67)  # Upper third

# Assign colors based on edge weights
edge_colors <- ifelse(E(g)$Count <= low_threshold, "gray", 
                      ifelse(E(g)$Count <= high_threshold, "green", "red"))

# Calculate the number of red and green edges
red_edges <- sum(edge_colors == "red")
green_edges <- sum(edge_colors == "green")
total_edges <- length(E(g)$Count)

# Calculate the percentages
percentage_red <- (red_edges / total_edges) * 100
percentage_green <- (green_edges / total_edges) * 100

# Print the percentages
cat("Percentage of red edges:", percentage_red, "%\n")
cat("Percentage of green edges:", percentage_green, "%\n")

# Plot the graph with labels inside nodes
plot(g, 
     vertex.size = 15,
     vertex.label.cex = 0.8,
     vertex.label.dist = 0,  
     vertex.color = "skyblue",
     edge.width = E(g)$Count / 10,
     edge.color = edge_colors,  
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


cat("Node with highest degree:", most_significant_node, "with a degree of", max(node_degrees), "\n")
average_path_symptom <- mean_distance(g, directed = FALSE)
print(average_path_symptom)
print(node_degrees)


