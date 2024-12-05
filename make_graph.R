
library(igraph)
library(dplyr)
library(tidyr)  # Make sure to load tidyr for the 'separate' function

# Step 1: Load the keyword pairs count CSV file
results <- read.csv("keyword_pairs_count.csv")

# Step 2: Prepare the data
# Ensure that the Pair column is split into two keywords
keyword_pairs <- results %>%
  separate(Pair, into = c("Keyword1", "Keyword2"), sep = ", ") %>%
  mutate(Count = as.integer(Count))  # Make sure Count is an integer

# Step 3: Create an undirected graph
# Create an edge list with 'Keyword1' and 'Keyword2' as the nodes and 'Count' as the edge weight
edges <- keyword_pairs %>%
  select(Keyword1, Keyword2, Count) %>%
  filter(Count > 0)  # Optional: Filter out pairs with zero counts

# Create the graph from the edge list
g <- graph_from_data_frame(edges, directed = FALSE)

# Step 4: Plot the graph
# Set up layout and color scheme
plot(g, 
     vertex.size = 10,  # Adjust size of nodes
     vertex.label.cex = 0.8,  # Adjust the size of node labels
     vertex.color = "skyblue",  # Set color of nodes
     edge.width = 1,  # Make edge width proportional to count
     main = "Schizofrenia Co-occurrence Network")  # Title of the graph
