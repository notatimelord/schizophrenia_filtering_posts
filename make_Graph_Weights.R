library(igraph)
library(dplyr)
library(tidyr)

results <- read.csv("keyword_pairs_count.csv")

keyword_pairs <- results %>%
  separate(Pair, into = c("Keyword1", "Keyword2"), sep = ", ") %>%
  mutate(Count = as.integer(Count))

edges <- keyword_pairs %>%
  select(Keyword1, Keyword2, Count) %>%
  filter(Keyword1 != Keyword2)

g <- graph_from_data_frame(edges, directed = FALSE)

layout <- layout_with_fr(g)

plot(g, 
     vertex.size = 15,
     vertex.label.cex = 0.8,
     vertex.label.dist = 2.5,
     vertex.color = "skyblue",
     edge.width = E(g)$Count / 10,
     edge.color = "gray",
     layout = layout,
     main = "Schizofrenia Co-occurrence Network with
     Weights being the Edge thickness")
