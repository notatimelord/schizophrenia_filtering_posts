library(igraph)
library(dplyr)
library(tidyr)

data <- read.csv("schizo_data_2.csv")
data <- data %>%
  select(sex, IQ, education, specific.disorder) %>%
  filter(specific.disorder %in% c("Schizophrenia", "Healthy control")) %>%
  na.omit()

data <- data %>%
  mutate(
    IQ_category = case_when(
      IQ <= 90 ~ "IQ<=90",
      IQ > 90 & IQ < 110 ~ "90<IQ<110",
      TRUE ~ "IQ>=110"
    ),
    education_category = ifelse(education <= 16, "Educ.<=16", "Educ.>16")
  )

data <- data %>%
  mutate(
    category = paste(sex, IQ_category, specific.disorder, education_category, sep = ", ")
  )

category_counts <- data %>%
  group_by(category) %>%
  summarise(count = n(), .groups = "drop")

nodes <- unique(c(unlist(strsplit(category_counts$category, ", "))))

edges <- data.frame()

for (i in 1:nrow(category_counts)) {
  groups <- strsplit(category_counts$category[i], ", ")[[1]]
  
  for (j in 1:(length(groups) - 1)) {
    for (k in (j + 1):length(groups)) {
      edges <- rbind(edges, data.frame(from = groups[j], to = groups[k], weight = category_counts$count[i]))
    }
  }
}

graph <- graph_from_data_frame(edges, directed = FALSE, vertices = nodes)

E(graph)$normalized_weight <- (E(graph)$weight - min(E(graph)$weight)) / (max(E(graph)$weight) - min(E(graph)$weight))

cat("Edge Weights for Each Combination:\n")
for (i in 1:nrow(edges)) {
  cat(paste(edges$from[i], "-", edges$to[i], ": Weight =", edges$weight[i]), "\n")
}

layout <- layout_with_fr(graph)

plot(graph,
     layout = layout, 
     vertex.size = 30,
     vertex.label.cex = 1.2,
     vertex.label.color = "black",
     vertex.label.dist = 1,
     edge.width = E(graph)$normalized_weight * 5,
     edge.color = "gray",
     main = "Simplified Network of Group Combinations"
)
