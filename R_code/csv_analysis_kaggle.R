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
    education_category = case_when(
      education <= 9 ~ "Educ.<=9",
      education > 9 & education <= 12 ~ "Educ. between 9 and 12",
      TRUE ~ "Educ.>12"
    )
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

cat("Healthy vs Schizophrenic:\n")
for (i in 1:nrow(edges)) {
  cat(paste(edges$from[i], "-", edges$to[i], ": Weight =", edges$weight[i]), "\n")
}

layout <- layout_with_fr(graph)

plot(graph, 
     vertex.size = 15, 
     vertex.label.cex = 0.8,  
     vertex.label.dist = 1.5,  
     vertex.label.degree = pi / 2,  
     vertex.color = "skyblue",  
     edge.width = E(graph)$normalized_weight * 5, 
     edge.color = "gray",  
     layout = layout, 
     main = "Simplified Network of Group Combinations with Weights as Edge Thickness") 

write.csv(edges, "edges_with_weights.csv", row.names = FALSE)

# Calculate and print means for schizophrenic males vs females
schizo_data <- data %>% filter(specific.disorder == "Schizophrenia")

schizo_males <- schizo_data %>% filter(sex == "M")
schizo_females <- schizo_data %>% filter(sex == "F")

mean_schizo_males <- mean(schizo_males$IQ)
mean_schizo_females <- mean(schizo_females$IQ)

cat("\nMean IQ for Schizophrenic Males: ", mean_schizo_males, "\n")
cat("Mean IQ for Schizophrenic Females: ", mean_schizo_females, "\n")

# Calculate the mean IQ values for Healthy vs Schizophrenia
healthy_data <- data %>% filter(specific.disorder == "Healthy control")

mean_schizo_IQ <- mean(schizo_data$IQ)
mean_healthy_IQ <- mean(healthy_data$IQ)

cat("\nMean IQ for Schizophrenia: ", mean_schizo_IQ, "\n")
cat("Mean IQ for Healthy control: ", mean_healthy_IQ, "\n")
