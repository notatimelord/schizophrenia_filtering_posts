
library(igraph)
library(stringr)
library(dplyr)
library(tidyr)


data <- read.csv("clean.csv")
total_posts <- nrow(data)
colnames(data)[1] <- "clean_csv"


extract_valid_number <- function(text) {
   pattern <- "\\b(?:a\\s+|an\\s+|was\\s+|at\\s+|around\\s+|age\\s+of\\s+|im\\s+|about\\s+)?(\\d{1,2})(?=(\\s*(?:s|'s|\\s+and\\b|\\s+or\\b|\\s+when\\b|\\s+which\\b|\\s*(?:\\d{1,2}'s|20\\s*s|30\\s*s|40\\s*s|50\\s*s|60\\s*s|70\\s*s|twenty|thirty|forty|fifty|sixty|seventy|twenty\\s+years?))|[\\s\\b]*$))"
  matches <- str_match_all(text, pattern)
  numbers <- as.numeric(matches[[1]][, 2])
  if (length(numbers) > 0) return(numbers) else return(NA)
}

# Function to classify age group based on keywords
classify_age_group_from_keywords <- function(text) {
  text <- as.character(text)
  text <- iconv(text, from = "latin1", to = "UTF-8", sub = "byte")
  age_word_patterns <- list(
    "Kid" = c("kid", "child", "childhood", "primary school"),
    "Teenager" = c("teen", "teenager", "adolescent", "adolescence"),
    "Young Adult" = c("college", "uni", "university", "young adult"),
    "Adult" = c("adult", "adulthood")
  )
  exclusion_conditions <- c("graduated", "passed", "failed")
  for (age_group in names(age_word_patterns)) {
    keywords <- age_word_patterns[[age_group]]
    for (keyword in keywords) {
      if (str_detect(tolower(text), fixed(keyword)) && !any(str_detect(tolower(text), fixed(exclusion_conditions)))) {
        text <- str_replace_all(text, fixed(keyword), switch(age_group, "Kid" = "10", "Teenager" = "15", "Young Adult" = "20", "Adult" = "30"))
      }
    }
  }
  return(text)
}

# Apply the age group classification and save updated CSV
data$clean_csv <- sapply(data$clean_csv, classify_age_group_from_keywords)
write.csv(data, "clean_with_replaced_keywords.csv", row.names = FALSE)

# Extract numbers and process data
data$Numbers_Found <- sapply(data$clean_csv, extract_valid_number)
data <- data[!is.na(data$Numbers_Found), ]
data$post_id <- 1:nrow(data)
data <- data %>%
  mutate(Numbers_Found = sapply(Numbers_Found, function(x) paste(x, collapse = ", "))) %>%
  separate_rows(Numbers_Found, sep = ",") %>%
  mutate(Numbers_Found = as.numeric(Numbers_Found))

# Classify age groups based on extracted numbers
classify_age_group <- function(num) {
  if (4 < num & num < 13) return("Kid")
  if (num >= 13 & num <= 17) return("Teenager")
  if (num >= 18 & num <= 24) return("Young Adult")
  if (num >= 25 & num <= 100) return("Adult")
  return(NA)
}
data$Age_Group <- sapply(data$Numbers_Found, classify_age_group)

# Remove rows with NA in 'Age_Group' and duplicate rows
data <- data[!is.na(data$Age_Group), ]
data <- data %>% distinct(post_id, Age_Group, .keep_all = TRUE)

# Count occurrences of each age group
age_group_counts <- table(data$Age_Group)
print(age_group_counts)

# Create adjacency matrix with "Onset" as the central node
age_groups <- c(names(age_group_counts), "Onset")
adj_matrix <- matrix(0, nrow = length(age_groups), ncol = length(age_groups))
rownames(adj_matrix) <- colnames(adj_matrix) <- age_groups
for (i in 1:length(age_group_counts)) {
  adj_matrix[i, length(age_groups)] <- age_group_counts[i]
  adj_matrix[length(age_groups), i] <- age_group_counts[i]
}

# Create and plot the graph
g <- graph_from_adjacency_matrix(adj_matrix, mode = "undirected", weighted = TRUE)
E(g)$label <- paste(round(E(g)$weight / sum(age_group_counts) * 100, 1), "%", sep = "")
plot(g, 
     vertex.size = 60, 
     vertex.color = "lightblue", 
     vertex.label.cex = 1, 
     vertex.label.dist = 0, 
     edge.width = E(g)$weight / 10, 
     edge.color = "darkgrey", 
     layout = layout_with_fr(g, niter = 700, repulserad = 700), 
     edge.label = E(g)$label, 
     edge.label.cex = 1.2, 
     edge.label.color = "black")

# Calculate average age and percentage of posts with onset age
average_age <- mean(data$Numbers_Found, na.rm = TRUE)
posts_with_onset_age <- sum(!is.na(data$Numbers_Found))
print(paste("The average age is:", round(average_age, 2)))
print(paste("Number of posts with onset-age:", posts_with_onset_age))
print(paste("Total number of posts:", total_posts))
print(paste("Percentage of posts with onset-age:", round((posts_with_onset_age / total_posts) * 100, 2), "%"))
