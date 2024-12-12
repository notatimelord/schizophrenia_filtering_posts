library(igraph)
library(stringr)
library(dplyr)

# Load and clean the data
data <- read.csv("clean.csv")
colnames(data)[1] <- "clean_csv"

# Function to classify numeric age groups
classify_age_group <- function(num) {
  if (4 < num && num < 13) {
    return("Kid")
  } else if (num >= 13 && num <= 17) {
    return("Teenager")
  } else if (num >= 18 && num <= 24) {
    return("Young Adult")
  } else if (num >= 25) {
    return("Adult")
  }
}

# Function to extract numbers from text
extract_number <- function(text) {
  matches <- str_extract(text, "\\d+")
  if (!is.na(matches)) {
    return(as.numeric(matches))
  } else {
    return(NA)
  }
}

# Function to classify text into age groups based on keywords
age_word_patterns <- list(
  "Kid" = c("kid", "child", "childhood", "primary school"),
  "Teenager" = c("teen", "teenager", "adolescent", "adolescence"),
  "Young Adult" = c("college", "young adult"),
  "Adult" = c("adult", "adulthood")
)

classify_age_group_from_text <- function(text) {
  text <- iconv(text, from = "UTF-8", to = "ASCII", sub = "") # Convert to ASCII
  text <- tolower(text)
  for (age_group in names(age_word_patterns)) {
    if (any(sapply(age_word_patterns[[age_group]], function(pattern) str_detect(text, pattern)))) {
      return(age_group)
    }
  }
  return(NA) # Return NA if no match
}

# Apply text classification
data$age_group_from_text <- sapply(data$clean_csv, classify_age_group_from_text)

# Handle NA cases in age group classification
data$age_group_from_text[is.na(data$age_group_from_text)] <- "Kid"

# Count occurrences of each age group
age_groups <- c("Kid", "Young Teenager", "Teenager", "Young Adult", "Adult")
occurrences <- sapply(age_groups, function(group) {
  sum(data$age_group_from_text == group)
})


# Process and split data into a new structured data frame
result_df <- data.frame(
  Post_ID = seq_len(nrow(data)),
  Numbers_Found = sapply(data$clean_csv, extract_number),
  Age = data$age_group_from_text
)

# Remove rows with no numbers found
result_df <- result_df[!is.na(result_df$Numbers_Found), ]

# Update 'Age' based on numeric ranges
result_df$Age <- ifelse(
  is.na(result_df$Age),
  sapply(result_df$Numbers_Found, classify_age_group),
  result_df$Age
)

result_df <- result_df[!duplicated(result_df), ]
result_df <- result_df %>%
  mutate(
    `Age Group` = case_when(
      Numbers_Found >= 4 & Numbers_Found <= 12 ~ "kid",
      Numbers_Found >= 13 & Numbers_Found <= 17 ~ "teenager",
      Numbers_Found >= 18 & Numbers_Found <= 25 ~ "young adult",
      Numbers_Found >= 26 ~ "adult",
      TRUE ~ NA_character_  # To handle cases that do not fit into the above ranges
    )
  )
result_df <- result_df[!duplicated(result_df), ]
result_df <- result_df %>%
  select(-`Age`)

# Remove rows with NA in the 'Age Group' column
result_df <- result_df %>%
  filter(!is.na(`Age Group`))

# Save the final result
write.csv(result_df, "classified_posts_final.csv", row.names = FALSE)

# Count the occurrences of each value in the "Age Group" column
age_group_counts <- table(result_df$`Age Group`)
print(age_group_counts)


total_count <- sum(age_group_counts)

# Create a vector for the names (age groups and "diagnosis")
age_groups <- c(names(age_group_counts), "diagnosis")

# Create an empty adjacency matrix (5x5)
adj_matrix <- matrix(0, nrow = 5, ncol = 5)
rownames(adj_matrix) <- colnames(adj_matrix) <- age_groups

# Fill the matrix with the counts
for (i in 1:length(age_group_counts)) {
  adj_matrix[i, 5] <- age_group_counts[i]  # Connect age group to "diagnosis"
  adj_matrix[5, i] <- age_group_counts[i]  # Connect "diagnosis" to age group
}

# Create the graph object
g <- graph_from_adjacency_matrix(adj_matrix, mode = "undirected", weighted = TRUE)
# Add percentages to the edges
edge_labels <- round(E(g)$weight / total_count * 100, 1)
E(g)$label <- paste(edge_labels, "%", sep = "")

# Plot the graph with larger nodes and longer edges
plot(g, 
     vertex.size = 60,             
     vertex.color = "lightblue",     
     vertex.label.cex = 1,         
     vertex.label.dist = 0,          
     vertex.label.pos = 0.5,         
     edge.width = E(g)$weight / 10,
     edge.color = "darkgrey",       
     layout = layout_with_fr(g, niter = 500, repulserad = 700), 
     edge.label = E(g)$label,       
     edge.label.cex = 1.2,           
     edge.label.color = "black",     
     edge.label.dist = 0.5)          
