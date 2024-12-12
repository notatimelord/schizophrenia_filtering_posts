library(igraph)
library(stringr)
library(dplyr)
library(tidyr)

# Load and clean the data
data <- read.csv("clean.csv")
colnames(data)[1] <- "clean_csv"

# Function to extract and validate numbers based on the described conditions
extract_valid_number <- function(text) {
  pattern <- "\\b(?:a\\s+|an\\s+|was\\s+|at\\s+|around\\s+)?(\\d{1,2})(?=(\\s*(?:s|'s|\\s+and\\b|\\s+or\\b|\\s+when\\b|\\s+which\\b|\\s+or\\b|\\s*(?:\\d{1,2}'s|20\\s*s|30\\s*s|40\\s*s|50\\s*s|60\\s*s|70\\s*s|twenty|thirty|forty|fifty|sixty|seventy|twenty\\s+years?))|[\\s\\b]*$))"

  matches <- str_match_all(text, pattern)
  
  numbers <- as.numeric(matches[[1]][, 2])
  
  if (length(numbers) > 0) {
    return(numbers)
  } else {
    return(NA)
  }
}

classify_age_group_from_keywords <- function(text) {
  text <- iconv(text, from = "latin1", to = "UTF-8", sub = "byte")
  
  age_word_patterns <- list(
    "Kid" = c("kid", "child", "childhood", "primary school"),
    "Teenager" = c("teen", "teenager", "adolescent", "adolescence"),
    "Young Adult" = c("college", "uni", "university", "young adult"),
    "Adult" = c("adult", "adulthood")
  )
  
  # Replace keywords with their corresponding number
  for (age_group in names(age_word_patterns)) {
    keywords <- age_word_patterns[[age_group]]
    for (keyword in keywords) {
      if (str_detect(tolower(text), keyword)) {
        # Debugging: Print to check which keyword matched
        print(paste("Matched keyword:", keyword, "for age group:", age_group))
        
        # Replace the keyword with the corresponding number
        if (age_group == "Kid") {
          text <- str_replace_all(text, fixed(keyword), "10")
        } else if (age_group == "Teenager") {
          text <- str_replace_all(text, fixed(keyword), "15")
        } else if (age_group == "Young Adult") {
          text <- str_replace_all(text, fixed(keyword), "20")
        } else if (age_group == "Adult") {
          text <- str_replace_all(text, fixed(keyword), "30")
        }
      }
    }
  }
  
  return(text)
}

# Apply the age group classification and replace keywords with numbers in the clean_csv column
data$clean_csv <- sapply(data$clean_csv, classify_age_group_from_keywords)

# Save the updated CSV with replaced keywords
write.csv(data, "clean_with_replaced_keywords.csv", row.names = FALSE)

# Apply the age group classification from keywords to the data
data$Age_Group_from_keywords <- sapply(data$clean_csv, classify_age_group_from_keywords)

# Ensure we save the updated `Age_Group_from_keywords` directly to the CSV
write.csv(data, "updated_clean_with_keywords.csv", row.names = FALSE)





# Apply the function to extract numbers from the text
data$Numbers_Found <- sapply(data$clean_csv, extract_valid_number)

# Remove rows with no valid numbers
data <- data[!is.na(data$Numbers_Found), ]

# Add a new column 'post_id' for identification, indicating the original row
data$post_id <- 1:nrow(data)

# Split rows for multiple numbers, but maintain the 'post_id' for each new row
data <- data %>%
  mutate(Numbers_Found = sapply(Numbers_Found, function(x) paste(x, collapse = ", "))) %>%
  separate_rows(Numbers_Found, sep = ",") %>%
  mutate(Numbers_Found = as.numeric(Numbers_Found)) %>%
  mutate(post_id = rep(data$post_id, times = lengths(strsplit(as.character(data$Numbers_Found), ","))))

# Function to classify numeric age groups
classify_age_group <- function(num) {
  if (4 < num & num < 13) {
    return("Kid")
  } else if (num >= 13 & num <= 17) {
    return("Teenager")
  } else if (num >= 18 & num <= 24) {
    return("Young Adult")
  } else if (num >= 25 & num <= 100) {
    return("Adult")
  } else {
    return(NA)
  }
}

# Combine numeric and keyword-based age classification
data$Age_Group <- ifelse(!is.na(data$Age_Group_from_keywords), 
                         data$Age_Group_from_keywords, 
                         sapply(data$Numbers_Found, classify_age_group))

# Validate and correct the age group based on the number
validate_age_group <- function(age, group) {
  if (age >= 4 & age < 13) {
    if (group != "Kid") {
      return("Kid")
    }
  } else if (age >= 13 & age <= 17) {
    if (group != "Teenager") {
      return("Teenager")
    }
  } else if (age >= 18 & age <= 24) {
    if (group != "Young Adult") {
      return("Young Adult")
    }
  } else if (age >= 25 & age <= 100) {
    if (group != "Adult") {
      return("Adult")
    }
  }
  return(group)
}

# Apply the validation function to correct age group if necessary
data$Age_Group <- mapply(validate_age_group, data$Numbers_Found, data$Age_Group)

# Remove rows with NA in the 'Age_Group' column
data <- data[!is.na(data$Age_Group), ]

# Remove duplicate rows where post_id and Age_Group are the same
data <- data %>%
  distinct(post_id, Age_Group, .keep_all = TRUE)

# Remove the third column 'Age_Group_from_keywords'
data <- data %>%
  select(-Age_Group_from_keywords)

# Remove rows where the first and last columns are the same
data <- data %>%
  filter(clean_csv != Age_Group)

# Save the processed data
write.csv(data, "classified_posts_final.csv", row.names = FALSE)

# Count the occurrences of each age group
age_group_counts <- table(data$Age_Group)
print(age_group_counts)

# Total count of age groups
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
