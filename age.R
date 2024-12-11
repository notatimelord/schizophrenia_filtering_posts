
library(igraph)
library(stringr)

classify_age_group <- function(num) {
  if (4 < num && num < 13) {
    return("Kid")
  } else if (num >= 13 && num <= 15) {
    return("Young Teenager")
  } else if (num >= 16 && num <= 18) {
    return("Teenager")
  } else if (num >= 18 && num <= 24) {
    return("Young Adult")
  } else if (num >= 25) {
    return("Adult")
  }
}

# Extract numbers from the text using regex
extract_number <- function(text) {
  # Extract the first number from the text
  matches <- str_extract(text, "\\d+")
  if (!is.na(matches)) {
    return(as.numeric(matches))
  } else {
    return(NA)
  }
}
# Ensure the 'age' column exists and apply age group classification (replace 'age' with the correct column name if needed)
# If you don't have an 'age' column, you can skip this part and classify directly based on text
if ("age" %in% names(data)) {
  data$age_group <- sapply(data$age, classify_age_group)
}

# Now classify the words in the text based on specific keywords
# Define patterns to map words to age groups
age_word_patterns <- list(
  "Kid" = c("kid", "child", "childhood", "primary school"),
  "Teenager" = c("teen", "teenager", "adolescent", "adolescence"),
  "Young Adult" = c("college", "young adult"),
  "Adult" = c("adult")
)
# Function to classify age group based on keywords in the text
classify_age_group_from_text <- function(text) {
  text <- tolower(text)  # Convert text to lowercase
  matched <- FALSE  # Flag to track if any keyword matched
  
  for (age_group in names(age_word_patterns)) {
    if (any(sapply(age_word_patterns[[age_group]], function(pattern) str_detect(text, pattern)))) {
      matched <- TRUE  # Set the flag if a match is found
      return(age_group)
    }
  }
  
  # If no keyword matched, return NA instead of "Kid" and handle it later
  if (!matched) {
    return(NA)  # Return NA to indicate no match was found
  }
}

# Apply the text classification to the data
data$age_group_from_text <- sapply(data$clean_csv, classify_age_group_from_text)

# Handle the case where no matches were found (NA values) and default to "Kid"
data$age_group_from_text[is.na(data$age_group_from_text)] <- "Kid"


# Apply the text classification to the data
data$age_group_from_text <- sapply(data$clean_csv, classify_age_group_from_text)

# Count occurrences of each age group
age_groups <- c("Kid", "Young Teenager", "Teenager", "Young Adult", "Adult")
occurrences <- sapply(age_groups, function(group) {
  sum(data$age_group_from_text == group)  # Count occurrences of each age group
})

# Prepare graph data: "Beginning of Schizophrenia" is a central node
edges <- c()  # Vector to store edges
edge_weights <- c()  # Vector to store edge weights (thickness)

# Add edges from each age group to "Beginning of Schizophrenia"
for (i in 1:length(age_groups)) {
  edges <- c(edges, "Beginning of Schizophrenia", age_groups[i])  # Add edge to graph
  edge_weights <- c(edge_weights, occurrences[i])  # Use occurrences as edge thickness
}

# Create a graph object using the igraph package from the edge list
g <- graph_from_edgelist(matrix(edges, ncol = 2, byrow = TRUE), directed = FALSE)

# Normalize edge weights to a range from 1 to 20 (to make them thicker)
max_weight <- max(edge_weights)  # Find the maximum weight
normalized_weights <- (edge_weights / max_weight) * 19 + 1  # Scale between 1 and 20 (thicker edges)

# Apply the normalized edge weights to the graph
E(g)$weight <- normalized_weights
E(g)$label <- edge_weights  # Set the edge labels to display the weights

# Set a circular layout where all nodes will be at equal distance from the "Beginning of Schizophrenia"
layout_matrix <- layout_in_circle(g)

# Plot the graph with adjusted properties
plot(g, 
     layout = layout_matrix,  # Use circular layout
     vertex.size = 30,  # Size of nodes
     vertex.label.cex = 1.2,  # Size of labels
     vertex.label.color = "black",  # Label color
     vertex.color = "lightblue",  # Node color
     edge.width = E(g)$weight,  # Edge thickness based on normalized weight
     vertex.label.position = 0.15,  # Position labels slightly above the nodes
     edge.label.cex = 1.2,  # Size of edge labels
     edge.label.color = "black",  # Color of edge labels
     main = "Age Groups and Beginning of Schizophrenia")

# Create a new data frame to store the original posts, the numbers found, and the age group
result_df <- data.frame(
  Numbers_Found = data$matches,  # Add the found numbers in the posts
  Age_Group = data$age_group_from_text  # Add the classified age group
)
# Remove rows where 'Numbers_Found' column is empty (NA or "")
result_df <- result_df[!is.na(result_df$Numbers_Found) & result_df$Numbers_Found != "", ]

# Add a Post_ID column to uniquely identify each row
result_df$Post_ID <- seq_len(nrow(result_df))

# Function to split rows with commas into separate rows while preserving Post_ID
split_rows_with_id <- function(data) {
  split_numbers <- strsplit(data$Numbers_Found, ",")  # Split the Numbers_Found column by comma
  split_words <- strsplit(data$Age_Group, ",")  # Split the Age_Group column by comma (if needed)
  
  # Create a new data frame with the split values and original Post_ID
  data.frame(
    Post_ID = rep(data$Post_ID, lengths(split_numbers)),  # Repeat the Post_ID
    Numbers_Found = unlist(split_numbers),  # Flatten the split Numbers_Found
    Age_Group = unlist(split_words)  # Flatten the split Age_Group
  )
}# Apply the splitting function to each row, ensuring Age_Group is a character vector
split_result_df <- do.call(rbind, lapply(seq_len(nrow(result_df)), function(i) {
  result_df$Age_Group[i] <- as.character(result_df$Age_Group[i])  # Convert to character
  split_rows_with_id(result_df[i, ])
}))

# Save the result to a new CSV file
write.csv(split_result_df, "classified_posts_split_with_id.csv", row.names = FALSE)
colnames(split_result_df)[3] <- "Age"
split_result_df$Age[grep("\\b(kid|childhood|child|primary school)\\b", split_result_df$Numbers_Found, ignore.case = TRUE)] <- "Child"
split_result_df$Age[grep("\\b(adolescent|adolescence|teenager|teen|high school)\\b", split_result_df$Numbers_Found, ignore.case = TRUE)] <- "Teenager"
split_result_df$Age[grep("\\b(college|young adult|university)\\b", split_result_df$Numbers_Found, ignore.case = TRUE)] <- "Young Adult"
split_result_df$Age[grep("\\b(adulthood|adult)\\b", split_result_df$Numbers_Found, ignore.case = TRUE)] <- "Adult"

split_result_df$Numbers_Found <- ifelse(
  is.na(split_result_df$Age),
  gsub(".*?(\\d+).*", "\\1", split_result_df$Numbers_Found), # Extract the number
  split_result_df$Numbers_Found                             # Otherwise, keep the original value
)
# Save the result to a new CSV file
write.csv(split_result_df, "classified_posts_split_with_id.csv", row.names = FALSE)

# Rename the last column to 'Age'
colnames(split_result_df)[3] <- "Age"

# Update the 'Age' column based on specific keywords
split_result_df$Age[grep("\\b(kid|childhood|child|primary school)\\b", split_result_df$Numbers_Found, ignore.case = TRUE)] <- "Child"
split_result_df$Age[grep("\\b(adolescent|adolescence|teenager|teen|high school)\\b", split_result_df$Numbers_Found, ignore.case = TRUE)] <- "Teenager"
split_result_df$Age[grep("\\b(college|young adult|university)\\b", split_result_df$Numbers_Found, ignore.case = TRUE)] <- "Young Adult"
split_result_df$Age[grep("\\b(adulthood|adult)\\b", split_result_df$Numbers_Found, ignore.case = TRUE)] <- "Adult"

# Modify 'Numbers_Found' column to keep only numbers if 'Age' is NA
split_result_df$Numbers_Found <- ifelse(
  is.na(split_result_df$Age),
  gsub(".*?(\\d+).*", "\\1", split_result_df$Numbers_Found), # Extract the number
  split_result_df$Numbers_Found                             # Otherwise, keep the original value
)

# Convert 'Numbers_Found' to numeric for further classification
split_result_df$Numbers_Found <- as.numeric(split_result_df$Numbers_Found)

# Update 'Age' based on numeric range if it is still NA
split_result_df$Age <- ifelse(
  is.na(split_result_df$Age) & !is.na(split_result_df$Numbers_Found), # Check if 'Age' is NA and 'Numbers_Found' is valid
  ifelse(
    split_result_df$Numbers_Found >= 4 & split_result_df$Numbers_Found <= 13, "Child",
    ifelse(
      split_result_df$Numbers_Found > 13 & split_result_df$Numbers_Found <= 15, "Teenager",
      ifelse(
        split_result_df$Numbers_Found > 15 & split_result_df$Numbers_Found <= 18, "Young Adult",
        "Adult" # Default to Adult if none of the above ranges match
      )
    )
  ),
  split_result_df$Age # Retain existing 'Age' if not NA
)
# Remove duplicate rows with the same ID and Age
split_result_df <- split_result_df[!duplicated(split_result_df[c("Post_ID", "Age")]), ]

# Print the updated data frame
print(split_result_df)

# Save the updated result to a new CSV file
write.csv(split_result_df, "classified_posts_final.csv", row.names = FALSE)
