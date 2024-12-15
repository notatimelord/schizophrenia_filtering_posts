library(igraph)
library(stringr)
library(dplyr)
library(tidyr)

# Load and clean the data
data <- read.csv("clean.csv")
colnames(data)[1] <- "clean_csv"

keywords <- list(
  depressed = c("depression", "depressive episode", "low mood", "sadness", "hopelessness", "feeling like failure", 
                "loss of interest", "lethargy", "slow thinking", "fatigue", "no energy", "crying spells", "feeling empty", 
                "worthlessness", "feeling numb", "sleeping too much", "can't get out of bed", "darker days", "no motivation", 
                "overwhelmed", "disconnection from others", "falling into a hole", "depressed", "depressive", "feeling down", 
                "melancholy", "blue", "disheartened", "sad vibes", "like shit", "emotionally drained", "lost interest", 
                "feeling horrible", "heavy-hearted", "in the dumps", "apathetic", "apathy", "feel nothing", "cared less"),
  sadness = c("sadness", "sorrow", "unhappiness", "grief", "desolation", "misery", "miserable", "crying inside", "tearful", 
              "broken-hearted", "aching heart", "cry", "not happy", "not okay", "crying", "broken", "overwhelmed by sadness", 
              "melancholy mood", "emotional pain"),
  sleep_issues = c("insomnia", "sleep problems", "sleep deprived", "can't sleep", "difficulty sleeping", "restless nights", 
                   "wide awake", "tossing and turning", "no shut-eye", "broken sleep", "wakeful", "unable to rest", "sleep deprivation",
                   "staring at the ceiling", "sleepless", "fatigue", "tired", "tiredness", "exhausted", "sleepy", "worn out", "drained", 
                   "burned out", "lethargic", "dead tired", "can't keep my eyes open", "zapped", "physically done", "mentally done", "run down"),
  guilt = c("guilt", "guilty", "feel bad", "remorse", "regret", "ashamed", "self-blame", "sorry", "feeling at fault", 
            "full of regret", "blame myself", "can't forgive myself", "feeling responsible"),
  anger = c("anger", "rage", "angry", "fury", "furious", "hate", "irritated", "annoyed", "resentful", "outraged", "wrath", 
            "boiling inside", "seeing red", "pissed off", "mad", "frustrated", "ready to snap", "lost my temper", 
            "gritting teeth"),
  emptiness = c("emptiness", "empty", "void", "hollow", "numb", "nothing inside", "lack of purpose", "feeling blank", 
                "soul feels empty", "feeling dead inside", "missing something", "vacant", "meaningless existence", 
                "emotionally void", "disconnected"),
  self_harm = c("self-harm", "carve", "carved", "burned myself", "hurt myself", "harm myself", "cut myself", "self-injury", "stabbed myself", "self-inflicted pain", 
                "self-destructive", "punish myself", "want to feel pain", "bleeding on purpose", "scarring myself"),
  suicide = c("suicide", "suicidal thoughts", "it ends", "i will end it", "don't want life", "hate life", "want to end it", 
              "end my life", "no reason to live", "give up on life", "kill myself", "thinking of ending it all", 
              "life's too hard", "wish I was gone", "suicidal", "to die", "hang myself", "hang himself", "hang herself"),
  social_withdrawal = c("social withdrawal", "no friends", "don't want to go out", "haven't got friends", "alone", "isolated", 
                        "avoiding people", "keeping to myself", "stay home alone", "don't want company", "pushed everyone away", 
                        "socially distant", "don't feel like talking", "don't fit in", "loneliness", "lonely", "isolation",
                        "want to scream", "feeling alone", "solitude", "can't socialize", "abandoned", "unwanted", "nobody around", "no one cares", 
                        "socially invisible", "cut off from everyone", "detached", "feeling friendless", "by myself", "ghosting",
                        "withdraw from", "lonesome", "lonesomeness"),
  alcohol = c("alcohol", "alcoholism", "drink", "drinking problem", "alcoholic", "booze", "liquor", "beer", "whiskey", "wine", 
              "spirits", "intoxicated", "dependence on alcohol", "getting drunk", "wasted", "tipsy", "too much alcohol", 
              "need a drink", "drank", "drunk"),
  addiction = c("substances", "substance abuse", "drugs", "drug", "meth", "drug abuse", "pills", "addicted", "dependency", "using", "edibles", "mushrooms", 
                "can't stop", "usage", "chemical dependence", "craving drugs", "pill addiction", "habit I can't break", "LSD", "heroine", "weed"),
  abuse = c("sexual abuse", "physical abuse", "emotional abuse", "abuse in relationships", "domestic violence", "abusive behavior", 
            "manipulative relationships", "control in relationships", "toxic relationships", "self-destructive relationships", 
            "victim of abuse", "trauma from abuse", "coercive control", "abusive", "abused", "abuse", "rape", "raped"),
  hallucinations = c("2D-cartoon", "cartoon", "cartoonish", "cartoon-ish", "imagination", "fantasy", "imagine", "imaginative", "episode", "abstract",
                     "voices", "voice", "see things", "god", "devil", "jesus", "derealization",
                     "music", "musical", "songs", "hear noise", "hearing things", "seeing things", "demons", "demon", "hallucination", "hallucination", "hallucinate",
                     "hallucinations", "dream", "dreams", "wasn't real", "isn't real", "was not real", "is not real", "seeing silhouettes", "imaginary"),
  paranoia = c("paranoia", "paranoid", "illusion", "illusions", "delusions", "delusion", "delusional"),
  anxiety  = c("anxiety", "anxious", "panic attacks", "heart racing","racing heart", "pound", "pounding"),
  homicidal_tendencies = c("homicidal", "kill", "killing", "killed", "hurt his", "hurt her", "hurt my"),
  genes = c("genetics", "genes", "genetical", "gene", "hereditary"),
  schizophrenia = c("schizophrenic", "schizophrenia", "schizoaffective", "schizo", "schizotypical"),
  poor_academic_perf = c("failing", "fail", "failed", "barely passed", "barely graduated", "barely passing", "not passed", "didn't pass", "can't study", "barely finished", "kicked out of the university",
                         "kicked out of university", "kicked out of uni", "kicked out of school", "kicked out of college", "pity passed me", "failed", "never do any work",
                         "i'm behind", "i am behind", "i was behind", "school life was very affected", "school life was affected", "not graduating")
  
)


# Flatten the keywords list into a vector
all_keywords <- unlist(keywords)

# Function to check if a text contains any keywords
contains_keywords <- function(text, keywords) {
  # Create a regex pattern from the keywords
  pattern <- paste0("\\b(", paste(keywords, collapse = "|"), ")\\b")
  return(str_detect(text, regex(pattern, ignore_case = TRUE)))
}

# Add a column to indicate if the post contains any keywords
data <- data %>%
  mutate(Contains_Keywords = sapply(clean_csv, contains_keywords, keywords = all_keywords))

# Filter rows containing keywords
data_with_keywords <- data %>%
  filter(Contains_Keywords == TRUE)

# Function to extract and validate numbers based on conditions
extract_valid_number <- function(text) {
  pattern <- "\\b(?:a\\s+|an\\s+|was\\s+|at\\s+|around\\s+|age\\s+of\\s+|im\\s+|about\\s+)?(\\d{1,2})(?=(\\s*(?:s|'s|\\s+and\\b|\\s+or\\b|\\s+when\\b|\\s+which\\b|\\s*(?:\\d{1,2}'s|20\\s*s|30\\s*s|40\\s*s|50\\s*s|60\\s*s|70\\s*s|twenty|thirty|forty|fifty|sixty|seventy|twenty\\s+years?))|[\\s\\b]*$))"
  matches <- str_match_all(text, pattern)
  numbers <- as.numeric(matches[[1]][, 2])
  if (length(numbers) > 0) return(numbers) else return(NA)
}

# Classify age group based on extracted numbers
classify_age_group <- function(num) {
  if (4 < num & num < 13) return("Kid")
  if (num >= 13 & num <= 17) return("Teenager")
  if (num >= 18 & num <= 24) return("Young Adult")
  if (num >= 25 & num <= 100) return("Adult")
  return(NA)
}

# Process each post to extract numbers and classify age groups
data_with_keywords <- data_with_keywords %>%
  mutate(Numbers_Found = sapply(clean_csv, extract_valid_number)) %>%
  filter(!is.na(Numbers_Found)) %>%
  unnest_longer(Numbers_Found) %>%
  mutate(
    Age_Group = sapply(Numbers_Found, classify_age_group)
  ) %>%
  group_by(clean_csv) %>%
  filter(n_distinct(Age_Group) > 1 | row_number() == 1) %>%  # Keep only distinct age groups per post
  ungroup()

# Write the final data to CSV
output_data <- data_with_keywords %>%
  select(Post = clean_csv, Age = Numbers_Found, Age_Group) %>%
  arrange(Post, Age)
write.csv(output_data, "matched_posts_with_ages.csv", row.names = FALSE)

# Summary of the results
print("CSV created: matched_posts_with_ages.csv")




library(dplyr)
library(stringr)

# Read the CSV file
df <- read.csv("cleaned_matched_posts_with_ages.csv")
# Function to check for the presence of keywords in a text
check_traits <- function(text, keywords_list) {
  matches <- sapply(names(keywords_list), function(trait) {
    pattern <- paste0("\\b(", paste(keywords_list[[trait]], collapse = "|"), ")\\b")
    if (str_detect(text, regex(pattern, ignore_case = TRUE))) {
      return(trait)
    }
    return(NA)
  })
  return(na.omit(matches))  # Return non-NA matches
}

# Apply the function to each row and split rows for multiple matches
df_traits <- df %>%
  rowwise() %>%
  mutate(traits = list(check_traits(Post, keywords))) %>%  # Replace `post_text` with the correct column
  unnest(traits)  # Expand rows for multiple matches

# Save the updated data to a new CSV
# Remove the first two columns
df_traits <- df_traits %>%
  select(-1, -2)  # Removes the first two columns by index

df_traits <- df_traits %>%
  mutate(Pair = paste0(Age_Group, ", ", traits)) %>%  # Replace `Age_Group` and `Pairs` with actual column names
  select(-Age_Group, -traits)  # Drop the original columns after combining


# Save the updated data to a new CSV
write.csv(df_traits, "matched_posts_with_traits.csv", row.names = FALSE)

# Calculate counts of unique pairs
pair_counts <- df_traits %>%
  group_by(Pair) %>%
  summarise(Count = n(), .groups = "drop")  # Group by Pair and count occurrences
pair_counts <- pair_counts %>%
  mutate(Percentage = 0)
# Save the unique pairs and their counts to a new CSV
write.csv(pair_counts, "pair_counts.csv", row.names = FALSE)

print("CSV created with unique pairs and their counts.")
# Load the other CSV file
keyword_pairs_count <- read.csv("keyword_pairs_count.csv")

# Merge the two datasets by the 'Pair' column
merged_data <- full_join(pair_counts, keyword_pairs_count, by = "Pair")

# Combine Count.x and Count.y into one column called 'Count'
merged_data <- merged_data %>%
  mutate(Count = coalesce(Count.x, 0) + coalesce(Count.y, 0)) %>%
  select(-Count.x, -Count.y, -Percentage.x, -Percentage.y)  # Remove old count columns and Percentage

# Save the final merged data to a new CSV
write.csv(merged_data, "final_merged_keyword_pair_counts.csv", row.names = FALSE)

print("CSV created with merged counts and without the Percentage column.")



library(igraph)
library(dplyr)
library(tidyr)

results <- read.csv("final_merged_keyword_pair_counts.csv")

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

global_clustering <- transitivity(g, type = "global")
print(global_clustering)

local_clustering <- transitivity(g, type = "localaverage")
print(local_clustering)

# Calculate degree for each node
node_degrees <- degree(g, mode = "all")  # Use "all" for an undirected graph

# Find the node with the highest degree
most_significant_node <- names(which.max(node_degrees))

# Print the most significant node and its degree
cat("The most significant node is:", most_significant_node, "with a degree of", max(node_degrees), "\n")

# Optional: View all nodes and their degrees
print(node_degrees)


keywords <- c("anger", "paranoia", "hallucinations", "addiction", 
              "emptiness", "depressed", "alcohol", "social_withdrawal", 
              "sadness", "abuse", "homicidal_tendencies", "suicide", 
              "anxiety", "sleep_issues", "genes", "self_harm", 
              "guilt", "schizophrenia", "poor_academic_perf", "Adult", 
              "Kid", "Teenager", "Young Adult")

values <- c(17, 20, 22, 20, 10, 19, 8, 11, 12, 7, 13, 13, 15, 11, 9, 4, 8, 18, 9, 
            11, 7, 9, 9) # Adjusted values based on node degrees.

# Create an edge list
edges <- unlist(lapply(keywords, function(keyword) c("Degrees", keyword)))

# Create the graph
g <- graph_from_edgelist(matrix(edges, ncol = 2, byrow = TRUE), directed = FALSE)

# Set edge weights (values)
E(g)$weight <- values

# Create a custom layout for the circular structure
# Calculate positions manually for even distribution
num_nodes <- length(keywords)
theta <- seq(0, 2*pi, length.out = num_nodes + 1)[-1]  # Angles for the nodes
layout <- cbind(cos(theta), sin(theta))  # Coordinates for the nodes in a circle

# Adjust layout so that "Degrees" stays at the center
layout <- rbind(c(0, 0), layout)  # Add "Degrees" at the center

# Plot the graph with light blue nodes and names inside the nodes
plot(g, layout = layout, vertex.size = 35, vertex.label.cex = 0.8, 
     vertex.label.dist = 0,  # Place the labels inside the nodes
     vertex.label.color = "black", 
     vertex.color = "lightblue",  # Set node color to light blue
     edge.label = E(g)$weight, edge.label.cex = 0.8, edge.label.color = "blue", 
     main = "Degrees - Circular Graph")
