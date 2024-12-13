library(igraph)
library(ggraph)
library(tidygraph)
library(dplyr)

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
            "victim of abuse", "trauma from abuse", "coercive control", "abusive", "abused", "abuse"),
  hallucinations = c("2D-cartoon", "cartoon", "cartoonish", "cartoon-ish", "imagination", "fantasy", "imagine", "imaginative", "episode", "abstract",
                     "voices", "voice", "see things", "illusion", "illusions", "delusions", "delusion", "delusional", "god", "devil", "jesus", "derealization",
                     "music", "musical", "songs", "hear noise", "hearing things", "seeing things", "demons", "demon", "hallucination", "hallucination", "hallucinate",
                     "hallucinations", "dream", "dreams", "wasn't real", "isn't real", "was not real", "is not real", "seeing silhouettes", "imaginary"),
  paranoia = c("paranoia", "paranoid"),
  anxiety  = c("anxiety", "anxious", "panic attacks", "heart racing","racing heart", "pound", "pounding"),
  homicidal_tendencies = c("homicidal", "kill", "killing", "killed", "hurt his", "hurt her", "hurt my")
)

# Load the data
data <- read.csv("clean.csv")
names(data)[1] <- "clean_csv"  # Renaming column from 'x' to 'clean_csv'

# Function to clean the text
clean_text <- function(text) {
  # Convert to UTF-8 encoding if necessary
  text <- iconv(text, from = "latin1", to = "UTF-8", sub = "")
  # Remove punctuation
  text <- gsub("[[:punct:]]", " ", text)
  # Remove numbers
  text <- gsub("[0-9]", " ", text)
  # Convert to lowercase
  text <- tolower(text)
  # Remove extra spaces
  text <- gsub("\\s+", " ", text)
  text <- trimws(text)
  return(text)
}

# Clean the text column
data$clean_csv <- sapply(data$clean_csv, clean_text)

# Initialize a list to store the counts for each keyword group
keyword_counts <- setNames(vector("list", length(keywords)), names(keywords))

# Loop through each keyword group and count occurrences in the posts
for (keyword_group_name in names(keywords)) {
  keyword_group <- keywords[[keyword_group_name]]
  
  # Initialize a counter for the current keyword group
  count <- 0
  
  # Loop through each post and count the occurrences of keywords from the current group
  for (post in data$clean_csv) {
    # Check if any keyword from the group is found in the post (count only once per post)
    if (any(sapply(keyword_group, function(keyword) {
      grepl(keyword, post, ignore.case = TRUE)
    }))) {
      count <- count + 1
    }
  }
  
  # Store the count in the list
  keyword_counts[[keyword_group_name]] <- count
}

# Convert the keyword counts to a data frame
keyword_counts_df <- data.frame(
  keyword = names(keyword_counts),
  count = unlist(keyword_counts),
  stringsAsFactors = FALSE
)

# Print the result data frame
print(keyword_counts_df)

# Optionally, save to a new CSV file
write.csv(keyword_counts_df, "keyword_counts.csv", row.names = FALSE)
# Load required libraries

# Step 1: Load the data
keyword_counts <- read.csv("keyword_counts.csv")

# Step 2: Calculate percentages
total_count <- sum(keyword_counts$count)
keyword_counts$percentage <- (keyword_counts$count / total_count) * 100

# Step 3: Create edges dataframe
edges <- data.frame(
  from = keyword_counts$keyword,  # Keywords as "from" nodes
  to = rep("symptoms", nrow(keyword_counts)),  # Central node "symptoms"
  weight = keyword_counts$percentage  # Edge weights are percentages
)

# Step 4: Create the graph
g <- graph_from_data_frame(edges, directed = FALSE)

# Step 5: Create a radial layout
central_node <- which(V(g)$name == "symptoms")
num_keywords <- length(V(g)) - 1  # Exclude the central node

# Define a circular layout for all nodes except the central node
angles <- seq(0, 2 * pi, length.out = num_keywords + 1)[-1]
radius <- 3  # Distance from the central node

# Create x and y coordinates for the layout
layout <- matrix(0, nrow = length(V(g)), ncol = 2)
layout[central_node, ] <- c(0, 0)  # Central node at origin
layout[-central_node, ] <- cbind(radius * cos(angles), radius * sin(angles))

# Step 6: Define edge labels with percentages
edge_labels <- paste0(round(E(g)$weight, 1), "%")

# Step 7: Plot the graph
plot(
  g,
  layout = layout,              # Use radial layout
  vertex.label = V(g)$name,     # Node names as labels
  vertex.label.cex = 0.8,       # Adjust label size
  vertex.size = 35,             # Increase node size for readability
  vertex.color = "skyblue",     # Node color
  edge.color = "darkgray",      # Edge color
  edge.width = 2,               # Edge thickness
  edge.label = edge_labels,     # Percentages as edge labels
  edge.label.cex = 0.7,         # Adjust edge label size
  edge.label.color = "black",   # Edge label color
  main = "Keyword-Symptoms Graph with Radial Layout"
)
