# Load necessary libraries
library(dplyr)
library(stringr)

if (!require(igraph)) install.packages("igraph", dependencies = TRUE)
library(igraph)


keywords <- list(
  depressed = c("depression", "depressive episode", "low mood", "sadness", "hopelessness", "feeling like failure", 
                "loss of interest", "lethargy", "slow thinking", "fatigue", "no energy", "crying spells", "feeling empty", 
                "worthlessness", "feeling numb", "sleeping too much", "can't get out of bed", "darker days", "no motivation", 
                "overwhelmed", "disconnection from others", "falling into a hole", "depressed", "depressive", "feeling down", 
                "melancholy", "blue", "disheartened", "sad vibes", "like shit", "emotionally drained", "lost interest", 
                "feeling horrible", "heavy-hearted", "desperate", "desperation", "despair", "apathetic", "apathy", "feel nothing", "cared less"),
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
                        "withdraw from", "lonesome", "lonesomeness", "isolate"),
  alcohol = c("alcohol", "alcoholism", "drink", "drinking problem", "alcoholic", "booze", "liquor", "beer", "whiskey", "wine", 
              "spirits", "intoxicated", "dependence on alcohol", "getting drunk", "wasted", "tipsy", "too much alcohol", 
              "need a drink", "drank", "drunk"),
  addiction = c("substances", "substance abuse", "drugs", "drug", "meth", "drug abuse", "pills", "addicted", "dependency", "using", "edibles", "mushrooms", 
                "can't stop", "usage", "chemical dependence", "craving drugs", "pill addiction", "habit I can't break", "LSD", "heroine", "weed"),
  abuse = c("sexual abuse", "physical abuse", "emotional abuse", "abuse in relationships", "domestic violence", "abusive behavior", 
            "manipulative relationships", "control in relationships", "toxic relationships", "self-destructive relationships", 
            "victim of abuse", "trauma from abuse", "coercive control", "abusive", "abused", "abuse", "rape", "raped"),
  hallucinations = c("2D-cartoon", "positive symptoms", "cartoon", "cartoonish", "cartoon-ish", "imagination", "fantasy", "imagine", "imaginative", "episode", "abstract",
                     "voices", "voice", "see things", "god", "devil", "jesus", "derealization", "feels vivid", "feels real",
                     "music", "musical", "songs", "hear noise", "hearing things", "seeing things", "demons", "demon", "hallucination", "hallucination", "hallucinate",
                     "hallucinations", "dream", "dreams", "wasn't real", "isn't real",  "not real", "seeing silhouettes", "imaginary"),
  paranoia = c("paranoia", "paranoid", "illusion", "illusions", "delusions", "delusion", "delusional"),
  anxiety  = c("anxiety", "anxious", "panic", "panicked", "panic attacks", "heart racing","racing heart", "pound", "pounding"),
  homicidal_tendencies = c("homicidal", "kill", "killing", "killed", "hurt his", "hurt her", "hurt my"),
  genes = c("genetics", "genes", "genetical", "gene", "hereditary"),
  bipolar = c( "bipolar"),
  fear = c("fear", "scared", "afraid", "terrified", "horrified", "fearful"),
  psychosis = c( "psychosis", "psychotic"),
  schizophrenia = c("schizophrenic", "schizophrenia", "schizoaffective", "schizo", "schizotypical"),
  poor_academic_perf = c("failing", "fail", "failed", "barely passed", "barely graduated", "barely passing", "not passed", "didn't pass", "can't study", "barely finished", "kicked out of the university",
                         "kicked out of university", "kicked out of uni", "kicked out of school", "kicked out of college", "pity passed me", "failed", "never do any work",
                         "i'm behind", "i am behind", "i was behind", "school life was very affected", "school life was affected", "not graduating"),
  trauma = c("PTSD", "trauma", "traumatic", "traumatised", "maltreatment", "suffering", "suffered"),
  confusion = c("confused", "confusion", "confusing", "puzzled", "dazzled", "disorientation", "disorientated", "uncertain", "uncertainty", "don't know", "can't tell", "fogginess", "like fog", "don't understand",
                "can't understand", "not understand")
)

data <- read.csv("clean.csv")
total_posts <- nrow(data) 

# Function to check if a group of keywords appears in a post
contains_keywords <- function(post, keyword_list) {
  post_text <- iconv(as.character(post), to = "UTF-8", sub = "byte")  # Convert to UTF-8 encoding
  post_text <- tolower(post_text)  # Convert to lowercase
  
  # Avoid issues with NA values by checking if post_text is not empty
  if (is.na(post_text) || post_text == "") {
    return(FALSE)
  }
  
  # Check if any of the keywords in the group match
  matches <- sapply(keyword_list, function(keyword) {
    grepl(keyword, post_text, fixed = TRUE)  # Match keywords exactly
  })
  return(any(matches))
}

# Initialize results dataframes
pair_counts <- data.frame(Pair = character(), Count = integer(), stringsAsFactors = FALSE)
matched_posts <- data.frame(Post = character(), Pair = character(), stringsAsFactors = FALSE)

# Ensure keyword pairs are correctly formed
keyword_pairs <- combn(names(keywords), 2, simplify = FALSE)

# Iterate through posts and check for matches
for (post in data$x) {
  matched_pairs <- c()  # Keep track of pairs matched in the current post
  
  for (pair in keyword_pairs) {
    # Ensure pair is valid
    if (!all(pair %in% names(keywords))) {
      next
    }
    
    keywords_1 <- keywords[[pair[1]]]
    keywords_2 <- keywords[[pair[2]]]
    
    # Check if both keyword groups appear in the post
    if (contains_keywords(post, keywords_1) && contains_keywords(post, keywords_2)) {
      pair_name <- paste(pair[1], pair[2], sep = ", ")
      matched_pairs <- c(matched_pairs, pair_name)  # Add the pair to matched pairs
      matched_posts <- rbind(matched_posts, data.frame(Post = post, Pair = pair_name))
    }
  }
  
  # Count unique matches for the current post
  if (length(matched_pairs) > 0) {
    for (unique_pair in unique(matched_pairs)) {
      existing_count <- pair_counts$Count[pair_counts$Pair == unique_pair]
      if (length(existing_count) == 0) {
        pair_counts <- rbind(pair_counts, data.frame(Pair = unique_pair, Count = 1))
      } else {
        pair_counts$Count[pair_counts$Pair == unique_pair] <- existing_count + 1
      }
    }
  }
}


# Calculate percentages
pair_counts <- pair_counts %>%
  mutate(Percentage = round((Count / total_posts) * 100, 2))  # Add percentage column

# Get the top three pairs by percentage
top_3_pairs <- pair_counts %>%
  arrange(desc(Percentage)) %>%
  head(3)

# Print the top three pairs
print("Top 3 pairs with the highest percentages:")
print(top_3_pairs)

# Save results to CSV files
write.csv(pair_counts, "keyword_pairs_count.csv", row.names = FALSE)
write.csv(matched_posts, "matched_posts_with_pairs.csv", row.names = FALSE)
