# Load necessary libraries
library(dplyr)
library(stringr)

if (!require(igraph)) install.packages("igraph", dependencies = TRUE)
library(igraph)

# Define the keyword categories
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
  hopelessness = c("hopelessness", "hopeless", "no hope", "despair", "desperate", "feeling lost", "no way out", "pessimistic", 
                   "lost hope", "losing hope", "can't see a future", "feeling doomed", "out of options", "giving up", 
                   "nothing left", "pointless existence", "defeated", "will give up", "i am done", "i'm done", "this is it"),
  sleep_issues = c("insomnia", "sleep problems", "sleep deprived", "can't sleep", "difficulty sleeping", "restless nights", 
                   "wide awake", "tossing and turning", "no shut-eye", "broken sleep", "wakeful", "unable to rest", 
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
  self_harm = c("self-harm", "hurt myself", "harm myself", "cut myself", "self-injury", "self-inflicted pain", 
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
  addiction = c("substances", "substance abuse", "drugs", "drug abuse", "pills", "addicted", "dependency", "using", "edibles", "mushrooms", 
                "can't stop", "chemical dependence", "craving drugs", "pill addiction", "habit I can't break", "LSD", "heroine", "weed"),
  abuse = c("sexual abuse", "physical abuse", "emotional abuse", "abuse in relationships", "domestic violence", "abusive behavior", 
            "manipulative relationships", "control in relationships", "toxic relationships", "self-destructive relationships", 
            "victim of abuse", "trauma from abuse", "coercive control"),
  hallucinations = c("2D-cartoon", "cartoon", "cartoonish", "cartoon-ish", "imagination", "fantasy", "imagine", "imaginative", "episode", "abstract",
                     "voices", "voice", "see things", "illusion", "illusions", "delusions", "delusion", "delusional", "god", "devil", "jesus", "derealization",
                     "music", "musical", "songs", "hear noise", "hearing things", "seeing things", "demons", "demon", "hallucination", "hallucination", "hallucinate",
                     "hallucinations", "dream", "dreams", "wasn't real", "isn't real", "was not real", "is not real", "seeing silhouettes", "imaginary"),
  paranoia = c("paranoia", "paranoid"),
  anxiety  = c("anxiety", "anxious", "panic attacks", "heart racing","racing heart", "pound", "pounding"),
  homicidal_tendencies = c("homicidal", "kill", "killing", "killed", "hurt his", "hurt her", "hurt my")
)

# Function to count keywords in a post
count_keywords_in_post <- function(post, keyword_list) {
  post_text <- iconv(as.character(post), to = "UTF-8", sub = "byte")  # Convert to UTF-8 encoding
  post_text <- tolower(post_text)  # Convert to lowercase
  
  # Avoid issues with NA values by checking if post_text is not empty
  if (is.na(post_text) || post_text == "") {
    return(FALSE)
  }
  
  # Check if any of the keywords match
  matches <- sapply(keyword_list, function(keyword) {
    grepl(keyword, post_text, fixed = TRUE)  # Match keywords exactly
  })
  return(any(matches))
}

# Create all possible keyword pairs
keyword_pairs <- combn(names(keywords), 2, simplify = FALSE)

# Initialize results dataframe
results <- data.frame(Pair = character(), Count = integer(), stringsAsFactors = FALSE)

# Iterate through keyword pairs and count matches
for (pair in keyword_pairs) {
  keywords_1 <- keywords[[pair[1]]]
  keywords_2 <- keywords[[pair[2]]]
  
  count <- sum(sapply(data$clean_csv, function(post) {
    count_keywords_in_post(post, c(keywords_1, keywords_2))
  }))
  
  # Append the result to the results dataframe
  results <- rbind(results, data.frame(Pair = paste(pair[1], pair[2], sep = ", "), Count = count))
}

# Save the results to a new CSV file
write.csv(results, "keyword_pairs_count.csv", row.names = FALSE)

