library(stringdist) 
keywords <- list(
  depressed = c("depression", "depressive episode", "low mood", "sadness", "hopelessness", "feeling like failure", "loss of interest", "lethargy",
                "slow thinking", "fatigue", "no energy", "crying spells", "feeling empty", "worthlessness", "feeling numb", "sleeping too much", 
                "can't get out of bed", "darker days", "no motivation", "overwhelmed", "disconnection from others", "falling into a hole", "depressed", 
                "depressive", "feeling down", "melancholy", "blue", "disheartened", "sad vibes", "like shit", "emotionally drained", "lost interest", 
                "feeling horrible", "heavy-hearted", "desperate", "desperation", "despair", "apathetic", "apathy", "feel nothing", "cared less"),
  sadness = c("sadness", "sorrow", "unhappiness", "grief", "desolation", "misery", "miserable", "crying inside", "tearful", "broken-hearted", "aching heart", 
              "cry", "not happy", "not okay", "crying", "broken", "overwhelmed by sadness", "melancholy", "emotional pain"),
  sleep_issues = c("insomnia", "sleep problems", "sleep deprived", "can't sleep", "difficulty sleeping", "restless nights", "wide awake", "tossing and turning", 
                   "no shut-eye", "broken sleep", "wakeful", "unable to rest", "sleep deprivation", "staring at the ceiling", "sleepless", "fatigue", "tired",
                   "tiredness", "exhausted", "sleepy", "worn out", "drained", "burned out", "lethargic", "dead tired", "can't keep my eyes open", "zapped",
                   "physically done", "mentally done", "run down"),
  guilt = c("guilt", "guilty", "feel bad", "remorse", "regret", "ashamed", "self-blame", "sorry", "feeling at fault", "full of regret", "blame myself",
            "can't forgive myself", "feeling responsible"),
  anger = c("anger", "rage", "angry", "fury", "furious", "hate", "irritated", "annoyed", "resentful", "outraged", "wrath", "boiling inside", "seeing red",
            "pissed off", "mad", "frustrated", "ready to snap", "lost my temper", "gritting teeth"),
  emptiness = c("emptiness", "empty", "void", "hollow", "numb", "nothing inside", "lack of purpose", "feeling blank", "soul feels empty", "feeling dead inside",
                "missing something", "vacant", "meaningless existence", "emotionally void", "disconnected"),
  self_harm = c("self-harm", "carve", "carved", "burned myself", "hurt myself", "harm myself", "cut myself", "self-injury", "stabbed myself", "self-inflicted pain", 
                "self-destructive", "punish myself", "want to feel pain", "bleeding on purpose", "scarring myself"),
  suicide = c("suicide", "suicidal thoughts", "it ends", "i will end it", "don't want life", "hate life", "want to end it", "end my life", "no reason to live", 
              "give up on life", "kill myself", "thinking of ending it all", "life's too hard", "wish I was gone", "suicidal", "to die", "hang myself",
              "hang himself", "hang herself"),
  social_withdrawal = c("social withdrawal", "no friends", "don't want to go out", "haven't got friends", "alone", "isolated", "avoiding people", 
                        "keeping to myself", "stay home alone", "don't want company", "pushed everyone away", "socially distant", "don't feel like talking", 
                        "don't fit in", "loneliness", "lonely", "isolation", "want to scream", "feeling alone", "solitude", "can't socialize", "abandoned", "unwanted", "nobody around", "no one cares", 
                        "socially invisible", "cut off from everyone", "detached", "feeling friendless", "by myself", "ghosting", "withdraw from", "lonesome", "lonesomeness", "isolate"),
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
  paranoia = c("paranoia", "can't trust anyone", "paranoid", "my thoughts aren't my own", "illusion", "illusions", "delusions", "delusion", "delusional"),
  anxiety  = c("anxiety", "anxious", "panic", "panicked", "panic attacks", "heart racing","racing heart", "pound", "pounding"),
  homicidal_tendencies = c("homicidal", "kill", "killing", "killed", "hurt his", "hurt her", "hurt my"),
  genes = c("genetics", "genes", "genetical", "gene", "hereditary"),
  bipolar = c( "bipolar"),
  fear = c("fear", "scared", "afraid", "terrified", "horrified", "fearful"),
  psychosis = c( "psychosis", "psychotic"),
  schizophrenia = c("schizophrenic", "losing touch with reality", "schizophrenia", "schizoaffective", "schizo", "schizotypical"),
  poor_academic_perf = c("failing", "fail", "failed", "barely passed", "barely graduated", "barely passing", "not passed", "didn't pass", "can't study", "barely finished", "kicked out of the university",
                         "kicked out of university", "kicked out of uni", "kicked out of school", "kicked out of college", "pity passed me", "failed", "never do any work",
                         "i'm behind", "i am behind", "i was behind", "school life was very affected", "school life was affected", "not graduating"),
  trauma = c("PTSD", "trauma", "traumatic", "traumatised", "maltreatment", "suffering", "suffered"),
  confusion = c("confused", "can't tell what's real", "confusion", "confusing", "puzzled", "dazzled", "disorientation", "disorientated", "uncertain", "uncertainty", "don't know", "can't tell", "fogginess", "like fog", "don't understand",
                "can't understand", "not understand")
)




keyword_group <- c("hallucinations", "paranoia", "addiction", "schizophrenia", 
                   "anger", "depressed", "homicidal tendencies", "psychosis", 
                   "fear", "anxiety", "emptiness", "trauma", 
                   "confusion", "alcohol", "sadness", "suicide", 
                   "genes", "bipolar", "sleep issues", "guilt",
                   "poor academics", "self_harm")

values <- c(23, 22, 22, 20, 20, 20, 18, 17, 17, 16, 16, 16, 16, 13, 13, 13, 12, 11, 11, 9, 7, 4)
calculate_score <- function(input_text) {
  input_text <- tolower(input_text)
  input_text <- gsub("[[:punct:]]", "", input_text) 
  input_text <- gsub("\\s+", " ", input_text)        
  
  # words that if exist before our keywords, the keywords won't be taken into consideration
  negation_words <- c("not", "no", "never", "don't", "doesn't", "didn't", "can't", "won't", "isn't", "aren't")
  negation_pattern <- paste0("\\b(", paste(negation_words, collapse = "|"), ")\\s+\\w+")
  input_text <- gsub(negation_pattern, "", input_text)
  
  words <- unlist(strsplit(input_text, "\\s+"))  # Split text into words
  
  total_score <- 0
  matched_groups <- c()
  
  for (i in seq_along(keyword_group)) {
    group <- keyword_group[i]
    score <- values[i]
    synonyms <- keywords[[group]]
    
    if (any(words %in% tolower(synonyms))) {
      total_score <- total_score + score
      matched_groups <- c(matched_groups, group)
    }
  }
  
  if ((any(words %in% tolower(keywords[["hallucinations"]])) && 
       any(words %in% tolower(keywords[["paranoia"]]))) || 
      (any(words %in% tolower(keywords[["schizophrenia"]])))) {
    total_score <- total_score + 30
  }
  
  if (score>=50 && (!any(words %in% tolower(keywords[["hallucinations"]])) && 
       any(!words %in% tolower(keywords[["paranoia"]]))) || 
      (!any(words %in% tolower(keywords[["schizophrenia"]])))) {
    total_score <- total_score - 30
  }
  if (!any(words %in% tolower(keywords[["schizophrenia"]]))) {
    total_score <- total_score - 10
  }
  
  
  return(list(score = total_score, matched_groups = matched_groups))
}


data <- read.csv("d.csv", header = FALSE, stringsAsFactors = FALSE)
colnames(data) <- c("text")  
data$scores <- sapply(data$text, function(text) calculate_score(text)$score)
filtered_data <- data[data$scores >= 50, ]

write.csv(filtered_data, "filtered_scored_text.csv", row.names = FALSE)

total_posts <- nrow(data)
print(total_posts)
posts_above_50 <- nrow(filtered_data)
percentage_above_50 <- (posts_above_50 / total_posts) * 100

cat("Percentage of posts with a score >= 50:", round(percentage_above_50, 2), "%\n")
