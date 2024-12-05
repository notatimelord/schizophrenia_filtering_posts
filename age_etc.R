data <- read.csv("schizo_data_2.csv")
data <- data[, c("sex", "IQ", "education", "specific.disorder")]
data <- subset(data, specific.disorder %in% c("Schizophrenia", "Healthy control"))  # Filter by disorder
data <- na.omit(data)  
write.csv(data, "IQ.csv", row.names = FALSE)

# Initialize a data frame to store group counts
categories <- data.frame(
  Category = c(
    # Sex + IQ
    "female, IQ<=90", "female 90<IQ<110", "female IQ>=110",
    "male, IQ<=90", "male 90<IQ<110", "male IQ>=110",
    # Sex + Disorder
    "female, healthy", "female, schizophrenic",
    "male, healthy", "male, schizophrenic",
    # Disorder + IQ
    "schizophrenic, IQ<=90", "schizophrenic, 90<IQ<110", "schizophrenic, IQ>=110",
    "healthy, IQ<=90", "healthy, 90<IQ<110", "healthy, IQ>=110",
    # Education + IQ
    "Educ.<=16, IQ<=90", "Educ.<=16, 90<IQ<110", "Educ.<=16, IQ>=110",
    "Educ.>16, IQ<=90", "Educ.>16, 90<IQ<110", "Educ.>16, IQ>=110",
    # Education + Sex
    "Educ.<=16, Male", "Educ.<=16, Female",
    "Educ.>16, Male", "Educ.>16, Female",
    # Education + Disorder
    "Educ.<=16, Schizophrenia", "Educ.<=16, Healthy control",
    "Educ.>16, Schizophrenia", "Educ.>16, Healthy control"
  ),
  Count = 0
)

# Loop through the rows of the filtered data and count occurrences in each category
for (i in 1:nrow(data)) {
  sex <- data$sex[i]
  iq <- data$IQ[i]
  disorder <- data$specific.disorder[i]
  education <- data$education[i]
  
  # IQ and sex-based categories
  if (sex == "F" && iq <= 90) {
    categories$Count[1] <- categories$Count[1] + 1
  } else if (sex == "F" && iq > 90 && iq < 110) {
    categories$Count[2] <- categories$Count[2] + 1
  } else if (sex == "F" && iq >= 110) {
    categories$Count[3] <- categories$Count[3] + 1
  } else if (sex == "M" && iq <= 90) {
    categories$Count[4] <- categories$Count[4] + 1
  } else if (sex == "M" && iq > 90 && iq < 110) {
    categories$Count[5] <- categories$Count[5] + 1
  } else if (sex == "M" && iq >= 110) {
    categories$Count[6] <- categories$Count[6] + 1
  }
  
  # Health status categories (with IQ conditions properly handled)
  # Health status categories (with IQ conditions properly handled)
  if (disorder == "Healthy control") {
    if (sex == "F") {
      categories$Count[7] <- categories$Count[7] + 1  # female, healthy
    } else if (sex == "M") {
      categories$Count[9] <- categories$Count[9] + 1  # male, healthy
    }
  } else if (disorder == "Schizophrenia") {
    if (sex == "F") {
      categories$Count[8] <- categories$Count[8] + 1  # female, schizophrenic
    } else if (sex == "M") {
      categories$Count[10] <- categories$Count[10] + 1  # male, schizophrenic
    }
  }
  
  
  # Additional categories for disorder + IQ combined (Schizophrenia and Healthy control)
  if (disorder == "Schizophrenia" && iq <= 90) {
    categories$Count[11] <- categories$Count[11] + 1  # Schizophrenic, IQ<=90
  } else if (disorder == "Healthy control" && iq <= 90) {
    categories$Count[12] <- categories$Count[12] + 1  # Healthy, IQ<=90
  }
  if (disorder == "Schizophrenia" && iq > 90 && iq < 110) {
    categories$Count[13] <- categories$Count[13] + 1  # Schizophrenic, 90<IQ<110
  } else if (disorder == "Healthy control" && iq > 90 && iq < 110) {
    categories$Count[14] <- categories$Count[14] + 1  # Healthy, 90<IQ<110
  }
  if (disorder == "Schizophrenia" && iq >= 110) {
    categories$Count[15] <- categories$Count[15] + 1  # Schizophrenic, IQ>=110
  } else if (disorder == "Healthy control" && iq >= 110) {
    categories$Count[16] <- categories$Count[16] + 1  # Healthy, IQ>=110
  }
  
  # Education-based categories
  if (education <= 16 && iq <= 90) {
    categories$Count[17] <- categories$Count[17] + 1
  } else if (education <= 16 && iq > 90 && iq < 110) {
    categories$Count[18] <- categories$Count[18] + 1
  } else if (education <= 16 && iq >= 110) {
    categories$Count[19] <- categories$Count[19] + 1
  } else if (education > 16 && iq <= 90) {
    categories$Count[20] <- categories$Count[20] + 1
  } else if (education > 16 && iq > 90 && iq < 110) {
    categories$Count[21] <- categories$Count[21] + 1
  } else if (education > 16 && iq >= 110) {
    categories$Count[22] <- categories$Count[22] + 1
  }
  
  # Education + Sex categories
  if (education <= 16 && sex == "M") {
    categories$Count[23] <- categories$Count[23] + 1
  } else if (education <= 16 && sex == "F") {
    categories$Count[24] <- categories$Count[24] + 1
  } else if (education > 16 && sex == "M") {
    categories$Count[25] <- categories$Count[25] + 1
  } else if (education > 16 && sex == "F") {
    categories$Count[26] <- categories$Count[26] + 1
  }
  
  # Education + Disorder categories
  if (education <= 16 && disorder == "Schizophrenia") {
    categories$Count[27] <- categories$Count[27] + 1
  } else if (education <= 16 && disorder == "Healthy control") {
    categories$Count[28] <- categories$Count[28] + 1
  } else if (education > 16 && disorder == "Schizophrenia") {
    categories$Count[29] <- categories$Count[29] + 1
  } else if (education > 16 && disorder == "Healthy control") {
    categories$Count[30] <- categories$Count[30] + 1
  }
}


# Save the results to a new CSV
write.csv(categories, "IQ_sex_disorder_education_categories.csv", row.names = FALSE)

# Print the result for verification
print(categories)
