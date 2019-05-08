unique(trainings_offered$title_of_training)

unique_trainings_offered <- length(unique(trainings_offered$title_of_training))

total_training_hours <- sum(trainings_offered$hours, na.rm = TRUE)