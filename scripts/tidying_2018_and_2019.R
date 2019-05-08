#second pass of cleaning then combining
library(tidyverse)
library(lubridate)

source(file.path("scripts/helper_functions.R"))

training_2018 <- file.path("tidy_inputs/clean_training_2018.RDS")
training_2019 <- file.path("tidy_inputs/clean_training_2019.RDS")
#load_rds would work if I had named lists. I dont.

load_rds("tidy_inputs/clean_training_2018.RDS")

all_trainings_2018 <- all_trainings
rm(all_trainings)

cleaned_support_professionals_2018 <- cleaned_support_professionals
rm(cleaned_support_professionals)

full_employee_table_2018 <- full_employee_table
rm(full_employee_table)

officer_training_2018 <- officer_training
rm(officer_training)

trainings_offered_2018 <- trainings_offered
rm(trainings_offered)

load_rds(training_2019)

all_trainings <- all_trainings %>% full_join(all_trainings_2018) %>%
  arrange(desc(date_started))

all_trainings <- all_trainings %>%
  mutate(date_ended = date_started + hours)

all_court_staff <- full_employee_table %>% 
  full_join(full_employee_table_2018) %>%
  unique() %>%
  mutate(position_start_date = "",
         position_end_date ="")

all_court_staff[2, ]$Division <- "Detention Diversion"

all_court_staff %>%
  group_by(First_Name, Last_Name) %>%
  summarize(occurance = n()) %>%
  arrange(desc(occurance))

PO <- all_court_staff %>%
  filter(PO_Title == "I" | PO_Title == "II") %>%
  unique() %>%
  group_by(First_Name, Last_Name)


PO %>% 
  summarize(count = n ()) %>%
  filter(count >= 2)


