#second pass of cleaning then combining
library(tidyverse)
library(lubridate)

source(file.path("scripts/helper_functions.R"))
source(file.path("scripts/find_replace.R"))

today <- as.Date("2019-05-09") #date i decided. I am an arbitrary dude.
end_date <- as.Date("2019-05-01")

training_2018 <- file.path("tidy_inputs/clean_training_2018.RDS")
training_2019 <- file.path("tidy_inputs/clean_training_2019.RDS")
#load_rds would work if I had named lists. I dont.

load_rds(training_2018)
load_rds(training_2019)
rm(training_2018)


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
rm(all_trainings_2018)


all_trainings %>%
  group_by(title_of_training, date_started) %>%
  summarise(count =n()) %>% 
  arrange(desc(count))

all_trainings <- all_trainings %>%
  mutate(date_ended = date_started + hours)


all_court_staff <- full_employee_table %>% 
  full_join(full_employee_table_2018) %>%
  unique() %>%
  mutate(position_start_date = as.Date("2018-05-01"),
         position_end_date = as.Date(NA),
         Section = as.integer(NA),
         Unit = as.character(NA),
         ID = 1:nrow(all_court_staff)) %>% 
  select(ID, First_Name, Last_Name, Section, Division, Unit, PO_Title, 
         position_start_date, position_end_date)
#

#cleaning
(dv <- all_court_staff$Division %>% unique())

all_court_staff <- find_replace(all_court_staff, Division, "ChicagoEast", "Chicago East")
all_court_staff <- find_replace(all_court_staff, Division, "Chicago East", "Southeast")
all_court_staff <- find_replace(all_court_staff, Division, "South East", "Southeast")
all_court_staff <-find_replace(all_court_staff, Division, "Detention, Diversion", "Detention Diversion")
all_court_staff <- find_replace(all_court_staff, Division, "Advocay & Finance", 
                                "Advocacy & Finance")
all_court_staff <- find_replace(all_court_staff, Division, "Advocacy & Finance & Finance", 
                                "Advocacy & Finance")


all_court_staff <- find_replace(all_court_staff, Division, "Advocacy & Finace", 
                                "Advocacy & Finance")
all_court_staff <- find_replace(all_court_staff, Division, "Advocacy", 
                                "Advocacy & Finance")
all_court_staff <- find_replace(all_court_staff, Division, "Advocacy/Drug Unit" , 
                                "Educational Services")

all_court_staff <- find_replace(all_court_staff, Division,
                                "Pre-Trial Services/Drug Unit", 
                                "Educational Services Division")
all_court_staff <- find_replace(all_court_staff, Division, "SouthEast", "Southeast")
all_court_staff <- find_replace(all_court_staff, Division, "MIS", "Southeast")

# would be so much easier if I got regex
all_court_staff <- find_replace(all_court_staff, Division, "Clinical Interventions", "Clinical Assessment and Support")
all_court_staff <- find_replace(all_court_staff, Division, "Clinical Asses & Support", "Clinical Assessment and Support")
all_court_staff <- find_replace(all_court_staff, Division, "Clinical", "Clinical Assessment and Support")
all_court_staff <- find_replace(all_court_staff, Division, "Clinical Assessment and Support Assessment and Support", "Clinical Assessment and Support")
all_court_staff <- find_replace(all_court_staff, Division, "Clinical Assessment and Support Assessment and Support", "Clinical Assessment and Support")
all_court_staff <- find_replace(all_court_staff, Division, "Clinical Assessment and Support Assess & Support", "Clinical Assessment and Support")
all_court_staff <- find_replace(all_court_staff, Division, "Chicago North", "Chicago South")
all_court_staff <- find_replace(all_court_staff, Division, "Chicago South/Southeast", "Chicago South")
all_court_staff <- find_replace(all_court_staff, Division, 
                                "Chicago Southwest",
                                "Chicago South")


all_court_staff <- find_replace(all_court_staff, Division, "Grants &Technology", "Grants & Technology")
"Advocacy & Finance/Drug Unit"  
all_court_staff <- find_replace(all_court_staff, Division, 
                                "Advocacy & Finance/Drug Unit", 
                                "Educational Services")


all_court_staff <- find_replace(all_court_staff, Division, "Chicago West/Educational Services", "Educational Services")
all_court_staff <- find_replace(all_court_staff, Division, "Receptionist" , "Operational Support Services")
all_court_staff <- find_replace(all_court_staff, Division, 
                                "Stenographic" , 
                                "Operational Support Services")

all_court_staff <- find_replace(all_court_staff, Division, 
                                "Operational Support", 
                                "Operational Support Services")

all_court_staff <- find_replace(all_court_staff, Division, 
                                "Record Library", 
                                "Operational Support Services")


all_court_staff <- find_replace(all_court_staff, Division, "Financial Support", "Advocacy & Finance")
all_court_staff <- find_replace(all_court_staff, Division, "Payroll", "Advocacy & Finance")
all_court_staff <- find_replace(all_court_staff, Division, "Chicago Court&Diversion/Det. Diversion", "Chicago Court & Diversion Services")
all_court_staff <- find_replace(all_court_staff, Division, 
                                "Records and Info Processor", 
                                "Operational Support Services")

all_court_staff <- find_replace(all_court_staff, Division, 
                                "Career and IT Services",
                                "Office of Career Services")

all_court_staff <- find_replace(all_court_staff, Division, 
                                "Purchasing",
                                "Grants & Technology")

all_court_staff <- find_replace(all_court_staff, Division, 
                                "Director of HR"  ,
                                "Personnel")


all_court_staff <- find_replace(all_court_staff, Division, 
                                "Markham Division",
                                "Markham")

all_court_staff <- find_replace(all_court_staff, Division, 
                                "Markham Suburban",
                                "Markham")
all_court_staff <- find_replace(all_court_staff, Division, 
                                "West Suburban",
                                "Southwest Suburban")

all_court_staff <- find_replace(all_court_staff, Division, 
                                "Detention Alternatives",
                                "Detention Diversion")











temp_sworn_staff <- all_court_staff %>%
  group_by(First_Name, Last_Name) %>%
  summarise(count = n()) %>%
  arrange(Last_Name)

#"Spooner-Parise", "O'Connel."

wrong_name <- c("Anderson","Blackmond","Kasperski",
                "Pacelt", "Salazar")

right_name <- c("Bridgitte", "Michael", "Steve", "Daniel",
                "Joseph", "Arnulfo")

# temp_sworn_staff <- 
#   temp_sworn_staff %>%
#   mutate_at(vars(contains(wrong_name), First_Name == right_name)) %>%
#   str_replace(temp_sworn_staff$First_Name, right_name)
# 
# temp_sworn_staff <-
#   temp_sworn_staff$Last_Name %>% 
#   str_locate(wrong_name)


### Division Work ####
Divisions_df <- all_court_staff %>%
  group_by(Division) %>%
  summarize(count = n())


all_court_staff[2, ]$Division <- "Detention Diversion"

staff_occurance <- all_court_staff %>%
  group_by(First_Name, Last_Name) %>%
  summarize(occurance = n()) %>%
  arrange(desc(occurance)) %>% 
  ungroup()

staff_one <- staff_occurance %>%
  filter(occurance == 1) %>%
  select(First_Name, Last_Name)

staff_one <- all_court_staff %>% 
  inner_join(staff_one) %>%
  mutate(position_start_date = today) %>%
  arrange(Last_Name)

duplicates <- staff_occurance %>%
  filter(occurance >= 2) %>%
  arrange(Last_Name)

all_court_staff %>%
  semi_join(duplicates) %>%
  arrange(Last_Name)

all_court_staff$Division %>% unique()

all_court_staff %>%
  filter(Last_Name == "Aguilar")

all_court_staff %>%
  filter(position_start_date != today)

#creation of PO I and II Table
PO <- all_court_staff %>%
  filter(PO_Title == "I" | PO_Title == "II") %>%
  unique() %>%
  group_by(First_Name, Last_Name)

dup_PO <- PO %>% 
  group_by(First_Name, Last_Name) %>%
  summarize(occurance = n()) %>%
  arrange(desc(occurance)) %>%
  filter(occurance >= 2) %>%
  select(First_Name, Last_Name)

PO_temp <- PO %>%
  anti_join(dup_PO) %>%
  arrange(Last_Name) %>%
  mutate(position_start_date = today)

PO %>%
  inner_join(dup_PO) %>%
  arrange(Last_Name)

support_professionals <- all_court_staff %>%
  filter(PO_Title == "Support Professional")

all_court_staff$Division <- all_court_staff$Division %>%
  str_replace_all("ChicagoEast", "Chicago East")

divisions <- all_court_staff %>%
  select(Division) %>% 
  unique()

all_court_staff %>% 
  filter(Division == "Advocacy") %>%
  mutate(position_end_date = end_date)

all_court_staff %>%
  filter(Division == "Detention Alternatives")

all_court_staff$position_end_date[all_court_staff$Division == "Detention Alternatives"] <- as.Date(end_date)
all_court_staff$position_end_date[all_court_staff$Division == "Advocacy"] <- as.Date(end_date)

 all_court_staff %>%
  filter()

#turn this into a function
class(div_List[[6]]$First_Name)

