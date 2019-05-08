#import and inital cleaning of spreadsheets
library(lubridate)
library(tidyverse)

#I need to tie in file.choose to the eventual rds name
rds_to_clean <- file.choose()

credited_trainings <- readRDS(rds_to_clean)

date_origin <- as_date("1899-12-30")

before <- '^[^_]+'
after <- "(?<=:)[^\\]]+"

sworn_staff <- credited_trainings[[1]]
support_professionals <- credited_trainings[[2]]
presenter <- credited_trainings[[3]]

training_dimensions <- dim(sworn_staff) + 
  dim(support_professionals) + 
  dim(presenter)

#Trainings for sworn staff
trainings_offered <- sworn_staff %>%
  select(title_of_training = `TITLE OF TRAINING`,
         date = DATE,
         hours = HOURS) %>%
  filter(!is.na(date)) %>%
  unique()


trainings_offered$date <- as.Date(as.numeric(trainings_offered$date), date_origin)
trainings_offered$date <- as.POSIXct(trainings_offered$date)

support_professionals_training <- support_professionals %>%
  select(title_of_training = `TITLE OF TRAINING`, 
         date = DATE,
         hours = HOURS) %>%
  filter(!is.na(date)) %>%
  unique()



#Create tidy trainings table
all_trainings <- trainings_offered %>%
  full_join(support_professionals_training) %>%
  rename(date_started = date) %>%
  arrange(date_started)


#test for missing date
missing_date <- all_trainings %>%
  filter(is.na(date_started)) %>%
  select(title_of_training) %>%
  pull()

all_trainings %>%
  mutate(date_ending = date_started + hours)

#find training with missing dates


training_occurances <- trainings_offered %>% 
  group_by(title_of_training) %>% 
  summarize(n = n()) %>% 
  arrange(desc(n))
  
#employee cleaning
employee_table <- sworn_staff %>%
  select(1:3) %>% 
  unique() %>% 
  mutate("Last_Name" = str_extract(`EMPLOYEE NAME`, pattern = "^[^,]+") %>% 
           str_trim("both")) %>% 
  mutate("First_Name" = str_extract(`EMPLOYEE NAME`, pattern ="(?<=,)[^\\]]+") %>% 
           str_trim("both")) %>%
  mutate("Supervisor_First_Name" = str_extract(`SUPERVISOR`, pattern ="(?<=,)[^\\]]+") %>% 
           str_trim("both")) %>%
  mutate("Supervisor_Last_Name" = str_extract(`SUPERVISOR`, pattern = "^[^,]+") %>% 
           str_trim("both")) %>%
  select(Division = DIVISION, Supervisor_First_Name, Supervisor_Last_Name, First_Name, Last_Name)

no_first <- employee_table %>% 
  filter(is.na(First_Name))

no_first <- no_first %>%
  mutate(fixed_Last_Name = str_extract(Last_Name, "^[^\\s]+")) %>%
  mutate(fixed_First_Name = str_extract(Last_Name, "(?<=\\s)[^\\]]+")) %>%
  mutate(First_Name = str_trim(fixed_First_Name)) %>%
  select(Division, Supervisor_First_Name, Supervisor_Last_Name, 
         First_Name, Last_Name = fixed_Last_Name)

patterson <- employee_table %>% 
  filter(Last_Name == "Patterson") %>%
  mutate(PO_Title = Division) %>%
  select(First_Name, Last_Name, Division, PO_Title)

employee_table <- employee_table %>%
  bind_rows(no_first)

SPO_DCPO <- employee_table %>%
    select(Supervisor_First_Name, Supervisor_Last_Name, Division)

PO_SPO <- employee_table %>%
  anti_join(SPO_DCPO, by = c("First_Name" = "Supervisor_First_Name", 
                             "Last_Name" = "Supervisor_Last_Name")) %>%
  select(Division, First_Name, Last_Name, Supervisor_First_Name, Supervisor_Last_Name)

PO <- PO_SPO %>%
  select(First_Name, Last_Name, Division) %>%
  mutate(PO_Title = "II")

PO %>% group_by(Division) %>%
  summarise(per_division = n()) %>%
  arrange(desc(per_division))

francisco <- PO %>%
  filter(Last_Name == "Arenas") %>%
  select(First_Name, Last_Name, Division, PO_Title) %>%
  mutate(PO_Title = "III")

SPO_most <- PO_SPO %>% 
  select(Supervisor_First_Name, Supervisor_Last_Name, Division) %>%
  unique() %>% 
  mutate(PO_Title = "III") %>%
  arrange(Supervisor_Last_Name)

SPO <- SPO_most %>%
  select(First_Name = Supervisor_First_Name, 
         Last_Name = Supervisor_Last_Name, Division, PO_Title) %>%
  bind_rows(francisco)

avik <- SPO %>%
  filter(Last_Name == "Das") %>%
  mutate(PO_Title = "VII", Division = "Director")

PO <- PO %>%
  filter(First_Name != "Francisco" & Last_Name != "Arenas") %>%
  filter(First_Name != "Avik" & Last_Name != "Das")

donna <- PO %>%
  filter(Last_Name == "Neal") %>%
  mutate(PO_Title = "V")

PO <- PO %>% filter(Last_Name != "Neal")

#QA on previous errors in SPO Table
SPO %>% filter(Last_Name == "Arenas")
SPO %>% filter(Last_Name == "Patterson")

kevin <- SPO %>%
  filter(Last_Name == "Hickey") %>%
  mutate(SPO_First_Name = First_Name,
         SPO_Last_Name = Last_Name,
         PO_Title = "IV")

SPO <- SPO %>%
  filter(Last_Name != "Hickey")

SPO <- SPO %>%
  filter(Last_Name != "Das")

SPO <- SPO %>%
  filter(Last_Name != "Patterson")

SPO %>%
  filter(Last_Name == "Hickey")

DCPO <- SPO_DCPO %>%
  anti_join(SPO, 
            by = c("Supervisor_First_Name" = "First_Name", 
                   "Supervisor_Last_Name" = "Last_Name")) %>%
  bind_rows(kevin) %>%
  select(First_Name = Supervisor_First_Name,
         Last_Name = Supervisor_Last_Name,
         PO_Title,
         Division) %>%
  mutate(PO_Title = "IV") %>%
  arrange(Last_Name) %>%
  unique()

DCPO <- DCPO %>%
  filter(Last_Name != "Patterson") %>%
  filter(Last_Name != "Das")

#QA on Kevin
DCPO  %>% filter(Last_Name == "Hickey")

Division <- employee_table %>% 
  select(Division) %>%
  unique()

Full_Employee_Table <- PO %>%
  full_join(SPO) %>%
  full_join(DCPO) %>%
  arrange(Last_Name)

Full_Employee_Table <- Full_Employee_Table %>%
  bind_rows(patterson) %>% 
  bind_rows(avik) %>%
  bind_rows(donna) %>%
  unique()

Full_Employee_Table %>%
  filter(First_Name == "Avik" & Last_Name == "Das")

appearances_in_table <- Full_Employee_Table %>%
  group_by(First_Name, Last_Name) %>%
  summarise(appearances = n ()) %>%
  filter(appearances >= 2) %>%
  arrange(desc(appearances))

#QA on SPO/DCPO with limited supervision duties of field officers
Full_Employee_Table %>% filter(Last_Name == "Spooner")
Full_Employee_Table %>% filter(Last_Name == "Arenas")
Full_Employee_Table %>% filter(Last_Name == "Neal")
Full_Employee_Table %>% filter(Last_Name == "Patterson")

#individual office training
officer_training <- sworn_staff %>%
  mutate("Last_Name" = str_extract(`EMPLOYEE NAME`, pattern = "^[^,]+") %>% 
                    str_trim("both")) %>% 
  mutate("First_Name" = str_extract(`EMPLOYEE NAME`, pattern ="(?<=,)[^\\]]+") %>% 
           str_trim("both")) %>% 
  select(First_Name, Last_Name, 
         title_of_training = `TITLE OF TRAINING`, 
         date = DATE, hours = HOURS)


officer_training$date <- date <- as.Date(as.numeric(officer_training$date), origin = "1899-12-30")

officer_training <- officer_training %>% 
  filter(!is.na(date))

#Support Professionals

cleaned_support_professionals <- support_professionals %>% 
  mutate("Last_Name" = str_extract(`EMPLOYEE NAME`, pattern = "^[^,]+") %>% 
           str_trim("both")) %>% 
  mutate("First_Name" = str_extract(`EMPLOYEE NAME`, pattern ="(?<=,)[^\\]]+") %>% 
           str_trim("both")) %>%
  mutate("Supervisor_First_Name" = str_extract(`SUPERVISOR`, pattern ="(?<=,)[^\\]]+") %>% 
           str_trim("both")) %>%
  mutate("Supervisor_Last_Name" = str_extract(`SUPERVISOR`, pattern = "^[^,]+") %>% 
           str_trim("both")) %>%
  select(Division = DIVISION, Supervisor_First_Name, Supervisor_Last_Name, 
         First_Name, Last_Name, Title_of_Training = `TITLE OF TRAINING`, 
         Date = DATE, Hours = HOURS)

#example of dplyr filling the rolling of indexing
#might just be easier to index
spo_first_name_support_Mark <- cleaned_support_professionals %>%
  filter(Division == "North Suburban") %>%
  mutate(Supervisor_First_Name = replace(Supervisor_First_Name,
                                  is.na(Supervisor_First_Name), 
                                  "Mark"),
         Supervisor_Last_Name = "Dean-Myrda") %>%
  as_tibble()

cleaned_support_professionals[cleaned_support_professionals$Division == "North Suburban", ] <- spo_first_name_support_Mark

#replacing weird values
missing_spo_Wanda <- cleaned_support_professionals %>%
  filter(Last_Name == "Hackett") %>% 
  mutate(Supervisor_First_Name = "Jose/Dwayne")

cleaned_support_professionals[cleaned_support_professionals$Last_Name == "Hackett", ] <- missing_spo_Wanda

missing_spo_Tamara <- cleaned_support_professionals %>%
  filter(Last_Name == "Tucker") %>% 
  mutate(Supervisor_First_Name = "Ore/Karen")

cleaned_support_professionals[cleaned_support_professionals$Last_Name == "Tucker", ] <- missing_spo_Tamara
  

missing_spo_Ramona <- cleaned_support_professionals %>%
  filter(Last_Name == "Turner") %>% 
  mutate(Supervisor_First_Name = "William/Sharon")

cleaned_support_professionals[cleaned_support_professionals$Last_Name == "Turner", ] <- missing_spo_Ramona

cleaned_support_professionals_title <- cleaned_support_professionals %>%
  select(First_Name, Last_Name, Division) %>%
  mutate(PO_Title = "Support Professional")

Full_Employee_Table <- Full_Employee_Table %>%
  bind_rows(cleaned_support_professionals_title)

?#Final QA
View(Full_Employee_Table)

all_trainings %>% 
  filter(title_of_training == "Markham Walking")


easy_dupes <- Full_Employee_Table %>%
  group_by(First_Name, Last_Name) %>%
  summarise(Count = n()) %>% 
  arrange(desc(Count)) %>%
  filter(Count >= 2)

#Save objects
clean_training <- list(full_employee_table = Full_Employee_Table, 
                       trainings_offered = trainings_offered, 
                       officer_training = officer_training,
                       all_trainings = all_trainings, 
                       cleaned_support_professionals = cleaned_support_professionals)


write_rds(clean_training, file.path("tidy_inputs/clean_training_2019.RDS"))
