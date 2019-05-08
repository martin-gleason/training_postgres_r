#import and inital cleaning of spreadsheets
library(lubridate)
library(tidyverse)


credited_training_2018 <- readRDS("2018_training.RDS")

date_origin <- as_date("1899-12-30")

before <- '^[^_]+'
after <- "(?<=:)[^\\]]+"

sworn_staff <- credited_training_2018[[1]]
support_professionals <- credited_training_2018[[2]]
presenter <- credited_training_2018[[3]]

training_dimensions <- dim(sworn_staff) + 
  dim(support_professionals) + 
  dim(presenter)

#Trainings for sworn staff
trainings_2018 <- sworn_staff %>%
  select(title_of_training = `TITLE OF TRAINING`,
         date = DATE,
         hours = HOURS) %>%
  filter(!is.na(date)) %>%
  unique()

trainings_2018$date <- as.Date(as.numeric(trainings_2018$date), 
                               origin = date_origin)

trainings_2018$date <- as.POSIXct(trainings_2018$date)

support_professionals_training <- support_professionals %>%
  select(title_of_training = `TITLE OF TRAINING`, 
         date = DATE,
         hours = HOURS) %>%
  filter(!is.na(date)) %>%
  unique()

all_trainings_2018 <- trainings_2018 %>%
  full_join(support_professionals_training) %>%
  arrange(date)


View(training_titles_2018)

missing_date <- trainings_2018 %>%
  filter(is.na(date)) %>%
  select(title_of_training) %>%
  pull()

#trainings_2018 %>%
#  filter(title_of_training == missing_date)
#find training with missing dates

total_training_hours <- sum(trainings_2018$hours, na.rm = TRUE)

trainings_2018 %>%
  filter(title_of_training == "The 5 B's: Child Abuse Part 2")

unique(trainings_2018$title_of_training)

training_2018_occurances <- trainings_2018 %>% 
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
  mutate("SPO_First_Name" = str_extract(`SUPERVISOR`, pattern ="(?<=,)[^\\]]+") %>% 
           str_trim("both")) %>%
  mutate("SPO_Last_Name" = str_extract(`SUPERVISOR`, pattern = "^[^,]+") %>% 
           str_trim("both")) %>%
  select(Division = DIVISION, SPO_First_Name, SPO_Last_Name, First_Name, Last_Name)

no_first <- employee_table %>% 
  filter(is.na(First_Name))

no_first <- no_first %>%
  mutate(fixed_Last_Name = str_extract(Last_Name, "^[^\\s]+")) %>%
  mutate(fixed_First_Name = str_extract(Last_Name, "(?<=\\s)[^\\]]+")) %>%
  mutate(First_Name = str_trim(fixed_First_Name)) %>%
  select(Division, SPO_First_Name, SPO_Last_Name, 
         First_Name, Last_Name = fixed_Last_Name)

patterson <- employee_table %>% 
  filter(Last_Name == "Patterson") %>%
  mutate(PO_Title = Division) %>%
  select(First_Name, Last_Name, Division, PO_Title)

employee_table <- employee_table %>%
  bind_rows(no_first)

SPO_DCPO <- employee_table %>%
    select(SPO_First_Name, SPO_Last_Name, Division)

PO_SPO <- employee_table %>%
  anti_join(SPO_DCPO, by = c("First_Name" = "SPO_First_Name", 
                             "Last_Name" = "SPO_Last_Name")) %>%
  select(Division, First_Name, Last_Name, SPO_First_Name, SPO_Last_Name)

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
  select(SPO_First_Name, SPO_Last_Name, Division) %>%
  unique() %>% 
  mutate(PO_Title = "III") %>%
  arrange(SPO_Last_Name)

SPO <- SPO_most %>%
  select(First_Name = SPO_First_Name, 
         Last_Name = SPO_Last_Name, Division, PO_Title) %>%
  bind_rows(francisco)

avik <- SPO %>%
  filter(Last_Name == "Das") %>%
  mutate(PO_Title = "VII", Division = "Acting Director")

PO <- PO %>%
  filter(First_Name != "Francisco" & Last_Name != "Arenas") %>%
  filter(First_Name != "Avik" & Last_Name != "Das")

donna <- PO %>%
  filter(Last_Name == "Neal") %>%
  mutate(PO_Title = "V")

PO <- PO %>% filter(Last_Name != "Neal")

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
            by = c("SPO_First_Name" = "First_Name", 
                   "SPO_Last_Name" = "Last_Name")) %>%
  bind_rows(kevin) %>%
  select(First_Name = SPO_First_Name,
         Last_Name = SPO_Last_Name,
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
  mutate("SPO_First_Name" = str_extract(`SUPERVISOR`, pattern ="(?<=,)[^\\]]+") %>% 
           str_trim("both")) %>%
  mutate("SPO_Last_Name" = str_extract(`SUPERVISOR`, pattern = "^[^,]+") %>% 
           str_trim("both")) %>%
  select(Division = DIVISION, SPO_First_Name, SPO_Last_Name, 
         First_Name, Last_Name, Title_of_Training = `TITLE OF TRAINING`, 
         Date = DATE, Hours = HOURS)

#example of dplyr fillingthe rolling of indexing
#might just be easier to index
spo_first_name_support_Mark <- cleaned_support_professionals %>%
  filter(Division == "North Suburban") %>%
  mutate(SPO_First_Name = replace(SPO_First_Name,
                                  is.na(SPO_First_Name), 
                                  "Mark"),
         SPO_Last_Name = "Dean-Myrda") %>%
  as.tibble()

cleaned_support_professionals[cleaned_support_professionals$Division == "North Suburban", ] <- spo_first_name_support_Mark

#actual replacing weird values
missing_spo_Wanda<- cleaned_support_professionals %>%
  filter(Last_Name == "Hackett") %>% 
  mutate(SPO_First_Name = "Jose/Dwayne")

cleaned_support_professionals[cleaned_support_professionals$Last_Name == "Hackett", ] <- missing_spo_Wanda

missing_spo_Tamara <- cleaned_support_professionals %>%
  filter(Last_Name == "Tucker") %>% 
  mutate(SPO_First_Name = "Ore/Karen")

cleaned_support_professionals[cleaned_support_professionals$Last_Name == "Tucker", ] <- missing_spo_Tamara
  

missing_spo_Ramona <- cleaned_support_professionals %>%
  filter(Last_Name == "Turner") %>% 
  mutate(SPO_First_Name = "William/Sharon")

cleaned_support_professionals[cleaned_support_professionals$Last_Name == "Turner", ] <- missing_spo_Ramona

cleaned_support_professionals_title <- cleaned_support_professionals %>%
  select(First_Name, Last_Name, Division) %>%
  mutate(PO_Title = "Support Professional")

Full_Employee_Table %>%
  bind_rows(cleaned_support_professionals_title)

?#Final QA
View(Full_Employee_Table %>%
       arrange(Last_Name))

all_trainings_2018 %>% 
  filter(title_of_training == "Markham Walking")


easy_dupes <- Full_Employee_Table %>%
  group_by(First_Name, Last_Name) %>%
  summarise(Count = n()) %>% 
  arrange(desc(Count)) %>%
  filter(Count >= 2)

#Save objects
clean_training <- list(Full_Employee_Table, trainings_2018, 
                       officer_training, all_trainings_2018, 
                       cleaned_support_professionals)


write_rds(clean_training, "clean_training.RDS")
