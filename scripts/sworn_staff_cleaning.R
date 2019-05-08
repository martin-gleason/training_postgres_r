#sworn staff cleaning

library(tidyverse)
library(lubridate)

credited_training_2018 <- readRDS("inputs/2018_training.RDS")

date_origin <- as_date("1899-12-30")

before <- '^[^_]+'
after <- "(?<=:)[^\\]]+"

sworn_staff <- credited_training_2018[[1]]

#seperate trainings from sworn staff
trainings_2018 <- sworn_staff %>%
  select(title_of_training = `TITLE OF TRAINING`,
         date = DATE,
         hours = HOURS) %>%
  filter(!is.na(date)) %>%
  unique() %>% 
  arrange(desc(date))

trainings_2018$date <- as.Date(as.numeric(trainings_2018$date), 
                               origin = date_origin)

trainings_2018$date <- as.POSIXct(trainings_2018$date)



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

Supervisor_DCPO <- employee_table %>%
  select(Supervisor_First_Name, Supervisor_Last_Name, Division)

PO_Supervisor <- employee_table %>%
  anti_join(Supervisor_DCPO, by = c("First_Name" = "Supervisor_First_Name", 
                             "Last_Name" = "Supervisor_Last_Name")) %>%
  select(Division, First_Name, Last_Name, Supervisor_First_Name, Supervisor_Last_Name)

PO <- PO_Supervisor %>%
  select(First_Name, Last_Name, Division) %>%
  mutate(PO_Title = "II")

PO %>% group_by(Division) %>%
  summarise(per_division = n()) %>%
  arrange(desc(per_division))

francisco <- PO %>%
  filter(Last_Name == "Arenas") %>%
  select(First_Name, Last_Name, Division, PO_Title) %>%
  mutate(PO_Title = "III")

Supervisor_most <- PO_Supervisor %>% 
  select(Supervisor_First_Name, Supervisor_Last_Name, Division) %>%
  unique() %>% 
  mutate(PO_Title = "III") %>%
  arrange(Supervisor_Last_Name)

Supervisor <- Supervisor_most %>%
  select(First_Name = Supervisor_First_Name, 
         Last_Name = Supervisor_Last_Name, Division, PO_Title) %>%
  bind_rows(francisco)

avik <- Supervisor %>%
  filter(Last_Name == "Das") %>%
  mutate(PO_Title = "VII", Division = "Acting Director")

PO <- PO %>%
  filter(First_Name != "Francisco" & Last_Name != "Arenas") %>%
  filter(First_Name != "Avik" & Last_Name != "Das")

donna <- PO %>%
  filter(Last_Name == "Neal") %>%
  mutate(PO_Title = "V")

PO <- PO %>% filter(Last_Name != "Neal")

Supervisor %>% filter(Last_Name == "Arenas")
Supervisor %>% filter(Last_Name == "Patterson")

kevin <- Supervisor %>%
  filter(Last_Name == "Hickey") %>%
  mutate(Supervisor_First_Name = First_Name,
         Supervisor_Last_Name = Last_Name,
         PO_Title = "IV")

Supervisor <- Supervisor %>%
  filter(Last_Name != "Hickey")

Supervisor <- Supervisor %>%
  filter(Last_Name != "Das")

Supervisor <- Supervisor %>%
  filter(Last_Name != "Patterson")

Supervisor %>%
  filter(Last_Name == "Hickey")

DCPO <- Supervisor_DCPO %>%
  anti_join(Supervisor, 
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
  full_join(Supervisor) %>%
  full_join(DCPO) %>%
  arrange(Last_Name)

#employee + division + title
Full_Employee_Table <- Full_Employee_Table %>%
  bind_rows(patterson) %>% 
  bind_rows(avik) %>%
  bind_rows(donna) %>%
  unique()

#employee + title
Employee_Title <- Full_Employee_Table %>%
  select(First_Name, Last_Name, PO_Title) %>%
  unique()

#QA on full employee table.
Full_Employee_Table %>%
  filter(First_Name == "Avik" & Last_Name == "Das") 

easy_dupes <- Full_Employee_Table %>%
  group_by(First_Name, Last_Name) %>%
  count() %>%
  filter(n >= 2) %>%
  select(First_Name, Last_Name)

fix_divisions <- (Full_Employee_Table %>%
  semi_join(easy_dupes))

