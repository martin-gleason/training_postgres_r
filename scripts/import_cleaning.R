#import and inital cleaning of spreadsheets

library(tidyverse)
library(readxl)


origin <- "1899-12-30"

before <- '^[^_]+'
after <- "(?<=:)[^\\]]+"

sworn_staff_file <- file.choose()
support_professional_file <- file.choose()

sworn_staff <- read_excel(sworn_staff_file, sheet = "2018", col_names = TRUE)
support_professionals <- read_excel(support_professional_file, sheet = "2018", col_names = TRUE)
training_providers <- read_excel(sworn_staff_file, sheet = "PRESENTER INFO.", col_names = TRUE)

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
  select(DIVISION, SPO_First_Name, SPO_Last_Name, First_Name, Last_Name)

division_equity <- employee_table %>%
  group_by(DIVISION, SPO_Last_Name, SPO_First_Name) %>% 
  summarise(count = n()) %>%
  arrange(desc(count))

division_equity %>% 
  ggplot(aes(x = reorder(SPO_Last_Name, count), y = count, fill = DIVISION)) + 
  geom_bar(stat = "identity") + 
  coord_flip()
  
  DCPO <- employee_table %>%
    unique() %>%
    group_by(First_Name, Last_Name, DIVISION) %>%
    summarise(n = n()) %>% 
    arrange(desc(n)) 

class(employee_table$First_Name)
class(employee_table$Last_Name)
