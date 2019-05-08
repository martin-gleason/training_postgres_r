#Analysis

library(tidyverse)
library(readr)

clean_training_2018 <- read_rds("clean_training.RDS")

employee_table <- clean_training_2018[[1]]

offered_training_2018 <- clean_training_2018[[2]]

officer_trainings <- clean_training_2018[[3]]

all_trainings_2018 <- clean_training_2018[[4]]

support_professionals <- clean_training_2018[[5]]

support_professionals %>%
  filter(!is.na(Title_of_Training)) %>%
  select(First_Name, Last_Name, Title_of_Training, Hours) %>%
  mutate(total_hours = sum(Hours))

#Equity examination
division_equity <- employee_table %>%
  group_by(Last_Name, Division, PO_T) %>% 
  summarise(count = n()) %>%
  arrange(desc(count))
# 
division_equity %>%
  ggplot(aes(x = reorder(Division, count), fill = Division)) +
  geom_histogram(stat = "count") +
  coord_flip()

officer_trainings_2018 <- officer_trainings %>%
  group_by(First_Name, Last_Name) %>%
  mutate(total_hours = sum(hours)) %>%
  ungroup() %>%
  select(First_Name, Last_Name, total_hours) %>%
  group_by(First_Name, Last_Name, total_hours) %>%
  summarize(Number_of_Trainings = n())

officer_trainings_2018 %>% 
  filter(total_hours < 20) %>%
  group_by(total_hours) %>%
  summarize(probation_officers = n()) %>%
  arrange(desc(probation_officers )) %>%
  ggplot(aes(x = total_hours, y = probation_officers, fill = total_hours)) + 
  geom_bar(stat = "identity") + scale_fill_gradient(low = "red", high = "blue") +
  labs(title = "Probation Officers With Less Than 20 Hours", 
       subtitle = today(), 
       y = "Tally of PO by Hours Earned",
       x = "Total Hours Earned", 
       fill = "Total Hours")

training_titles_2018 <- trainings_2018 %>% 
  group_by(title_of_training) %>%
  summarise(., occurance = n()) %>%
  ungroup() %>%
  arrange(desc(occurance))

ggsave("pos_without_20.jpg")


#QA
officer_trainings_2018 %>%
  filter(Last_Name == "Williams")

Staff_by_division <- employee_table %>%
  group_by(Division, PO_Title) %>%
  summarize(n = n()) %>%
  arrange(Division, desc(n))


