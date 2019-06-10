library(tidyverse)
library(gitsum)
library(lubridate)

time_log <- parse_log_detailed() %>%
  select(short_hash, author_name, date, message, 
         total_insertions, total_deletions) %>%
  arrange(desc(date))

time_log <- time_log %>% 
  distinct(short_hash, .keep_all = TRUE)

time_log <- time_log %>%
  mutate(start_work = TRUE)


s_time <- time_log[nrow(time_log)-1, 2] 

stop_time <- time_log[nrow(time_log), 2]

time_spent <- stop_time - s_time

as.character(time_spent$date)