#QA Scripts: Additonal passes

library(lubridate)
library(tidyverse)
source("scripts/helper_functions.R")
source("scripts/find_replace.R")
all_court_staff <- read_rds(file.path("tidy_inputs/clean_all_court_staff.RDS"))

#table breakdown
staff_id_tbl <- all_court_staff %>%
  select(ID, First_Name, Last_Name, 
         seniority_start = position_start_date, 
         seniority_end = position_end_date)

unit_id <- all_court_staff %>%
  select(Unit) %>%
  distinct(Unit) %>%
  mutate(unit_ID = 1:nrow(.))

division_id <- all_court_staff %>%
  select(Division) %>%
  distinct(Division) %>%
  mutate(division_ID = 1:nrow(.))

section_id <- all_court_staff %>%
  select(Section) %>%
  distinct(Section) %>%
  mutate(section_ID = 1:nrow(.))

#6/1compare

all_court_staff %>%
  inner_join(section_id) %>%
  inner_join(division_id) %>%
  inner_join(unit_id) %>%
  group_by(section_ID) %>%
  arrange(Last_Name) %>%
  View()

all_court_staff <- all_court_staff %>%
  by_id(ID, 134, Division, "Grants & Technology")

all_court_staff <- all_court_staff %>%
  by_id(ID, 7, Section, "Court Services") %>%
  by_id(ID, 7, Division, "Chicago Court & Diversion Services")

all_court_staff <- all_court_staff %>%
  by_id(Division, "Southwest Suburban", Division, "South Suburban")

all_court_staff <- all_court_staff %>%
  by_id(Division, "Markham", Division, "South Suburban")

all_court_staff <- all_court_staff %>%
  by_id(Unit, "Maywood 2", Division, "North & West Suburban")

all_court_staff <- all_court_staff %>%
  by_id(Unit, "Maywood 1", Division, "North & West Suburban")

all_court_staff <- all_court_staff %>%
  by_id(ID, 24, Last_Name, "Bailey-Holland")

all_court_staff <- all_court_staff %>%
  by_id(ID, 66, position_end_date, as.Date("06-01-2019"))

all_court_staff <- all_court_staff %>%
  by_id(ID, 70, Last_Name, "Chiappetta")

all_court_staff <- all_court_staff %>%
  by_id(Unit, "Rolling Meadows", Division, "North & West Suburban")

all_court_staff <- all_court_staff %>%
  by_id(Unit, "Skokie", Division, "North & West Suburban")

all_court_staff <- all_court_staff %>%
  by_id(Division, "North Suburban", Division, "North & West Suburban")

all_court_staff <- all_court_staff %>%
  by_id(ID, 78, Division, "South Suburban") %>%
  by_id(ID, 78, Section, "Suburban Probation Services")

all_court_staff <- all_court_staff %>%
  by_id(ID, 88, Last_Name, "Dominquez")

all_court_staff <- all_court_staff %>%
  by_id(ID, 89, position_end_date, as.Date("06-01-2019"))

all_court_staff <- all_court_staff %>%
  by_id(ID, 97, Last_Name, "Ceci")

all_court_staff <- all_court_staff %>%
  by_id(ID, 98, Section, "Court Services") %>%
  by_id(ID, 98, Division, "Detention Diversion")

all_court_staff <- all_court_staff %>%
  by_id(ID, 119, Section, "Court Services") %>%
  by_id(ID, 119, Division, "Operational Support Services")

all_court_staff <- all_court_staff %>%
  by_id(ID, 129, Last_Name, "Newton-Hart")


all_court_staff <- all_court_staff %>%
  by_id(ID, 146, Unit, "pd_1, pd_2, pd_12, pd_21")

all_court_staff <- all_court_staff %>%
  by_id(ID, 150, Division, "North & West Suburban")

all_court_staff <- all_court_staff %>%
  by_id(ID, 176, position_end_date, as.Date("05-01-2019"))

#final sec/div/unit pass

#repalce names with ids
court_staff_by_ID <- all_court_staff %>%
  inner_join(unit_id) %>%
  select(ID, unit_ID)

##position table
#staff id, unit_id, position_start_date, position_end_date
staff_positions <-  all_court_staff %>%
  inner_join(unit_id) %>%
  select(ID, unit_ID, position_start_date, position_end_date)

#sec-div-unit breakdown
#section_id, division_id, unit_id, assign_date, assigment_end_date
sec_div_unit <- all_court_staff %>%
  inner_join(section_id) %>%
  inner_join(division_id) %>%
  inner_join(unit_id) %>%
  select(section_ID, division_ID, unit_ID,
         assign_date = position_start_date,
         assignment_end_date = position_end_date) %>%
  mutate(assign_date = as.Date("2018-05-01"))


#Save
tables <- list(division_tbl = division_id, 
               section_tbl = section_id,
               sec_div_unit_tbl = sec_div_unit,
               positions_tbl = staff_positions,
               all = all_court_staff,
               unit_tbl = unit_id,
               staff_tbl = staff_id_tbl)

