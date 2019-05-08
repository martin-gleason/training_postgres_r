#Helper script, for functions used all the damn time.

require(tidyverse)

find_employee <- function(name, table = employee_table, lastname = Last_Name, ...){
  table %>% 
    filter(lastname == name)
}

#takes file location! 
load_rds <- function(rds, ...){

  lists <- list()
  lists <- readRDS(rds)
  list2env(lists, .GlobalEnv)
  rm(lists)
  
}

