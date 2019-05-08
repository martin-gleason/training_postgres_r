#Helper script
#For functions used all the damn time.
require(tidyverse)

find_employee <- function(name, table = employee_table, lastname = Last_Name, ...){
  table %>% 
    filter(lastname == name)
}

