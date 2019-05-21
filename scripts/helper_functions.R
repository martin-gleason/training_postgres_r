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

# find_replace <- function(column, pattern, replacement, ...){
#   require(tidyverse)
#   column %>%
#     str_replace_all(pattern, replacement)
#   }

#seperate_calendars <- function(xlsx, ...){
#   sheet_names <- excel_sheets(xlsx)
#   months <- lapply(sheet_names, function(X) read_excel(xlsx, sheet = X))
#   lapply(months, as.data.frame)
#   names(months) <- sheet_names
#   list2env(months, .GlobalEnv)
# }
# 
# end_date_division <- function(l, tibble_instance, col, 
#                               changes = lubridate::today()){
# 
#   l[[tibble_instance]]$enquo(col) <- changes
#  return(l[tibble_instance])
# }
# 
# # tibble_instance <- all_court_staff %>%
# #   select(Division)
# 
# 
# div_List %>% end_date_division()

# View(div_List)seperate_calendars <- function(xlsx, ...){
#   sheet_names <- excel_sheets(xlsx)
#   months <- lapply(sheet_names, function(X) read_excel(xlsx, sheet = X))
#   lapply(months, as.data.frame)
#   names(months) <- sheet_names
#   list2env(months, .GlobalEnv)
# }
