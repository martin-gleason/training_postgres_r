library(tidyverse)


# just use split
staff_by_division <- function(df, df_name, column,...){
  column <- enquo(column)
  df_name <- df_name %>% unique()
  for(i in df_name){
    name <- df %>%
       filter(!!column == df_name[i])
    names(name) <- paste0(df_name[i], "_df")
    return(name)
  }
}

all_court_staff %>% staff_by_division(df_name = divisions_v, Division)

name <- divisions %>% map(paste0, "_df")
name <- divisions_v %>% 
  map(paste0, "_df") %>%
  unlist()

div <- all_court_staff %>%
  select(Division)

name(paste0("df", "fd", sep = " = ")) <- 12
