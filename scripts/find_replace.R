#Find_replace
require(tidyverse)


find <- function(data, column_to_find, pattern,...){
   column_to_find <- dplyr::enquo(column_to_find)
   pattern <- dplyr::enquo(pattern)
  
    temp <- data %>% 
      dplyr::filter(!! column_to_find %in% !! pattern)
     return(temp)
}

find_replace <- function(data, column, pattern, replacement,...){
  column <- ensym(column)
  pattern <- enquo(pattern)
  replacement <- enquo(replacement)

  if (is_grouped_df(data)){
    data <- data %>% ungroup()
  }
  data <- data %>%
    mutate( !!column := str_replace(!!column, !!pattern, !!replacement))
  return(data)
}
