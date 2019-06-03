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


find_else_replace <- function(data, column, pattern, replacement,...){
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

add_section <- function(data, locate_column = ID, pattern, replace_column, replacement, ...){
  locate_column <- ensym(locate_column)
  pattern <- enquo(pattern)
  replacement <- enquo(replacement)
  replace_column <- ensym(replace_column)
  
  #data %>% if_else(str_detect(!!locate_column), mutate(!!replace_column := !!replacement))
  df <- data %>%
    filter(!!locate_column == !!pattern)
  
  
  data <- data %>% 
    anti_join(df)
  
  df <- df %>%
    mutate(!!replace_column := !!replacement)
  
  
  data <- data %>% 
    bind_rows(df)
  
  return(data)
}

finish_table <- function(data, locate_column, pattern, 
                         replace_ID, replacement_ID, 
                         replace_Section, replacement_Section,
                         replace_Title, Replacement_Title,
                         replace_start_date, replacement_start_date,
                         replace_end_date, replacement_end_date,...){
  
  locate_column <- ensym(locate_column)
  pattern <- enquo(pattern)

  replace_ID <- ensym(replace_ID)
  replacement_ID <- enquo(replacemen_D)
  
  replace_Section <- ensym(replace_Section)
  replacement_Section <- enquo(replacement_Section)

  replace_Title<- ensym(replace_Title)
  replacement_Title <- enquo(replacement_Title)
  
  replace_start_date <- ensym(replace_start_date)
  replacement_start_date<- enquo(replacement_start_date)

  replace_end_date <- ensym(replace_end_date)
  replacement_end_date<- enquo(replacement_end_date)
  
  #data %>% if_else(str_detect(!!locate_column), mutate(!!replace_column := !!replacement))
  df <- data %>%
    filter(!!locate_column == !!pattern)
  
  
  data <- data %>% 
    anti_join(df)
  
  df <- df %>%
    mutate(!!replace_column := !!replacement)
  
  
  data <- data %>% 
    bind_rows(df)
  
  return(data)
}


by_id <- function(data, locate_column, pattern, replace_column, replacement, ...){
  locate_column <- ensym(locate_column)
  pattern <- enquo(pattern)
  replacement <- enquo(replacement)
  replace_column <- ensym(replace_column)
  
  if(length(pattern) > 1){
    df <- data %>%
      filter(!!locate_column %in% !!pattern)
  }else{
    df <- data %>%
      filter(!!locate_column == !!pattern)
  }
  
  data <- data %>% 
    anti_join(df)
  
  df <- df %>%
    mutate(!!replace_column := !!replacement)
  
  data <- data %>% 
    bind_rows(df)
  
  return(data)
  
}

remove_row <- function(data, remove...){
  data <- data[-remove, ]
}
