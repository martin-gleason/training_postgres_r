

list_to_env <- function(xlsx, ...){
  sheet_names <- excel_sheets(xlsx)
  months <- lapply(sheet_names, function(X) read_excel(xlsx, sheet = X))
  lapply(months, as.data.frame)
  names(months) <- sheet_names
  list2env(months, .GlobalEnv)
}