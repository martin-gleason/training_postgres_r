#import of spreadsheets

library(tidyverse)
library(readxl)

#Find file
sworn_staff_file <- file.choose()
support_professional_file <- file.choose()

#Get names programmatically
sheets <- excel_sheets(sworn_staff_file)
support_sheets <- excel_sheets(support_professional_file)

#Create Tibbles
sworn_staff <- read_excel(sworn_staff_file, 
                          sheet = sheets[2], col_names = TRUE)
support_professionals <- read_excel(support_professional_file,
                                    sheet = support_sheets[1], col_names = TRUE)

sworn_training_info <- read_excel(sworn_staff_file, 
                                  sheet = sheets[5], col_names = TRUE)

training_import<- list(sworn_staff, support_professionals, sworn_training_info)

#save to RDS
#next version would be to paste a name together.
write_rds(training_import, file.path("inputs/2019_training.RDS"))
