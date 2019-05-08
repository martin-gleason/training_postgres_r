#import of spreadsheets

library(tidyverse)
library(readxl)

sworn_staff_file <- file.choose()
support_professional_file <- file.choose()

sworn_staff <- read_excel(sworn_staff_file, sheet = "2018", col_names = TRUE)
support_professionals <- read_excel(support_professional_file, sheet = "2018", col_names = TRUE)

sworn_training_info <- read_excel(sworn_staff_file, sheet = "PRESENTER INFO.", col_names = TRUE)

# training_2018<- list(sworn_staff, support_professionals, sworn_training_info)
# 
# write_rds(training_2018, "2018_training.RDS")
