#tidying Divisions and Unit
source(file.path("scripts/find_replace.R"))
all_court_staff <- read_rds("tidy_inputs/all_court_staff.RDS")

### Division Work ####
Divisions_df <- all_court_staff %>%
  group_by(Division) %>%
  summarize(count = n())

staff_occurance <- all_court_staff %>%
  group_by(First_Name, Last_Name) %>%
  summarize(occurance = n()) %>%
  arrange(desc(occurance))

staff_of_one <- staff_occurance %>%
  filter(occurance == 1) %>%
  select(First_Name, Last_Name)

duplicates <- staff_occurance %>%
  filter(occurance >= 2) %>%
  arrange(Last_Name) 

duplicates_table <- duplicates %>%
  mutate(ID = as.integer(NA),
         Section = as.integer(NA),
         Unit = as.character(NA), 
         PO_Title = as.character(NA), 
         position_start_date = as.Date(NA),
         position_start_date = as.Date(NA)) %>%
  select(-occurance)

all_court_staff <- all_court_staff %>% anti_join(duplicates)

x <- c(49:60)

#data[duplicates_table$locate_column == !!pattern, ]$replace_column <- !!replacement
#duplicates_table[duplicates_table$Last_Name == "Bufano", ]$ID <- 49

duplicates_id <- duplicates_table %>%
  select(ID, First_Name, Last_Name)

all_court_ID <- all_court_staff %>% 
  select(ID, First_Name, Last_Name) %>%
  full_join(duplicates_id) %>%
  arrange(Last_Name) %>%
  mutate(ID = 1:nrow(.)) %>%
  pull(ID)
 
duplicates_table

all_court_staff <- all_court_staff %>%
  bind_rows(duplicates_table) %>% 
  arrange(Last_Name)

all_court_staff$ID <- all_court_ID

all_court_staff[all_court_staff$Last_Name == "Nunez", ]$PO_Title <- "V"

all_court_staff %>%
  filter(is.na(PO_Title))

PO_Title <- c("III", "V", "IV", "V", "III", "III", "III", "III", "VI", "III", "IV")

all_court_staff[is.na(all_court_staff$PO_Title), ]$PO_Title <- PO_Title 

all_court_staff <- all_court_staff[-c(12, 23, 35, 253), ]

all_court_staff<- all_court_staff %>%
  arrange(Last_Name)

all_court_staff$ID <- 1:nrow(all_court_staff)

all_court_staff$Division <- as.character(all_court_staff$Division)
all_court_staff$Section <- as.character(all_court_staff$Section)

af_fix <- all_court_staff %>%
  filter(Division == "Advocacy & Finance & Finance") %>%
  mutate(Division = "Advocacy & Finance")

all_court_staff %>% 
  filter(ID  %in% af_fix$ID)

all_court_staff[all_court_ID %in% af_fix$ID, ] <- af_fix

all_court_staff[all_court_staff$Last_Name == "Werner", ]$PO_Title <- "V"
all_court_staff[all_court_staff$Last_Name == "Neal", ]$PO_Title <- "V"
all_court_staff[all_court_staff$PO_Title == "V", ]$Division <- NA

ocs_fix <- all_court_staff %>%
  filter(Division == "Career & IT Services") %>%
  mutate(Division = "Office of Career Services") 

all_court_staff[all_court_staff$ID %in% ocs_fix$ID, ] <- ocs_fix

all_court_staff <- all_court_staff[-257, ]



sections <- c("Specialized Probation Services", "Chicago Field Probation Services", "Personnel", 
              "Suburban Probation Services", "Court Services")

all_court_staff[all_court_staff$PO_Title == "V", ]$Section <- sections

all_court_staff[all_court_staff$ID == 306, ]$PO_Title <- "Supervisor"
all_court_staff[all_court_staff$ID == 306, ]$Division <- "Grants & Technology"


all_court_staff <- all_court_staff %>%
  by_id(locate_column = "ID", pattern = 139,
        replace_column = "Division", replacement = "Operational Support Services")

View(all_court_staff %>%
  filter(Division == "Chicago Court & Diversion Services"))

jb <- c(29, 102, 281, 284)


length(jb)

all_court_staff <- all_court_staff %>%
  by_id(locate_column = "Division", pattern = "Chicago Court & Diversion Services",
        replacement = "Adjudication", replace_column = "Unit")

all_court_staff <- all_court_staff %>%
  by_id(locate_column = "ID", pattern = jb,
        replace_column = "Unit", replacement = "Intake Screening Unit")

all_court_staff <- all_court_staff %>% by_id(locate_column = "ID", 
                          pattern = 38, 
                          replace_column = "Unit", replacement = "Payroll")

all_court_staff %>%
  filter(Last_Name %in% c("Chapman", "Ivy", "Hill", "Kendricks", "Perez", "Starks",
                          "Stutley", "Whalen"))

erc <- c(67, 135, 148, 177, 263, 328, 361)

all_court_staff <- all_court_staff %>%
  by_id(locate_column = "ID", pattern = erc,
        replace_column = "Unit", replacement = "Evening Reporting Centers")

fm <- c("Lugo", "Stanton")

all_court_staff <- all_court_staff %>%
  by_id(locate_column = "Last_Name", pattern = fm,
        replace_column = "Unit", replacement = "Financial Management")

ocs_staff <- c("Gleason", "Pacelt", "Pacelt", "Barrera")

all_court_staff <- all_court_staff %>%
  by_id(locate_column = "Last_Name", pattern = ocs_staff, 
        replace_column = "Unit", replacement = "Training")

all_court_staff <- all_court_staff %>%
  by_id(locate_column = "ID", pattern = 196,
        replace_column = "Section", "1")

ossd_G_C <- c("Blaszkiewicz", "Walker", "Bovino", "Gresham")

all_court_staff <- all_court_staff %>%
  by_id(locate_column = "ID", pattern = 205,
        replace_column = "Unit", "Office Services - Ground & Concourse Level")

all_court_staff <- all_court_staff %>%
  by_id(locate_column = "Last_Name", pattern = ossd_G_C, 
        replace_column = "Unit", replacement = "Office Services - Ground & Concourse Level")

rl <- c("Coker", "Vale")

all_court_staff <- all_court_staff %>%
  by_id(locate_column = "Last_Name", pattern = rl, 
        replace_column = "Unit", replacement = "Record Library")

all_court_staff <- all_court_staff %>%
  by_id(locate_column = "ID", pattern = 139,
        replace_column = "Unit", "Stenographic Department")

s_d <- c("Ayala", "Balezentis", "Bizzle", 
         "Gallardo", "Holmes", "LaPlaca", "Serino")

all_court_staff <- all_court_staff %>%
  by_id(locate_column = "ID", pattern = rs, 
        replace_column = "Unit", "Stenographic Department")

all_court_staff <- all_court_staff %>%
  by_id(locate_column = "Last_Name", pattern = s_d,
        replace_column = "Unit", "Stenographic Department")


rs <- c(162, 163)



rp <- "Petchenik"

all_court_staff <- all_court_staff %>%
  by_id(locate_colum = "Last_Name", pattern = rp,
        replace_column = "Unit", replacement = str_to_title("INTER-AGENCY COORDINATION"))


all_court_staff <- all_court_staff %>%
  by_id("ID", pattern = 117, "Unit", str_to_title("RECORDS & INFORMATION PROCESSOR"))

all_court_staff <- all_court_staff %>%
  by_id("ID", pattern = 117, "PO_Title", "IV")

no_units <- all_court_staff %>%
  filter(is.na(Unit))

all_court_staff <- all_court_staff %>%
  by_id("ID", 341, "Division", "Detention Diversion")

all_court_staff <- all_court_staff %>%
  by_id("Division", "Detention Diversion", "Section", "Court Services")

all_court_staff <- all_court_staff %>%
  by_id("ID", 363, "Unit", "Home Confinement/Electronic Monitoring PM")

View(all_court_staff %>% 
  filter(Division  == "Detention Diversion"))

all_court_staff[all_court_staff$Last_Name == "Calderon", ]$Last_Name <- "Dunski"

all_court_staff %>% filter(Last_Name == "Fiorentino")

hcempm <- c(8, 24, 69, 79, 57, 109, 155, 159, 316)
hcemam <- c(33, 96, 208, 211, 228, 245, 271, 335)
em247 <- c(93, 160, 214, 233, 320, 287, 262, 98)
rur <- c(218, 90, 310, 314)
dsu <- c(200, 52, 181, 185, 215, 275, 356, 368)

all_court_staff <- all_court_staff %>%
  by_id("ID", hcempm, "Unit", "Home Confinement/Electronic Monitoring PM")

all_court_staff <- all_court_staff %>%
  by_id("ID", hcemam, "Unit", "Home Confinement/Electronic Monitoring AM")

all_court_staff <- all_court_staff %>%
  by_id("ID", em247, "Unit", "Electronic Monitoring 24/7")

all_court_staff <- all_court_staff %>%
  by_id("ID", rur, "Unit", "RUR")

all_court_staff <- all_court_staff %>%
  by_id("ID", dsu, "Unit", "Detention Screening")

all_court_staff$Division <- as.factor(all_court_staff$Division)
divisions <- split(all_court_staff, all_court_staff$Division) # useful as a way to figure out what I swrong
