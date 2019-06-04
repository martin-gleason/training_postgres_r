#tidying Divisions and Unit
library(lubridate)

source(file.path("scripts/find_replace.R"))
all_court_staff <- read_rds("tidy_inputs/all_court_staff.RDS")

start_date <- as.Date("2019-05-01")

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
rs <- c(162, 163)

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






rp <- "Petchenik"

all_court_staff <- all_court_staff %>%
  by_id(locate_colum = "Last_Name", pattern = rp,
        replace_column = "Unit", replacement = str_to_title("INTER-AGENCY COORDINATION"))


all_court_staff <- all_court_staff %>%
  by_id("ID", pattern = 117, "Unit", str_to_title("RECORDS & INFORMATION PROCESSOR"))

all_court_staff <- all_court_staff %>%
  by_id("ID", pattern = 117, "PO_Title", "IV")



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
cc <- c(115, 267, 350)
cc_rur <- c(2, 114, 123, 319, 342)

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

all_court_staff <- all_court_staff %>%
  by_id("ID", cc, "Unit", "Custody Call - Adjudicatory")

all_court_staff <- all_court_staff %>%
  by_id("ID", cc_rur, "Unit", "Custody Call - RUR")

#section 3, g&t division
all_court_staff %>%
  filter(Division == "Grants & Technology")

all_court_staff <- all_court_staff %>%
  by_id("Division", "Grants & Technology", "Section", "Specialized Probation Services")

all_court_staff<- all_court_staff %>%
  by_id("ID", 331, "Division", "Grants & Technology")

all_court_staff<- all_court_staff %>%
  by_id("ID", 331, "position_start_date", start_date)

all_court_staff <- all_court_staff %>%
  by_id("ID", 15, "Unit", "Grants")

all_court_staff <- all_court_staff %>%
  by_id("ID", 286, "Unit", "C-Five Project Manager")

all_court_staff <- all_court_staff %>%
  by_id("ID", 231, "Unit", "C-Five Project Manager")

all_court_staff <- all_court_staff %>%
  by_id("ID", 286, "PO_Title", "Assistant Project Administrator")

all_court_staff<- all_court_staff %>%
  by_id("ID", 276, "Division", "Grants & Technology")

all_court_staff<- all_court_staff %>%
  by_id("ID", 276, "Unit", "Technology")

all_court_staff<- all_court_staff %>%
  by_id("ID", 306, "Unit", "IT Services")

all_court_staff<- all_court_staff %>%
  by_id("ID", 207, "Unit", "Title IV-E")

#Clinical
all_court_staff %>%
  filter(Division == "Clinical Assessment and Support") %>%
  View()

all_court_staff <- all_court_staff %>%
  by_id("Division", "Clinical Assessment and Support", 
        "Section", "Specialized Probation Services")

ccintake <- c(43, 213, 253, 270)
csn <- c(42, 97, 246, 224, 291, 272)
art <- c(289, 130, 80, 323)

all_court_staff<- all_court_staff %>%
  by_id("ID", ccintake, "Unit", "Central Clinical Intake")

all_court_staff<- all_court_staff %>%
  by_id("ID", 241, "Unit", "Clinical Support")

all_court_staff<- all_court_staff %>%
  by_id("ID", csn, "Unit", "Clinical Services")

all_court_staff <- all_court_staff %>%
  by_id("ID", art, "Unit", "Art Therapy")

all_court_staff<- all_court_staff %>%
  by_id("ID", 202, "PO_Title", "Coordinator")

#EdServices

all_court_staff %>%
  filter(Division == "Educational Services") %>%
  View()

all_court_staff<- all_court_staff %>%
  by_id("Division", "Educational Services", "Section", "Specialized Probation Services")

exp <- c(12, 6, 95, 186, 209, 367)
all_court_staff <- all_court_staff %>%
  by_id("ID", exp, "Unit", "Expeditor")

all_court_staff <- all_court_staff %>%
  by_id("ID", 297, "Unit", "Drug Unit")

edad <- c(10, 210, 174, 254)
all_court_staff <- all_court_staff %>%
  by_id("ID", edad, "Unit", "Educational Advocacy")

comserv <- c(370, 44, 75, 101, 131, 133, 183, 191, 305)

all_court_staff <- all_court_staff %>%
  by_id("ID", comserv, "Unit", "Community Service")

edout <- c(372, 128, 149, 203, 249, 301)

all_court_staff <- all_court_staff %>%
  by_id("ID", edout, "Unit", "Educational Outreach")

#IPS
all_court_staff %>%
  filter(Division == "IPS") %>%
  View()

all_court_staff<- all_court_staff %>%
  by_id("Division", "IPS", 
        "Section", "Specialized Probation Services")

gsst <- c(204, 74, 172, 282)

all_court_staff <- all_court_staff %>%
  by_id("ID", gsst, "Unit", "Gang School Saftey Team")

ipsintake <- c(288, 45, 94, 166, 294)

all_court_staff <- all_court_staff %>%
  by_id("ID", ipsintake, "Unit", "IPS Intake")

ips_north <- c(124, 302, 261, 327, 19, 54, 171, 336)
ips_south <- c(1, 351, 175, 283, 58, 71, 127, 251, 81, 110)

all_court_staff <- all_court_staff %>%
  by_id("ID", ips_north, "Unit", "North Teams")

all_court_staff <- all_court_staff %>%
  by_id("ID", ips_south, "Unit", "South Teams")

#Chicago SOuth
all_court_staff %>%
  filter(Division == "Chicago South") %>%
  View()

all_court_staff <- all_court_staff %>%
  by_id(ID, 53, "Unit", "pd_14, pd_17, pd_18, pd_19, pd_20, pd_24")

all_court_staff <- all_court_staff %>%
  by_id(ID, 53, "Division", "Chicago South")

all_court_staff <- all_court_staff %>%
  by_id(ID, 187, "Division", "Chicago South")

all_court_staff <- all_court_staff %>%
  by_id(ID, 243, "Division", "Chicago South")

all_court_staff <- all_court_staff %>%
  by_id("Division", "Chicago South", "Section", "Chicago Field Probation Services")
  

pd_14 <- c(53, 89, 187, 212, 250, 273, 292)
pd_17 <- pd_14
pd_18 <- pd_14
pd_19 <- pd_14
pd_20 <- pd_14
pd_24 <- pd_14

all_court_staff <- all_court_staff %>%
  by_id("ID", pd_14, "Unit", "pd_14, pd_17, pd_18, pd_19, pd_20, pd_24")

pd_16 <- c(157, 106, 220, 225, 242, 247)
pd_25 <- pd_16

all_court_staff <- all_court_staff %>%
  by_id("ID", pd_16, "Unit", "pd_16, pd_25")

eng <- c(165, 5, 40, 85, 304, 312)

all_court_staff <- all_court_staff %>%
  by_id("ID", eng, "Unit", "Englewood")


pd_5 <- c(229, 61, 161, 188, 236, 311, 167)

all_court_staff <- all_court_staff %>%
  by_id("ID", pd_5, "Unit", "Chicago 5th District")

pd_8 <- c(239, 9, 16, 25, 243, 279)

all_court_staff <- all_court_staff %>%
  by_id("ID", pd_8, "Unit", "Chicago 8th District")

pd_6 <- c(258, 88, 107, 144, 362, 366)

all_court_staff <- all_court_staff %>%
  by_id("ID", pd_6, "Unit", "pd_6, pd_22")

#southeast

all_court_staff %>% 
  filter(Division == "Southeast") %>%
  view()

all_court_staff <- all_court_staff %>%
  by_id(Division, "Southeast", "Section", "Chicago Field Probation Service")

pd_11 <- c(59, 136, 142, 145, 129, 252, 339)

all_court_staff <- all_court_staff %>%
  by_id("ID", pd_11, "Unit", "Chicago 11th District")

all_court_staff %>% filter(Division == "Chicago South" & is.na(Unit)) %>% View()
pd_1 <- c(116, 99, 332, 349)

all_court_staff <- all_court_staff %>%
  by_id("ID", pd_1, "Unit", "pd_1, pd_2, pd_12, pd_21")

all_court_staff <- all_court_staff %>%
  by_id(ID, pd_1, Division, "Southeast")

woodlawn <- c(143, 190, 195, 232, 290)

all_court_staff <- all_court_staff %>%
  by_id("ID", woodlawn, "Unit", "Woodlawn")

pd_4 <- c(194, 36, 87, 264, 369)

all_court_staff <- all_court_staff %>%
  by_id("ID", pd_4, "Unit", "Chicago 4th Police District")

all_court_staff <- all_court_staff %>%
  by_id("Unit", "Chicago 4th Police District", "Division", "Southeast")

pd_9 <- c(169, 4, 17, 37, 104, 153, 313)

all_court_staff <- all_court_staff %>%
  by_id("ID", pd_9, "Unit", "Chicago 9th Police District")



#QA/Facilitation of cases
all_court_staff$Division <- as.factor(all_court_staff$Division)
divisions <- split(all_court_staff, all_court_staff$Division) # useful as a way to figure out what I swrong
no_units <- all_court_staff %>%
  filter(is.na(Unit)) 

no_units %>% View()


calculation <- nrow(no_units)/nrow(all_court_staff)
