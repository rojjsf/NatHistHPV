life.table <- read.csv("I:/Projects/International Correlation of HPV and Cervical Cancer/codes and documents/data/mortality data for R codes/UN World Population Prospects 2017/WPP2017_LifeTable.csv")
 # $LocID following officially assigned ISO 3166-1 numeric codes 
head(life.table)

country.names <- c("Algeria",
                   # "Guinea", | no inc data
                   "Kenya",
                   # "Nigeria", | no inc data
                   # "Rwanda", | no inc data
                   "South Africa", # general mort available
                   "Uganda",
                   "Argentina",# general mort available
                   "Canada",# general mort available
                   "Chile",# general mort available
                   "Colombia",# general mort available
                   "Costa Rica",# general mort available
                   # "Mexico",# general mort available| no inc data
                   "Uruguay", # general mort available
                   # "Bhutan", | no inc data
                   "China",
                   "India",
                   "Iran (Islamic Republic of)",
                   # "Mongolia", | no inc data
                   # "Nepal", | no inc data
                   "Pakistan", 
                   "Republic of Korea", # general mort available
                   "Thailand", # general mort available
                   "Viet Nam", 
                   "Estonia", # general mort available
                   # "Georgia", # general mort available| no inc data
                   "Italy", # general mort available
                   "Lithuania", # general mort available
                   "Netherlands", # general mort available
                   "Poland", # general mort available
                   "Spain", # general mort available
                   "United Kingdom", # general mort available
                   "Australia/New Zealand") # general mort available
                   # "Fiji", # general mort available| no inc data
                   # "Vanuatu" | no inc data

mortality <- life.table %>%
  filter(Sex == "Female") %>%
  filter(Location %in% country.names) %>%
  filter(MidPeriod %in% 1993:2018) %>%
  select(ï..LocID, Location, Time, MidPeriod, SexID, Sex, AgeGrp, mx)
tail(mortality)
stat.table(Location, count(), mortality)
