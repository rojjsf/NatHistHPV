library(haven)
library(Epi)
library(dplyr)
library(tidyr)
library(stringr)

#### data import ####
life.table <- read.csv("I:/Projects/International Correlation of HPV and Cervical Cancer/codes and documents/data/mortality data for R codes/UN World Population Prospects 2017/WPP2017_LifeTable.csv")
 # $LocID following officially assigned ISO 3166-1 numeric codes 
 # https://esa.un.org/unpd/wpp/Download/Standard/CSV/
head(life.table)

#### vectors containing country names ####
country.names <- c("Algeria",
                   # "Guinea", | no inc data
                   # "Kenya", | no inc data | so far no indiv. prev data
                   # "Nigeria", | no inc data
                   # "Rwanda", | no inc data
                   #"South Africa", # general mort available | so far no indiv. prev data
                   "Uganda",
                   "Argentina",# general mort available
                   # "Canada",# general mort available | so far no indiv. prev data
                   "Chile",# general mort available
                   "Colombia",# general mort available
                   "Costa Rica",# general mort available
                   # "Mexico",# general mort available| no inc data
                   # "Uruguay", # general mort available | so far no indiv. prev data
                   # "Bhutan", | no inc data
                   "China",
                   "India",
                   "Iran (Islamic Republic of)",
                   # "Mongolia", | no inc data
                   # "Nepal", | no inc data
                   "Pakistan", 
                   "Republic of Korea", # general mort available
                   "Thailand", # general mort available
                   "Thailand",
                   "Viet Nam", 
                   # "Estonia", # general mort available | so far no indiv. prev data
                   # "Georgia", # general mort available| no inc data
                   "Italy", # general mort available
                   # "Lithuania", # general mort available | so far no indiv. prev data
                   "Netherlands", # general mort available
                   "Poland", # general mort available
                   "Spain", # general mort available
                   "Spain") # general mort available (twice as two incidence locations used)
                   # "United Kingdom", # general mort available | so far no indiv. prev data
                   # "Australia/New Zealand") # general mort available | so far no indiv. prev data
                   # "Fiji", # general mort available| no inc data
                   # "Vanuatu" | no inc data
cid.mort <- c(1, #"Algeria",
              # "Guinea", | no inc data
              # "Kenya", | no individual prevalence data
              # "Nigeria", | no inc data
              # "Rwanda", | no inc data
              #"South Africa", | so far no individual prevalence data
              2, #"Uganda",
              3, #"Argentina",# general mort available
              #"Canada",# general mort available| so far no individual prevalence data
              4, #"Chile",# general mort available
              6, #"Colombia",# general mort available
              5, #"Costa Rica",# general mort available
              # "Mexico",# general mort available| no inc data
              #"Uruguay", # general mort available | so far no individual prevalence data
              # "Bhutan", | no inc data
              8, #"China",
              9, #"India",
              10, #"Iran (Islamic Republic of)",
              # "Mongolia", | no inc data
              # "Nepal", | no inc data
              25, #"Pakistan", 
              12, #"Republic of Korea", # general mort available
              15, #"Thailand" (~Lampang), # general mort available
              16, #"Thailand" (~Songkhla), # general mort available
              13, #"Viet Nam", 
              #"Estonia", # general mort available| so far no individual prevalence data
              # "Georgia", # general mort available| no inc data
              20, #"Italy", # general mort available
              #"Lithuania", # general mort available| so far no individual prevalence data
              19, #"Netherlands", # general mort available
              22, #"Poland", # general mort available
              17, #"Spain", # general mort available
              18) #"Spain", # general mort available
              #"United Kingdom", # general mort available| so far no individual prevalence data
              #"Australia/New Zealand")| so far no individual prevalence data
info.mort <- data.frame("cid" = cid.mort, "Location" = country.names)


mortality <- life.table %>%
  filter(Sex == "Female") %>%
  filter(Location %in% country.names) %>%
  filter(MidPeriod %in% 1993:2013) %>%
  mutate("mx100" = mx * 100000) %>% # deaths per 100 000 personyears
  select(?..LocID, Location, Time, MidPeriod, AgeGrp, mx100)%>%
  filter(AgeGrp != 0) %>%
  filter(AgeGrp != "1-4") %>%
  filter(AgeGrp != "5-9") %>%
  filter(AgeGrp != "10-14") %>%
  filter(AgeGrp != "80-84") %>%
  filter(AgeGrp != "85+") %>%
  spread(AgeGrp, mx100) 
# mx: Central death rate, nmx, for the age interval (x, x+n)
# mx100: annual deaths per 100 000 persons (in life table mx is per hypothetical person)

mortality <- merge(mortality, info.mort, by = "Location", all = TRUE)
colnames(mortality) <- c("Location", "Time", "Midperiod", "M15_19", "M20_24", "M25_29", "M30_34", "M35_39", "M40_44", "M45_49", 
                         "M50_54", "M55_59", "M60_64", "M65_69", "M70_74", "M75_79", "cid")

# different time intervals than incidence -> new data frame with one row per year
mortality <- mortality %>% 
  separate(Time, into = c("Time.Start", "Time.End"))
rowmort <- nrow(mortality)
mortality <- mortality[rep(1:rowmort,each=5),] # time intervals are five years, so each registry entry per ci5 must be repeated 5 times
addyear <- rep(0:4, rowmort) # dummy variable to calculate years in each time interval
mortality$add <- addyear 
mortality$Time.Start <- as.numeric(mortality$Time.Start)
mortality$Year <- mortality$Time.Start + mortality$add
mortality <- mortality %>%
  select(-add, - Time.Start, - Time.End)
head(mortality)

##### mortality and incidence rates per 100 000 per 5-year age group and country ####
# inc.ci5.all from incidence.over.time.R
#--------------------
# REGISTRY: is the registry number assigned by ci5, differs in every volume
# R12_34: incidence rate in age group
# M12_34: mortality rate in age group
# cid: refers to prevalence study, computed by Rosa
# Midperiod: middle of 5 year UN mortality rate period
# loc: cancer registry locations copied from registy.txt of according ci5 volume
# Location: refers to country for which UN mortality rates were estimated
risk.rates <- full_join(inc.ci5.all, mortality, by = c("Year", "cid"))
head(risk.rates)
risk.rates<- risk.rates %>%
  filter(Year <= 2012) # no incidence rates after 2012 (last ci5 is 2008-2012
# risk.rates[risk.rates$Location == "Viet Nam", ]
