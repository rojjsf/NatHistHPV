#' Estimate transition rate HPV -> icc by Incidence Rates in HPV+ women over time


library(haven)
library(foreign)
library(Epi)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lcmm)
library(gridGraphics)
library(stringr)

####`1. info table ####
# CHina, shanxi added
# Spain, Girona removed

info <- data.frame(matrix(nrow = 18, ncol = 0))
info <- info %>%
  mutate("cid" = c(1,  2,  5,  6,  8,  9, 10, 12, 13, 15,  3, 16, 17, 
                   #18,  
                   4, 20, 19, 22, 71), # assigned by Rosa
         "sgcentre" = c(44, 100, 19, 12, 23, 18, 61, 15,  2,  7,  9, 14,  3,  
                        #3, 
                        16, 83,  4, 41, 20), # defined by IARC prevalence studies
         "stY" = c("2007-2008", "2002-2004", "1993-1994", "1993-1995", "2005", "2004", "2013-2014", "1999-2000", "1997", "1997-1998", 
                   "1998", "1990-2000", "1998", 
                   #"1998", 
                   "2001", "2002", "1995-1998", "2006", "2004"),
         "loc.prev" = c("Algeria, Setif","Uganda, Kyadondo County", "Costa Rica",                
                        "Colombia, Bucaramanga","China, Shenyang","India, Dindigul, Ambilikkai",                  
                        "Iran (Islamic Republic of), Golestan Province", "Republic of Korea, Busan",   "Viet Nam, Ho Chi Minh City",                   
                        "Thailand, Lampang","Argentina, Entre Rios Province", "Thailand, Songkhla",        
                        "Spain, Tarragona", 
                        #"Spain, Girona", 
                        "Chile, Region of Antofagasta",                 
                        "Italy, Turin", "The Netherlands", "Poland, Poznan", "China, Shanxi"),  # Prevalence STudy location
         "country" =  c("Algeria", "Uganda", "Costa Rica", "Colombia", "China", "India", "Iran (Islamic Republic of)",  "Republic of Korea", "Viet Nam", "Thailand", # for mortality, population
                        "Argentina", "Thailand", "Spain", 
                        #"Spain", 
                        "Chile", "Italy", "Netherlands", "Poland", "China"),
         "Location" = c("Algeria", "Uganda", "Costa Rica", "Colombia", "China(Shenyang)", "India", "Iran (Islamic Republic of)",  "Republic of Korea", "Viet Nam", "Thailand (Lampang)", # for graph
                        "Argentina", "Thailand (Songkhla)", "Spain (Tarragona)", 
                        #"Spain (Girona)", 
                        "Chile", "Italy", "Netherlands", "Poland", "China(Shanxi)"),
         "REG.VIII" = c(NA, # IRR Algeria, S?tif 
                        NA, # IRR Uganda, Kyadondo County 
                        12, # *Costa Rica (1995-1996) prev: 1993/4
                        11, #  Colombia, Cali (1992-1996) (NO Bucamaranga in this volume) prev: 1993/5
                        NA, # IRR China, Shenyang 
                        NA, # IRR India, Dindigul, Ambilikkai 
                        NA, # IRR Iran (Islamic Republic of), Golestan Province 
                        NA, # IRR Korea, Busan (1996-1997) 
                        NA, # IRR Viet Nam, Ho Chi Minh City 
                        NA, #	IRR Thailand, Lampang 
                        NA, #	IRR Argentina, Bahia Blanca 
                        NA, #	IRR Thailand, Songkhla 
                        NA, #	IRR Spain, Tarragona 
                        #NA, #	IRR Spain, Girona 
                        NA, # Chile, Region of Antofagasta  
                        NA, # IRR Italy, Turin 
                        NA, # IRR The Netherlands
                        NA, # IRR Poland, Poznan
                        NA),# IRR China, Shanxi
         "files.IX" = c(NA, # IRR Algeria, S?tif 
                        NA, # IRR Uganda, Kyadondo County 
                        "21880099.csv", #	Costa Rica (1998-2002)
                        "21700199.csv", # Colombia, Cali (1998-2002) (NO Bucamaranga in this volume)
                        NA, # IRR China, Shenyang 
                        NA, # IRR India, Dindigul, Ambilikkai 
                        NA, # IRR Iran (Islamic Republic of), Golestan Province 
                        "44100299.csv", #	Korea, Busan (1998-2002) 
                        NA, # Viet Nam, Ho Chi Minh City (NO data on Viet Nam in this volume)
                        "47640599.csv", #	Thailand, Lampang (1998-2002)
                        "20320199.csv", #	Argentina, Bahia Blanca (1998-2002) (NO Concordia/Entre Rios Province in this volume)
                        "47640499.csv", #	Thailand, Songkhla (1998-2002) 
                        "57240199.csv", #	Spain, Tarragona (1998-2001)
                        #"57241099.csv", #	Spain, Girona (1998-2002)
                        NA, # Chile, Region of Antofagasta  (NO data for chile, prevalence study in 2001 - very close to 2002)
                        "53800899.csv", # IRR Italy, Turin 
                        "55280099.csv", #	The Netherlands (1998-2002)
                        NA, # IRR Poland, Poznan 
                        NA),# IRR China, Shanxi
         "REG.IX" = c(NA, # IRR Algeria, S?tif 
                      NA, # IRR Uganda, Kyadondo County 
                      21880099, #	Costa Rica (1998-2002)
                      21700199, # Colombia, Cali (1998-2002) (NO Bucamaranga in this volume)
                      NA, # IRR China, Shenyang 
                      NA, # IRR India, Dindigul, Ambilikkai 
                      NA, # IRR Iran (Islamic Republic of), Golestan Province 
                      44100299, #	Korea, Busan (1998-2002) 
                      NA, # Viet Nam, Ho Chi Minh City (NO data on Viet Nam in this volume)
                      47640599, #	Thailand, Lampang (1998-2002)
                      20320199, #	Argentina, Bahia Blanca (1998-2002) (NO Concordia/Entre Rios Province in this volume)
                      47640499, #	Thailand, Songkhla (1998-2002) 
                      57240199, #	Spain, Tarragona (1998-2001)
                      #57241099, #	Spain, Girona (1998-2002)
                      NA, # Chile, Region of Antofagasta  (NO data for chile, prevalence study in 2001 - very close to 2002)
                      53800899, # Italy, Turin 
                      55280099, #	The Netherlands (1998-2002) 
                      NA, # IRR Poland, Poznan 
                      NA),# IRR China, Shanxi
         "REG.X" = c(1201, # *Algeria, S?tif (2003-2007)
                     80002, # Uganda, Kyadondo county (2003-2007)
                     18800, #  Costa Rica (2003-2007)
                     17002, # Colombia, Bucaramanga (2003-2007)
                     15615,	#China, Harbin City, Nangang District (2003-2007) (NOT the correct district, not available)
                     35611,	# India, Dindigul, Ambilikkai (2003-2007) 
                     36403,	# Iran (Islamic Republic of), Golestan Province (2005-2007)
                     41002, # Republic of Korea, Busan (2003-2007)
                     NA, #  NO Viet Nam, Ho Chi Minh City (2009-2012)
                     76405, # Thailand, Lampang (2003-2007)
                     3201, # Argentina, Bahia Blanca (2003-2007) (!! NOT Concordia like Prevalence! NO Entre Rios Province in Registry) 
                     76404, # Thailand, Songkhla (2004-2007
                     72401, # Spain, Tarragona (2003-2007)
                     #72410, # Spain, Girona (2003-2007)
                     15203, # *Chile, Region of Antofagasta (2003-2007)
                     38008,	# Italy, Turin (2003-2007
                     52800, # The Netherlands (2003-2007)
                     61607, # Poland, Kielce (2003-2007)  
                     15606),#China, Cixian County (2003-2007) 
         "REG.XI" = c(101200199, # *Algeria, S?tif (2008-2011)
                      180000299, # *Uganda, Kyadondo County (2008-2012)
                      218800099, #  Costa Rica (2008-2011)
                      217000299, # Colombia, Bucaramanga (2008-2012)
                      415608399, # China, Shenyang (2008-2012)
                      435601199, # *India, Dindigul, Ambilikkai (2008-2012)
                      436400399, # Iran (Islamic Republic of), Golestan Province (2008-2011)
                      441000299, # Republic of Korea, Busan (2008-2012)
                      470400299, # *Viet Nam, Ho Chi Minh City (2009-2012)
                      476400599, # Thailand, Lampang (2008-2012)
                      203200599, # Argentina, Entre Rios Province (2008-2011)
                      476400499, # Thailand, Songkhla (2008-2012)
                      572400199, # Spain, Tarragona (2008-2012)
                      #572401099, # Spain, Girona (2008-2012)
                      215200399, # *Chile, Region of Antofagasta (2008-2010)
                      538000899, # Italy, Turin (2008-2012)
                      552800099, # *The Netherlands (2008-2012)
                      561600799, #Poland, Kielce (2008-2012)
                      415600699)) %>% # China, Cixian County (2008-2012)
  separate(stY, into = paste0("stY", c(0, "")))
info$stY[info$cid %in% c(8, 9, 13, 3, 17, 4, 20, 22, 71)] <- c(2005, 2004, 1997, 1998, 2000, 2003, 2002, 2006, 2004) # filling NA if only one study year
info

fact.lbl <- c("[15,20)", "[20,25)", "[25,30)", "[30,35)", "[35,40)", "[40,45)", "[45,50)", "[50,55)",
              "[55,60)", "[60,65)", "[65,70)", "[70,75)", "[75,80)")

####`2. load all data ####
# mortality
# load from mortality_rates.UN, $LocID following officially assigned ISO 3166-1 numeric codes 
life.table <- read.csv("I:/Projects/International Correlation of HPV and Cervical Cancer/codes and documents/data/mortality data for R codes/UN World Population Prospects 2017/WPP2017_LifeTable.csv")
# population 
NP <- read.csv("I:/Projects/International Correlation of HPV and Cervical Cancer/codes and documents/data/population data for R codes/WPP2017_PopulationBySingleAgeSex .csv", header = TRUE)
# incidence
dataVIII <- read.csv("I:/Projects/International Correlation of HPV and Cervical Cancer/codes and documents/data/incidence data for R codes/CI5-VIII/CI5-VIII.csv", header = FALSE)
files.IX <- na.exclude(info$files.IX)
REG.IX <- na.exclude(info$REG.IX)
pathIX <- file.path("I:/Projects/International Correlation of HPV and Cervical Cancer/codes and documents/data/incidence data for R codes/CI5-IXd", files.IX)
dataIX <- lapply(pathIX, read.csv, header = FALSE)
casesX <- read.csv("I:/Projects/International Correlation of HPV and Cervical Cancer/codes and documents/data/incidence data for R codes/CI5-X/cases.csv")
popX <- read.csv("I:/Projects/International Correlation of HPV and Cervical Cancer/codes and documents/data/incidence data for R codes/CI5-X/pop.csv")
casesXI <- read.csv("I:/Projects/International Correlation of HPV and Cervical Cancer/codes and documents/data/incidence data for R codes/CI5-XI/cases.csv")
popXI <- read.csv("I:/Projects/International Correlation of HPV and Cervical Cancer/codes and documents/data/incidence data for R codes/CI5-XI/pop.csv")
# prevalence
pooled.data <- read_dta("I:/Projects/International Correlation of HPV and Cervical Cancer/codes and documents/data/prevalence data for R codes/HPVPREV_POOL_V29-1.dta")



####`3. select age groups cohort and prevalence ####
# for cohort, model, graph
# mina <- 20
# maxa <- 25
# for prevalence
ageintp <- 10
minp <- 20
maxp <- 70


####`4. mortality ####

# calculated from rates or numbers of deaths for CI??? 
# load from mortality_rates.UN
# life.table <- read.csv("I:/Projects/International Correlation of HPV and Cervical Cancer/codes and documents/data/mortality data for R codes/UN World Population Prospects 2017/WPP2017_LifeTable.csv")
# $LocID following officially assigned ISO 3166-1 numeric codes 
# https://esa.un.org/unpd/wpp/Download/Standard/CSV/
mort <- life.table %>%
  filter(Sex == "Female") %>%
  filter(Location %in% info$country) %>%
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
colnames(mort) <- c("country", "Time", "Midperiod", "M15_19", "M20_24", "M25_29", "M30_34", "M35_39", "M40_44", "M45_49", 
                    "M50_54", "M55_59", "M60_64", "M65_69", "M70_74", "M75_79")
mort <- merge(mort, info, by = "country", all = TRUE)
# different time intervals than incidence -> new data frame with one row per year
mort <- mort %>% 
  separate(Time, into = c("Time.Start", "Time.End"))
rowmort <- nrow(mort)
mort <- mort[rep(1:rowmort,each=5),] # time intervals are five years, so each registry entry per ci5 must be repeated 5 times
addyear <- rep(0:4, rowmort) # dummy variable to calculate years in each time interval
mort$add <- addyear 
mort$Time.Start <- as.numeric(mort$Time.Start)
mort$Year <- mort$Time.Start + mort$add
mort <- mort %>%
  select(-add, - Time.Start, - Time.End)
head(mort)



####`5. population ####
# NP <- read.csv("C:/Users/schultefrohlinder/Documents/R/NatHistHPV/data sets/WPP2017_PopulationBySingleAgeSex.csv", header = TRUE)
# NP$Age <- as.numeric(NP$AgeGrp)
Npop <- NP %>%  # number population(age, year)
  select(c(-1, -PopMale, -PopTotal,  - AgeGrpSpan, - AgeGrpStart)) %>%
  filter(Location %in% info$country) %>%
  filter(Time %in% 1990:2012) %>%
  mutate(Year = Time)  %>% # AgeGrpSpan = 1 
  mutate(country  = Location) %>%
  select(-Location, -Time, -VarID, - Variant)
#Npop$Location <- factor(Npop$Location, levels = levels(info$country))  
Npop <- Npop %>%
  full_join(., info, by = c("country"))
head(Npop)




####`6. incidence ####

####`CI5-VIII 1993-1997####
#First column is population number (see population dictionary)
#Second column is sex (1 Male/2 Female)
#Third column is cancer number (see cancer dictionary)
#Fourth column is age (1-19: 0-4,5-9,..,85+,unknown age)
#Fifth column is number of cancer cases by age
#Sixth column is person-years at risk by age

# dataVIII <- read.csv("I:/Projects/International Correlation of HPV and Cervical Cancer/codes and documents/data/incidence data for R codes/CI5-VIII/CI5-VIII.csv", header = FALSE)
incVIII <- dataVIII %>%
  filter(V1 %in% info$REG.VIII) %>%
  filter(V2 == 2 & V3 == 120 & V4 >= 4 & V4 <= 16) %>% # filter females, cervical cancer, age >= 15 & <= 79 (age groups here 1-19)
  mutate("V7" = round(V5 * 100000 / V6, 2)) %>% # new column with incidence rate per 100000
  select(-V2, -V3, -V5, - V6) %>%
  spread(V4, V7) %>%
  mutate("ci5" = 8) %>%
  mutate("country" = c("Colombia", "Costa Rica"))  %>% # which registry exactly see Excel overview
  mutate("cid" = c(6, 5))
incVIII <- incVIII[rep(1:2, each=5),] # one line per year as incidence time intervals differ from mortality's
incVIII$Year <- rep(1993:1997, 2)
colnames(incVIII) <- c("REG.VIII", "R15", "R20", "R25", "R30", "R35", "R40", "R45", 
                       "R50", "R55", "R60", "R65", "R70", "R75", "ci5", "country", "cid", "Year")

####`CI5-IX 1998-2002 ####`
#First column is sex (1 male/2 female)  
#Second column is the cancer site number (1 to 244, see "cancer.txt") 
#Third column is age-group, 0-4,5-9,10-14,...,80-84,85+,age unknown (1 to 19)  
#Fourth column is number of cases  
#Fifth column is person-years at risk
# 117 cervix uteri (C53)


# files.IX <- na.exclude(info$files.IX)
# REG.IX <- na.exclude(info$REG.IX)
# pathIX <- file.path("I:/Projects/International Correlation of HPV and Cervical Cancer/codes and documents/data/incidence data for R codes/CI5-IXd", files.IX)
# dataIX <- lapply(pathIX, read.csv, header = FALSE)
# all files in one list, elements of which are named by registry numbers
names(dataIX) <- REG.IX # will be added as own column during transformation into data frame
dataIX<- plyr::ldply(dataIX, data.frame) # transform list into data.frame
incIX <- dataIX %>%
  filter(V1 == 2 & V2 == 117 & V3 >= 4 & V3 <= 16) %>% # filter females, cervical cancer, age >= 15 & <= 79 (age groups here 1-19)
  mutate("V6" = round(V4*100000/V5, 2)) %>% # incidence rates
  select("REG.IX" = .id, V3, V6, -V1, -V2, -V4, -V5) %>% # drop sex, age, cases, personyears
  spread(V3, V6) %>% # horizontal orientation of data frame to match other volumes
  mutate(ci5 = 9)
incIX <- merge(incIX, info[, c("REG.IX", "country", "cid")], by = "REG.IX")
rowsIX <- nrow(incIX)
incIX <- incIX[rep(1:rowsIX, each=5),] # one line per year as incidence time intervals differ from mortality's
incIX$Year <- rep(1998:2002, rowsIX) 
colnames(incIX) <- c("REG.IX", "R15", "R20", "R25", "R30", "R35", "R40", "R45", 
                     "R50", "R55", "R60", "R65", "R70", "R75", "ci5", "country", "cid", "Year") 

####`CI5-X 2003-2007 ####
# casesX <- read.csv("I:/Projects/International Correlation of HPV and Cervical Cancer/codes and documents/data/incidence data for R codes/CI5-X/cases.csv")
# popX <- read.csv("I:/Projects/International Correlation of HPV and Cervical Cancer/codes and documents/data/incidence data for R codes/CI5-X/pop.csv")
casesX <- casesX %>%
  filter(REGISTRY %in% info$REG.X)%>%
  filter(SEX == 2) %>% # females
  filter(CANCER == 32) %>% # cervical cancer
  select(c(1, 8:20))  # age grps (upper limit of age groups: 8-4 = 4, 4*5 = 20. > 20y & <= 80 y)
popX <- popX  %>%
  filter(REGISTRY %in% info$REG.X)%>%
  filter(SEX == 2) %>% # females
  select(c(1, 7:19)) # age grps (7-3 = 4, 4*5 = 20. > 20y & <= 80 y)

incX <- merge(casesX, popX, by = "REGISTRY") 
dc <- dim(casesX)[2]  # length of data frame cases (so script can be run even if centres added)
dp <- dim(popX)[2] # length of data frame pyears
incX <- data.frame(incX, round(incX[, 2:dc] * 100000 / incX[, (dp +1 ):(2*dp-1)], 1)) # select colums with values, calculate rates
incX[, c(2:(dc), (dp+1):(2*dp-1))] <- NULL # delete cases and pyears columns
incX$REG.X <- incX$REGISTRY
incX<- incX %>% mutate(ci5 = 10) %>% select(-REGISTRY)
incX <- merge(incX, info[, c("REG.X", "country", "cid")], by = "REG.X")
rowsX <- nrow(incX)
incX <- incX[rep(1:rowsX, each=5),] # one line per year as incidence time intervals differ from mortality's
incX$Year <- rep(2003:2007, rowsX) 
colnames(incX) <- c("REG.X", "R15", "R20", "R25", "R30", "R35", "R40", "R45", 
                    "R50", "R55", "R60", "R65", "R70", "R75", "ci5", "country", "cid", "Year")

####`CI5-XI 2008-2011 ####
# casesXI <- read.csv("I:/Projects/International Correlation of HPV and Cervical Cancer/codes and documents/data/incidence data for R codes/CI5-XI/cases.csv")
# popXI <- read.csv("I:/Projects/International Correlation of HPV and Cervical Cancer/codes and documents/data/incidence data for R codes/CI5-XI/pop.csv")
casesXI <- casesXI %>%
  filter(REGISTRY %in% info$REG.XI)%>%
  filter(SEX == 2) %>% # females
  filter(CANCER == 32) %>% # cervical cancer
  select(c(1, 8:20))  # age grps (upper limit of age groups: 8-4 = 4, 4*5 = 20. > 20y & <= 80 y)
popXI <- popXI  %>%
  filter(REGISTRY %in% info$REG.XI)%>%
  filter(SEX == 2) %>% # females
  select(c(1, 7:19)) # age grps (7-3 = 4, 4*5 = 20. > 20y & <= 80 y)

incXI <- merge(casesXI, popXI, by = "REGISTRY") 
dc <- dim(casesXI)[2]  # length of data frame cases (so script can be run even if centres added)
dp <- dim(popXI)[2] # length of data frame pyears
incXI <- data.frame(incXI, round(incXI[, 2:dc] * 100000 / incXI[, (dp +1 ):(2*dp-1)], 1)) # select colums with values, calculate rates
incXI[, c(2:(dc), (dp+1):(2*dp-1))] <- NULL # delete cases and pyears columns
incXI$REG.XI <- incXI$REGISTRY
incXI<- incXI %>% mutate(ci5 = 11) %>% select(-REGISTRY)
incXI <- merge(incXI, info[, c("REG.XI", "country", "cid")], by = "REG.XI")
rowsXI <- nrow(incXI)
incXI <- incXI[rep(1:rowsXI, each=5),] # one line per year as incidence time intervals differ from mortality's
incXI$Year <- rep(2008:2012, rowsXI) 
colnames(incXI) <- c("REGISTRY", "R15", "R20", "R25", "R30", "R35", "R40", "R45", 
                     "R50", "R55", "R60", "R65", "R70", "R75", "ci5", "country", "cid", "Year")

####`all available incidence rates from ci5 volume 9-11 ####`
# matched with cid for locations in which hpv prevalence studies were conducted
# intervals are filled with year under assumption that incidence constant over time interval.
inc.ci5.all <- rbind(incVIII[, 2:18], incIX[, 2:18], incX[,2:18], incXI[,2:18], na.omit = TRUE)
inc.ci5.all$cid <- as.numeric(inc.ci5.all$cid)



####`7. prevalence ####
# pooled.data <- read_dta("I:/Projects/International Correlation of HPV and Cervical Cancer/codes and documents/data/prevalence data for R codes/HPVPREV_POOL_V29-1.dta")

Hrisk <- c("ahpv16", "ahpv18", "ahpv31","ahpv33","ahpv35","ahpv39","ahpv45", 
           "ahpv51","ahpv52","ahpv56","ahpv58","ahpv59","ahpv68", "ahpv73", "ahpv82") # omitted apvhrx for NA reason
cdata <- pooled.data %>%
  filter(!is.na(ahpv16))%>% # dplyr calculation cannot handle NA. if ahpv16 == NA, all ahpv columns NA
  filter(sgcentre ==19) 
if(pooled.data$sgcentre != 19){ # in costa rica all sel_paper) == 0 as np paper yet
  prev.data <- pooled.data[-which(pooled.data$sel_paper0 == 0), ]
}
prev.data <- rbind(prev.data, cdata)


pooled.hrisk <- prev.data %>%
  select(sgcentre, sgid, sga3, Hrisk) %>%
  filter(sgcentre %in% info$sgcentre) %>%
  filter(sga3 >= minp &  sga3 < maxp)
prvl <- pooled.hrisk %>%
  mutate(hpvpos = rowSums(pooled.hrisk[, Hrisk])) %>% # number of different hpv infections
  mutate(hpvsino = ifelse(hpvpos > 0, 1, 0)) %>% # factor if hpv  positive or negative for high risk types. NA now as 0 !?
  mutate(hpvsino = factor(hpvsino, levels = c(0,1), labels = c("neg", "pos"))) %>%
  mutate(age.grp = factor(cut(sga3, seq(minp, maxp, ageintp), right = FALSE), labels = stringr::str_c("ag", 1:((maxp-minp)/ageintp), collapse = NULL))) %>%
  ungroup() %>%
  group_by(sgcentre, age.grp) %>%
  summarise(prev = sum(hpvsino == "pos")/n(), se = round(1.96*sqrt((sum(hpvsino == "pos")/n())*((1-sum(hpvsino == "pos"))/n())/n()), 4)) %>%
  full_join(., info, by = "sgcentre")
prvl %>% filter(sgcentre == 19)
# uganda
uga.data <- read_dta("C:/Users/schultefrohlinder/Documents/R/NatHistHPV/data sets/girls-baseline-part-quest-clin-lab-sample-hpvres-fup-cyto-updbasefupoct2007-subtypes.dta")
uHrisk <- c("h16", "h18", "h31","h33","h35","h39","h45","h51","h52","h56","h58","h59","h68_73", "h82")
uga.data <- uga.data %>%
  filter(select_paper_baseline == 1) %>%
  select(HPVNUM, "sga3" = AGE, uHrisk) %>%
  mutate(sgcentre = 100)  %>%
  filter(sga3> minp & sga3 < maxp)
uga.data[is.na(uga.data)] <- 0 
if(nrow(uga.data) > 0) {
  prvl <- uga.data %>%
    mutate(hpvpos = rowSums(uga.data[, uHrisk])) %>% # number of different hpv infections
    mutate(hpvsino = ifelse(hpvpos > 0, 1, 0)) %>% # factor if hpv  positive or negative for high risk types. NA now as 0 !?
    mutate(hpvsino = factor(hpvsino, levels = c(0,1), labels = c("neg", "pos"))) %>%
    mutate(age.grp = factor(cut(sga3, seq(minp, maxp, ageintp), right = FALSE), labels = stringr::str_c("ag", 10/ageintp))) %>%
    ungroup() %>%
    group_by(sgcentre, age.grp) %>%
    summarise(prev = sum(hpvsino == "pos")/n(), se = round(1.96*sqrt(sum(hpvsino == "pos")/n())*(1-sum(hpvsino == "pos")/n())/n(), 4)) %>%
    left_join(., info, by = "sgcentre") %>%
    union(., prvl)
}
head(prvl)

prvl$age.grp <- as.factor(prvl$age.grp)
prvl.long <- prvl[rep(1:nrow(prvl), each=ageintp),] # one line per year
nbag <- length(levels(prvl$age.grp))
if(nbag < 9) {
  prvl.long$age <- c(((as.numeric(stringr::str_sub(prvl.long$age.grp, -1))-1)*ageintp + minp) + 0:(ageintp-1))
}else{
  prvl.long$age <- c((as.numeric(stringr::str_sub(prvl.long$age.grp, -2))*ageintp - minp) + 0:(ageintp-1))
}
prvl.long$Year <- as.numeric(prvl.long$stY) # to be able to join 
prvl.long$cid <- as.numeric(prvl.long$cid)
prvl.long$stY[prvl.long$cid == 4 ] <- 2003 # chile. as nor inc rates before 2003
prvl.long <- prvl.long %>% select(sgcentre, age.grp, prev, se, cid, loc.prev, country, stY, Location, Year, age)
prvl.long


####`8. joint table ####
# prev.long
prvl$age.grp <- as.factor(prvl$age.grp)
prvl.long <- prvl[rep(1:nrow(prvl), each=ageintp),] # one line per year
nbag <- length(levels(prvl$age.grp))
if(nbag < 9) {
  prvl.long$age <- c(((as.numeric(stringr::str_sub(prvl.long$age.grp, -1))-1)*ageintp + minp) + 0:(ageintp-1))
}else{
  prvl.long$age <- c((as.numeric(stringr::str_sub(prvl.long$age.grp, -2))*ageintp - minp) + 0:(ageintp-1))
}
prvl.long$Year <- as.numeric(prvl.long$stY) # to be able to join 
prvl.long$cid <- as.numeric(prvl.long$cid)
prvl.long$stY[prvl.long$cid == 4 ] <- 2003 # chile. as nor inc rates before 2003
prvl.long <- prvl.long %>% select(sgcentre, prev, se, cid, stY, Year, age)
prvl.long
# inc.long
inc.long <- inc.ci5.all %>%
  tidyr::gather(., "age.grp", "IR", 1:13) 
inc.long <- inc.long[rep(1:nrow(inc.long),each=5),] # time intervals are five years, so each registry entry per ci5 must be repeated 5 times
inc.long <- inc.long %>%
  mutate(age = c(as.numeric(stringr::str_sub(inc.long$age.grp, 2,3)) + 0:4))
# mort.long
mort.long <- mort %>%
  tidyr::gather(., "age.grp", "MR", 3:15) 
mort.long <- mort.long[rep(1:nrow(mort.long),each=5),]
mort.long <- mort.long %>%
  mutate(age = c(as.numeric(stringr::str_sub(mort.long$age.grp, 2,3)) + 0:4)) %>%
  select(sgcentre, MR, cid, Year, age)
# Npop
pop.long <- Npop %>%
  mutate(age = AgeGrp) %>%
  select(age, PopFemale, Year, cid)
pop.long$age <- as.numeric(pop.long$age)
# transition rate data
Dpop <- mort.long %>%
  full_join(., prvl.long, by = c("cid", "age", "Year", "sgcentre")) %>% 
  full_join(., inc.long, by = c("cid", "age", "Year")) %>%
  full_join(., pop.long, by = c("cid", "age", "Year"))%>%
  select(-stY) %>%
  full_join(., info[, c("sgcentre", "stY", "cid", "Location")], by = c("cid", "sgcentre"))

Dpop <- Dpop[order(Dpop$cid, Dpop$Year, Dpop$age),] 
Dpop <- Dpop[which(!is.na(Dpop$IR)), ]
tail(Dpop)

####`9. calculate Nicc, Npos, YsncS ####

for (i in 1:nrow(Dpop)){
  Dpop$PopFemale[i] <- Dpop$PopFemale[i] * 1000 # exact number of women per year and age group and country
  Dpop$Nicc[i] <- round(Dpop$PopFemale[i] * Dpop$IR[i] *10^(-5), 1)# number of cancer cases per age and year
  Dpop$Npos[i] <- round(Dpop$prev[i] * Dpop$PopFemale[i], 1)  # number of hpv positive women in Year of hpv study and age group
  
}

for(i in 1:(nrow(Dpop))){
  if(!is.na(Dpop$Npos[i])) {
    a <- Dpop$age[i]
    y <- Dpop$Year[i]
    loc <- Dpop$cid[i]
    Dpop$Npos[Dpop$age == (a+1) & Dpop$Year == (y+1) & Dpop$cid == loc] <- round(Dpop$Npos[i] - Dpop$Npos[i]*Dpop$MR[i]*10^(-5) - Dpop$Nicc[i], 1)
    #Dpop$prev[Dpop$Age == (a+1) & Dpop$Year == (y+1) & Dpop$cid == loc] <- round(Dpop$Npos[i] *100 / Dpop$PopFemale[i], 2)
  }
}


#### Loop 1: high screening coverage ####
Dpop_1 <- Dpop %>%
  filter(country %in% c("Spain" ,"Netherlands", "Italy"))
info_1 <- info %>%
  filter(country %in% c("Spain" ,"Netherlands", "Italy"))


ggout1 <- list()
DpCout1 <- list()
lcmm_out1 <- list("tr_lcmm", "tr_bic", "tr_pred", "tr_prob", "tr_conv", "tr_lambda")
Dp_out1 <- list()


for(d in c(4, 9)){
  cat("\n ageint: ", d) # locating error
  #browser()
  for(f in c(20, 25, 30, 35, 40, 45)) {
    cat("\n age.beg: ", f)
    mina <- f 
    maxa <- sum(f, d)
    
    Dp <- data.frame(matrix(nrow = 0, ncol = 16)) # Dp = Data population (as nested in of Dpop_1)
    colnames(Dp) <- colnames(Dpop_1)
    
    for(j in info_1$cid){
      sist <- (2012 - as.numeric(info_1$stY[info_1$cid == j])) # nb of years since prevalence Study
      if(info_1$stY[info_1$cid == j] < 2012){ # excluding Iran (in 2014)
        # cat("\n cid: ", j) # locating error
        for(i in 0:sist){
          a <- (mina+i):(maxa+i) # moving age group by one year
          y <- as.numeric(Dpop_1$stY) + i # i years added to year of prev. Study
          Dp <- rbind(Dp, Dpop_1[Dpop_1$age %in% a & Dpop_1$Year==y & Dpop_1$cid== j, ])
        }
      }
    }
    Dp <- Dp[!is.na(Dp$IR), ]
    # Years since Prevalence Study:
    Dp <- Dp %>%
      mutate(YsncS = as.numeric(Dp$Year) - as.numeric(Dp$stY)) 
    Dp_out1[[stringr::str_c("Dp", mina, "_", maxa)]] <- Dp
    #data frame DpC: cumulative rates for full cohort per country, year (not over full time period) ####
    # Sum all cases and women of all ages (in previously selected age group) in each Year and country (location, cid)
    DpC <- Dp %>%
      ungroup() %>%
      group_by(YsncS, cid, Location) %>% 
      summarise(Npos = sum(Npos), 
                Nicc = sum(Nicc), 
                se = sqrt(sum(Nicc))/sum(Npos), # standard error of rates = rate^2/cases = sqrt(cases)/personyears (Esteve p. 52)
                logRate = log(sum(Nicc)/sum(Npos)), 
                selog = sqrt(1/sum(Npos))) %>% # var(log(k/m)) = 1/(Rate*pyears); se(log(k/m)) = sqrt(1/k). Esteve p. 53
      filter(Location != "Uganda") %>%
      filter(Location != "Viet Nam")         
    DpC$Location <- as.factor(DpC$Location)
    DpC$cid <- as.factor(DpC$cid)
    DpC[which(DpC$Nicc <= 0 | DpC$Npos <= 0 ), c("Nicc", "Npos", "logRate", "se")] <- NA # to avoid -Inf when Nicc = 0
    DpCout1[[stringr::str_c("DpC", mina, "_", maxa)]] <- DpC # to have all age cohort for mixed model
    
    
    #11.Plot
    
    col <- c('#d8b365', '#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#67001f', '#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#003c30','#b15928', '#dd1c77', '#1c9099')
    DpCplot <- ggplot2::ggplot(DpC, aes(x = YsncS, y = logRate)) + 
      geom_point(aes(color = Location)) + 
      geom_line(aes(color = Location)) +
      scale_color_manual(values = col) +
      ggtitle(stringr::str_c("cohort of HPV+ women ", mina, "-", maxa, "y")) +
      ylab("cervical cancer inidence Rate in HPV+ women") +
      xlab("Years since HPV detection") +
      ylim(-10.5, -2) +
      geom_linerange(aes(x = YsncS, ymin = logRate - selog, ymax = logRate + selog, color = Location)) + # add standard error bars
      theme_bw()
    ggout1$DpC_plot[[stringr::str_c(mina, "_", maxa)]] <- DpCplot

    
    
    
    #12. Model 
    
    # ln(Nicc/Npos) = alpha + beta*YsncS
    # beta is rate of change 
    nd <- data.frame(YsncS = 0:18)
    
    # latent class mixed model
    # par(mfrow = c(2, 4))
    # select age group from loop
    
    lcmm_out1$tr_lcmm[[stringr::str_c(mina, "_", maxa)]] <- 
      tr_lcmm <- lcmm(logRate ~ YsncS, random = ~ YsncS, subject = "Location",  ng = 1, data = DpC, link = "4-equi-splines")
    
    # bic
    lcmm_out1$tr_bic[[stringr::str_c(mina, "_", maxa)]] <- 
      bic <- round(tr_lcmm$BIC, 2)
    
    # predictions
    lcmm_out1$tr_pred[[stringr::str_c(mina, "_", maxa)]] <- 
      tr_pred <- predictY(tr_lcmm, nd, methInteg = 1, draws = TRUE)
    
    # groups
    lcmm_out1$tr_prob[[stringr::str_c(mina, "_", maxa)]] <- tr_lcmm$pprob
    
    # convergence criteria
    lcmm_out$tr_conv[[stringr::str_c(mina, "_", maxa)]] <- tr_lcmm$conv
    
    ##`slope
    lcmm_out1$tr_lambda[[stringr::str_c(mina, "_", maxa)]] <- # 1 gamma per year
      tr_lambda <- data.frame("YsncS" = tr_pred$times[2:19,]-0.5, 
                              "lambda" = diff(tr_pred$pred[, 1]))
    
    lcmm_out1$logr[[stringr::str_c(mina, "_", maxa)]] <- 
      m_logr <- data.frame("logr" = tr_pred$pred[, 1], 
                           "logr_lo" = tr_pred$pred[, 2],
                           "logr_hi" = tr_pred$pred[, 3],
                           "YsncS" = tr_pred$times$YsncS)
    DpC <- DpC %>%
      full_join(., m_logr, by = "YsncS")
    
    col <- c('#d8b365', '#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#67001f', '#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#003c30','#b15928', '#dd1c77', '#1c9099')
    
    ggout1$tr_plot[[stringr::str_c(mina, "_", maxa)]] <- 
      ggplot2::ggplot(DpC, aes(x = YsncS, y = logRate)) + 
      theme_bw() + 
      #geom_point(aes(color = Location)) + 
      #geom_ribbon(aes(x = YsncS, ymin = logr_lo, ymax = logr_hi), fill = 'azure4', alpha = 0.4)+
      geom_line(aes(color = Location), size = 0.75) +
      scale_color_manual(values = col) +
      #geom_line(aes(x = YsncS, y =  logr), col = "black", size = 1.7, linetype = 1) +
      #ggtitle(stringr::str_c(mina, "_", maxa, "y")) +
      labs(y = "log Incidence Rate in hrHPV+ women", 
           x = "Years since HPV detection"
           #, caption = "Mixed Model (CI)"
      ) +
      ylim(-10.5, -2.5)  +
      guides(color = guide_legend(ncol = 2)) +
      theme(legend.title = element_text(size = 16),
            legend.text = element_text(size = 14),
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 14),
            legend.key.height = unit(0.8, "cm"),
            legend.key.width = unit(1.2, "cm")) +
      annotate("text", x = 13, y = -9, label = stringr::str_c(mina, "-", maxa, "y"), size = 12) 
    # geom_linerange(aes(x = YsncS, ymin = logRate - selog, ymax = logRate + selog, color = Location)) + # add standard error bars
    #geom_point(aes(x = YsncS, y =  logr), col = "red") +
    
  }
}


### wrap plot cpnsecutive ages ##

DpC20 <- DpCout1$DpC20_29 %>%
  mutate(ag = "ag1") %>%
  full_join(lcmm_out1$logr$`20_29`, by = "YsncS")
DpC30 <- DpCout1$DpC30_39 %>%
  mutate(ag = "ag2")%>%
  full_join(lcmm_out1$logr$`30_39`, by = "YsncS")
DpC40 <- DpCout1$DpC40_49 %>%
  mutate(ag = "ag3")%>%
  full_join(lcmm_out1$logr$`40_49`, by = "YsncS")
DpC_df_1 <- rbind(DpC20, DpC30, DpC40)
DpC_df_1 <- DpC_df_1 %>%
  filter(!is.na(cid))
labels <- c(`ag1` = "20-29", `ag2` = "30-39", `ag3` = "40-49")

tr_model_wrap_screen1 <- ggplot(DpC_df_1, aes(x = YsncS, y = logRate)) +
  theme_minimal() +
  ggtitle("Transition rate from HPV to cervical cancer given high screening coverage") +
  geom_line(aes(col = Location), size = 0.75) +
  scale_color_manual(values = col) +
  facet_wrap(~ag, labeller = labeller(ag = labels), nrow = 1) +
  theme(strip.text.x = element_text(size = 20, face = "bold")) +
  geom_ribbon(aes(x = YsncS, ymin = logr_lo, ymax = logr_hi), fill = 'azure4', alpha = 0.4)+
  labs(y = "log Incidence Rate in hrHPV+ women", 
       x = "Years since HPV detection") +
  ylim(-10.2, -2.5)  +
  xlim(0, 11) +
  theme(title = element_text(size = 18, face = "bold"),
        legend.title = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 14),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 12),
        legend.key.height = unit(0.8, "cm"),
        legend.key.width = unit(1.2, "cm")) +
  geom_line(aes(x = YsncS, y =  logr), col = "black", size = 1.7, linetype = 1) 


#lcmm_out$tr_lambda$
df_g <- data.frame(lcmm_out1$tr_lambda$`20_24`)
df_lambda <- df_g %>%
  mutate("20_24" = lambda,
         "25_29" = lcmm_out1$tr_lambda$`25_29`[, 2],
         "30_34" = lcmm_out1$tr_lambda$`30_34`[, 2],
         "35_39" = lcmm_out1$tr_lambda$`35_39`[, 2],
         "40_44" = lcmm_out1$tr_lambda$`40_44`[, 2],
         "20_29" = lcmm_out1$tr_lambda$`20_29`[, 2],
         "30_39" = lcmm_out1$tr_lambda$`30_39`[, 2],
         "40_49" = lcmm_out1$tr_lambda$`40_49`[, 2]) %>%
  select(-lambda) %>%
  gather("AgeGroup", "lambda", 2:9) 
df_lambda$AgeGroup <- factor(df_lambda$AgeGroup, levels = c("20_24", "25_29", "30_34","35_39", "40_44", "20_29", "30_39", "40_49"),  
                             labels = c("20-24", "25-29", "30-34", "35-39","40-44", "20-29", "30-39", "40-49"))

cols <- c('#de2d26','#fb6a4a','#fc9272','#fcbba1','#fee5d9','#54278f', '#998ec3','#d8daeb')
line <- c("solid", "solid", "solid", "solid", "solid", "dashed",  "dashed", "dashed")

plot_lambda_screen1 <-  ggplot(df_lambda, aes(YsncS, lambda)) +
  geom_smooth(aes(color = AgeGroup,  linetype = AgeGroup), se = FALSE) +
  scale_linetype_manual(values = line) +
  scale_color_manual(values = cols) +
  labs(title = "Change in cervical cancer transition rate given high screening coverage",
       subtitle = "per woman tested hrHPV+ at a given age, estimated in ecological study.",
       x = "Years since hrHPV detection", 
       y = "Change in cervical cancer transition rate(ln)",
       color = "Age at hrHPV detection",
       linetype = "Age at hrHPV detection") + 
  xlim(0, 15) +
  theme_classic() +
  guides(color = guide_legend(ncol = 2, label.hjust = 1),
         linetype = guide_legend(ncol = 2, label.hjust = 1)) +
  theme(plot.title=element_text(size=20,
                                face = "bold"),
        plot.subtitle = element_text(size = 16),
        plot.margin = unit(c(0.5, 0.5, 1, 1), "cm")) +
  theme(legend.title = element_text(size=16, face="bold"),
        legend.text = element_text(size = 14),
        legend.justification= "center",
        legend.position = c(0.8, 0.7),
        legend.key.size = unit(c(0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8), "cm"),
        legend.key.width = unit(c(1.5 ), "cm")) +
  theme(axis.title.x = element_text(face="bold", size=18),
        axis.text.x  = element_text(size=16),
        axis.title.y = element_text(face="bold", size=18),
        axis.text.y  = element_text(size=16)) 


#### Loop 2: medium sceening coverage ####
Dpop_2 <- Dpop %>%
  filter(country %in% c("Colombia", "Chile", "Argentina", "Republic of Korea", "Thailand"))
info_2 <- info %>%
  filter(country %in% c("Colombia", "Chile", "Argentina", "Republic of Korea", "Thailand"))

ggout2 <- list()
DpCout2 <- list()
lcmm_out2 <- list("tr_lcmm", "tr_bic", "tr_pred", "tr_prob", "tr_conv", "tr_lambda")
Dp_out2 <- list()


for(d in c(4, 9)){
  cat("\n ageint: ", d) # locating error
  #browser()
  for(f in c(20, 25, 30, 35, 40, 45)) {
    cat("\n age.beg: ", f)
    mina <- f 
    maxa <- sum(f, d)
    
    Dp <- data.frame(matrix(nrow = 0, ncol = 16)) # Dp = Data population (as nested in of Dpop_2)
    colnames(Dp) <- colnames(Dpop_2)
    
    for(j in info_2$cid){
      sist <- (2012 - as.numeric(info_2$stY[info_2$cid == j])) # nb of years since prevalence Study
      if(info_2$stY[info_2$cid == j] < 2012){ # excluding Iran (in 2014)
        # cat("\n cid: ", j) # locating error
        for(i in 0:sist){
          a <- (mina+i):(maxa+i) # moving age group by one year
          y <- as.numeric(Dpop_2$stY) + i # i years added to year of prev. Study
          Dp <- rbind(Dp, Dpop_2[Dpop_2$age %in% a & Dpop_2$Year==y & Dpop_2$cid== j, ])
        }
      }
    }
    Dp <- Dp[!is.na(Dp$IR), ]
    # Years since Prevalence Study:
    Dp <- Dp %>%
      mutate(YsncS = as.numeric(Dp$Year) - as.numeric(Dp$stY)) 
    Dp_out2[[stringr::str_c("Dp", mina, "_", maxa)]] <- Dp
    #data frame DpC: cumulative rates for full cohort per country, year (not over full time period) ####
    # Sum all cases and women of all ages (in previously selected age group) in each Year and country (location, cid)
    DpC <- Dp %>%
      ungroup() %>%
      group_by(YsncS, cid, Location) %>% 
      summarise(Npos = sum(Npos), 
                Nicc = sum(Nicc), 
                se = sqrt(sum(Nicc))/sum(Npos), # standard error of rates = rate^2/cases = sqrt(cases)/personyears (Esteve p. 52)
                logRate = log(sum(Nicc)/sum(Npos)), 
                selog = sqrt(1/sum(Npos))) %>% # var(log(k/m)) = 1/(Rate*pyears); se(log(k/m)) = sqrt(1/k). Esteve p. 53
      filter(Location != "Uganda") %>%
      filter(Location != "Viet Nam")         
    DpC$Location <- as.factor(DpC$Location)
    DpC$cid <- as.factor(DpC$cid)
    DpC[which(DpC$Nicc <= 0 | DpC$Npos <= 0 ), c("Nicc", "Npos", "logRate", "se")] <- NA # to avoid -Inf when Nicc = 0
    DpCout2[[stringr::str_c("DpC", mina, "_", maxa)]] <- DpC # to have all age cohort for mixed model
    
    
    #11.Plot 
    
    col <- c('#d8b365', '#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#67001f', '#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#003c30','#b15928', '#dd1c77', '#1c9099')
    DpCplot <- ggplot2::ggplot(DpC, aes(x = YsncS, y = logRate)) + 
      geom_point(aes(color = Location)) + 
      geom_line(aes(color = Location)) +
      scale_color_manual(values = col) +
      ggtitle(stringr::str_c("cohort of HPV+ women ", mina, "-", maxa, "y")) +
      ylab("cervical cancer inidence Rate in HPV+ women") +
      xlab("Years since HPV detection") +
      ylim(-10.5, -2) +
      geom_linerange(aes(x = YsncS, ymin = logRate - selog, ymax = logRate + selog, color = Location)) + # add standard error bars
      theme_bw()
    ggout2$DpC_plot[[stringr::str_c(mina, "_", maxa)]] <- DpCplot

    
    
    #12. Model
    
    # ln(Nicc/Npos) = alpha + beta*YsncS
    # beta is rate of change 
    nd <- data.frame(YsncS = 0:18)
    
    # latent class mixed model
    # par(mfrow = c(2, 4))
    # select age group from loop
    
    lcmm_out2$tr_lcmm[[stringr::str_c(mina, "_", maxa)]] <- 
      tr_lcmm <- lcmm(logRate ~ YsncS, random = ~ YsncS, subject = "Location",  ng = 1, data = DpC, link = "4-equi-splines")
    
    # bic
    lcmm_out2$tr_bic[[stringr::str_c(mina, "_", maxa)]] <- 
      bic <- round(tr_lcmm$BIC, 2)
    
    # predictions
    lcmm_out2$tr_pred[[stringr::str_c(mina, "_", maxa)]] <- 
      tr_pred <- predictY(tr_lcmm, nd, methInteg = 1, draws = TRUE)
    
    # groups
    lcmm_out2$tr_prob[[stringr::str_c(mina, "_", maxa)]] <- tr_lcmm$pprob
    
    # convergence criteria
    lcmm_out2$tr_conv[[stringr::str_c(mina, "_", maxa)]] <- tr_lcmm$conv
    
    ##`slope
    lcmm_out2$tr_lambda[[stringr::str_c(mina, "_", maxa)]] <- # 1 gamma per year
      tr_lambda <- data.frame("YsncS" = tr_pred$times[2:19,]-0.5, 
                              "lambda" = diff(tr_pred$pred[, 1]))
    
    lcmm_out2$logr[[stringr::str_c(mina, "_", maxa)]] <- 
      m_logr <- data.frame("logr" = tr_pred$pred[, 1], 
                           "logr_lo" = tr_pred$pred[, 2],
                           "logr_hi" = tr_pred$pred[, 3],
                           "YsncS" = tr_pred$times$YsncS)
    DpC <- DpC %>%
      full_join(., m_logr, by = "YsncS")
    
    col <- c('#d8b365', '#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#67001f', '#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#003c30','#b15928', '#dd1c77', '#1c9099')
    
    ggout2$tr_plot[[stringr::str_c(mina, "_", maxa)]] <- 
      ggplot2::ggplot(DpC, aes(x = YsncS, y = logRate)) + 
      theme_bw() + 
      #geom_point(aes(color = Location)) + 
      geom_ribbon(aes(x = YsncS, ymin = logr_lo, ymax = logr_hi), fill = 'azure4', alpha = 0.4)+
      geom_line(aes(color = Location), size = 0.75) +
      scale_color_manual(values = col) +
      geom_line(aes(x = YsncS, y =  logr), col = "black", size = 1.7, linetype = 1) +
      #ggtitle(stringr::str_c(mina, "_", maxa, "y")) +
      labs(y = "log Incidence Rate in hrHPV+ women", 
           x = "Years since HPV detection"
           #, caption = "Mixed Model (CI)"
      ) +
      ylim(-10.5, -2.5)  +
      guides(color = guide_legend(ncol = 2)) +
      theme(legend.title = element_text(size = 16),
            legend.text = element_text(size = 14),
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 14),
            legend.key.height = unit(0.8, "cm"),
            legend.key.width = unit(1.2, "cm")) +
      annotate("text", x = 13, y = -9, label = stringr::str_c(mina, "-", maxa, "y"), size = 12) 
    # geom_linerange(aes(x = YsncS, ymin = logRate - selog, ymax = logRate + selog, color = Location)) + # add standard error bars
    #geom_point(aes(x = YsncS, y =  logr), col = "red") +
    
  }
}



### wrap plot cpnsecutive ages ##

DpC20 <- DpCout2$DpC20_29 %>%
  mutate(ag = "ag1") %>%
  full_join(lcmm_out2$logr$`20_29`, by = "YsncS")
DpC30 <- DpCout2$DpC30_39 %>%
  mutate(ag = "ag2")%>%
  full_join(lcmm_out2$logr$`30_39`, by = "YsncS")
DpC40 <- DpCout2$DpC40_49 %>%
  mutate(ag = "ag3")%>%
  full_join(lcmm_out2$logr$`40_49`, by = "YsncS")
DpC_df <- rbind(DpC20, DpC30, DpC40)
DpC_df <- DpC_df %>%
  filter(!is.na(cid))
labels <- c(`ag1` = "20-29", `ag2` = "30-39", `ag3` = "40-49")

tr_model_wrap_screen2 <- ggplot(DpC_df, aes(x = YsncS, y = logRate)) +
  theme_minimal() +
  ggtitle("Transition rate from HPV to cervical cancer given medium screening coverage") +
  geom_line(aes(col = Location), size = 0.75) +
  scale_color_manual(values = col) +
  facet_wrap(~ag, labeller = labeller(ag = labels), nrow = 1) +
  theme(strip.text.x = element_text(size = 20, face = "bold")) +
  geom_ribbon(aes(x = YsncS, ymin = logr_lo, ymax = logr_hi), fill = 'azure4', alpha = 0.4)+
  labs(y = "log Incidence Rate in hrHPV+ women", 
       x = "Years since HPV detection") +
  ylim(-10.2, -2.5)  +
  xlim(0, 11) +
  theme(title = element_text(size = 18, face = "bold"),
        legend.title = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 14),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 12),
        legend.key.height = unit(0.8, "cm"),
        legend.key.width = unit(1.2, "cm")) +
  geom_line(aes(x = YsncS, y =  logr), col = "black", size = 1.7, linetype = 1) 


#lcmm_out$tr_lambda$
df_g <- data.frame(lcmm_out2$tr_lambda$`20_24`)
df_lambda <- df_g %>%
  mutate("20_24" = lambda,
         "25_29" = lcmm_out2$tr_lambda$`25_29`[, 2],
         "30_34" = lcmm_out2$tr_lambda$`30_34`[, 2],
         "35_39" = lcmm_out2$tr_lambda$`35_39`[, 2],
         "40_44" = lcmm_out2$tr_lambda$`40_44`[, 2],
         "20_29" = lcmm_out2$tr_lambda$`20_29`[, 2],
         "30_39" = lcmm_out2$tr_lambda$`30_39`[, 2],
         "40_49" = lcmm_out2$tr_lambda$`40_49`[, 2]) %>%
  select(-lambda) %>%
  gather("AgeGroup", "lambda", 2:9) 
df_lambda$AgeGroup <- factor(df_lambda$AgeGroup, levels = c("20_24", "25_29", "30_34","35_39", "40_44", "20_29", "30_39", "40_49"),  
                             labels = c("20-24", "25-29", "30-34", "35-39","40-44", "20-29", "30-39", "40-49"))

cols <- c('#de2d26','#fb6a4a','#fc9272','#fcbba1','#fee5d9','#54278f', '#998ec3','#d8daeb')
line <- c("solid", "solid", "solid", "solid", "solid", "dashed",  "dashed", "dashed")

plot_lambda_screen2 <-  ggplot(df_lambda, aes(YsncS, lambda)) +
  geom_smooth(aes(color = AgeGroup,  linetype = AgeGroup), se = FALSE) +
  scale_linetype_manual(values = line) +
  scale_color_manual(values = cols) +
  labs(title = "Change in cervical cancer transition rate given medium screening coverage",
       subtitle = "per woman tested hrHPV+ at a given age, estimated in ecological study.",
       x = "Years since hrHPV detection", 
       y = "Change in cervical cancer transition rate(ln)",
       color = "Age at hrHPV detection",
       linetype = "Age at hrHPV detection") + 
  xlim(0, 15) +
  theme_classic() +
  guides(color = guide_legend(ncol = 2, label.hjust = 1),
         linetype = guide_legend(ncol = 2, label.hjust = 1)) +
  theme(plot.title=element_text(size=20,
                                face = "bold"),
        plot.subtitle = element_text(size = 16),
        plot.margin = unit(c(0.5, 0.5, 1, 1), "cm")) +
  theme(legend.title = element_text(size=16, face="bold"),
        legend.text = element_text(size = 14),
        legend.justification= "center",
        legend.position = c(0.8, 0.7),
        legend.key.size = unit(c(0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8), "cm"),
        legend.key.width = unit(c(1.5 ), "cm")) +
  theme(axis.title.x = element_text(face="bold", size=18),
        axis.text.x  = element_text(size=16),
        axis.title.y = element_text(face="bold", size=18),
        axis.text.y  = element_text(size=16)) 









#### Loop 3: Low screening coverage ####

Dpop_3 <- Dpop %>%
  #filter(Location != "China(Shanxi)") %>%
  filter(country %in% c("Algeria", "China", "India", "Vietnam", "Costa Rica", "Poland"))
info_3 <- info %>%
  #filter(Location != "China(Shanxi)") %>%
  filter(country %in% c("Algeria", "China", "India", "Vietnam", "Costa Rica", "Poland"))


ggout3 <- list()
DpCout3 <- list()
lcmm_out3 <- list("tr_lcmm", "tr_bic", "tr_pred", "tr_prob", "tr_conv", "tr_lambda")
Dp_out3 <- list()


for(d in c(4, 9)){
  cat("\n ageint: ", d) # locating error
  #browser()
  for(f in c(20, 25, 30, 35, 40, 45)) {
    cat("\n age.beg: ", f)
    mina <- f 
    maxa <- sum(f, d)
    
    Dp <- data.frame(matrix(nrow = 0, ncol = 16)) # Dp = Data population (as nested in of Dpop_3)
    colnames(Dp) <- colnames(Dpop_3)
    
    for(j in info_3$cid){
      sist <- (2012 - as.numeric(info_3$stY[info_3$cid == j])) # nb of years since prevalence Study
      if(info_3$stY[info_3$cid == j] < 2012){ # excluding Iran (in 2014)
        # cat("\n cid: ", j) # locating error
        for(i in 0:sist){
          a <- (mina+i):(maxa+i) # moving age group by one year
          y <- as.numeric(Dpop_3$stY) + i # i years added to year of prev. Study
          Dp <- rbind(Dp, Dpop_3[Dpop_3$age %in% a & Dpop_3$Year==y & Dpop_3$cid== j, ])
        }
      }
    }
    Dp <- Dp[!is.na(Dp$IR), ]
    # Years since Prevalence Study:
    Dp <- Dp %>%
      mutate(YsncS = as.numeric(Dp$Year) - as.numeric(Dp$stY)) 
    Dp_out3[[stringr::str_c("Dp", mina, "_", maxa)]] <- Dp
    #data frame DpC: cumulative rates for full cohort per country, year (not over full time period) ####
    # Sum all cases and women of all ages (in previously selected age group) in each Year and country (location, cid)
    DpC <- Dp %>%
      ungroup() %>%
      group_by(YsncS, cid, Location) %>% 
      summarise(Npos = sum(Npos), 
                Nicc = sum(Nicc), 
                se = sqrt(sum(Nicc))/sum(Npos), # standard error of rates = rate^2/cases = sqrt(cases)/personyears (Esteve p. 52)
                logRate = log(sum(Nicc)/sum(Npos)), 
                selog = sqrt(1/sum(Npos))) %>% # var(log(k/m)) = 1/(Rate*pyears); se(log(k/m)) = sqrt(1/k). Esteve p. 53
      filter(Location != "Uganda") %>%
      filter(Location != "Viet Nam")         
    DpC$Location <- as.factor(DpC$Location)
    DpC$cid <- as.factor(DpC$cid)
    DpC[which(DpC$Nicc <= 0 | DpC$Npos <= 0 ), c("Nicc", "Npos", "logRate", "se")] <- NA # to avoid -Inf when Nicc = 0
    DpCout3[[stringr::str_c("DpC", mina, "_", maxa)]] <- DpC # to have all age cohort for mixed model
    
    
    ####`11.Plot ####
    
    col <- c('#d8b365', '#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#67001f', '#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#003c30','#b15928', '#dd1c77', '#1c9099')
    DpCplot <- ggplot2::ggplot(DpC, aes(x = YsncS, y = logRate)) + 
      geom_point(aes(color = Location)) + 
      geom_line(aes(color = Location)) +
      scale_color_manual(values = col) +
      ggtitle(stringr::str_c("cohort of HPV+ women ", mina, "-", maxa, "y")) +
      ylab("cervical cancer inidence Rate in HPV+ women") +
      xlab("Years since HPV detection") +
      ylim(-10.5, -2) +
      geom_linerange(aes(x = YsncS, ymin = logRate - selog, ymax = logRate + selog, color = Location)) + # add standard error bars
      theme_bw()
    ggout3$DpC_plot[[stringr::str_c(mina, "_", maxa)]] <- DpCplot
    
    
 
    
    
    
    #12. Model
    
    # ln(Nicc/Npos) = alpha + beta*YsncS
    # beta is rate of change 
    nd <- data.frame(YsncS = 0:18)
    
    # latent class mixed model
    # par(mfrow = c(2, 4))
    # select age group from loop
    
    lcmm_out3$tr_lcmm[[stringr::str_c(mina, "_", maxa)]] <- 
      tr_lcmm <- lcmm(logRate ~ YsncS, random = ~ YsncS, subject = "Location",  ng = 1, data = DpC, link = "4-equi-splines")
    
    # bic
    lcmm_out3$tr_bic[[stringr::str_c(mina, "_", maxa)]] <- 
      bic <- round(tr_lcmm$BIC, 2)
    
    # predictions
    lcmm_out3$tr_pred[[stringr::str_c(mina, "_", maxa)]] <- 
      tr_pred <- predictY(tr_lcmm, nd, methInteg = 1, draws = TRUE)
    
    # groups
    lcmm_out3$tr_prob[[stringr::str_c(mina, "_", maxa)]] <- tr_lcmm$pprob
    
    # convergence criteria
    lcmm_out3$tr_conv[[stringr::str_c(mina, "_", maxa)]] <- tr_lcmm$conv
    
    ##`slope
    lcmm_out3$tr_lambda[[stringr::str_c(mina, "_", maxa)]] <- # 1 gamma per year
      tr_lambda <- data.frame("YsncS" = tr_pred$times[2:19,]-0.5, 
                              "lambda" = diff(tr_pred$pred[, 1]))
    
    lcmm_out3$logr[[stringr::str_c(mina, "_", maxa)]] <- 
      m_logr <- data.frame("logr" = tr_pred$pred[, 1], 
                           "logr_lo" = tr_pred$pred[, 2],
                           "logr_hi" = tr_pred$pred[, 3],
                           "YsncS" = tr_pred$times$YsncS)
    DpC <- DpC %>%
      full_join(., m_logr, by = "YsncS")
    
    col <- c('#d8b365', '#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#67001f', '#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#003c30','#b15928', '#dd1c77', '#1c9099')
    
    ggout3$tr_plot[[stringr::str_c(mina, "_", maxa)]] <- 
      ggplot2::ggplot(DpC, aes(x = YsncS, y = logRate)) + 
      theme_bw() + 
      #geom_point(aes(color = Location)) + 
      geom_ribbon(aes(x = YsncS, ymin = logr_lo, ymax = logr_hi), fill = 'azure4', alpha = 0.4)+
      geom_line(aes(color = Location), size = 0.75) +
      scale_color_manual(values = col) +
      geom_line(aes(x = YsncS, y =  logr), col = "black", size = 1.7, linetype = 1) +
      #ggtitle(stringr::str_c(mina, "_", maxa, "y")) +
      labs(y = "log Incidence Rate in hrHPV+ women", 
           x = "Years since HPV detection"
           #, caption = "Mixed Model (CI)"
      ) +
      ylim(-10.5, -2.5)  +
      guides(color = guide_legend(ncol = 2)) +
      theme(legend.title = element_text(size = 16),
            legend.text = element_text(size = 14),
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 14),
            legend.key.height = unit(0.8, "cm"),
            legend.key.width = unit(1.2, "cm")) +
      annotate("text", x = 13, y = -9, label = stringr::str_c(mina, "-", maxa, "y"), size = 12) 
    # geom_linerange(aes(x = YsncS, ymin = logRate - selog, ymax = logRate + selog, color = Location)) + # add standard error bars
    #geom_point(aes(x = YsncS, y =  logr), col = "red") +
    
  }
}


### wrap plot cpnsecutive ages ##

DpC20 <- DpCout3$DpC20_29 %>%
  mutate(ag = "ag1") %>%
  full_join(lcmm_out3$logr$`20_29`, by = "YsncS")
DpC30 <- DpCout3$DpC30_39 %>%
  mutate(ag = "ag2")%>%
  full_join(lcmm_out3$logr$`30_39`, by = "YsncS")
DpC40 <- DpCout2$DpC40_49 %>%
  mutate(ag = "ag3")%>%
  full_join(lcmm_out3$logr$`40_49`, by = "YsncS")
DpC_df <- rbind(DpC20, DpC30, DpC40)
DpC_df <- DpC_df %>%
  filter(!is.na(cid))
labels <- c(`ag1` = "20-29", `ag2` = "30-39", `ag3` = "40-49")

tr_model_wrap_screen3 <- ggplot(DpC_df, aes(x = YsncS, y = logRate)) +
  theme_minimal() +
  ggtitle("Transition rate from HPV to cervical cancer given low screening coverage") +
  geom_line(aes(col = Location), size = 0.75) +
  scale_color_manual(values = col) +
  facet_wrap(~ag, labeller = labeller(ag = labels), nrow = 1) +
  theme(strip.text.x = element_text(size = 20, face = "bold")) +
  geom_ribbon(aes(x = YsncS, ymin = logr_lo, ymax = logr_hi), fill = 'azure4', alpha = 0.4)+
  labs(y = "log Incidence Rate in hrHPV+ women", 
       x = "Years since HPV detection") +
  ylim(-10.2, -2.5)  +
  xlim(0, 11) +
  theme(title = element_text(size = 18, face = "bold"),
        legend.title = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 14),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 12),
        legend.key.height = unit(0.8, "cm"),
        legend.key.width = unit(1.2, "cm")) +
  geom_line(aes(x = YsncS, y =  logr), col = "black", size = 1.7, linetype = 1) 


#lcmm_out$tr_lambda$
df_g <- data.frame(lcmm_out3$tr_lambda$`20_24`)
df_lambda <- df_g %>%
  mutate("20_24" = lambda,
         "25_29" = lcmm_out3$tr_lambda$`25_29`[, 2],
         "30_34" = lcmm_out3$tr_lambda$`30_34`[, 2],
         "35_39" = lcmm_out3$tr_lambda$`35_39`[, 2],
         "40_44" = lcmm_out3$tr_lambda$`40_44`[, 2],
         "20_29" = lcmm_out3$tr_lambda$`20_29`[, 2],
         "30_39" = lcmm_out3$tr_lambda$`30_39`[, 2],
         "40_49" = lcmm_out3$tr_lambda$`40_49`[, 2]) %>%
  select(-lambda) %>%
  gather("AgeGroup", "lambda", 2:9) 
df_lambda$AgeGroup <- factor(df_lambda$AgeGroup, levels = c("20_24", "25_29", "30_34","35_39", "40_44", "20_29", "30_39", "40_49"),  
                             labels = c("20-24", "25-29", "30-34", "35-39","40-44", "20-29", "30-39", "40-49"))

cols <- c('#de2d26','#fb6a4a','#fc9272','#fcbba1','#fee5d9','#54278f', '#998ec3','#d8daeb')
line <- c("solid", "solid", "solid", "solid", "solid", "dashed",  "dashed", "dashed")

plot_lambda_screen3 <-  ggplot(df_lambda, aes(YsncS, lambda)) +
  geom_smooth(aes(color = AgeGroup,  linetype = AgeGroup), se = FALSE) +
  scale_linetype_manual(values = line) +
  scale_color_manual(values = cols) +
  labs(title = "Change in cervical cancer transition rate given low screening coverage",
       subtitle = "per woman tested hrHPV+ at a given age, estimated in ecological study.",
       x = "Years since hrHPV detection", 
       y = "Change in cervical cancer transition rate(ln)",
       color = "Age at hrHPV detection",
       linetype = "Age at hrHPV detection") + 
  xlim(0, 15) +
  theme_classic() +
  guides(color = guide_legend(ncol = 2, label.hjust = 1),
         linetype = guide_legend(ncol = 2, label.hjust = 1)) +
  theme(plot.title=element_text(size=20,
                                face = "bold"),
        plot.subtitle = element_text(size = 16),
        plot.margin = unit(c(0.5, 0.5, 1, 1), "cm")) +
  theme(legend.title = element_text(size=16, face="bold"),
        legend.text = element_text(size = 14),
        legend.justification= "center",
        legend.position = c(0.8, 0.7),
        legend.key.size = unit(c(0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8), "cm"),
        legend.key.width = unit(c(1.5 ), "cm")) +
  theme(axis.title.x = element_text(face="bold", size=18),
        axis.text.x  = element_text(size=16),
        axis.title.y = element_text(face="bold", size=18),
        axis.text.y  = element_text(size=16)) 



#### all screening groups: wrap plot cpnsecutive ages ####
screen <- data.frame("country" = info$country, "cid" = as.factor(info$cid)) 
screen[screen$country %in% info_1$country, "cov"] <- 1
screen[screen$country %in% info_2$country, "cov"] <- 2
screen[screen$country %in% info_3$country, "cov"] <- 3
screen$cov <- factor(screen$cov, levels = c(1, 2, 3), labels = c("high", "medium", "low"))


DpC20 <- DpCout$DpC20_29 %>%
  mutate(ag = "ag1") %>%
  right_join(., screen[!is.na(screen$cov),], by = "cid")
DpC20_h <- merge(DpC20[DpC20$cov == "high", ], lcmm_out1$logr$`20_29`, by = "YsncS")
DpC20_m <- merge(DpC20[DpC20$cov == "medium", ], lcmm_out2$logr$`20_29`, by = "YsncS")
DpC20_l  <- merge(DpC20[DpC20$cov == "low", ], lcmm_out3$logr$`20_29`, by = "YsncS")
DpC20 <- rbind(DpC20_h, DpC20_m, DpC20_l)

DpC30 <- DpCout$DpC30_39 %>%
  mutate(ag = "ag2")%>%
  right_join(., screen[!is.na(screen$cov),], by = "cid")
DpC30_h <- merge(DpC30[DpC30$cov == "high", ], lcmm_out1$logr$`30_39`, by = "YsncS")
DpC30_m <- merge(DpC30[DpC30$cov == "medium", ], lcmm_out2$logr$`30_39`, by = "YsncS")
DpC30_l  <- merge(DpC30[DpC30$cov == "low", ], lcmm_out3$logr$`30_39`, by = "YsncS")
DpC30 <- rbind(DpC30_h, DpC30_m, DpC30_l)

DpC40 <- DpCout$DpC40_49 %>%
  mutate(ag = "ag3")%>%
  right_join(., screen[!is.na(screen$cov),], by = "cid")
DpC40_h <- merge(DpC40[DpC40$cov == "high", ], lcmm_out1$logr$`40_49`, by = "YsncS")
DpC40_m <- merge(DpC40[DpC40$cov == "medium", ], lcmm_out2$logr$`40_49`, by = "YsncS")
DpC40_l  <- merge(DpC40[DpC40$cov == "low", ], lcmm_out3$logr$`40_49`, by = "YsncS")
DpC40 <- rbind(DpC40_h, DpC40_m, DpC40_l)

DpC_df <- rbind(DpC20, DpC30, DpC40)
labels <- c(`ag1` = "20-29", `ag2` = "30-39", `ag3` = "40-49")


ggplot(DpC_df, aes(x = YsncS, y = logRate)) +
  theme_minimal() +
  ggtitle("Transition rate from HPV to cervical cancer by age and screening coverage") +
  geom_line(aes(col = Location), size = 0.75) +
  scale_color_manual(values = col) +
  geom_line(aes(x = YsncS, y =  logr), col = "black", size = 1.7, linetype = 1) +
  facet_wrap(~ cov + ag, labeller = labeller(ag = labels), nrow = 3, ncol = 3) +
  theme(strip.text.x = element_text(size = 12, face = "bold")) +
  geom_ribbon(aes(x = YsncS, ymin = logr_lo, ymax = logr_hi), fill = 'azure4', alpha = 0.4)+
  labs(y = "log Incidence Rate in hrHPV+ women", 
       x = "Years since HPV detection") +
  ylim(-10.2, -2.5)  +
  xlim(0, 11) +
  theme(title = element_text(size = 18, face = "bold"),
        legend.title = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 14),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 12),
        legend.key.height = unit(0.8, "cm"),
        legend.key.width = unit(1.2, "cm"))  






####`12.   Plot all lambda ####`
#lcmm_out$tr_lambda$
df_g <- data.frame(lcmm_out$tr_lambda$`20_24`)
df_lambda <- df_g %>%
  mutate("20_24" = lambda,
         "25_29" = lcmm_out$tr_lambda$`25_29`[, 2],
         "30_34" = lcmm_out$tr_lambda$`30_34`[, 2],
         "35_39" = lcmm_out$tr_lambda$`35_39`[, 2],
         "40_44" = lcmm_out$tr_lambda$`40_44`[, 2],
         "20_29" = lcmm_out$tr_lambda$`20_29`[, 2],
         "30_39" = lcmm_out$tr_lambda$`30_39`[, 2],
         "40_49" = lcmm_out$tr_lambda$`40_49`[, 2]) %>%
  select(-lambda) %>%
  gather("AgeGroup", "lambda", 2:9) 
df_lambda$AgeGroup <- factor(df_lambda$AgeGroup, levels = c("20_24", "25_29", "30_34","35_39", "40_44", "20_29", "30_39", "40_49"),  
                             labels = c("20-24", "25-29", "30-34", "35-39","40-44", "20-29", "30-39", "40-49"))

cols <- c('#de2d26','#fb6a4a','#fc9272','#fcbba1','#fee5d9','#54278f', '#998ec3','#d8daeb')
line <- c("solid", "solid", "solid", "solid", "solid", "dashed",  "dashed", "dashed")

plot_lambda<-  ggplot(df_lambda, aes(YsncS, lambda)) +
  geom_smooth(aes(color = AgeGroup,  linetype = AgeGroup), se = FALSE) +
  scale_linetype_manual(values = line) +
  scale_color_manual(values = cols) +
  labs(title = "Change in cervical cancer transition rate given low screening coverage",
       subtitle = "per woman tested hrHPV+ at a given age, estimated in ecological study.",
       x = "Years since hrHPV detection", 
       y = "Change in cervical cancer transition rate(ln)",
       color = "Age at hrHPV detection",
       linetype = "Age at hrHPV detection") + 
  xlim(0, 15) +
  theme_classic() +
  guides(color = guide_legend(ncol = 2, label.hjust = 1),
         linetype = guide_legend(ncol = 2, label.hjust = 1)) +
  theme(plot.title=element_text(size=20,
                                face = "bold"),
        plot.subtitle = element_text(size = 16),
        plot.margin = unit(c(0.5, 0.5, 1, 1), "cm")) +
  theme(legend.title = element_text(size=16, face="bold"),
        legend.text = element_text(size = 14),
        legend.justification= "center",
        legend.position = c(0.8, 0.7),
        legend.key.size = unit(c(0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8), "cm"),
        legend.key.width = unit(c(1.5 ), "cm")) +
  theme(axis.title.x = element_text(face="bold", size=18),
        axis.text.x  = element_text(size=16),
        axis.title.y = element_text(face="bold", size=18),
        axis.text.y  = element_text(size=16)) 


