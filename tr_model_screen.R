
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

info <- data.frame(matrix(nrow = 17, ncol = 0))
info <- info %>%
  mutate("cid" = c(1,  #2,  
                   5,  6,  8,  9, 10, 12, 13, 15,  3, 16, 17, 
                   #18,  
                   4, 20, 19, 22, 71), # assigned by Rosa
         "sgcentre" = c(44, #100, 
                        19, 12, 23, 18, 61, 15,  2,  7,  9, 14,  3,  
                        #3, 
                        16, 83,  4, 41, 20), # defined by IARC prevalence studies
         "stY" = c("2007-2008", #"2002-2004", 
                   "1993-1994", "1993-1995", "2005", "2004", "2013-2014", "1999-2000", "1997", "1997-1998", 
                   "1998", "1990-2000", "1998", 
                   #"1998", 
                   "2001", "2002", "1995-1998", "2006", "2004"),
         "loc.prev" = c("Algeria, Setif",#"Uganda, Kyadondo County", 
                        "Costa Rica",                
                        "Colombia, Bucaramanga","China, Shenyang","India, Dindigul, Ambilikkai",                  
                        "Iran (Islamic Republic of), Golestan Province", "Republic of Korea, Busan",   "Viet Nam, Ho Chi Minh City",                   
                        "Thailand, Lampang","Argentina, Entre Rios Province", "Thailand, Songkhla",        
                        "Spain", 
                        #"Spain, Girona", 
                        "Chile, Region of Antofagasta",                 
                        "Italy, Turin", "The Netherlands", "Poland, Poznan", "China, Shanxi"),  # Prevalence STudy location
         "country" =  c("Algeria", #"Uganda", 
                        "Costa Rica", "Colombia", "China", "India", "Iran (Islamic Republic of)",  "Republic of Korea", "Viet Nam", "Thailand", # for mortality, population
                        "Argentina", "Thailand", "Spain", 
                        #"Spain", 
                        "Chile", "Italy", "Netherlands", "Poland", "China"),
         "Location" = c("Algeria", #"Uganda", 
                        "Costa Rica", "Colombia", "China(Shenyang)", "India", "Iran",  "Republic of Korea", "Viet Nam", "Thailand(Lampang)", # for graph
                        "Argentina", "Thailand(Songkhla)", "Spain", 
                        #"Spain (Girona)", 
                        "Chile", "Italy", "Netherlands", "Poland", "China(Shanxi)"),
         #"cov" = c(3, #3, 
        #           3, 2, 3, 3, 3, 2, 3, 2, 2, 2, 1, 2, 1, 1, 3, 3), # three screening groups (>70%, 50-70%, <50%)
         "cov" = c(2, #2, 
                   2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 2, 1, 1, 2, 2),
         
         "REG.VIII" = c(NA, # IRR Algeria, S?tif 
                        #NA, # IRR Uganda, Kyadondo County 
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
                        #NA, # IRR Uganda, Kyadondo County 
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
                      #NA, # IRR Uganda, Kyadondo County 
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
                     #80002, # Uganda, Kyadondo county (2003-2007)
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
                     #15639), #China, Yangcheng County (2003-2007) 
                     15606), #China, Cixian County (2003-2007) 
         "REG.XI" = c(101200199, # *Algeria, S?tif (2008-2011)
                      #180000299, # *Uganda, Kyadondo County (2008-2012)
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
                      #415600899)) %>% #China, Linzhou County (2008-2012)
                      415600699)) %>% # China, Cixian County (2008-2012)
  separate(stY, into = paste0("stY", c(0, "")))
info$stY[info$cid %in% c(8, 9, 13, 3, 17, 4, 20, 22, 71)] <- c(2005, 2004, 1997, 1998, 2000, 2003, 2002, 2006, 2004) # filling NA if only one study year
# info$cov <- factor(info$cov, levels = c(1, 2, 3), labels = c("high", "medium", "low"))


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
  full_join(., info[, c("sgcentre", "stY", "cid", "Location", "cov")], by = c("cid", "sgcentre"))

Dpop <- Dpop[order(Dpop$cid, Dpop$Year, Dpop$age),] 
Dpop <- Dpop[which(!is.na(Dpop$IR)), ]
Dpop <- Dpop[which(!is.na(Dpop$sgcentre)), ]





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

Dpop <- Dpop[which(!is.na(Dpop$Npos)), ]


####`10. Loop start: Extract cohort age mina - maxa in study year ####

# a loop with plot and models for each age group as output
ggout <- list()
DpCout <- list()
lcmm_out <- list("tr_lcmm","tr_lcmm_1","tr_lcmm_2", 
                 "tr_bic", 
                 "tr_pred","tr_pred_1","tr_pred_2", 
                 "tr_lambda")
Dp_out <- list()


for(d in c(4, 9)){
  cat("\n ageint: ", d) # locating error
  #browser()
  for(f in c(20, 25, 30, 35, 40, 45)) {
    cat("\n age.beg: ", f)
    mina <- f 
    maxa <- sum(f, d)
    
    Dp <- data.frame(matrix(nrow = 0, ncol = 17)) # Dp = Data population (as nested in of Dpop)
    colnames(Dp) <- colnames(Dpop)
    
    for(j in info$cid){
      sist <- (2012 - as.numeric(info$stY[info$cid == j])) # nb of years since prevalence Study
      if(info$stY[info$cid == j] < 2012){ # excluding Iran (in 2014)
        # cat("\n cid: ", j) # locating error
        for(i in 0:sist){
          a <- (mina+i):(maxa+i) # moving age group by one year
          y <- as.numeric(Dpop$stY) + i # i years added to year of prev. Study
          Dp <- rbind(Dp, Dpop[Dpop$age %in% a & Dpop$Year==y & Dpop$cid== j, ])
        }
      }
    }
    Dp <- Dp[!is.na(Dp$IR), ]
    
    # exculde songkla
    Dp <- Dp %>% filter(cid != 16)

    # Years since Prevalence Study:
    Dp <- Dp %>%
      mutate(YsncS = as.numeric(Dp$Year) - as.numeric(Dp$stY)) 
    Dp_out[[stringr::str_c("Dp", mina, "_", maxa)]] <- Dp
    #data frame DpC: cumulative rates for full cohort per country, year (not over full time period) ####
    # Sum all cases and women of all ages (in previously selected age group) in each Year and country (location, cid)
    DpC <- Dp %>%
      ungroup() %>%
      group_by(Year, YsncS, cid, Location, cov) %>% 
      summarise(Npos = sum(Npos), 
                Nicc = sum(Nicc), 
                prev = sum(Npos)*100/sum(PopFemale),
                IR = sum(Nicc)*100000/sum(PopFemale),
                logIR = log(sum(Nicc)/sum(PopFemale)),
                se = sqrt(sum(Nicc))/sum(Npos), # standard error of rates = rate^2/cases = sqrt(cases)/personyears (Esteve p. 52)
                log_tr = log(sum(Nicc)/sum(Npos)),
                tr = sum(Nicc)/sum(Npos),
                selog = sqrt(1/sum(Npos))) %>% # var(log(k/m)) = 1/(Rate*pyears); se(log(k/m)) = sqrt(1/k). Esteve p. 53
      filter(Location != "Uganda") %>%
      filter(Location != "Viet Nam")%>%
      filter(YsncS <= 14) %>% # > 15 years of follow up only Costa Rica and Colombia. 14years: 5 Locations
      mutate(ag = stringr::str_c(mina, "_", maxa))
    
    DpC$Location <- as.factor(DpC$Location)
    DpC$cid <- as.factor(DpC$cid)
    DpC[which(DpC$Nicc <= 0 | DpC$Npos <= 0 ), c("Nicc", "Npos", "log_tr", "se")] <- NA # to avoid -Inf when Nicc = 0
    DpC <- DpC[!is.na(DpC$IR), ]
    DpCout[[stringr::str_c("DpC", mina, "_", maxa)]] <- DpC # to have all age cohort for mixed model
    

    ## screening coverage
    #DpC$cov <- factor(DpC$cov, levels = c(2, 1, 3), labels = c("medium", "high", "low"))
    #DpC$cov <- factor(DpC$cov, levels = c(1, 2, 3), labels = c("high", "medium",  "low"))
    DpC$cov <- factor(DpC$cov, levels = c(1, 2), labels = c("yes", "no"))
    
    ####` Model ####
    
    # latent class mixed model
    
    ## log...........
    lcmm_out$tr_lcmm[[stringr::str_c(mina, "_", maxa)]] <-
      tr_lcmm <- lcmm(log_tr ~ YsncS * cov, random = ~ YsncS, subject = "Location",  ng = 1, data = DpC, link = "4-equi-splines")
    lcmm_out$tr_lcmm[[stringr::str_c(mina, "_", maxa, "_1")]] <-
      tr_lcmm_1 <- lcmm(log_tr ~ YsncS, random = ~ YsncS, subject = "Location",  ng = 1, data = DpC[DpC$cov == "yes", ], link = "4-equi-splines")
    lcmm_out$tr_lcmm[[stringr::str_c(mina, "_", maxa, "_2")]] <-
      tr_lcmm_2 <- lcmm(log_tr ~ YsncS, random = ~ YsncS, subject = "Location",  ng = 1, data = DpC[DpC$cov == "no", ], link = "4-equi-splines")

    # ## no log...............
    # lcmm_out$tr_lcmm[[stringr::str_c(mina, "_", maxa)]] <-
    #   tr_lcmm <- lcmm(tr ~ YsncS * cov, random = ~ YsncS, subject = "Location",  ng = 1, data = DpC, link = "3-equi-splines")
    # lcmm_out$tr_lcmm[[stringr::str_c(mina, "_", maxa, "_1")]] <-
    #   tr_lcmm_1 <- lcmm(tr ~ YsncS, random = ~ YsncS, subject = "Location",  ng = 1, data = DpC[DpC$cov == "yes", ], link = "3-equi-splines")
    # lcmm_out$tr_lcmm[[stringr::str_c(mina, "_", maxa, "_2")]] <-
    #   tr_lcmm_2 <- lcmm(tr ~ YsncS, random = ~ YsncS, subject = "Location",  ng = 1, data = DpC[DpC$cov == "no", ], link = "3-equi-splines")

    # 
    # bic
    lcmm_out$tr_bic[[stringr::str_c(mina, "_", maxa)]] <- 
      bic <- round(tr_lcmm$BIC, 2)
    
    # prediction
    nd <- data.frame(YsncS = 0:14)
    lcmm_out$tr_pred[[stringr::str_c(mina, "_", maxa, "_1")]] <- 
      tr_pred_1 <- predictY(tr_lcmm_1, nd, methInteg = 1, draws = TRUE)
    lcmm_out$tr_pred[[stringr::str_c(mina, "_", maxa, "_2")]] <- 
      tr_pred_2 <- predictY(tr_lcmm_2, nd, methInteg = 1, draws = TRUE)
    
    #lambda
    lcmm_out$tr_lambda[[stringr::str_c(mina, "_", maxa, "_1")]] <- 
      tr_lambda_1 <- data.frame("YsncS" = tr_pred_1$times[2:15,]-0.5, 
                              "lambda" = diff(tr_pred_1$pred[, 1]))
    lcmm_out$tr_lambda[[stringr::str_c(mina, "_", maxa, "_2")]] <- 
      tr_lambda_2 <- data.frame("YsncS" = tr_pred_2$times[2:15,]-0.5, 
                              "lambda" = diff(tr_pred_2$pred[, 1]))
    

  }
}




### 11. data aggregation of two-group, random effect models ####

## 10y age groups#.....................................................

col <- c('#d8b365', '#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#67001f', '#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#003c30','#b15928', '#dd1c77', '#1c9099')

DpC20 <- DpCout$DpC20_29 %>%
  mutate(ag = "ag1") 
DpC30 <- DpCout$DpC30_39 %>%
  mutate(ag = "ag2")
DpC40 <- DpCout$DpC40_49 %>%
  mutate(ag = "ag3")
DpC_df <- rbind(DpC20, DpC30, DpC40)
labels <- c(`ag1` = "20-29", `ag2` = "30-39", `ag3` = "40-49")
DpC_df$ag <- as.factor(DpC_df$ag)
DpC_df$cov <- factor(DpC_df$cov, levels = c(1, 2), labels = c("yes", "no"))

# new data 
nd <- data.frame(YsncS = 0:14)

## prediction
pred_1 <- rbind(data.frame("tr_m" = lcmm_out$tr_pred$`20_29_1`$pred[,1],
                           "YsncS" = c(0:14), "ag" = "ag1", "cov" = 1),
                data.frame("tr_m" = lcmm_out$tr_pred$`30_39_1`$pred[,1],
                           "YsncS" = c(0:14), "ag" = "ag2", "cov" = 1),
                data.frame("tr_m" = lcmm_out$tr_pred$`40_49_1`$pred[,1],
                           "YsncS" = c(0:14), "ag" = "ag3", "cov" = 1))
pred_2 <- rbind(data.frame("tr_m" = lcmm_out$tr_pred$`20_29_2`$pred[,1],
                           "YsncS" = c(0:14), "ag" = "ag1", "cov" = 2),
                data.frame("tr_m" = lcmm_out$tr_pred$`30_39_2`$pred[,1],
                           "YsncS" = c(0:14), "ag" = "ag2", "cov" = 2),
                data.frame("tr_m" = lcmm_out$tr_pred$`40_49_2`$pred[,1],
                           "YsncS" = c(0:14), "ag" = "ag3", "cov" = 2))

pred_1$cov <- factor(pred_1$cov, levels = c(1, 2), labels = c("yes", "no"))
pred_2$cov <- factor(pred_2$cov, levels = c(1, 2), labels = c("yes", "no"))

DpC_1 <- left_join(DpC_df, pred_1, by = c("YsncS", "cov", "ag"))
DpC_2 <- left_join(DpC_df, pred_2, by = c("YsncS", "cov", "ag"))
DpC_12 <- rbind(DpC_1, DpC_2)
DpC_12 <- DpC_12[!is.na(DpC_12$tr_m),]



## 5y age groups #.....................................................

col <- c('#d8b365', '#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#67001f', '#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#003c30','#b15928', '#dd1c77', '#1c9099')

DpC20 <- DpCout$DpC20_24 %>%
  mutate(ag = "ag1") 
DpC25 <- DpCout$DpC25_29 %>%
  mutate(ag = "ag2") 
DpC30 <- DpCout$DpC30_34 %>%
  mutate(ag = "ag3")
DpC35 <- DpCout$DpC35_39 %>%
  mutate(ag = "ag4")
DpC40 <- DpCout$DpC40_44 %>%
  mutate(ag = "ag5")
DpC45 <- DpCout$DpC45_49 %>%
  mutate(ag = "ag6")

DpC_df <- rbind(DpC20, DpC25, DpC30, DpC35, DpC40, DpC45)
labels <- c(`ag1` = "20-24", `ag2` = "25-29", `ag3` = "30-34", `ag4` = "35-39", `ag5` = "40-44", `ag6` = "45-49")
DpC_df$ag <- as.factor(DpC_df$ag)
DpC_df$cov <- factor(DpC_df$cov, levels = c(1, 2), labels = c("yes", "no"))

# new data 
nd <- data.frame(YsncS = 0:14)

## prediction
pred_1 <- rbind(data.frame("tr_m" = lcmm_out$tr_pred$`20_24_1`$pred[,1],
                           "YsncS" = c(0:14), "ag" = "ag1", "cov" = 1),
                data.frame("tr_m" = lcmm_out$tr_pred$`25_29_1`$pred[,1],
                           "YsncS" = c(0:14), "ag" = "ag2", "cov" = 1),
                data.frame("tr_m" = lcmm_out$tr_pred$`30_34_1`$pred[,1],
                           "YsncS" = c(0:14), "ag" = "ag3", "cov" = 1),
                data.frame("tr_m" = lcmm_out$tr_pred$`35_39_1`$pred[,1],
                           "YsncS" = c(0:14), "ag" = "ag4", "cov" = 1),
                data.frame("tr_m" = lcmm_out$tr_pred$`40_44_1`$pred[,1],
                           "YsncS" = c(0:14), "ag" = "ag5", "cov" = 1),
                data.frame("tr_m" = lcmm_out$tr_pred$`45_49_1`$pred[,1],
                           "YsncS" = c(0:14), "ag" = "ag6", "cov" = 1))

pred_2 <- rbind(data.frame("tr_m" = lcmm_out$tr_pred$`20_24_2`$pred[,1],
                           "YsncS" = c(0:14), "ag" = "ag1", "cov" = 2),
                data.frame("tr_m" = lcmm_out$tr_pred$`25_29_2`$pred[,1],
                           "YsncS" = c(0:14), "ag" = "ag2", "cov" = 2),
                data.frame("tr_m" = lcmm_out$tr_pred$`30_34_2`$pred[,1],
                           "YsncS" = c(0:14), "ag" = "ag3", "cov" = 2),
                data.frame("tr_m" = lcmm_out$tr_pred$`35_39_2`$pred[,1],
                           "YsncS" = c(0:14), "ag" = "ag4", "cov" = 2),
                data.frame("tr_m" = lcmm_out$tr_pred$`40_44_2`$pred[,1],
                           "YsncS" = c(0:14), "ag" = "ag5", "cov" = 2),
                data.frame("tr_m" = lcmm_out$tr_pred$`45_49_2`$pred[,1],
                           "YsncS" = c(0:14), "ag" = "ag6", "cov" = 2))

pred_1$cov <- factor(pred_1$cov, levels = c(1, 2), labels = c("yes", "no"))
pred_2$cov <- factor(pred_2$cov, levels = c(1, 2), labels = c("yes", "no"))

DpC_1 <- left_join(DpC_df, pred_1, by = c("YsncS", "cov", "ag"))
DpC_2 <- left_join(DpC_df, pred_2, by = c("YsncS", "cov", "ag"))
DpC_12 <- rbind(DpC_1, DpC_2)
DpC_12 <- DpC_12[!is.na(DpC_12$tr_m),]






#### 12. Transition Plots ####

#### Joint transition plot, seperate models by screening ####
# ...........................................................................................
# change log_tr to tr if non-log scale wanted (change in loop aswell!)
ggplot(DpC_12, aes(x = YsncS, y = tr)) +
  theme_minimal() +
  geom_line(aes(col = Location, linetype = cov), size = 0.6) +
  scale_color_manual(values = col) +
  scale_linetype_manual(values = c(1, 3), labels = c("high screening", "low screening")) +
  geom_line(aes(x = YsncS, y = tr_m, linetype = cov), size = 2, color = "black") +
  facet_wrap(~ ag, labeller = labeller(ag = labels)) +
  theme(strip.text.x = element_text(size = 12, face = "bold")) +
  #geom_ribbon(aes(x = YsncS, ymin = logr_lo, ymax = logr_hi), fill = 'azure4', alpha = 0.4)+
  labs(title = "Transition rate from HPV to cervical cancer by age and screening",
       y = "Incidence Rate in hrHPV+ women", 
       x = "Years since HPV detection",
       subtitle = "Mixed effects model using splines. Model fit over 14y.") +
  #ylim(-10.2, -2.5)  +
  ylim(0, 0.02)+
  xlim(0, 11) +
  theme(plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle  = element_text(size = 16),
        legend.title = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 14),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 12),
        legend.key.height = unit(0.8, "cm"),
        legend.key.width = unit(1.5, "cm")) + 
  guides(linetype = guide_legend(title = "Mixed effects model"))



#### Seperate transition plots by screening group ####
#...............................................................
###only Locations without screening #............................
ggplot(DpC_2[DpC$cov == "no",], aes(x = YsncS, y = log_tr)) +
  theme_minimal() +
  geom_line(aes(col = Location, linetype = cov), size = 0.7) +
  scale_color_manual(values = col) +
  geom_line(aes(x = YsncS, y = tr, linetype = cov), size = 2, color = "black") +
  scale_linetype_manual(values = c(1), labels = c("Mixed effects model")) +
  facet_wrap(~ ag, labeller = labeller(ag = labels)) +
  theme(strip.text.x = element_text(size = 12, face = "bold")) +
  labs(title = "Transition rate from HPV to cervical cancer by age",
       y = "log Incidence Rate in hrHPV+ women", 
       x = "Years since HPV detection",
       subtitle = "Mixed effects model using splines. Model fit over 14y.") +
  ylim(-10.2, -2.5)  +
  xlim(0, 11) +
  theme(plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle  = element_text(size = 16),
        legend.title = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 14),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 12),
        legend.key.height = unit(0.8, "cm"),
        legend.key.width = unit(1.2, "cm")) + 
  guides(linetype = guide_legend(title= ""))



### Locations with screening #....................................
ggplot(DpC_1[DpC$cov == "yes",], aes(x = YsncS, y = log_tr)) +
  theme_minimal() +
  geom_line(aes(col = Location, linetype = cov), size = 0.7) +
  scale_color_manual(values = col) +
  geom_line(aes(x = YsncS, y = tr, linetype = cov), size = 2, color = "black") +
  scale_linetype_manual(values = c(1), labels = c("Mixed effects model")) +
  facet_wrap(~ ag, labeller = labeller(ag = labels)) +
  theme(strip.text.x = element_text(size = 12, face = "bold")) +
  labs(title = "Transition rate from HPV to cervical cancer by age",
       y = "log Incidence Rate in hrHPV+ women", 
       x = "Years since HPV detection",
       subtitle = "Mixed effects model using splines. Model fit over 14y.") +
  ylim(-10.2, -2.5)  +
  xlim(0, 11) +
  theme(plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle  = element_text(size = 16),
        legend.title = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 14),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 12),
        legend.key.height = unit(0.8, "cm"),
        legend.key.width = unit(1.2, "cm")) + 
  guides(linetype = guide_legend(title= "")) 






#### time trend in incidence unadjusted for prevalence #### 
#....................................................................................
col <- c('#d8b365', '#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#67001f', '#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#003c30','#b15928', '#dd1c77', '#1c9099')

#20-29
gg_inc_20 <- ggplot(DpCout$DpC20_29, aes(x = YsncS, y = IR)) +
  theme_minimal()+
  geom_line(aes(color = Location, linetype = cov), size = 1.5) +
  scale_colour_manual(values = col) +
  scale_linetype_manual(values = c(1, 2, 3), name = "Screening Coverage")+
  labs(title = "Cervical Cancer Incidence in women 20-29y at HPV detection",
       y = "Incidence Rate per 100000 women", 
       x = "Years since HPV detection") +
  theme(legend.key.width = unit(2, "cm"),
        legend.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        title = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.key.height = unit(0.8, "cm"),
        plot.margin = unit(c(1.5, 1, 1.5, 1.5), "cm"))
#30-39
gg_inc_30 <- ggplot(DpCout$DpC30_39, aes(x = YsncS, y = IR)) +
  theme_minimal()+
  geom_line(aes(color = Location, linetype = cov), size = 1.5) +
  scale_colour_manual(values = col) +
  scale_linetype_manual(values = c(1, 2, 3), name = "Screening Coverage")+
  labs(title = "Cervical Cancer Incidence in women 30-39y at HPV detection",
       y = "Incidence Rate per 100000 women", 
       x = "Years since HPV detection") +
  theme(legend.key.width = unit(2, "cm"),
        legend.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        title = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.key.height = unit(0.8, "cm"),
        plot.margin = unit(c(1.5, 1, 1.5, 1.5), "cm"))
#40-49
gg_inc_40 <- ggplot(DpCout$DpC40_49, aes(x = YsncS, y = IR)) +
  theme_minimal()+
  geom_line(aes(color = Location, linetype = cov), size = 1.5) +
  scale_colour_manual(values = col) +
  scale_linetype_manual(values = c(1, 2, 3), name = "Screening Coverage")+
  labs(title = "Cervical Cancer Incidence in women 40-49y at HPV detection",
       y = "Incidence Rate per 100000 women", 
       x = "Years since HPV detection") +
  theme(legend.key.width = unit(2, "cm"),
        legend.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        title = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.key.height = unit(0.8, "cm"),
        plot.margin = unit(c(1.5, 1, 1.5, 1.5), "cm"))

#time trends in prevalence in age cohort

#20-29
gg_prev_20 <- ggplot(DpCout$DpC20_29, aes(x = YsncS, y = prev)) +
  theme_classic()+
  geom_line(aes(color = Location, linetype = cov), size = 1.5) +
  scale_colour_manual(values = col) +
  scale_linetype_manual(values = c(1, 2, 3), name = "Screening Coverage")+
  labs(title = "hrHPV Prevalence (%)") +
  xlim(0, 1) +
  theme(legend.key.width = unit(2, "cm"),
        legend.title = element_text(size = 16, face = "bold"),
        axis.title = element_blank(),
        axis.text.x = element_blank(), 
        axis.text.y = element_text(size = 12),
        title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.key.height = unit(0.8, "cm"),
        plot.margin = unit(c(1, 1.5, 1, 1), "cm")) 
#30-39
gg_prev_30 <- ggplot(DpCout$DpC30_39, aes(x = YsncS, y = prev)) +
  theme_classic()+
  geom_line(aes(color = Location, linetype = cov), size = 1.5) +
  scale_colour_manual(values = col) +
  scale_linetype_manual(values = c(1, 2, 3), name = "Screening Coverage")+
  labs(title = "hrHPV Prevalence (%)") +
  xlim(0, 1) +
  theme(legend.key.width = unit(2, "cm"),
        legend.title = element_text(size = 16, face = "bold"),
        axis.title = element_blank(),
        axis.text.x = element_blank(), 
        axis.text.y = element_text(size = 12),
        title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.key.height = unit(0.8, "cm"),
        plot.margin = unit(c(1, 1.5, 1, 1), "cm")) 
#40-49
gg_prev_40 <- ggplot(DpCout$DpC40_49, aes(x = YsncS, y = prev)) +
  theme_classic()+
  geom_line(aes(color = Location, linetype = cov), size = 1.5) +
  scale_colour_manual(values = col) +
  scale_linetype_manual(values = c(1, 2, 3), name = "Screening Coverage")+
  labs(title = "hrHPV Prevalence (%)") +
  xlim(0, 1) +
  theme(legend.key.width = unit(2, "cm"),
        legend.title = element_text(size = 16, face = "bold"),
        axis.title = element_blank(),
        axis.text.x = element_blank(), 
        axis.text.y = element_text(size = 12),
        title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.key.height = unit(0.8, "cm"),
        plot.margin = unit(c(1, 1.5, 1, 1), "cm")) 


leg <- cowplot::get_legend(gg_inc_20)
cowplot::plot_grid(gg_prev_20 + theme(legend.position = "none"), 
                   gg_inc_20 + theme(legend.position = "none"),
                   leg, nrow = 1, rel_widths = c(0.3, 1, 0.4))
cowplot::plot_grid(gg_prev_30 + theme(legend.position = "none"), 
                   gg_inc_30 + theme(legend.position = "none"),
                   leg, nrow = 1, rel_widths = c(0.3, 1, 0.4))
cowplot::plot_grid(gg_prev_40 + theme(legend.position = "none"), 
                   gg_inc_40 + theme(legend.position = "none"),
                   leg, nrow = 1, rel_widths = c(0.3, 1, 0.4))




#### time trend in LOG incidence unadjusted for prevalence######
#20-29
gg_inc_20 <- ggplot(DpCout$DpC20_29, aes(x = YsncS, y = logIR)) +
  theme_minimal()+
  geom_line(aes(color = Location, linetype = cov), size = 1.5) +
  scale_colour_manual(values = col) +
  scale_linetype_manual(values = c(1, 2, 3), name = "Screening Coverage")+
  labs(title = "Cervical Cancer Incidence in women 20-29y at HPV detection",
       y = "log Incidence Rate per women", 
       x = "Years since HPV detection") +
  theme(legend.key.width = unit(2, "cm"),
        legend.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        title = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.key.height = unit(0.8, "cm"),
        plot.margin = unit(c(1.5, 1, 1.5, 1.5), "cm"))
#30-39
gg_inc_30 <- ggplot(DpCout$DpC30_39, aes(x = YsncS, y = logIR)) +
  theme_minimal()+
  geom_line(aes(color = Location, linetype = cov), size = 1.5) +
  scale_colour_manual(values = col) +
  scale_linetype_manual(values = c(1, 2, 3), name = "Screening Coverage")+
  labs(title = "Cervical Cancer Incidence in women 30-39y at HPV detection",
       y = "log Incidence Rate per women", 
       x = "Years since HPV detection") +
  theme(legend.key.width = unit(2, "cm"),
        legend.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        title = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.key.height = unit(0.8, "cm"),
        plot.margin = unit(c(1.5, 1, 1.5, 1.5), "cm"))
#40-49
gg_inc_40 <- ggplot(DpCout$DpC40_49, aes(x = YsncS, y = logIR)) +
  theme_minimal()+
  geom_line(aes(color = Location, linetype = cov), size = 1.5) +
  scale_colour_manual(values = col) +
  scale_linetype_manual(values = c(1, 2, 3), name = "Screening Coverage")+
  labs(title = "Cervical Cancer Incidence in women 40-49y at HPV detection",
       y = "log Incidence Rate per women", 
       x = "Years since HPV detection") +
  theme(legend.key.width = unit(2, "cm"),
        legend.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        title = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.key.height = unit(0.8, "cm"),
        plot.margin = unit(c(1.5, 1, 1.5, 1.5), "cm"))




#time trends in prevalence in age cohort
#20-29
gg_prev_20 <- ggplot(DpCout$DpC20_29, aes(x = YsncS, y = prev)) +
  theme_classic()+
  geom_line(aes(color = Location, linetype = cov), size = 1.5) +
  scale_colour_manual(values = col) +
  scale_linetype_manual(values = c(1, 2, 3), name = "Screening Coverage")+
  labs(title = "hrHPV Prevalence (%)") +
  xlim(0, 1) +
  theme(legend.key.width = unit(2, "cm"),
        legend.title = element_text(size = 16, face = "bold"),
        axis.title = element_blank(),
        axis.text.x = element_blank(), 
        axis.text.y = element_text(size = 12),
        title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.key.height = unit(0.8, "cm"),
        plot.margin = unit(c(1, 1.5, 1, 1), "cm")) 
#30-39
gg_prev_30 <- ggplot(DpCout$DpC30_39, aes(x = YsncS, y = prev)) +
  theme_classic()+
  geom_line(aes(color = Location, linetype = cov), size = 1.5) +
  scale_colour_manual(values = col) +
  scale_linetype_manual(values = c(1, 2, 3), name = "Screening Coverage")+
  labs(title = "hrHPV Prevalence (%)") +
  xlim(0, 1) +
  theme(legend.key.width = unit(2, "cm"),
        legend.title = element_text(size = 16, face = "bold"),
        axis.title = element_blank(),
        axis.text.x = element_blank(), 
        axis.text.y = element_text(size = 12),
        title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.key.height = unit(0.8, "cm"),
        plot.margin = unit(c(1, 1.5, 1, 1), "cm")) 
#40-49
gg_prev_40 <- ggplot(DpCout$DpC40_49, aes(x = YsncS, y = prev)) +
  theme_classic()+
  geom_line(aes(color = Location, linetype = cov), size = 1.5) +
  scale_colour_manual(values = col) +
  scale_linetype_manual(values = c(1, 2, 3), name = "Screening Coverage")+
  labs(title = "hrHPV Prevalence (%)") +
  xlim(0, 1) +
  theme(legend.key.width = unit(2, "cm"),
        legend.title = element_text(size = 16, face = "bold"),
        axis.title = element_blank(),
        axis.text.x = element_blank(), 
        axis.text.y = element_text(size = 12),
        title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.key.height = unit(0.8, "cm"),
        plot.margin = unit(c(1, 1.5, 1, 1), "cm")) 


leg <- cowplot::get_legend(gg_inc_20)
cowplot::plot_grid(gg_prev_20 + theme(legend.position = "none"), 
                   gg_inc_20 + theme(legend.position = "none"),
                   leg, nrow = 1, rel_widths = c(0.3, 1, 0.4))
cowplot::plot_grid(gg_prev_30 + theme(legend.position = "none"), 
                   gg_inc_30 + theme(legend.position = "none"),
                   leg, nrow = 1, rel_widths = c(0.3, 1, 0.4))
cowplot::plot_grid(gg_prev_40 + theme(legend.position = "none"), 
                   gg_inc_40 + theme(legend.position = "none"),
                   leg, nrow = 1, rel_widths = c(0.3, 1, 0.4))






#### 13. Derivative: lambda ####


#10y age groups .............................................................
## screen
df_g <- data.frame(lcmm_out$tr_lambda$`20_29_1`)
df_lambda_1 <- df_g %>%
  mutate("20_29" = lambda,
         "30_39" = lcmm_out$tr_lambda$`30_39_1`[, 2],
         "40_49" = lcmm_out$tr_lambda$`40_49_1`[, 2]) %>%
  select(-lambda) %>%
  gather("AgeGroup", "lambda", 2:4)%>%
  mutate(screen = 1) 
df_lambda_1$AgeGroup <- factor(df_lambda_1$AgeGroup, levels = c("20_29", "30_39", "40_49"),  
                             labels = c("20-29","30-39", "40-49"))
## no screen
df_g <- data.frame(lcmm_out$tr_lambda$`20_29_2`)
df_lambda_2 <- df_g %>%
  mutate("20_29" = lambda,
         "30_39" = lcmm_out$tr_lambda$`30_39_2`[, 2],
         "40_49" = lcmm_out$tr_lambda$`40_49_2`[, 2]) %>%
  select(-lambda) %>%
  gather("AgeGroup", "lambda", 2:4)%>%
  mutate(screen = 2) 
df_lambda_2$AgeGroup <- factor(df_lambda_2$AgeGroup, levels = c("20_29", "30_39", "40_49"),  
                             labels = c("20-29","30-39", "40-49"))


df_lambda <- rbind(df_lambda_1, df_lambda_2)
df_lambda$screen <- factor(df_lambda$screen, levels = c(1, 2), labels = c("high coverage", "low coverage"))



# 5y age groups: BETTER RESULTS .............................................................
## screen
df_g <- data.frame(lcmm_out$tr_lambda$`20_24_1`)
df_lambda_1 <- df_g %>%
  mutate("20_24" = lambda,
         "25_29" = lcmm_out$tr_lambda$`25_29_1`[, 2],
         "30_34" = lcmm_out$tr_lambda$`30_34_1`[, 2],
         "35_39" = lcmm_out$tr_lambda$`35_39_1`[, 2],
         "40_44" = lcmm_out$tr_lambda$`40_44_1`[, 2],
         "45_49" = lcmm_out$tr_lambda$`45_49_1`[, 2]) %>%
  select(-lambda) %>%
  gather("AgeGroup", "lambda", 2:7)%>%
  mutate(screen = 1) 
df_lambda_1$AgeGroup <- factor(df_lambda_1$AgeGroup, levels = c("20_24", "25_29", "30_34", "35_39", "40_44", "45_49"),  
                             labels = c("20-24", "25-29", "30-34", "35-39", "40-44", "45-49"))
## no screen
df_g <- data.frame(lcmm_out$tr_lambda$`20_24_2`)
df_lambda_2 <- df_g %>%
  mutate("20_24" = lambda,
         "25_29" = lcmm_out$tr_lambda$`25_29_2`[, 2],
         "30_34" = lcmm_out$tr_lambda$`30_34_2`[, 2],
         "35_39" = lcmm_out$tr_lambda$`35_39_2`[, 2],
         "40_44" = lcmm_out$tr_lambda$`40_44_2`[, 2],
         "45_49" = lcmm_out$tr_lambda$`45_49_2`[, 2]) %>%
  select(-lambda) %>%
  gather("AgeGroup", "lambda", 2:7) %>%
  mutate(screen = 2)
df_lambda_2$AgeGroup <- factor(df_lambda_2$AgeGroup, levels = c("20_24", "25_29", "30_34", "35_39", "40_44", "45_49"),  
                             labels = c("20-24", "25-29", "30-34", "35-39", "40-44", "45-49"))

df_lambda <- rbind(df_lambda_1, df_lambda_2)
df_lambda$screen <- factor(df_lambda$screen, levels = c(1, 2), labels = c("high coverage", "low coverage"))



## plots ............................................................................
# cols <- c('#ae017e', '#dd3497','#f768a1','#fa9fb5', '#fcc5c0','#fde0dd', '#fff7f3')
cols <- c('#006d2c','#66a61e','#377eb8','#7570b3','#e7298a','#d95f02')
## screen
gg_lambda_1 <- ggplot(df_lambda_1, aes(YsncS, lambda)) +  # choose df_lambda
  geom_smooth(aes(color = AgeGroup), se = FALSE) +
  scale_color_manual(values = cols) +
  labs(title = "Change in cervical cancer transition rate (ln)",
       subtitle = "per woman tested hrHPV+ at a given age in countries with high screening", 
       x = "Years since hrHPV detection", 
       y = "Change in cervical cancer transition rate(ln)",
       color = "Age at hrHPV detection",
       linetype = "Age at hrHPV detection") + 
  xlim(0, 15) +
  theme_classic() +
  guides(color = guide_legend(ncol = 1, label.hjust = 1)) +
  theme(plot.title=element_text(size=24,
                                face = "bold"),
        plot.subtitle = element_text(size = 16),
        plot.margin = unit(c(0.5, 0.5, 1, 1), "cm")) +
  theme(legend.title = element_text(size=16, face="bold"),
        legend.text = element_text(size = 16),
        legend.justification= "center",
        legend.position = c(0.8, 0.7),
        legend.key.width = unit(c(1.5 ), "cm"),
        legend.key.height = unit(c(1), "cm")) +
  theme(axis.title.x = element_text(face="bold", size=18),
        axis.text.x  = element_text(size=18),
        axis.title.y = element_text(face="bold", size=18),
        axis.text.y  = element_text(size=18)) 

## no screen
gg_lambda_2 <- ggplot(df_lambda_2, aes(YsncS, lambda)) +  # choose df_lambda
  geom_smooth(aes(color = AgeGroup), se = FALSE) +
  scale_color_manual(values = cols) +
  labs(title = "Change in cervical cancer transition rate (ln)",
       subtitle = "per woman tested hrHPV+ at a given age in countries with low screening", 
       x = "Years since hrHPV detection", 
       y = "Change in cervical cancer transition rate(ln)",
       color = "Age at hrHPV detection",
       linetype = "Age at hrHPV detection") + 
  xlim(0, 15) +
  theme_classic() +
  guides(color = guide_legend(ncol = 1, label.hjust = 1)) +
  theme(plot.title=element_text(size=24,
                                face = "bold"),
        plot.subtitle = element_text(size = 16),
        plot.margin = unit(c(0.5, 0.5, 1, 1), "cm")) +
  theme(legend.title = element_text(size=16, face="bold"),
        legend.text = element_text(size = 16),
        legend.justification= "center",
        legend.position = c(0.8, 0.7),
        legend.key.width = unit(c(1.5 ), "cm"),
        legend.key.height = unit(c(1), "cm")) +
  theme(axis.title.x = element_text(face="bold", size=18),
        axis.text.x  = element_text(size=16),
        axis.title.y = element_text(face="bold", size=18),
        axis.text.y  = element_text(size=16)) 



## both screening types
ggplot(df_lambda, aes(YsncS, lambda)) +
  theme_classic() +
  geom_smooth(aes(color = AgeGroup), size = 1, se = FALSE) +
  scale_color_manual(values = cols) +
  facet_wrap(~ screen, ncol = 2) +
  theme(strip.text.x = element_text(size = 16, face = "bold")) +
  labs(title = "Change in cervical cancer transition rate by screening coverage",
       subtitle = "per woman tested hrHPV+ at a given age", 
       x = "Years since hrHPV detection", 
       y = "Change in cervical cancer transition rate (ln)",
       color = "Age at hrHPV detection",
       linetype = "Age at hrHPV detection") + 
  xlim(0, 14) +
  guides(color = guide_legend(ncol = 1, label.hjust = 1)) +
  theme(plot.title=element_text(size=24,
                                face = "bold"),
        plot.subtitle = element_text(size = 16),
        plot.margin = unit(c(0.5, 0.5, 1, 1), "cm")) +
  theme(legend.title = element_text(size=16, face="bold"),
        legend.text = element_text(size = 16),
        legend.justification= "center",
        #legend.position = c(0.8, 0.7),
        legend.key.width = unit(c(1.5 ), "cm"),
        legend.key.height = unit(c(0.8), "cm")) +
  theme(axis.title.x = element_text(face="bold", size=18),
        axis.text.x  = element_text(size=16),
        axis.title.y = element_text(face="bold", size=18),
        axis.text.y  = element_text(size=16)) 



#### test for trend ####
# 10y age groups
DpC_df <- rbind(DpCout$DpC20_29, 
                DpCout$DpC30_39, 
                DpCout$DpC40_49) 
DpC_df$ag <- factor(DpC_df$ag, levels = c("20_29", "30_39","40_49"), 
                    labels = c(1, 2, 3))
ggplot(DpC_df, aes(YsncS, log_tr)) +
  theme_minimal() +
  geom_point(aes(col = ag)) + 
  geom_smooth(aes(col = ag), se = FALSE) +
  labs(title = "Transition Rate hrHPV to CC over Years since hrHPV detection by age group",
       subtitle = "smoothing by ggplot")


# categorical testing

tr_c <- lcmm(log_tr ~ YsncS * cov + YsncS * ag, random = ~ YsncS, subject = "Location",  ng = 1, 
             data = DpC_df, link = "4-equi-splines")

tr_1 <- lcmm(log_tr ~ YsncS* ag, random = ~ YsncS, subject = "Location",  ng = 1, 
             data = DpC_df[DpC_df$cov == 1, ], link = "4-equi-splines")

tr_2<- lcmm(log_tr ~ YsncS* ag, random = ~ YsncS, subject = "Location",  ng = 1, 
            data = DpC_df[DpC_df$cov == 2, ], link = "4-equi-splines")


## trend testing
DpC_df_t <- DpC_df
DpC_df_t$ag <- as.numeric(DpC_df_t$ag) # to make trend testing possible (???)

tr_c_t <- lcmm(log_tr ~ YsncS * cov * ag, random = ~ YsncS, subject = "Location",  ng = 1, 
               data = DpC_df_t, link = "4-equi-splines")

tr_1_t <- lcmm(log_tr ~ YsncS* ag, random = ~ YsncS, subject = "Location",  ng = 1, 
               data = DpC_df_t[DpC_df_t$cov == 1, ], link = "4-equi-splines")

tr_2_t <- lcmm(log_tr ~ YsncS* ag, random = ~ YsncS, subject = "Location",  ng = 1, 
               data = DpC_df_t[DpC_df_t$cov == 2, ], link = "4-equi-splines")
summary(tr_lcmm)





# 5y age groups
DpC_df <- rbind(DpCout$DpC20_24, 
                DpCout$DpC25_29,
                DpCout$DpC30_34, 
                DpCout$DpC34_39,
                DpCout$DpC40_44,
                DpCout$DpC45_49)

DpC_df$ag <- factor(DpC_df$ag, levels = c("20_24", "25_29", "30_34", "35_39", "40_44", "45_49"), 
                    labels = c("20-24", "25-29", "30-34", "35-39", "40-44", "45-49"))
ggplot(DpC_df, aes(YsncS, log_tr)) +
  theme_minimal() +
  geom_point(aes(col = ag)) + 
  geom_smooth(aes(col = ag), se = FALSE) +
  labs(title = "Transition Rate hrHPV to CC over Years since hrHPV detection by age group",
       subtitle = "smoothing by ggplot")


# categorical testing
DpC_df$ag <- factor(DpC_df$ag, levels = c("20_24", "25_29", "30_34", "35_39", "40_44", "45_49"), 
                    labels = c(1:6))
tr_c <- lcmm(log_tr ~ YsncS*cov + YsncS*ag, random = ~ YsncS, subject = "Location",  ng = 1, 
                data = DpC_df, link = "3-equi-splines")

tr_1 <- lcmm(log_tr ~ YsncS*ag, subject = "Location",  ng = 1, 
                data = DpC_df[DpC_df$cov == 1, ], link = "splines")

tr_2<- lcmm(log_tr ~ YsncS * ag, random = ~ YsncS, subject = "Location",  ng = 1, 
                data = DpC_df[DpC_df$cov == 2, ], link = "splines", maxiter = 200)


## trend testing
DpC_df_t <- DpC_df
DpC_df_t$ag <- as.numeric(DpC_df_t$ag) # to make trend testing possible (???)

tr_c_t <- lcmm(log_tr ~ YsncS * cov * ag, random = ~ YsncS, subject = "Location",  ng = 1, 
                data = DpC_df_t, link = "4-equi-splines")

tr_1_t <- lcmm(log_tr ~ YsncS* ag, random = ~ YsncS, subject = "Location",  ng = 1, 
                data = DpC_df_t[DpC_df_t$cov == 1, ], link = "4-equi-splines")

tr_2_t <- lcmm(log_tr ~ YsncS* ag, random = ~ YsncS, subject = "Location",  ng = 1, 
                data = DpC_df_t[DpC_df_t$cov == 2, ], link = "4-equi-splines")
summary(tr_lcmm)


### log likelyhood testing

