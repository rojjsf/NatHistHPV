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

info <- data.frame(matrix(nrow = 30, ncol = 0))
info <- info %>%
  mutate("cid" = c(1,  2,  5,  6,  8,  9, 10, 12, 13, 15,  3, 16, 17, 18,  4, 20, 19, 22,# assigned by Rosa
                   "14", # vietnam, hanoi
                   "25",# pakistan
                   "11", # bhutan
                   "31",# mongolia
                   "34",# nepal
                   "32", # rwanda
                   "26", # mexico
                   "30", # georgia
                   "33", # Vanuatu
                   "28", # Fiji
                   "7", # Nigeria
                   "29"), #Guinea
         "sgcentre" = c(44, 100, 19, 12, 23, 18, 61, 15,  2,  7,  9, 14,  3,  3, 16, 83,  4, 41,# defined by IARC prevalence studies
                        "1", # hanoi
                        "60", # pakistan
                        "65",# bhutan
                        "25", # mongolia
                        "42",# nepal
                        "66", # Rwanda
                        "8", # Mexico
                        "45", # georgia
                        "64", # Vanuatu
                        NA, # Fiji - not in pooled data
                        "10", # Nigeria
                        "11"), #Guinea 
         "stY" = c("2007-2008", "2002-2004", "1993-1994", "1993-1995", "2005", "2004", "2013-2014", "1999-2000", "1997", "1997-1998", 
                   "1998", "1990-2000", "1998", "1998", "2001", "2002", "1995-1998", "2006",
                   "1997",
                   "2004-2008",
                   "2004-2008",
                   "2005",
                   "2006-2007",
                   "2013-2014",
                   "1996-1999",
                   "2007",
                   "2009-2011",
                   "2003-2007",
                   "1999",
                   "2006"),
         "loc.prev" = c("Algeria, Setif","Uganda, Kyadondo County", "Costa Rica",                
                        "Colombia, Bucaramanga","China, Shenyang","India, Dindigul, Ambilikkai",                  
                        "Iran (Islamic Republic of), Golestan Province", "Republic of Korea, Busan",   "Viet Nam, Ho Chi Minh City",                   
                        "Thailand, Lampang","Argentina, Entre Rios Province", "Thailand, Songkhla",        
                        "Spain, Tarragona", "Spain, Girona", "Chile, Region of Antofagasta",                 
                        "Italy, Turin", "The Netherlands", "Poland, Poznan",
                        "Viet Nam, Hanoi",
                        "Pakistan, South Karachi",
                        "Bhutan",
                        "Mongolia, Ulanbataar",
                        "Nepal, Bharatpur",
                        "Rwanda, Kigali",
                        "Mexico, Morelos State",
                        "Georgia, Tbilisi",
                        "Vanuatu, Port Vila",
                        "Fiji",
                        "Nigeria, Ibadan",
                        "Guinea, Conakry"),  # Prevalence STudy location
         "country" =  c("Algeria", "Uganda", "Costa Rica", "Colombia", "China", "India", "Iran (Islamic Republic of)",  "Republic of Korea", "Viet Nam", "Thailand", # for mortality, population
                        "Argentina", "Thailand", "Spain", "Spain", "Chile", "Italy", "Netherlands", "Poland",
                        "Viet Nam",
                        "Pakistan",
                        "Bhutan",
                        "Mongolia",
                        "Nepal",
                        "Rwanda",
                        "Mexico",
                        "Georgia",
                        "Vanuatu",
                        "Fiji",
                        "Nigeria",
                        "Guinea"),
         "Location" = c("Algeria", "Uganda", "Costa Rica", "Colombia", "China", "India", "Iran (Islamic Republic of)",  "Republic of Korea", "Viet Nam", "Thailand (Lampang)",
                        "Argentina", "Thailand", "Spain", "Spain", "Chile", "Italy", "Netherlands", "Poland",
                        "Viet Nam, Hanoi",
                        "Pakistan, South Karachi",
                        "Bhutan",
                        "Mongolia, Ulanbataar",
                        "Nepal, Bharatpur",
                        "Rwanda, Kigali",
                        "Mexico, Morelos State",
                        "Georgia, Tbilisi",
                        "Vanuatu, Port Vila",
                        "Fiji",
                        "Nigeria, Ibadan",
                        "Guinea, Conakry"), # for graph
        
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
                      572401099, # Spain, Girona (2008-2012)
                      215200399, # *Chile, Region of Antofagasta (2008-2010)
                      538000899, # Italy, Turin (2008-2012)
                      552800099, # *The Netherlands (2008-2012)
                      561601099,  # Poland, Poznan (2008-2012)
                      NA,# vietnam, hanoi
                      NA, # pakistan
                      NA,# bhutan
                      NA, # mongolia
                      NA, # nepal
                      NA, # rwanda
                      NA, # mexico
                      NA, # georgia
                      NA, # Vanuatu
                      NA, # Fiji
                      NA, # Nigeria
                      NA)) %>% #Guinea
  
  separate(stY, into = paste0("stY", c(0, "")))
info$stY[info$cid %in% c(8, 9, 13, 3, 17, 18, 4, 20, 22, 14, 31, 30, 7, 29)] <- c(2005, 2004, 1997, 1998, 2000, 2000, 2003, 2002, 2006, 1997, 2005, 2007, 1999, 2006) # filling NA if only one study year
info$sgcentre <- as.numeric(info$sgcentre)

fact.lbl <- c("[15,20)", "[20,25)", "[25,30)", "[30,35)", "[35,40)", "[40,45)", "[45,50)", "[50,55)",
              "[55,60)", "[60,65)", "[65,70)", "[70,75)", "[75,80)")

####`2. load all data ####
# incidence
dataVIII <- read.csv("I:/Projects/International Correlation of HPV and Cervical Cancer/codes and documents/data/incidence data for R codes/CI5-VIII/CI5-VIII.csv", header = FALSE)
files.IX <- "45860199.csv"
pathIX <- file.path("I:/Projects/International Correlation of HPV and Cervical Cancer/codes and documents/data/incidence data for R codes/CI5-IXd", files.IX)
dataIX <- lapply(pathIX, read.csv, header = FALSE)
#casesX <- read.csv("I:/Projects/International Correlation of HPV and Cervical Cancer/codes and documents/data/incidence data for R codes/CI5-X/cases.csv")
#popX <- read.csv("I:/Projects/International Correlation of HPV and Cervical Cancer/codes and documents/data/incidence data for R codes/CI5-X/pop.csv")
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
  filter(V1 == 125) %>% 
  filter(V2 == 2 & V3 == 120 & V4 >= 4 & V4 <= 16) %>% # filter females, cervical cancer, age >= 15 & <= 79 (age groups here 1-19)
  mutate("V7" = round(V5 * 100000 / V6, 2)) %>% # new column with incidence rate per 100000
  select(-V2, -V3, -V5, - V6) %>%
  spread(V4, V7) %>%
  mutate("ci5" = 8) %>%
  mutate("country" = c("Viet Nam, Hanoi"))  %>% # which registry exactly see Excel overview
  mutate("cid" = c(14))
#incVIII <- incVIII[rep(1, each=5),] # one line per year as incidence time intervals differ from mortality's
#incVIII$Year <- c(1993:1997)
colnames(incVIII) <- c("REG.VIII", "R15", "R20", "R25", "R30", "R35", "R40", "R45", 
                       "R50", "R55", "R60", "R65", "R70", "R75", "ci5", "country", "cid") #, "Year")

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
names(dataIX) <- 45860199 # will be added as own column during transformation into data frame
dataIX<- plyr::ldply(dataIX, data.frame) # transform list into data.frame
incIX <- dataIX %>%
  filter(V1 == 2 & V2 == 117 & V3 >= 4 & V3 <= 16) %>% # filter females, cervical cancer, age >= 15 & <= 79 (age groups here 1-19)
  mutate("V6" = round(V4*100000/V5, 2)) %>% # incidence rates
  select("REG.IX" = .id, V3, V6, -V1, -V2, -V4, -V5) %>% # drop sex, age, cases, personyears
  spread(V3, V6) %>% # horizontal orientation of data frame to match other volumes
  mutate(ci5 = 9)
incIX <- incIX %>% mutate(country = "Pakistan", cid = 25)
#rowsIX <- nrow(incIX)
#incIX <- incIX[rep(1:rowsIX, each=5),] # one line per year as incidence time intervals differ from mortality's
#incIX$Year <- rep(1998:2002, rowsIX) 
colnames(incIX) <- c("REG.IX", "R15", "R20", "R25", "R30", "R35", "R40", "R45", 
                     "R50", "R55", "R60", "R65", "R70", "R75", "ci5", "country", "cid") #, "Year") 


####`CI5-XI 2008-2012 ####
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
#rowsXI <- nrow(incXI)
#incXI <- incXI[rep(1:rowsXI, each=5),] # one line per year as incidence time intervals differ from mortality's
#incXI$Year <- rep(2008:2012, rowsXI) 
colnames(incXI) <- c("REGISTRY", "R15", "R20", "R25", "R30", "R35", "R40", "R45", 
                     "R50", "R55", "R60", "R65", "R70", "R75", "ci5", "country", "cid") #, "Year")

####`all available incidence rates from ci5 volume 9-11 ####`
# matched with cid for locations in which hpv prevalence studies were conducted
# intervals are filled with year under assumption that incidence constant over time interval.
inc_iarc <- rbind(incVIII[, 2:dim(incVIII)[2]], incIX[, 2:dim(incIX)[2]], incXI[,2:dim(incXI)[2]], na.omit = TRUE)
inc_iarc$cid <- as.numeric(inc_iarc$cid)

# inc.long
inc <- inc_iarc %>%
  tidyr::gather(., "age.grp", "IR", 1:13) 
#inc.long <- inc.long[rep(1:nrow(inc.long),each=5),] # time intervals are five years, so each registry entry per ci5 must be repeated 5 times
inc.long <- inc.long %>%
  mutate(age = c(as.numeric(stringr::str_sub(inc.long$age.grp, 2,3)) + 0:4))


####`7. prevalence ####
# pooled.data <- read_dta("I:/Projects/International Correlation of HPV and Cervical Cancer/codes and documents/data/prevalence data for R codes/HPVPREV_POOL_V29-1.dta")

Hrisk <- c("ahpv16", "ahpv18", "ahpv31","ahpv33","ahpv35","ahpv39","ahpv45", 
           "ahpv51","ahpv52","ahpv56","ahpv58","ahpv59","ahpv68", "ahpv73", "ahpv82") # omitted apvhrx for NA reason
cdata <- pooled.data %>%
  filter(!is.na(ahpv16))%>% # dplyr calculation cannot handle NA. if ahpv16 == NA, all ahpv columns NA
  filter(sgcentre == 19) # in costa rica all sel_paper) == 0 as np paper yet

clmbdata <- pooled.data %>%
  filter(!is.na(ahpv16))%>% 
  filter(sgcentre == 12) %>% # colombia paper normal cyt only
  filter(!is.na(sgid))

mexdata <- pooled.data %>%
  filter(!is.na(ahpv16))%>% 
  filter(sgcentre == 8) %>% # mexico normal cyt only
  filter(!is.na(sgid))

orig <- info$sgcentre[info$Location != "Costa Rica" & info$Location != "Colombia" & info$Location != "Mexico" ]

if(pooled.data$sgcentre %in% orig){ 
  prev.data <- pooled.data[-which(pooled.data$sel_paper0 == 0), ]
}
prev.data <- rbind(prev.data, cdata, clmbdata, mexdata)

pooled.hrisk <- prev.data %>%
  filter(betag == 1) %>%
  select(sgcentre, sgid, sga3, Hrisk) %>%
  filter(sgcentre %in% info$sgcentre) %>%
  filter(sga3 >= minp &  sga3 < maxp) %>%
  filter(!is.na(sga3))
prvl <- pooled.hrisk %>%
  mutate(hpvpos = rowSums(pooled.hrisk[, Hrisk])) %>% # number of different hpv infections
  mutate(hpvsino = ifelse(hpvpos > 0, 1, 0)) %>% # factor if hpv  positive or negative for high risk types. NA now as 0 !?
  mutate(hpvsino = factor(hpvsino, levels = c(0,1), labels = c("neg", "pos"))) %>%
  mutate(age.grp = factor(cut(sga3, seq(minp, maxp, ageintp), right = FALSE), labels = stringr::str_c("ag", 1:((maxp-minp)/ageintp), collapse = NULL))) %>%
  filter(!is.na(hpvsino)) %>%
  filter(!is.na(sgcentre)) %>%
  ungroup() %>%
  group_by(sgcentre, age.grp) %>%
  summarise(prev = sum(hpvsino == "pos")/n(), se = round(1.96*sqrt((sum(hpvsino == "pos")/n())*((1-sum(hpvsino == "pos"))/n())/n()), 4)) %>%
  full_join(., info, by = "sgcentre")
prvl

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

#prvl$age.grp <- as.factor(prvl$age.grp)
#prvl.long <- prvl[rep(1:nrow(prvl), each=ageintp),] # one line per year
#nbag <- length(levels(prvl$age.grp))
#if(nbag < 9) {
#  prvl.long$age <- c(((as.numeric(stringr::str_sub(prvl.long$age.grp, -1))-1)*ageintp + minp) + 0:(ageintp-1))
#}else{
#  prvl.long$age <- c((as.numeric(stringr::str_sub(prvl.long$age.grp, -2))*ageintp - minp) + 0:(ageintp-1))
#}
#prvl.long$Year <- as.numeric(prvl.long$stY) # to be able to join 
#prvl.long$cid <- as.numeric(prvl.long$cid)
#prvl.long$stY[prvl.long$cid == 4 ] <- 2003 # chile. as nor inc rates before 2003
#prvl.long <- prvl.long %>% select(sgcentre, age.grp, prev, se, cid, loc.prev, country, stY, Location, Year, age)
#prvl.long


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