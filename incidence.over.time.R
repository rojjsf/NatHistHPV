library(haven)
library(Epi)
library(dplyr)
library(tidyr)

 # joint data table with all incidences

#### vectors containing variables ####
# locations from registry.txt
loc <- c("Algeria, Setif", # new
         "Uganda, Kyadondo County", # new
         "Costa Rica", # new
         "Colombia, Bucaramanga", # update
         "China, Shenyang", # new
         "India, Dindigul, Ambilikkai", # update
         "Iran (Islamic Republic of), Golestan Province", # new
         "Republic of Korea, Busan", # update
         "Viet Nam, Ho Chi Minh City", # update
         "Thailand, Lampang", # update
         "Argentina, Entre Rios Province", # new
         "Thailand, Songkhla", # update
         "Spain, Tarragona", # update (in former as Barcelona)
         "Spain, Girona", # -"-
         "Chile, Region of Antofagasta", # new
         "Italy, Turin", # update
         "The Netherlands", # update
         "pakistan, South Karachi",
         "Poland, Poznan")  # update
# "*Viet Nam, Hanoi", # same. in inc3 (added further down. kept sepereate as inc from ci5_9)
# "Pakistan, South Karachi")  # new(old incidence). in inc3
# countries included 2008 but now no incidences found: hanoi(Vietnam), Nigeria
# countries with iarc data but no incidence data: include with estimated incidences?



# cid = centre id, assigned by Rosa in order to idntify all matching incidences and prevalences, even from non-IARC data
cid <- c(1,  2,  5,  6,  8,  9, 10, 12, 13, 15,  3, 16, 17, 18,  4, 20, 19, 25, 22)


 # every volume of ci5 has slightly different registry codes 
 # to save time I only imported the coutries that there is incidence data for.
REG.VIII <- c(NA, # IRR Algeria, S?tif 
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
              NA, #	IRR Spain, Girona 
              NA, # Chile, Region of Antofagasta  
              NA, # IRR Italy, Turin 
              NA, # IRR The Netherlands
              113, # *Pakistan, South Karachi (1995-1997)
              NA) # IRR Poland, Poznan
# IRR = irrelevant as prevalence study was after volume time period

files.IX <- c(NA, # IRR Algeria, S?tif 
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
            "57241099.csv", #	Spain, Girona (1998-2002)
            NA, # Chile, Region of Antofagasta  (NO data for chile, prevalence study in 2001 - very close to 2002)
            "53800899.csv", # IRR Italy, Turin 
            "55280099.csv", #	The Netherlands (1998-2002)  
            NA,	# IRR Pakistan, South Karachi (1998-2002) 
            NA) # IRR Poland, Poznan 
REG.IX <- c(NA, # IRR Algeria, S?tif 
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
              57241099, #	Spain, Girona (1998-2002)
              NA, # Chile, Region of Antofagasta  (NO data for chile, prevalence study in 2001 - very close to 2002)
              53800899, # Italy, Turin 
              55280099, #	The Netherlands (1998-2002) 
              NA, # IRR Pakistan (No data)
              NA) # IRR Poland, Poznan 
REG.X <- c(1201, # *Algeria, S?tif (2003-2007)
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
         72410, # Spain, Girona (2003-2007)
         15203, # *Chile, Region of Antofagasta (2003-2007)
         38008,	# Italy, Turin (2003-2007
         52800, # The Netherlands (2003-2007)  
         NA, # Pakistan, South Karachi (NO data)
         61601) # Poland, Cracow (2003-2006) (!NOT Warsawa)

REG.XI <- c(101200199, # *Algeria, S?tif (2008-2011)
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
         NA, # Pakistan, Wouth karachi (NO data)
         561601099) # Poland, Poznan (2008-2012)



# extract incidence rates from each volume

#### CI5-VIII 1993-1997####
    #First column is population number (see population dictionary)
    #Second column is sex (1 Male/2 Female)
    #Third column is cancer number (see cancer dictionary)
    #Fourth column is age (1-19: 0-4,5-9,..,85+,unknown age)
    #Fifth column is number of cancer cases by age
    #Sixth column is person-years at risk by age

dataVIII <- read.csv("I:/Projects/International Correlation of HPV and Cervical Cancer/codes and documents/data/incidence data for R codes/CI5-VIII/CI5-VIII.csv", header = FALSE)
incVIII <- dataVIII %>%
  filter(V1 %in% REG.VIII) %>%
  filter(V2 == 2 & V3 == 120 & V4 >= 4 & V4 <= 16) %>% # filter females, cervical cancer, age >= 15 & <= 79 (age groups here 1-19)
  mutate("V7" = round(V5 * 100000 / V6, 2)) %>% # new column with incidence rate per 100000
  select(-V2, -V3, -V5, - V6) %>%
  spread(V4, V7) %>%
  mutate("loc" = c("Colombia, Cali", "Costa Rica", "Pakistan"))  %>%
  mutate("cid" = c(6, 5, 25)) %>%
  mutate("ci5" = 8)
incVIII <- incVIII[rep(1:2, each=5),] # one line per year as incidence time intervals differ from mortality's
incVIII$Year <- rep(1993:1997, 2)
colnames(incVIII) <- c("REGISTRY", "R15_19", "R20_24", "R25_29", "R30_34", "R35_39", "R40_44", "R45_49", 
                       "R50_54", "R55_59", "R60_64", "R65_69", "R70_74", "R75_79", "loc", "cid", "ci5", "Year")
head(incVIII)

#### CI5-IX 1998-2002 #### 
    #First column is sex (1 male/2 female)  
    #Second column is the cancer site number (1 to 244, see "cancer.txt") 
    #Third column is age-group, 0-4,5-9,10-14,...,80-84,85+,age unknown (1 to 19)  
    #Fourth column is number of cases  
    #Fifth column is person-years at risk
# 117 cervix uteri (C53)
info.IX <- data.frame("REGISTRY" = REG.IX, loc, cid, "ci5" = 9)
files.IX <- na.exclude(files.IX)
REG.IX <- na.exclude(REG.IX)
pathIX <- file.path("I:/Projects/International Correlation of HPV and Cervical Cancer/codes and documents/data/incidence data for R codes/CI5-IXd", files.IX)
dataIX <- lapply(pathIX, read.csv, header = FALSE)
# all files in one list, elements of which are named by registry numbers
names(dataIX) <- REG.IX # will be added as own column during transformation into data frame
dataIX<- plyr::ldply(dataIX, data.frame) # transform list into data.frame
incIX <- dataIX %>%
  filter(V1 == 2 & V2 == 117 & V3 >= 4 & V3 <= 16) %>% # filter females, cervical cancer, age >= 15 & <= 79 (age groups here 1-19)
  mutate("V6" = round(V4*100000/V5, 2)) %>% # incidence rates
  select("REGISTRY" = .id, V3, V6, -V1, -V2, -V4, -V5) %>% # drop sex, age, cases, personyears
  spread(V3, V6)# horizontal orientation of data frame to match other volumes
incIX <- merge(incIX, info.IX, by = "REGISTRY")
rowsIX <- nrow(incIX)
incIX <- incIX[rep(1:rowsIX, each=5),] # one line per year as incidence time intervals differ from mortality's
incIX$Year <- rep(1998:2002, rowsIX) 
colnames(incIX) <- c("REGISTRY", "R15_19", "R20_24", "R25_29", "R30_34", "R35_39", "R40_44", "R45_49", 
                     "R50_54", "R55_59", "R60_64", "R65_69", "R70_74", "R75_79", "loc","cid", "ci5", "Year")
head(incIX) 

#### CI5-X 2003-2007 ####
casesX <- read.csv("I:/Projects/International Correlation of HPV and Cervical Cancer/codes and documents/data/incidence data for R codes/CI5-X/cases.csv")
popX <- read.csv("I:/Projects/International Correlation of HPV and Cervical Cancer/codes and documents/data/incidence data for R codes/CI5-X/pop.csv")
head(popX)
casesX <- casesX %>%
  filter(REGISTRY %in% REG.X)%>%
  filter(SEX == 2) %>% # females
  filter(CANCER == 32) %>% # cervical cancer
  select(c(1, 8:20))  # age grps (upper limit of age groups: 8-4 = 4, 4*5 = 20. > 20y & <= 80 y)
popX <- popX  %>%
  filter(REGISTRY %in% REG.X)%>%
  filter(SEX == 2) %>% # females
  select(c(1, 7:19)) # age grps (7-3 = 4, 4*5 = 20. > 20y & <= 80 y)

incX <- merge(casesX, popX, by = "REGISTRY") 
dc <- dim(casesX)[2]  # length of data frame cases (so script can be run even if centres added)
dp <- dim(popX)[2] # length of data frame pyears
incX <- data.frame(incX, round(incX[, 2:dc] * 100000 / incX[, (dp +1 ):(2*dp-1)], 1)) # select colums with values, calculate rates
incX[, c(2:(dc), (dp+1):(2*dp-1))] <- NULL # delete cases and pyears columns
info.X <- data.frame("REGISTRY" = REG.X, loc, cid, "ci5" = 10)
incX <- merge(incX, info.X, by = "REGISTRY")
rowsX <- nrow(incX)
incX <- incX[rep(1:rowsX, each=5),] # one line per year as incidence time intervals differ from mortality's
incX$Year <- rep(2003:2007, rowsX) 
colnames(incX) <- c("REGISTRY", "R15_19", "R20_24", "R25_29", "R30_34", "R35_39", "R40_44", "R45_49", 
                    "R50_54", "R55_59", "R60_64", "R65_69", "R70_74", "R75_79", "loc", "cid", "ci5", "Year")
head(incX)

#### CI5-XI 2008-2011 ####
casesXI <- read.csv("I:/Projects/International Correlation of HPV and Cervical Cancer/codes and documents/data/incidence data for R codes/CI5-XI/cases.csv")
popXI <- read.csv("I:/Projects/International Correlation of HPV and Cervical Cancer/codes and documents/data/incidence data for R codes/CI5-XI/pop.csv")
casesXI <- casesXI %>%
  filter(REGISTRY %in% REG.XI)%>%
  filter(SEX == 2) %>% # females
  filter(CANCER == 32) %>% # cervical cancer
  select(c(1, 8:20))  # age grps (upper limit of age groups: 8-4 = 4, 4*5 = 20. > 20y & <= 80 y)
popXI <- popXI  %>%
  filter(REGISTRY %in% REG.XI)%>%
  filter(SEX == 2) %>% # females
  select(c(1, 7:19)) # age grps (7-3 = 4, 4*5 = 20. > 20y & <= 80 y)

incXI <- merge(casesXI, popXI, by = "REGISTRY") 
dc <- dim(casesXI)[2]  # length of data frame cases (so script can be run even if centres added)
dp <- dim(popXI)[2] # length of data frame pyears
incXI <- data.frame(incXI, round(incXI[, 2:dc] * 100000 / incXI[, (dp +1 ):(2*dp-1)], 1)) # select colums with values, calculate rates
incXI[, c(2:(dc), (dp+1):(2*dp-1))] <- NULL # delete cases and pyears columns
info.XI <- data.frame("REGISTRY" = REG.XI, loc, cid, "ci5" = 11)
incXI <- merge(incXI, info.XI, by = "REGISTRY")
rowsXI <- nrow(incXI)
incXI <- incXI[rep(1:rowsXI, each=5),] # one line per year as incidence time intervals differ from mortality's
incXI$Year <- rep(2008:2012, rowsXI) 
colnames(incXI) <- c("REGISTRY", "R15_19", "R20_24", "R25_29", "R30_34", "R35_39", "R40_44", "R45_49", "R50_54", 
                     "R55_59", "R60_64", "R65_69", "R70_74", "R75_79", "loc", "cid", "ci5", "Year")
head(incXI)


#### all available incidence rates from ci5 volume 9-11 #### 
# matched with cid for locations in which hpv prevalence studies were conducted
# intervals are filled with year under assumption that incidence constant over time interval.

inc.ci5.all <- rbind(incVIII, incIX, incX, incXI)
head(inc.ci5.all)
#inc.ci5.all <- inc.ci5.all[order(inc.ci5.all$Year), ]

inc.ci5.all[inc.ci5.all$cid == 17, ]

##### mortality and incidence per 100 000 per 5-year age group and country ####
# mortality from mortality rates UN.R

# legend --------------------
# REGISTRY: is the registry number assigned by ci5, differs in every volume
# R12_34: incidence rate in age group
# M12_34: mortality rate in age group
# cid: refers to prevalence study, computed by Rosa
# Midperiod: middle of 5 year UN mortality rate period
# loc: cancer registry locations copied from registy.txt of according ci5 volume
# Location: refers to country for which UN mortality rates were estimated

risk.rates <- full_join(inc.ci5.all, mortality, by = c("Year", "cid"))
head(risk.rates)
risk.rates[(risk.rates$Location == "Colombia"), ]



