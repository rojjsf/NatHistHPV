library(haven)
library(Epi)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

## creating a poisson of icc among hpv positives using registry data

## UN population data ####
NP <- read.csv("C:/Users/schultefrohlinder/Documents/R/NatHistHPV/data sets/WPP2017_PopulationBySingleAgeSex.csv", header = TRUE)
# downloaded on 27/9/2018 from https://population.un.org/wpp/Download/Standard/CSV/
# same country coding as mortality(mortality rates UN.R)
info.mort

#### incidence per age GROUP and year ####
 # incidence.over.time.R
head(inc.ci5.all) # horizontal age groups
inc.long <- inc.ci5.all %>%
  tidyr::gather(., "inc.age", "inc.rate", 2:14)
inc.long$age.grp <- factor(inc.long$inc.age, levels = c("R15_19", "R20_24", "R25_29", "R30_34", "R35_39", "R40_44", "R45_49", "R50_54", "R55_59", 
                                                        "R60_64", "R65_69", "R70_74","R75_79"),
                           labels = fact.lbl) 
inc.long <- inc.long[rep(1:nrow(inc.long), each=5),] # one row for each time step (per year) to enable joining all rates
inc.long <- inc.long %>%
  mutate(Age = as.numeric(c(10 + as.numeric(inc.long$age.grp)*5 + 0:4))) %>%
  full_join(., info.mort, by = c("cid")) %>%
  select(-loc)
head(inc.long)

# data frame: population data ####
NP$Age <- as.numeric(NP$AgeGrp)
Npop <- NP %>%  # number population(age, year)
  select(c(-1, -PopMale, -PopTotal,  - AgeGrpSpan, - AgeGrpStart)) %>%
  filter(Location %in% inc.long$Location) %>%
  filter(Time %in% 1990:2012) %>%
  mutate(Year = Time)  # AgeGrpSpan = 1 
Npop$Location <- factor(Npop$Location, levels = levels(inc.long$Location))  
Npop <- Npop %>%
  full_join(., info.mort, by = c("Location"))


# data frame: year of prev. study and Location ####
pry <- prevalence %>%
  separate(Year, into = paste0("Year", c(0, ""))) %>%
  filter(cid %in% Dpop$cid) %>%
  transmute("cid" = cid, "prY" = Year)
 # fill NA
pry$prY[pry$cid %in% c(8, 9, 13, 3, 17, 18, 4, 20, 22)] <- c(2005, 2004, 1997, 1998, 2000, 2000, 2003, 2002, 2006)
 
# data frame Dpop: full data on population ####
Dpop <- Npop %>%
  right_join(., inc.long, by = c("cid", "Age", "Year", "Location"))
pry$cid <- as.numeric(pry$cid)
Dpop <- Dpop %>%
  left_join(., prev.model, by = c("cid", "age.grp", "Year")) %>% # adding prevalence. Different loc vectors for inc and prev, so already removed from prevalence table
  select(c(-sgcentre, -Year0, - AgeGrp, -inc.age))%>%
  mutate(Nicc = NA)%>% # to be calculated
  mutate(Npos = NA)%>% # to be calculated
  full_join(., mortality.lexis, by = c("cid", "age.grp", "Year", "Location")) %>% # adding mortality
  select(- lex.dur, - mort.age, - Time, - MidPeriod) %>%
  full_join(., pry, by = "cid") 
Dpop <- Dpop[order(Dpop$cid, Dpop$Year, Dpop$Age),] 
head(Dpop)

### function: calculate Nicc, Npos, prev ####
if(is.na(Dpop$Age)){
  Dpop <- Dpop[-which(is.na(Dpop$Age)), ]  # no age = no incidence
}

for (i in 1:nrow(Dpop)){
  Dpop$PopFemale[i] <- Dpop$PopFemale[i] * 1000 # exact number of women per year and age group and country
  Dpop$Nicc[i] <- round(Dpop$PopFemale[i] * Dpop$inc.rate[i] *10^(-5), 1)# number of cancer cases per age and year
  Dpop$Npos[i] <- round(Dpop$prev[i]*10^(-2) * Dpop$PopFemale[i], 1)  # number of hpv positive women in Year of hpv study and age group

}

for(i in 1:(nrow(Dpop))){
  if(!is.na(Dpop$Npos[i])) {
    a <- Dpop$Age[i]
    y <- Dpop$Year[i]
    loc <- Dpop$cid[i]
    Dpop$Npos[Dpop$Age == (a+1) & Dpop$Year == (y+1) & Dpop$cid == loc] <- round(Dpop$Npos[i] - Dpop$Npos[i]*Dpop$mort.rate[i]*10^(-5) - Dpop$Nicc[i], 1)
    Dpop$prev[Dpop$Age == (a+1) & Dpop$Year == (y+1) & Dpop$cid == loc] <- round(Dpop$Npos[i] *100 / Dpop$PopFemale[i], 2)
  }
}

Dpop$cid <- as.factor(Dpop$cid)
Dpop[Dpop$Location == "Costa Rica" & Dpop$Year == 2006,]
#### data frame Dp: select HPV positive women cohort by age, year, location ####
 # select age group
 # this part is copied into Lexis_sim.ver_3.R
mina <- 15
maxa <- 80
Dp <- data.frame(matrix(nrow = 0, ncol = 18)) # Dp = Data population (as nested in of Dpop)
colnames(Dp) <- colnames(Dpop)

for(j in pry$cid){
  sist <- (2012 - as.numeric(pry$prY[pry$cid == j])) # nb of years since prevalence Study
  if(pry$prY[pry$cid == j] < 2012){ # excluding Iran (in 2014)
  # cat("\n cid: ", j) # locating error
  for(i in 0:sist){
    a <- (mina+i):(maxa+i) # moving age group by one year
    y <- as.numeric(Dpop$prY) + i # i years added to year of prev. Study
    Dp <- rbind(Dp, Dpop[Dpop$Age %in% a & Dpop$Year==y & Dpop$cid== j, ])
  }
  }
}
#column stating Years since Prevalence Study
Dp <- Dp %>%
  mutate(YsncS = as.numeric(Dp$Year) - as.numeric(Dp$prY)) 


#data frame DpC: cumulative rates for full cohort per country, year (not over full time period) ####
 # Sum all cases and women of all ages (in previously selected age group) in each Year and country (location, cid)
DpC <- Dp %>%
  ungroup() %>%
  group_by(YsncS, Location, cid) %>% 
  summarise(Npos = sum(Npos), Nicc = sum(Nicc)) %>%
  filter(Location != "Viet Nam" & Location != "Uganda" & cid != 17 & cid != 18) # Spain wierd(???), Ugande <25y, Viet Nam time gap inc 1998-2003
# DpC[DpC$Location == "Algeria" & DpC$YsncS == 0, 4] <- 0.01 # ?? if left to be 0, Error: no valid set of coefficients has been found: please supply starting values
head(DpC) 
DpC[DpC$cid == 15, "Location"] <- "Thailand.L"
DpC[DpC$cid == 16, "Location"] <- "Thailand.S"
DpC$Location <- as.factor(DpC$Location)

#### Model transition rates hpv+ cohort all locations ####
 # poisson model to determine overall progression rate from former-hpv-positive group to ICC
#yM <- glm(Nicc*100000/Npos ~ YsncS + Location +  Location*YsncS, family = poisson(link = identity), weights = Npos, data = DpC)
yM <- glm(Nicc*100000/Npos ~ YsncS + Location -1, family = poisson(link = identity), weights = Npos, data = DpC) # let rate vary over time
summary(yM)
ci.lin(yM)
 # correlation test
cor.test(x = DpC$YsncS, y = (DpC$Nicc*100000/DpC$Npos),
         alternative = "two.sided", 
         method = "spearman", 
         exact = FALSE)
 # plot
ggplot(DpC, aes(x = YsncS, y = Nicc*100000/Npos)) + 
  geom_point(aes(color = Location)) + 
  geom_line(aes(color = Location)) +
  ggtitle(stringr::str_c("Yearly ICC Incidence Rate in birth cohort of women ", mina, "-", maxa, "y and HPV + at study entry")) +
  ylab("ICC incidence rate per 100 000") +
  xlab("Years since Prevalence Study") +
  theme_bw() +
  geom_abline(aes(intercept = sum(ci.lin(yM)[2:length(levels(DpC$Location))])/length(levels(DpC$Location)), slope = ci.lin(yM)[1]))
# can I take intercept as average of all intercepts???

#### Model incidence rates full cohort ####
 #####  no allowed to take sum of rates!!!!
prevalence$cid <- as.factor(prevalence$cid)
DpCall <- Dp %>%
  ungroup() %>%
  group_by(YsncS, Location, cid) %>% 
  summarise(Nicc = sum(Nicc), Npop = sum(PopFemale)) %>% 
  filter(Location != "Viet Nam" & Location != "Spain" & Location != "Uganda")  %>%
  mutate(Rate = Nicc*100000/Npop) %>%
  full_join(., prevalence, by = "cid") %>%
  select(-sgcentre, - loc) %>%
  separate(Year, into = paste0("Year", c(0, "")))
DpCall$Year[DpCall$cid %in% c(8, 9, 13, 3, 17, 18, 4, 20, 22)] <- c(2005, 2004, 1997, 1998, 2000, 2000, 2003, 2002, 2006)
DpCall$Year <- as.numeric(DpCall$Year)
DpCall[DpCall$cid == 15, "Location"] <- "Thailand.L"
DpCall[DpCall$cid == 16, "Location"] <- "Thailand.S"
DpCall$Location <- as.factor(DpCall$Location)
cor.test(x = DpCall$Nicc * 10^5/DpCall$Npop, y = (DpCall$YsncS),
         alternative = "two.sided", 
         method = "spearman", 
         exact = FALSE)
str(DpCall)
# poisson model to determine overall progression rate from former-hpv-positive group to ICC
 # adjusting for initial prevalence in age group instead of trying to calculate transition rate (???)
yMall <- glm(Nicc * 10^5/Npop ~ P3 + YsncS + Location + YsncS*Location + YsncS*P3 -1, family = poisson(link = identity), weights = Npop, data = DpCall)
yMall <- glm(Nicc * 10^5/Npop ~  YsncS + P3 + Year -1, family = poisson(link = identity), data = DpCall)
summary(yMall)
ci.lin(yMall)

ggplot(DpCall, aes(x = YsncS, y = Nicc * 10^5/Npop )) + 
  geom_point(aes(color = Location)) + 
  geom_line(aes(color = Location)) +
  ggtitle(stringr::str_c("Yearly ICC Incidence Rate in birth cohort of women ", mina, "-", maxa, "y at study entry")) +
  ylab("ICC incidence rate per 100 000") +
  xlab("Years since Prevalence Study") +
  theme_bw() #+
  #geom_abline(aes(intercept = sum(ci.lin(yMall)[2:length(levels(DpCall$Location))])/length(levels(DpCall$Location)), slope = ci.lin(yMall)[1]))

head(DpCall)


#### poisson for specified country only ####
locDp <- Dp %>%
  filter(Location == "Costa Rica") %>%
  ungroup() %>%
  group_by(calender = Year, Location) %>% 
  summarise(Npos = sum(Npos), Nicc = sum(Nicc)) 
#locDp[locDp$Location == "Algeria" & locDp$YsncS == 0, 4] <- 0.01 # ?? if left to be 0, Error: no valid set of coefficients has been found: please supply starting values
head(locDp)
yM <- glm(Nicc*100000/Npos ~ calender, family = poisson(link = identity), weights = Npos, data = locDp, na.action = na.omit)
ci.lin(yM)

ggplot(locDp, aes(x = calender, y = Nicc*100000/Npos)) + 
  geom_point(aes(x = calender, y = Nicc*100000/Npos)) +
  ggtitle(stringr::str_c("Yearly ICC Incidence Rate in birth cohort of women", mina, "-", maxa, "y and HPV + at study entry in ", locDp$Location[1])) +
  ylab("ICC incidence rate per 100 000") +
  xlab("Years since Prevalence Study") +
  theme_classic() +
  geom_abline(aes(intercept = ci.lin(yM)[1], slope = ci.lin(yM)[2])) 
summary(yM)

