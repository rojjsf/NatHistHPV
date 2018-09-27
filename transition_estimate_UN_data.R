## creating a poisson of icc among hpv positives using registry data

## population
NP <- read.csv("C:/Users/schultefrohlinder/Documents/R/NatHistHPV/data sets/WPP2017_PopulationBySingleAgeSex.csv")
# downloaded on 27/9/2018 from https://population.un.org/wpp/Download/Standard/CSV/
# same country coding as mortality
info.mort

## incidence per age GROUP and year
head(inc.ci5.all) # horizontal age groups
# vertical age groups
inc.long <- inc.ci5.all %>%
  tidyr::gather(., "inc.age", "inc.rate", 2:14)
inc.long$age.grp <- factor(inc.long$inc.age, levels = c("R15_19", "R20_24", "R25_29", "R30_34", "R35_39", "R40_44", "R45_49", "R50_54", "R55_59", 
                                                        "R60_64", "R65_69", "R70_74","R75_79"),
                           labels = fact.lbl) 
inc.long <- inc.long[rep(1:nrow(inc.long), each=5),] # one row for each time step (per year)
inc.long <- inc.long %>%
  mutate(Age = as.numeric(c(10 + as.numeric(inc.long$age.grp)*5 + 0:4))) %>%
  full_join(., info.mort, by = c("cid")) %>%
  select(-loc)
head(inc.long)

# joint data frame population data
NP$Age <- as.numeric(NP$AgeGrp)
Npop <- NP %>%
  filter(Location %in% inc.long$Location) %>%
  filter(Time %in% 1990:2012) %>%
  mutate(Year = Time) %>%
  select(c(-PopMale, -PopTotal, -ï..LocID, - AgeGrpSpan, -AgeGrpStart)) # AgeGrpSpan = 1 
Npop$Location <- factor(Npop$Location, levels = levels(inc.long$Location))  
Npop <- Npop %>%
  full_join(., info.mort, by = c("Location"))

Dpop <- Npop %>%
  right_join(., inc.long, by = c("cid", "Age", "Year", "Location"))
Dpop <- Dpop %>%
  left_join(., prev.model, by = c("cid", "age.grp", "Year")) %>% # adding prevalence. Different loc vectors for inc and prev, so already removed from prevalence table
  select(c(-sgcentre, -Year0, - AgeGrp, -inc.age))%>%
  mutate(Nicc = NA)%>%
  mutate(Npos = NA)%>%
  full_join(., mortality.lexis, by = c("cid", "age.grp", "Year", "Location")) %>% # adding mortality
  select(- lex.dur, - mort.age, - Time, - MidPeriod)
Dpop <- Dpop[order(Dpop$cid, Dpop$Year, Dpop$Age),] 

## #HPV positive women per age and year
if(is.na(Dpop$Age)){
  Dpop <- Dpop[-which(is.na(Dpop$Age)), ] 
}

for (i in 1:nrow(Dpop)){
  Dpop$Nicc[i] <- round(Dpop$PopFemale[i] * Dpop$inc.rate[i] *10^(-5) * 1000, 1)# number of cancer cases per age and year
  Dpop$Npos[i] <- round(Dpop$prev[i]*10^(-2) * Dpop$PopFemale[i] * 1000, 1)  # number of hpv positive women in Year aof hPV study and age group
  Dpop$PopFemale[i] <- Dpop$PopFemale[i] * 1000 # exact number of women per year and age group and country
}

for(i in 1:(nrow(Dpop))){
  if(!is.na(Dpop$Npos[i])) {
    a <- Dpop$Age[i]
    y <- Dpop$Year[i]
    loc <- Dpop$Location[i]
    Dpop$Npos[Dpop$Age == (a+1) & Dpop$Year == (y+1) & Dpop$Location == loc] <- round(Dpop$Npos[i] - Dpop$Npos[i]*Dpop$mort.rate[i]*10^(-5) - Dpop$Nicc[i], 1)
    Dpop$prev[Dpop$Age == (a+1) & Dpop$Year == (y+1) & Dpop$Location == loc] <- round(Dpop$Npos[i] *100 / Dpop$PopFemale[i], 2)
  }
}

Dpop[Dpop$Location == "Algeria" & Dpop$Year == "2009", ]
str(Dpop)

## poisson
Costa <- Dpop %>%
  filter(Location == "Costa Rica")

Costa <- Costa %>%
  ungroup() %>%
  group_by(Year) %>% 
  summarise(Npos = sum(Npos), Nicc = sum(Nicc))
head(Costa)
costaM <- glm(Nicc/Npos ~ Year, family = poisson(link = log), weights = Npos, data = Costa)
summary(costaM)

Ypop <- Dpop %>%
  ungroup() %>%
  group_by(Year, Location) %>% 
  summarise(Npos = sum(Npos), Nicc = sum(Nicc))
Ypop
yM <- glm(Nicc/Npos ~ Location + Year, family = poisson(link = log), weights = Npos, data = Ypop)
summary(yM)
ci.exp(yM)
