library(haven)
library(Epi)
library(dplyr)
library(foreign)
library(tidyr)

## create semi-Lexis object with data from costa Rica
cori.pooled <- read.dta("C:/Users/schultefrohlinder/Documents/HPV_Prevalence/Data/Costa Rica/costaricapool.dta")
head(cori.pooled)
cori <- cori.pooled %>%
  filter(hpvsino <= 1 & betag == 1) %>%
  select(sgid = sgid, 
         hpv = hpvsino, # all hpv positive women for Lexis testing, hr has to be computed in correct calculation
         age = sga3,
         Year= sga1yy) %>%
  mutate(age.grp = (cut(age, seq(15, 80, 5))))
str(cori)
exit <- sample(1:3, size = dim(cori)[1], replace = TRUE)
Lcori <- Lexis(entry = list(entry.age = as.numeric(age.grp),
                            entry.year = as.numeric(Year)),
               entry.status = factor(hpv, levels = c(0,1), labels = c("free", "hpv")),
               exit.status = factor(exit, labels = c("free", "hpv", "icc", "death"), levels = c(0, 1, 2, 3)),
               dur = 1, 
               id = sgid,
               data = cori)
str(Lcori)

## create rate models
inc.ci5.lexis <- inc.ci5.all %>%
  tidyr::gather(., "age.grp", "inc.rate", 2:14) %>%
  filter(loc == "Costa Rica") %>%
  mutate(lex.dur = 1) %>%
  mutate(entry.age = as.numeric(factor(age.grp, levels = c("R15_19", "R20_24", "R25_29", "R30_34", "R35_39", "R40_44", "R45_49", "R50_54", 
                                                "R55_59", "R60_64", "R65_69", "R70_74", "R75_79"), labels = levels(cori$age.grp)))) %>%
  mutate(icc = inc.rate*10^(-5)) %>%
  mutate(year = as.numeric(Year))
tail(inc.ci5.lexis)


mortality.lexis <- mortality %>%
  tidyr::gather(., "age.grp", "mort.rate", 3:15) %>%
  filter(Location == "Costa Rica") %>%
  mutate(lex.dur = 1) %>%
  mutate(entry.age = as.numeric(factor(age.grp, levels = c("M15_19", "M20_24", "M25_29", "M30_34", "M35_39", "M40_44", "M45_49", "M50_54", 
                                                "M55_59", "M60_64", "M65_69", "M70_74", "M75_79"), labels = levels(cori$age.grp)))) %>%
  mutate(death = mort.rate*10^(-5)) %>%
  mutate(year = as.numeric(Year))
tail(mortality.lexis)

## models (does not work yet. possibility to adjust for different countries)
#ir <- glm(icc ~ entry.age + year, family = poisson, offset = log(lex.dur), data = inc.ci5.lexis)
#mr <- glm(death ~ entry.age + year, family = poisson, offset = lex.dur, data = mortality.lexis)


## Tr as a function that takes a Lexis object as argument 
# and returns average rates for each record in the same units as lex.dur 
#(http://BendixCarstensen.com/Epi/simLexis.pdf)

#mortality rate, same weather hpv pos or neg
mr <- function(x){
  for(a in 1:nrow(x)){
    id <- x[a, "entry.age"] 
    y <- x[a, "entry.year"] #name of a time scale -> will be 
    if(x[a, "lex.Cst"] == "hpv"){
      return(mortality.lexis[mortality.lexis$entry.age==id & mortality.lexis$year == y, "mort.rate"]*(10^(-5)))
    }
    if(x[a, "lex.Cst"] == "free"){
      return(mortality.lexis[mortality.lexis$entry.age==id & mortality.lexis$year == y, "mort.rate"]*(10^(-5)))
    }
  }
  }

# incidence rates for hpv pos only
# has to be adapted for hpv pos women only in denominator
ir <- function(x){
  for(b in 1:nrow(x)){
    if(x[b, "lex.Cst"] == "hpv"){
      id <- x[b, "entry.age"]
      y <- x[b, "entry.year"]
     return(inc.ci5.lexis[inc.ci5.lexis$entry.age==id & inc.ci5.lexis$year == y, "inc.rate"]*(10^(-5)))
    }
  }
}

## Lexis simulation - only for one country
Tr <- list("hpv" = list("icc" = ir,
                        "death" = mr),
           "free" = list("death" = mr))

simCori <- simLexis(Tr, Lcori, t.range = 20, N = 10)
summary(simCori)

## simulate cohort
par(mfrow= c(1,1))
nSt <- nState( subset(simCori, entry.age %in% 2),
               at=seq(0, 19, 1), from=1993, time.scale="entry.year")
nSt

## plot survival curves
pp <- pState( nSt, perm=c(3, 2, 4, 1) ) # perm changes order of states (recalculaes percentages)
#pp <- pState( nSt, perm=c(3, 2) ) cc risk for hpv positive only, given you are not dead 
head( pp )
plot( pp, col = c("darkred", "pink", "darkblue", "lightblue" ))
mtext("Costa Rica HPV Study 20-25y old (N=10), incidence rates as in ci5", side = 3, line = 3)
mtext("HPV neg in 1993/1994, event free", col = "lightblue", side = 3, line = 2, adj = 0)
mtext("Death", col = "darkblue", side = 3, line = 0, adj = 0)
mtext("HPV pos in 1993/1994, event free", col = "pink", side = 3, line = 1, adj = 0)
mtext("Cervical Cancer", col = "darkred", side = 3, line = 0, adj = 0.1)

