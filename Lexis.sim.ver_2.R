library(haven)
library(Epi)
library(dplyr)
library(tidyr)

#(http://BendixCarstensen.com/Epi/simLexis.pdf)
#### create semi-Lexis object with individual data from HPV prevalence studies####

pooled.data <- read_dta("I:/Projects/International Correlation of HPV and Cervical Cancer/codes and documents/data/prevalence data for R codes/HPVPREV_POOL_V29-1.dta")
Hrisk <- c("ahpv16", "ahpv18", "ahpv31","ahpv33","ahpv35","ahpv39","ahpv45","ahpv51",
           "ahpv52","ahpv56","ahpv58","ahpv59","ahpv68", "ahpv73", "ahpv82")
#fix country-specific variables:
c <- 5 # int.corr.overview.xlsx
sg <- 19 # codebook_pool_v1.doc
st <- 1994 # int.corr.overview.xlsx /or/ max(Ldata$sga1yy)
mina <- 25 # begin age group
maxa <- 30 # max age group

Ldata <- pooled.data %>%
  mutate(hpvh = rowSums(pooled.data[, Hrisk])) %>%
  filter(sgcentre == sg) %>% #  location. 
  filter(betag == 1) 
Ldata <- Ldata %>%
  mutate(hpv = ifelse(hpvh > 0, 1, 0)) %>% 
  mutate(hpv = factor(hpv, levels = c(0, 1), labels = c("free", "hpv"))) %>%
  # mutate(Year = max(Ldata$sga1yy)) %>%
  mutate(Year = st) %>%
  select(sgid = sgid, 
         entry.age = sga3,
         Year,
         hpv) %>%
  mutate(cid = c,  # location. 
         # entry.age = c(rep(25, nrow(Ldata))), # simulation for one age
         age.grp = (cut(entry.age, seq(15, 80, 5))))%>%
  filter(is.na(.$hpv) == FALSE) 
head(Ldata)
# fix age group 
Ldata <- Ldata %>%
  filter(Ldata$entry.age >= mina & Ldata$entry.age < maxa) 

# dummy variable for PreLexis object. Will be ignored in simulation.
exit <- sample(1:3, size = dim(Ldata)[1], replace = TRUE) 
PreLex <- Lexis(entry = list(age = as.numeric(entry.age),
                            calender = as.numeric(Year)),
               entry.status = hpv,
               exit.status = factor(exit, labels = c("free", "hpv", "icc", "death"), levels = c(0, 1, 2, 3)),
               dur = 1, 
               id = sgid,
               data = Ldata)


####Data frame with mortality and hpv+ adjusted incidence rates per age and calender year ####
# rates from inc.hpv.pos.R

hpv.inc.long <- hpv.inc[rep(1:nrow(hpv.inc), each=5),] # one row for each time step (per year)
Lrates <- hpv.inc.long %>%
  mutate(age = as.numeric(c(10 + as.numeric(hpv.inc.long$age.grp)*5 + 0:4))) %>%
  # location
  filter(cid == c) %>%  
  mutate(lex.dur = 1) %>%
  mutate(calender = as.numeric(Year)) # variables must be of same class in both simulation objects
Lrates <- Lrates[-which(is.na(Lrates$ih)), ] 
head(Lrates) 

## Tr as a function that takes a Lexis object as argument 
# and returns average rates for each record in the same units as lex.dur 

# same mortality rate for hpv and free, so only one output needed (?)
mr <- function(x){
  for(a in 1:nrow(x)){
    id <- x[a, "age"] 
    y <- x[a, "calender"] #name of a time scale  
    return(Lrates[Lrates$age==id & Lrates$calender == y, "mort.rate"]*(10^(-5))) # two time scales with same units
  }
}

# incidence rates for hpv pos only
ir <- function(x){
  for(b in 1:nrow(x)){
    if(x[b, "lex.Cst"] == "hpv"){
      id <- x[b, "age"]
      y <- x[b, "calender"]
      return(Lrates[Lrates$age==id & Lrates$calender == y, "ih"]*(10^(-5)))
    }
  }
}

Tr <- list("hpv" = list("icc" = ir,
                        "death" = mr),
           "free" = list("death" = mr))

## Lexis simulation - only for one country
hpvSim <- simLexis(Tr, PreLex, t.range = 20, N = 50)
summary(hpvSim)

## simulate cohort
nSt <- nState(hpvSim,
               at=seq(0, 18, 1), from= 1994, time.scale="calender")
nSt

## plot survival curves
pp <- pState( nSt, perm=c(3, 2, 4, 1) ) # perm changes order of states (recalculaes percentages)
tail( pp )
pp2 <- pState( nSt, perm=c(3, 2) ) #cc risk for hpv positive only, given you are not dead 
tail( pp2 )
plot( pp, col = c("darkred", "pink", "darkblue", "lightblue" ))

#mtext("Costa Rica HPV Study 50-54y old (N=10), incidence rates adjusted", side = 3, line = 3)
#mtext("HPV neg in 1993/1994, event free", col = "lightblue", side = 3, line = 2, adj = 0)
#mtext("Death", col = "darkblue", side = 3, line = 0, adj = 0)
#mtext("HPV pos in 1993/1994, event free", col = "pink", side = 3, line = 1, adj = 0)
#mtext("Cervical Cancer", col = "darkred", side = 3, line = 0, adj = 0.1)

