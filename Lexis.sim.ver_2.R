library(haven)
library(Epi)
library(dplyr)
library(tidyr)


## create semi-Lexis object with data from costa Rica
pooled.data <- read_dta("I:/Projects/International Correlation of HPV and Cervical Cancer/codes and documents/data/prevalence data for R codes/HPVPREV_POOL_V29-1.dta")

Hrisk <- c("ahpv16", "ahpv18", "ahpv31","ahpv33","ahpv35","ahpv39","ahpv45","ahpv51",
           "ahpv52","ahpv56","ahpv58","ahpv59","ahpv68", "ahpv73", "ahpv82")

Ldata <- pooled.data %>%
  mutate(hpvh = rowSums(pooled.data[, Hrisk])) %>%
  filter(sgcentre == 44)  #  location. For Italy must insert sga1yy = 2003 (true: 2002)
Ldata <- Ldata %>%
  mutate(hpv = ifelse(hpvh > 0, 1, 0)) %>% 
  mutate(hpv = factor(hpv, levels = c(0, 1), labels = c("free", "hpv"))) %>%
  select(sgid = sgid, 
         entry.age = sga3,
         Year = sga1yy,
         hpv) %>%
  mutate(cid = 1, # location 
         #Year = 2003, #For Italy must insert sga1yy = 2003 (true: 2002)
         # entry.age = rep(30, nrow(Ldata)), # simulation for one age grp
         age.grp = (cut(entry.age, seq(15, 80, 5))))%>%
  filter(is.na(.$hpv) == FALSE) 
head(Ldata)

#hist(Ldata$entry.age)
exit <- sample(1:3, size = dim(Ldata)[1], replace = TRUE)
PreLex <- Lexis(entry = list(age = as.numeric(entry.age),
                            calender = as.numeric(Year)),
               entry.status = hpv,
               exit.status = factor(exit, labels = c("free", "hpv", "icc", "death"), levels = c(0, 1, 2, 3)),
               dur = 1, 
               id = sgid,
               data = Ldata)
head(PreLex)

## create rate models

# one row for each exact age in every year and every location needed. FAKE as not really continuous
hpv.inc.long <- hpv.inc[rep(1:nrow(hpv.inc), each=5),]
Lrates <- hpv.inc.long %>%
  mutate(age = as.numeric(c(10 + as.numeric(hpv.inc.long$age.grp)*5 + 0:4))) %>%
  filter(cid == 1) %>%  # location
  mutate(lex.dur = 1) %>%
  mutate(calender = as.numeric(Year))
head(Lrates) # one row for each year-age combination. Yearly switch in next age group possible)

## models (does not work yet. possibility to adjust for different countries)
#ir <- glm(icc ~ entry.age + year, family = poisson, offset = log(lex.dur), data = inc.ci5.lexis)
#mr <- glm(death ~ entry.age + year, family = poisson, offset = lex.dur, data = mortality.lexis)


## Tr as a function that takes a Lexis object as argument 
# and returns average rates for each record in the same units as lex.dur 
#(http://BendixCarstensen.com/Epi/simLexis.pdf)

#mortality rate, same weather hpv pos or neg
#mr <- function(x){
#  for(a in 1:nrow(x)){
#    id <- x[a, "age"] 
#    y <- x[a, "entry.year"] #name of a time scale -> will be 
#    if(x[a, "lex.Cst"] == "hpv"){
#      return(Lrates[Lrates$age==id & Lrates$year == y, "mort.rate"]*(10^(-5)))
#    }
#    if(x[a, "lex.Cst"] == "free"){
#      return(Lrates[Lrates$age==id & Lrates$year == y, "mort.rate"]*(10^(-5)))
#    }
#  }
#}

# same mortality rate for hpv and free, so only one output needed (to be verified)
mr <- function(x){
  for(a in 1:nrow(x)){
    id <- x[a, "age"] 
    y <- x[a, "calender"] #name of a time scale  
    return(Lrates[Lrates$age==id & Lrates$calender == y, "mort.rate"]*(10^(-5))) # two time scales!
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

## Lexis simulation - only for one country
Tr <- list("hpv" = list("icc" = ir,
                        "death" = mr),
           "free" = list("death" = mr))

hpvSim <- simLexis(Tr, PreLex, t.range = 20, N = 1)
summary(hpvSim)

## simulate cohort
par(mfrow= c(1,1))
#nSt <- nState( subset(hpvSim, age %in% 65:70),
#               at=seq(0, 14, 1), from= 1998, time.scale="entry.year") # changing from year does NOT change probablities
nSt <- nState(hpvSim,
               at=seq(0, 14, 1), from= 1993, time.scale="entry.year")
nSt

## plot survival curves
pp <- pState( nSt, perm=c(3, 2, 4, 1) ) # perm changes order of states (recalculaes percentages)
pp <- pState( nSt, perm=c(3, 2) ) #cc risk for hpv positive only, given you are not dead 
tail( pp )
plot( pp, col = c("darkred", "pink", "darkblue", "lightblue" ))
mtext("Costa Rica HPV Study 50-54y old (N=10), incidence rates adjusted", side = 3, line = 3)
mtext("HPV neg in 1993/1994, event free", col = "lightblue", side = 3, line = 2, adj = 0)
mtext("Death", col = "darkblue", side = 3, line = 0, adj = 0)
mtext("HPV pos in 1993/1994, event free", col = "pink", side = 3, line = 1, adj = 0)
mtext("Cervical Cancer", col = "darkred", side = 3, line = 0, adj = 0.1)

inc.ci5.all[inc.ci5.all$cid == 6, ]
