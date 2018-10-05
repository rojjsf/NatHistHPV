library(haven)
library(Epi)
library(dplyr)
library(tidyr)

#(http://BendixCarstensen.com/Epi/simLexis.pdf)
#### create semi-Lexis object with individual data from HPV prevalence studies####

pooled.data <- read_dta("I:/Projects/International Correlation of HPV and Cervical Cancer/codes and documents/data/prevalence data for R codes/HPVPREV_POOL_V29-1.dta")
Hrisk <- c("ahpv16", "ahpv18", "ahpv31","ahpv33","ahpv35","ahpv39","ahpv45","ahpv51",
           "ahpv52","ahpv56","ahpv58","ahpv59","ahpv68", "ahpv73", "ahpv82")
# UGANDA. not in pooled data set
# pooled.data <- read_dta("C:/Users/schultefrohlinder/Documents/R/NatHistHPV/data sets/girls-baseline-part-quest-clin-lab-sample-hpvres-fup-cyto-updbasefupoct2007-subtypes.dta")
# Hrisk <- c("h16", "h18", "h31","h33","h35","h39","h45","h51","h52","h56","h58","h59","h68_73", "h82")
#pooled.data <- pooled.data %>% mutate (sgcentre = 100) %>% 
#  mutate(betag = 1) %>% 
#  mutate(sgid = hpvcode) %>% 
#  mutate(sga3 = AGE)

#fix country-specific variables:
c <- 5 # int.corr.overview.xlsx
sg <- 19 # codebook_pool_v1.doc
st <- 1994 # int.corr.overview.xlsx /or/ max(Ldata$sga1yy)
mina <- 25 # begin age group
maxa <- 35 # max age group

Ldata <- pooled.data %>%
  mutate(hpvh = rowSums(pooled.data[, Hrisk], na.rm = TRUE)) %>%
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
         age.grp = (cut(entry.age, seq(15, 80, 5), right = FALSE)))%>%
  filter(is.na(.$hpv) == FALSE) 
tail(Ldata)
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
if(is.na(Lrates$ih)){
  Lrates <- Lrates[-which(is.na(Lrates$ih)), ] 
}
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

#### data frame Dp: select HPV positive women cohort by age, year, location ####
 # as in transition_estimate_UN_data.R
Dp <- data.frame(matrix(nrow = 0, ncol = 18))
colnames(Dp) <- colnames(Dpop)

for(j in pry$cid){
  sist <- (2012 - as.numeric(pry$prY[pry$cid == j])) # nb of years since prevalence Study
  if(pry$prY[pry$cid == j] < 2012){ # excluding Iran
    # cat("\n cid: ", j) # locating error
    for(i in 0:sist){
      a <- (mina+i):(maxa+i)
      y <- as.numeric(Dpop$prY) + i # i years added to year of prev. Study
      Dp <- rbind(Dp, Dpop[Dpop$Age %in% a & Dpop$Year==y & Dpop$cid== j, ])
    }
  }
}

#column stating Years since Prevalence Study
Dp <- Dp %>%
  mutate(YsncS = as.numeric(Dp$Year) - as.numeric(Dp$prY)) 
locDp <- Dp %>%
  filter(cid == c) %>%
  ungroup() %>%
  group_by(calender = Year, Location) %>% 
  summarise(Npos = sum(Npos), Nicc = sum(Nicc)) 

head(locDp)
ir <- glm(Nicc/Npos ~ calender, family = poisson(link = identity), weights = Npos, data = locDp, na.action = na.omit)
ci.lin(ir)


##Transition object
Tr <- list("hpv" = list("icc" = ir,
                        "death" = mr),
           "free" = list("death" = mr))

## Lexis simulation - only for one country
hpvSim <- simLexis(Tr, PreLex, t.range = 20, N = 20)
summary(hpvSim)

## simulate cohort
nSt <- nState(hpvSim,
              at=seq(0, (2012-st), 1), from= st, time.scale="calender")
nSt

## plot survival curves
#pp <- pState( nSt, perm=c(3, 2, 4, 1) ) # perm changes order of states (recalculaes percentages)
#tail( pp )
#pp2 <- pState( nSt, perm=c(3, 2) ) #cc risk for hpv positive only, given you are not dead 
#tail( pp2 )
#par(mfrow = c(2,2))
#plot( pp, col = c("darkred", "pink", "darkblue", "lightblue" ), ylim = c(0, 0.3))
pp <- pState( nSt, perm=c(3, 4, 2, 1) ) # perm changes order of states (recalculaes percentages)
tail( pp )
plot( pp, col = c("black", "grey", "white", "white" ), ylim = c(0, 0.2))
mtext(stringr::str_c("Costa Rica ", mina, "-", maxa, "y old, cumProb(ICC) & cumProb(death), poisson"), side = 3, line = 3)
