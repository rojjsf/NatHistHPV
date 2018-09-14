#### I. Costa Rica ####
## create semi-Lexis object with data from costa Rica
pooled.data <- read_dta("I:/Projects/International Correlation of HPV and Cervical Cancer/codes and documents/data/prevalence data for R codes/HPVPREV_POOL_V29-1.dta")

Hrisk <- c("ahpv16", "ahpv18", "ahpv31","ahpv33","ahpv35","ahpv39","ahpv45","ahpv51",
           "ahpv52","ahpv56","ahpv58","ahpv59","ahpv68", "ahpv73", "ahpv82")

Ldata <- pooled.data %>%
  mutate(hpvh = rowSums(pooled.data[, Hrisk])) %>%
  filter(sgcentre == 12)  # location
Ldata <- Ldata %>%
  mutate(hpv = ifelse(hpvh > 0, 1, 0)) %>% 
  mutate(hpv = factor(hpv, levels = c(0, 1), labels = c("free", "hpv"))) %>%
  select(sgid = sgid, 
         age = sga3,
         Year= sga1yy,
         hpv) %>%
  mutate(cid = 6, # location
         age.grp = (cut(age, seq(15, 80, 5))))%>%
  filter(is.na(.$hpv) == FALSE) 
head(Ldata)

exit <- sample(1:3, size = dim(Ldata)[1], replace = TRUE)
PreLex <- Lexis(entry = list(entry.age = as.numeric(age.grp),
                            entry.year = as.numeric(Year)),
               entry.status = hpv,
               exit.status = factor(exit, labels = c("free", "hpv", "icc", "death"), levels = c(0, 1, 2, 3)),
               dur = 1, 
               id = sgid,
               data = Ldata)
head(PreLex)

## create rate models

hpv.inc$loc
Lrates <- hpv.inc %>%
  filter(loc == "Colombia, Bucaramanga") %>% 
  mutate(lex.dur = 1) %>%
  mutate(year = as.numeric(Year))
tail(Lrates)
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
      return(Lrates[Lrates$entry.age==id & Lrates$year == y, "mort.rate"]*(10^(-5)))
    }
    if(x[a, "lex.Cst"] == "free"){
      return(Lrates[Lrates$entry.age==id & Lrates$year == y, "mort.rate"]*(10^(-5)))
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
      return(Lrates[Lrates$entry.age==id & Lrates$year == y, "ih"]*(10^(-5)))
    }
  }
}

## Lexis simulation - only for one country
Tr <- list("hpv" = list("icc" = ir,
                        "death" = mr),
           "free" = list("death" = mr))

hpvSim <- simLexis(Tr, PreLex, t.range = 20, N = 10)
summary(hpvSim)

## simulate cohort
par(mfrow= c(1,1))
nSt <- nState( subset(hpvSim, entry.age %in% 4),
               at=seq(0, 19, 1), from=1995, time.scale="entry.year")
nSt <- nState(hpvSim,
               at=seq(0, 19, 1), from=1993, time.scale="entry.year")
nSt

## plot survival curves
pp <- pState( nSt, perm=c(3, 2, 4, 1) ) # perm changes order of states (recalculaes percentages)
#pp <- pState( nSt, perm=c(3, 2) ) cc risk for hpv positive only, given you are not dead 
head( pp )
plot( pp, col = c("darkred", "pink", "darkblue", "lightblue" ))
mtext("Costa Rica HPV Study 50-54y old (N=10), incidence rates adjusted", side = 3, line = 3)
mtext("HPV neg in 1993/1994, event free", col = "lightblue", side = 3, line = 2, adj = 0)
mtext("Death", col = "darkblue", side = 3, line = 0, adj = 0)
mtext("HPV pos in 1993/1994, event free", col = "pink", side = 3, line = 1, adj = 0)
mtext("Cervical Cancer", col = "darkred", side = 3, line = 0, adj = 0.1)

