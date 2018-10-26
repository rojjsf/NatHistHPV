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
tr_pp <- list() 

#fix country-specific variables:
c <- 17 # int.corr.overview.xlsx
sg <- 3 # codebook_pool_v1.doc
st <- 2000 # int.corr.overview.xlsx /or/ max(Ldata$sga1yy)
mina <- 25 # begin age group
maxa <- 34 # max age group
tr <- lcmm_out$tr_pred$`25-34`

### cum inc to compare
cum_dat <- Dp_out$Dp25_34 %>% ## specify age group!
  ungroup() %>%
  group_by(sgcentre, cid, Location, stY) %>%
  arrange(YsncS) %>%
  #summarise(cumR = last(cumsum(IR/5*10^(-5))))
  summarise(cumR = last(cumsum(Nicc)/cumsum(PopFemale[YsncS == 0])))
cum_dat


#### Pre-Lexis object ####

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
  filter(Ldata$entry.age >= mina & Ldata$entry.age <= maxa) 

# dummy variable for PreLexis object. Will be ignored in simulation.
exit <- sample(1:3, size = dim(Ldata)[1], replace = TRUE) 
PreLex <- Lexis(entry = list(age = as.numeric(entry.age),
                             calender = as.numeric(Year)),
                entry.status = hpv,
                exit.status = factor(exit, labels = c("free", "hpv", "icc", "death"), levels = c(0, 1, 2, 3)),
                dur = 1, 
                id = sgid,
                data = Ldata)



#### Rates ####
Dpop$Rate <-  Dpop$Nicc/Dpop$Npos
Dpop$YsncS <- as.numeric(Dpop$Year) - as.numeric(Dpop$stY)
lex_dat <- Dpop %>%  
  filter(YsncS >=0, !is.na(Rate)) %>%  
  mutate(lex.dur = 1) %>%
  mutate(calender = as.numeric(Year)) %>%
  mutate(mort.rate = MR*10^(-5))
head(lex_dat)


# mortality 
mr <- function(x){
  for(a in 1:nrow(x)){
    id <- x[a, "age"] 
    y <- x[a, "calender"] #name of a time scale  
    return(lex_dat[lex_dat$age==id & lex_dat$calender == y & lex_dat$cid == c, "mort.rate"]*(10^(-5))) # two time scales with same units
  }
}

# incidence rates from predicted model
ir <- function(x){
  for(b in 1:nrow(x)){
    if(x[b, "lex.Cst"] == "hpv"){
      y <- as.numeric(x[b, "calender"]) - st
      return(exp(tr$pred[tr$times$YsncS == y, 1]))
    }
  }
}

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

## plot
pp <- as.data.frame(pState( nSt, perm=c(3, 2, 1)))
pp$time <- c(st:2012)
tr_pp$pp[[stringr::str_c("c_", c, "_", mina, "-", maxa)]] <- pp["2012", "icc"]
tr_pp$cum[[stringr::str_c("c_", c, "_", mina, "-", maxa)]] <- cum_point <- cum_dat$cumR[cum_dat$cid==c]
tr_pp$plot[[stringr::str_c("c_", c, "_", mina, "-", maxa)]] <-
  ggplot(pp, aes(time, icc)) +
  theme_minimal() +
  geom_line() +
  ylim(0, 0.01) +
  geom_point(aes(x = 2012, y = cum_point), color = "red") + 
  labs(title = stringr::str_c("Lexis predicted cumProb(ICC|no death) in cid=", c, ", ", mina, "-", maxa, "y"),
       subtitle = "cumulative incidence in all women") +
  theme(plot.subtitle = element_text(color = "red"))


### comparision
cum_prob <- as.data.frame(t(bind_rows(tr_pp$cum, tr_pp$pp))) # transpose: switch rows and columns
colnames(cum_prob) <- c("cum", "pred")

ggplot(cum_prob, aes(pred, cum)) +
  geom_point() +
  xlim(0, 0.01) +
  ylim(0, 0.01)
 # diff colors per age group and cid!
