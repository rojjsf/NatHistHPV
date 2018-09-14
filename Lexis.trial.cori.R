library(Epi)
library(foreign)
library(dplyr)
?simLexis
?Lexis

# create a pre-Lexis object 
# defines prevalent population and specifies which variables are time scales

# example using pooled costa rica data
cori.pooled <- read.dta("C:/Users/schultefrohlinder/Documents/HPV_Prevalence/Data/Costa Rica/costaricapool.dta")
head(cori.pooled)
cori <- cori.pooled %>%
  filter(hpvsino <= 1, 
         betag == 1) %>%
  select(sgid = sgid, 
         hpv = hpvsino, # all hpv positive women for Lexis testing, hr has to be computed in correct calculation
         age = sga3,
         stud.entry = sga1yy
         )
cori$hpv <- factor(cori$hpv, levels = c(0, 1), labels = c("free", "hpv"))
str(cori)
exit <- sample(2:4, size = dim(cori)[1], replace = TRUE)
Lcori <- Lexis(entry = list(entry.age = age,
                            entry.year = stud.entry),
               entry.status = factor(hpv),
               exit.status = factor(exit, labels = c("free", "hpv", "icc", "death"), levels = c(1, 2, 3, 4)),
               dur = 15, 
               id = sgid,
               data = cori)


# example using rates for costa rica
# extract incidence rates from each volume

## CI5-VIII 1993-1997
incVIII <- read.csv("I:/Projects/International Correlation of HPV and Cervical Cancer/codes and documents/data/incidence data for R codes/CI5-VIII/CI5-VIII.csv")

## CI5-IX 1998-2002
casesIX <- read.csv("I:/Projects/International Correlation of HPV and Cervical Cancer/codes and documents/data/incidence data for R codes/CI5-IXd/cases.csv")
popIX <- read.csv("I:/Projects/International Correlation of HPV and Cervical Cancer/codes and documents/data/incidence data for R codes/CI5-IXd/pop.csv")
## CI5-X 2003-2007
# costa rica: registry 18800
casesX <- read.csv("I:/Projects/International Correlation of HPV and Cervical Cancer/codes and documents/data/incidence data for R codes/CI5-X/cases.csv")
popX <- read.csv("I:/Projects/International Correlation of HPV and Cervical Cancer/codes and documents/data/incidence data for R codes/CI5-X/pop.csv")
casesX <- casesX %>%
  filter(REGISTRY == 18800)%>%
  filter(SEX == 2) %>% # females
  filter(CANCER == 32) %>% # cervical cancer
  select(c(1, 8:20))  # age grps (upper limit of age groups: 8-4 = 4, 4*5 = 20. > 20y & <= 80 y)
# extract person years
popX <- popX  %>%
  filter(REGISTRY == 18800)%>%
  filter(SEX == 2) %>% # females
  select(c(1, 7:19)) # age grps (7-3 = 4, 4*5 = 20. > 20y & <= 80 y)
# inc: merged cases and pyears table
incX <- merge(casesX, popX, by = "REGISTRY") 
dc <- dim(casesX)[2]  # length of data frame cases (so script can be run even if centres added)
dp <- dim(popX)[2] # length of data frame pyears

incX <- data.frame(incX, round(incX[, 2:dc] * 100000 / incX[, (dp +1 ):(2*dp-1)], 1)) # select colums with values, calculate rates
incX[, c(2:(dc), (dp+1):(2*dp-1))] <- NULL # delete cases and pyears columns
incX
incX <- incX %>%
  mutate(year = )
  
## CI5-XI 2008-2011
casesXI <- read.csv("I:/Projects/International Correlation of HPV and Cervical Cancer/codes and documents/data/incidence data for R codes/CI5-XI/cases.csv")
popXI <- read.csv("I:/Projects/International Correlation of HPV and Cervical Cancer/codes and documents/data/incidence data for R codes/CI5-XI/pop.csv")


#################################################################
## example

exit <- sample(1:3, size = dim(cori)[1], replace = TRUE)
Lcori <- Lexis(entry = list(entry.age = age,
                            entry.year = stud.entry),
               entry.status = factor(hpv, levels = c(0,1), labels = c("free", "hpv")),
               exit.status = factor(exit, labels = c("free", "hpv", "icc", "death"), levels = c(0, 1, 2, 3)),
               dur = 15, 
               id = sgid,
               data = cori)

# fake glm
inc.test <- data.frame(entry.year = as.numeric(rep(c(1993:2007), each = 60)),
                       lex.dur = 1, 
                  entry.age = as.numeric(rep(c(20:79), 15)),
                  icc = as.numeric(1:60*(rep(c(1993:2007), each = 60)-1990)*10^(-5)/3))
head(inc.test)
ir <- glm(icc ~ entry.age, family = poisson, offset = log(lex.dur), data = inc.test)

mort.test <- data.frame(entry.year = as.numeric(rep(c(1993:2007), each = 60)),
                        lex.dur = 1,
                        entry.age = as.numeric(rep(c(20:79), 15)),
                        death = as.numeric(11:70*(rep(c(1993:2007), each = 60)-1990)*10^(-5)/3))
                        #mort =  rep(rep(c(10, 40, 100, 
                 #150, 150.1, 200, 
                 #190, 230, 230, 250, 
                 #300, 500), each = 5), 15))
mr <- glm(death ~ entry.age + entry.year, family = poisson, offset = log(lex.dur), data = mort.test)

## Tr as functions
md <- function(Lcori, mortality.lexis){
  cbind(Lcori[, c()])
}



## Lexis simulation
Tr <- list("hpv" = list("icc" = ir,
                        "death" = mr),
           "free" = list("death" = mr))

simCori <- simLexis(Tr, Lcori, t.range = 30, N = 10)
summary(simCori)
## simulate cohort
par(mfrow= c(2, 2))
nSt <- nState( subset(simCori, entry.age %in% 65:80),
               at=seq(0, 25, 1), from=1993, time.scale="entry.year")
nSt
## plot survival curves
pp <- pState( nSt, perm=c(3, 2, 4, 1) ) # perm changes order of states (recalculaes percentages)
head( pp )
plot( pp, col = c("darkred", "pink", "darkblue", "lightblue" ))

