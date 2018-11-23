library(haven)
library(Epi)
library(dplyr)
library(xlsx)
library(writexl)
library(tidyr)
library(ggplot2)
library(stringr)
library(gridExtra)


##################### I. info ########################

# locations from registry.txt
# spain, girona removed
# poland: kielce as also in lower bound as warsaw but not lowest
info <- data.frame(matrix(nrow = 30, ncol = 0))
info <- info %>%
  mutate("cid" = c(1,  2,  5,  6,  8,  9, 10, 12, 13, 15,  3, 16, 17,  4, 20, 19, 22,# assigned by Rosa
                   14, # vietnam, hanoi
                   25,# pakistan
                   11, # bhutan
                   31,# mongolia
                   34,# nepal
                   32, # rwanda
                   26, # mexico
                   30, # georgia
                   33, # Vanuatu
                   28, # Fiji
                   7, # Nigeria
                   29, #Guinea
                   71), #Shanxi
         sgcentre = c(44, 100, 19, 12, 23, 18, 61, 15,  2,  7,  9, 14,  3, 16, 83,  4, 41,# defined by IARC prevalence studies
                        1, # hanoi
                        60, # pakistan
                        65,# bhutan
                        25, # mongolia
                        42,# nepal
                        66, # Rwanda
                        8, # Mexico
                        45, # georgia
                        64, # Vanuatu
                        NA, # Fiji - not in pooled data
                        10, # Nigeria
                        11, #Guinea 
                        20 ), #Shanxi
         "stY" = c("2007-2008", "2002-2004", "1993-1994", "1993-1995", "2005", "2004", "2013-2014", "1999-2000", "1997", "1997-1998", 
                   "1998", "1990-2000", "1998", 
                   #"1998", 
                   "2001", "2002", "1995-1998", "2006",
                   "1997",
                   "2004-2008",
                   "2004-2008",
                   "2005",
                   "2006-2007",
                   "2013-2014",
                   "1996-1999",
                   "2007",
                   "2009-2011",
                   "2003-2007",
                   "1999",
                   "2006",
                   "2004"),
         "loc.prev" = c("Algeria, Setif","Uganda, Kyadondo County", "Costa Rica",                
                        "Colombia, Bucaramanga","China, Shenyang","India, Dindigul, Ambilikkai",                  
                        "Iran, Golestan Province", "Republic of Korea, Busan",   "Viet Nam, Ho Chi Minh City",                   
                        "Thailand, Lampang","Argentina, Entre Rios Province", "Thailand, Songkhla",        
                        "Spain, Tarragona", 
                        #"Spain, Girona", 
                        "Chile, Region of Antofagasta",                 
                        "Italy, Turin", "The Netherlands", "Poland, Warsaw",
                        "Viet Nam, Hanoi",
                        "Pakistan, South Karachi",
                        "Bhutan",
                        "Mongolia, Ulanbataar",
                        "Nepal, Bharatpur",
                        "Rwanda, Kigali",
                        "Mexico, Morelos State",
                        "Georgia, Tbilisi",
                        "Vanuatu, Port Vila",
                        "Fiji",
                        "Nigeria, Ibadan",
                        "Guinea, Conakry",
                        "China, Shanxi"),  # Prevalence STudy location
         "country" =  c("Algeria", "Uganda", "Costa Rica", "Colombia", "China", "India", "Iran (Islamic Republic of)",  "Republic of Korea", "Viet Nam", "Thailand", # for mortality, population
                        "Argentina", "Thailand", "Spain", 
                        #"Spain", 
                        "Chile", "Italy", "Netherlands", "Poland",
                        "Viet Nam",
                        "Pakistan",
                        "Bhutan",
                        "Mongolia",
                        "Nepal",
                        "Rwanda",
                        "Mexico",
                        "Georgia",
                        "Vanuatu",
                        "Fiji",
                        "Nigeria",
                        "Guinea",
                        "China"),
         "Location" = c("Algeria", "Uganda", "Costa Rica", "Colombia", "China(Shenyang)", "India", "Iran",  "Republic of Korea", "Viet Nam", "Thailand (Lampang)",
                        "Argentina", "Thailand (Songkla)", "Spain", 
                        #"Spain", 
                        "Chile", "Italy", "Netherlands", "Poland",
                        "Viet Nam, Hanoi",
                        "Pakistan",
                        "Bhutan",
                        "Mongolia",
                        "Nepal",
                        "Rwanda",
                        "Mexico",
                        "Georgia",
                        "Vanuatu",
                        "Fiji",
                        "Nigeria",
                        "Guinea",
                        "China(Shanxi)"), # for graph
         
         "REGISTRY" = c(101200199, # *Algeria, S?tif (2008-2011)
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
                      #572401099, # Spain, Girona (2008-2012)
                      215200399, # *Chile, Region of Antofagasta (2008-2010)
                      538000899, # Italy, Turin (2008-2012)
                      552800099, # *The Netherlands (2008-2012)
                      561600799, #Poland, Kielce (2008-2012)
                      125,# vietnam, hanoi
                      45860199, # pakistan
                      NA,# bhutan
                      NA, # mongolia
                      NA, # nepal
                      NA, # rwanda
                      NA, # mexico
                      NA, # georgia
                      NA, # Vanuatu
                      NA, # Fiji
                      NA, # Nigeria
                      NA, #Guinea
                      415600699)) %>% #China, Cixian County (2008-2012))) 
  separate(stY, into = paste0("stY", c(0, "")))

info$stY[info$cid %in% c(8, 9, 13, 3, 17, 4, 20, 22, 14, 31, 30, 7, 29)] <- c(2005, 2004, 1997, 1998, 2000, 2003, 2002, 2006, 1997, 2005, 2007, 1999, 2006) # filling NA if only one study year
info$sgcentre <- as.numeric(info$sgcentre)




########################## II. Prevalence ######################################################################


ageintp <- 10
minp <- 25
maxp <- 65

pooled.data <- read_dta("I:/Projects/International Correlation of HPV and Cervical Cancer/codes and documents/data/prevalence data for R codes/HPVPREV_POOL_V29-1.dta")

# high: 16, 18, 31, 33, 35, 39, 45, 51, 52, 56, 58, 59, 
# probably high: 26, 53, 66, 68, 73, 82
Hrisk <- c("ahpv16", "ahpv18", "ahpv31","ahpv33","ahpv35","ahpv39","ahpv45", 
           "ahpv51","ahpv52","ahpv56","ahpv58","ahpv59") # omitted apvhrx for NA reason (?)

cdata <- pooled.data %>%
  filter(!is.na(ahpv16))%>% # dplyr calculation cannot handle NA. if ahpv16 == NA, all ahpv columns NA
  filter(sgcentre == 19) # in costa rica all sel_paper) == 0 as np paper yet

clmbdata <- pooled.data %>%
  filter(!is.na(ahpv16))%>% 
  filter(sgcentre == 12) %>% # colombia paper normal cyt only
  filter(!is.na(sgid))

mexdata <- pooled.data %>%
  filter(!is.na(ahpv16))%>% 
  filter(sgcentre == 8) %>% # mexico normal cyt only
  filter(!is.na(sgid))

orig <- info$sgcentre[info$Location != "Costa Rica" & info$Location != "Colombia" & info$Location != "Mexico" ]

if(pooled.data$sgcentre %in% orig){ 
  prev.data <- pooled.data[-which(pooled.data$sel_paper0 == 0), ]
}
prev.data <- rbind(prev.data, cdata, clmbdata, mexdata)

pooled.hrisk <- prev.data %>%
  filter(betag == 1) %>%
  select(sgcentre, sgid, sga3, Hrisk) %>%
  filter(sgcentre %in% info$sgcentre) %>%
  filter(sga3 >= minp &  sga3 < maxp) %>%
  filter(!is.na(sga3))
prvl_iarc <- pooled.hrisk %>%
  mutate(hpvpos = rowSums(pooled.hrisk[, Hrisk])) %>% # number of different hpv infections
  mutate(hpvsino = ifelse(hpvpos > 0, 1, 0)) %>% # factor if hpv  positive or negative for high risk types. NA now as 0 !?
  mutate(hpvsino = factor(hpvsino, levels = c(0, 1), labels = c("neg", "pos"))) %>%
  mutate(age.grp = factor(cut(sga3, seq(minp, maxp, ageintp), right = FALSE), labels = stringr::str_c("ag", 1:((maxp-minp)/ageintp), collapse = NULL))) %>%
  filter(!is.na(hpvsino)) %>%
  filter(!is.na(sgcentre)) %>%
  ungroup() %>%
  group_by(sgcentre, age.grp) %>%
  summarise(prev = sum(hpvsino == "pos")/n(), 
            se = round(sqrt((sum(hpvsino == "pos")/n())*((1-sum(hpvsino == "pos")/n()))/n()), 4),
            n = n(),
            c = sum(hpvsino == "pos")) %>%
  # filter(c >= 5) %>% # to make sure estimates and standard errors are reliable
  full_join(., info, by = "sgcentre")
prvl_iarc

# uganda
uga.data <- read_dta("C:/Users/schultefrohlinder/Documents/R/NatHistHPV/data sets/girls-baseline-part-quest-clin-lab-sample-hpvres-fup-cyto-updbasefupoct2007-subtypes.dta")
uHrisk <- c("h16", "h18", "h31","h33","h35","h39","h45","h51","h52","h56","h58","h59")
uga.data <- uga.data %>%
  filter(select_paper_baseline == 1) %>%
  select(HPVNUM, "sga3" = AGE, uHrisk) %>%
  mutate(sgcentre = 100)  %>%
  filter(sga3> minp & sga3 < maxp)
uga.data[is.na(uga.data)] <- 0 
if(nrow(uga.data) > 0) {
  prvl_iarc <- uga.data %>%
    mutate(hpvpos = rowSums(uga.data[, uHrisk])) %>% # number of different hpv infections
    mutate(hpvsino = ifelse(hpvpos > 0, 1, 0)) %>% # factor if hpv  positive or negative for high risk types. NA now as 0 !?
    mutate(hpvsino = factor(hpvsino, levels = c(0,1), labels = c("neg", "pos"))) %>%
    mutate(age.grp = factor(cut(sga3, seq(minp, maxp, ageintp), right = FALSE), labels = stringr::str_c("ag", 10/ageintp))) %>%
    ungroup() %>%
    group_by(sgcentre, age.grp) %>%
    summarise(prev = sum(hpvsino == "pos")*100/n(), 
              se = round(sqrt(sum(hpvsino == "pos")*100/n()*(1-sum(hpvsino == "pos")*100/n())/n()), 4)) %>%
    left_join(., info, by = "sgcentre") %>%
    union(., prvl_iarc)
}


## Fiji
fiji.data <- read_dta("C:/Users/schultefrohlinder/Documents/HPV_Prevalence/Data/Fiji/HPVPrev_FIJI.dta")
head(fiji.data)
fiji[which(!is.na(fiji$HPV16)), ]
Hrisk <- c("HPV16", "HPV18", "HPV31","HPV33","HPV35","HPV39","HPV45", 
           "HPV51","HPV52","HPV56","HPV58","HPV59")

prvl_iarc <- fiji.data %>%
  mutate(hpvpos = rowSums(fiji.data[, Hrisk])) %>% # number of different hpv infections
  filter(!is.na(HPV16)) %>%
  mutate(hpvsino = ifelse(hpvpos > 0, 1, 0)) %>% # factor if hpv  positive or negative for high risk types. NA now as 0 !?
  mutate(hpvsino = factor(hpvsino, levels = c(0, 1), labels = c("neg", "pos"))) %>%
  mutate(age.grp = factor(cut(Q_Age, seq(minp, maxp, ageintp), right = FALSE), labels = stringr::str_c("ag", 1:((maxp-minp)/ageintp), collapse = NULL))) %>%
  filter(!is.na(hpvsino)) %>%
  ungroup() %>%
  group_by(age.grp) %>%
  summarise(prev = sum(hpvsino == "pos")/n(), 
            n = n(),
            c = sum(hpvsino == "pos"),
            se = round(sqrt((sum(hpvsino == "pos")/n())*((1-sum(hpvsino == "pos")/n()))/n()), 4)) %>%
  mutate(cid = 28) %>%
  # filter(c >= 5) %>% # to make sure estimates and standard errors are reliable
  left_join(., info, by = "cid") %>%
  union(., prvl_iarc)

head(prvl_iarc)

#prev_upd <- prvl_iarc %>%
#  spread(., age.grp, prev) %>%
#  select(Location, stY, age.grp, prev)

############################ III. Incidence #########################################################

#### III.1. inc: ci5-xi ####
# exctract incidence count ###
cases <- read.csv("C:/Users/schultefrohlinder/Documents/CI5-XI/cases.csv")
pyears <- read.csv("C:/Users/schultefrohlinder/Documents/CI5-XI/pop.csv")

cases <- cases %>%
  filter(REGISTRY %in% info$REGISTRY)%>%
  filter(SEX == 2) %>% # females
  filter(CANCER == 32) %>% # cervical cancer
  select(c(1, 10:17))  # age grps 
Ni <- merge(cases, info[, c("cid", "REGISTRY")], by = "REGISTRY")
l <- dim(Ni)[1]
Ni <- Ni %>%
  select(-REGISTRY) %>%
  gather("agN", "N", 1:8) %>%
  mutate(age.grp = rep(c("ag1", "ag2", "ag3", "ag4"), each = l*2)) %>%
  select(-agN)
# extract person years ###
pyears <- pyears  %>%
  filter(REGISTRY %in% info$REGISTRY)%>%
  filter(SEX == 2) %>% # females
  select(c(1, 9:16)) # age grps (7-3 = 4, 4*5 = 20. > 20y & <= 80 y)
Pi <- merge(pyears, info[, c("cid", "REGISTRY")], by = "REGISTRY")
l <- dim(Pi)[1]
Pi <- Pi %>%
  select(-REGISTRY) %>%
  gather("agP", "P", 1:8) %>%
  mutate(age.grp = rep(c("ag1", "ag2", "ag3", "ag4"), each = l*2))%>%
  select(-agP)

# inc: merged cases and pyears table ###
inc <- merge(Ni, Pi) # not by sgcentre as confusion when one centre twice (eg. spain = 3)
inc <- inc %>%
  ungroup() %>%
  group_by(age.grp, cid) %>%
  summarise("IR" = last(cumsum(N))*100000/last(cumsum(P)))

inc <- merge(inc, info)



#### III.2. inc 2: CI5 VIII-IX ####

# Viet Nam, Hanoi
incVIII <- dataVIII %>%
  filter(V1 == 125) %>% 
  filter(V2 == 2 & V3 == 120 & V4 >= 6 & V4 <= 13) %>% # filter females, cervical cancer, age >= 5 & <= 64 (age groups here 1-19)
  mutate("V7" = round(V5 * 100000 / V6, 2)) %>% # new column with incidence rate per 100000
  select(-V1, -V2, -V3, -V5, - V6) %>%
  mutate(age.grp = c("ag1", "ag1", "ag2", "ag2", "ag3", "ag3", "ag4", "ag4"))%>%  
  ungroup %>%
  group_by(age.grp) %>%
  summarise(IR = last(cumsum(V7))/2) %>%
  #mutate("ci5" = 8) %>%
  mutate("Location" = c("Viet Nam, Hanoi"))  %>% # which registry exactly see Excel overview
  mutate("cid" = c(14))


# Pakistan, Karachi
dataIX<- read.csv("C:/Users/schultefrohlinder/Documents/CI5-IXd/45860199.csv", head = FALSE) # transform list into data.frame
incIX <- dataIX %>%
  filter(V1 == 2 & V2 == 117 & V3 >= 6 & V3 <= 13) %>% # filter females, cervical cancer, age >= 25 & <= 64 (age groups here 1-19)
  mutate("V6" = round(V4*100000/V5, 2)) %>% # incidence rates
  select(V3, V6, -V1, -V2, -V4, -V5) %>% # drop sex, age, cases, personyears
  mutate(age.grp = c("ag1", "ag1", "ag2", "ag2", "ag3", "ag3", "ag4", "ag4"))%>%  
  ungroup %>%
  group_by(age.grp) %>%
  summarise(IR = last(cumsum(V6))/2) %>%
  #mutate("ci5" = 9) %>%
  mutate("Location" = c("Pakistan, South Karachi"))  %>% # which registry exactly see Excel overview
  mutate("cid" = 25)

inc2 <- rbind(incVIII, incIX)
inc2 <- merge(inc2, info, by = c("Location", "cid"))




#### III.3. inc_globo: Globocan 2018 incidence estimates####
 # download from http://gco.iarc.fr on 14/11/2018
glob_25_34 <- read.csv("C:/Users/schultefrohlinder/Documents/R/NatHistHPV/data sets/globocan_25_34.csv", row.names = NULL) %>%
  mutate(age.grp = "ag1")
glob_35_44 <- read.csv("C:/Users/schultefrohlinder/Documents/R/NatHistHPV/data sets/globocan_35_44.csv", row.names = NULL)%>%
  mutate(age.grp = "ag2")
glob_45_54 <- read.csv("C:/Users/schultefrohlinder/Documents/R/NatHistHPV/data sets/globocan_45_54.csv", row.names = NULL)%>%
  mutate(age.grp = "ag3")
glob_55_64 <- read.csv("C:/Users/schultefrohlinder/Documents/R/NatHistHPV/data sets/globocan_55_64.csv", row.names = NULL)%>%
  mutate(age.grp = "ag4")

glob.data <- rbind(glob_25_34, glob_35_44, glob_45_54, glob_55_64)

inc_glob <- glob.data %>%
  select(country = row.names, IR = Crude.Rate., age.grp) %>%
  filter(country %in% info$country[is.na(info$REGISTRY)])
inc_glob
inc_glob <- merge(inc_glob, info, by = c("country"))





#### III.4. inc_iarc: all incidences ####
 inc_iarc <- rbind(inc, inc2, inc_glob)
 inc_upd <- inc_iarc %>%
   spread(IR, age.grp) %>%
   select(Location, stY, age.grp, IR)



#### IV. inc.prev table for glm #### 
inc.prev.upd <- merge(prvl_iarc, inc_iarc)
inc.prev.upd$cid <- as.factor(inc.prev.upd$cid)
inc.prev.upd$age.grp <- factor(inc.prev.upd$age.grp, levels = c("ag1", "ag2", "ag3", "ag4"), labels = c("25-34", "35-44", "45-54", "55-64"))
inc.prev.upd$ci5 <- factor(is.na(inc.prev.upd$REGISTRY), levels = c(FALSE, TRUE), labels = c( "CI5", "Globocan 2018"))
inc.prev.upd$prev <- inc.prev.upd$prev*100  
#table.upd <- merge(prev_upd, inc_upd) %>%
#  mutate("ext" = ifelse(Location %in% cum_dat, "x", "")) # must run tr_pred_lex_cum_2.pdf


#### V. Spearman's correlation coefficient ####
spear <- list()
for (i in levels(inc.prev.upd$age.grp)){
  spear$all[[str_c(i)]] <- cor.test(x = inc.prev.upd$prev[inc.prev.upd$age.grp == i], y = inc.prev.upd$IR[inc.prev.upd$age.grp == i],
                     alternative = "two.sided", 
                     method = "spearman", 
                     exact = FALSE)
  
}

for (i in levels(inc.prev.upd$age.grp)){
  spear$ci5[[str_c(i)]] <- cor.test(x = inc.prev.upd$prev[inc.prev.upd$age.grp == i & inc.prev.upd$ci5 == "CI5"], 
                                    y = inc.prev.upd$IR[inc.prev.upd$age.grp == i & inc.prev.upd$ci5 == "CI5"],
                                alternative = "two.sided", 
                                method = "spearman", 
                                exact = FALSE)
  
}
rho_all <- as.vector(round(c(spear$all$`25-34`$estimate, 
                   spear$all$`35-44`$estimate, 
                   spear$all$`45-54`$estimate, 
                   spear$all$`55-64`$estimate), 2)) 




#### VI. Models ####
inc.prev.upd$wt <- 100000 

### all in one model
# poisson
corr.glm <- glm(IR ~ prev + prev*age.grp -1, family = poisson(link = "identity"), data = inc.prev.upd, weights = wt)
#ci.lin(corr.glm)
#summary(corr.glm)

### one age group after the other
#poisson
inc.prev.upd[inc.prev.upd$prev == 0, "prev"] <- 0.000001
corr.glm <- glm(IR ~ prev -1, family = poisson(link = "identity"), data = inc.prev.upd[inc.prev.upd$age.grp == "55-64",], weights = wt)

upd.glm <- list()
upd.coef <- list()
upd.p <- list()

for(i in levels(inc.prev.upd$age.grp)) {
upd.glm[[i]] <- glm(IR ~ prev -1, family = poisson(link = "identity"), data = inc.prev.upd[inc.prev.upd$age.grp == i,], weights = wt)
upd.coef[[i]] <- round(coef(upd.glm[[i]])[1], 2)
upd.p[[i]] <- ifelse(coef(summary(upd.glm[[i]]))[,4] < 0.001, "<0.0001", ">0.0001" )
}



## table overvie
prvl_iarc$age.grp <- factor(prvl_iarc$age.grp, levels = c("ag1", "ag2", "ag3", "ag4"), labels = c("25-34", "35-44", "45-54", "55-64"))
prvl_table <- prvl_iarc %>% 
  ungroup() %>%
  mutate(Year = stringr::str_c(stY0, "-", stY)) %>%
  mutate("Prevalence" = c(round(prvl_iarc$prev*100, 1))) %>%
  # filter(c>=5) %>%
  select(Location, Year, Prevalence, age.grp) %>%
  spread(age.grp, Prevalence) %>%
    select(-`<NA>`)

inc_iarc$age.grp <- factor(inc_iarc$age.grp, levels = c("ag1", "ag2", "ag3", "ag4"), labels = c("25-34", "35-44", "45-54", "55-64"))
inc_table <- inc_iarc %>% 
  ungroup() %>%
  mutate(Year = stringr::str_c(stY0, "-", stY)) %>%
  mutate("Incidence" = c(round(inc_iarc$IR, 1))) %>%
  mutate("est" = ifelse(is.na(REGISTRY), "globocan 2018", "CI5")) %>%
  select(Location, Year, Incidence, age.grp, est) %>%
  spread(age.grp, Incidence) 

corr_table <- full_join(prvl_table, inc_table, by = c("Location", "Year"))
write_xlsx(corr_table, "corr_table.xlsx")
####VII. plots####

options(scipen=999) # turns scientific notation off
labels <- c(`25-34` = str_c("                   25-34y
                            spearman: rho = ", round(spear$all$`25-34`$estimate, 2), ", p = " , round(spear$all$`25-34`$p.value, 5),
                            "
                            poisson: coef = ", upd.coef$`25-34`, ", p = ", upd.p$`25-34`),
            `35-44` = str_c("                   35-44y
                            spearman: rho = ", round(spear$all$`35-44`$estimate, 2), ", p = " , round(spear$all$`35-44`$p.value, 5), 
                            "
                            poisson: coef = ", upd.coef$`35-44`, ", p = ", upd.p$`35-44`),
            `45-54` = str_c("                   45-54y
                            spearman: rho = ", round(spear$all$`45-54`$estimate, 2), ", p = " , round(spear$all$`45-54`$p.value, 5),
                            "
                            poisson: coef = ", upd.coef$`45-54`, ", p = ", upd.p$`45-54`),
            `55-64` = str_c("                   55-64y 
                            spearman: rho = ", round(spear$all$`55-64`$estimate, 2), ", p = " , round(spear$all$`55-64`$p.value, 5),
                            "
                            poisson: coef = ", upd.coef$`55-64`, ", p = ", upd.p$`55-64`))


str(cntry)
cntry <- data.frame(cntry_ci5 = c(inc.prev.upd$loc.prev[inc.prev.upd$ci5 == "CI5" & inc.prev.upd$age.grp == "25-34"]),
                    cntry_globo = c(inc.prev.upd$loc.prev[inc.prev.upd$ci5 == "Globocan 2018" & inc.prev.upd$age.grp == "25-34"]), check.rows = FALSE)

inc.prev.upd$Location <- as.factor(inc.prev.upd$Location)

### by source type
ggplot(inc.prev.upd, aes(x = prev, y = IR)) +
  theme_classic() +
  geom_point(aes(color = ci5), size = 2) +
  scale_color_manual(values =  c('#7b3294', '#008837')) +
  #geom_text(aes(label = cid), hjust = 1.2, vjust = 1, size = 3) +
  facet_wrap(~age.grp, labeller = labeller(age.grp = labels), scales = "free_x") +
  theme(strip.text.x = element_text(hjust = -2, size = 12)) +
  guides(color = guide_legend(ncol = 1)) +
  labs(title = "International Correlation of hrHPV and Cervical Cancer",
       x = "hrHPV prevalence (%)",
       y = "Cervical Cancer Incidence (per 100 000 women)",
       color = "Incidence data source") +
  ylim(0, 155) +
  #xlim(0, 50) +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 24),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 18)) +
  geom_smooth(method = "glm", method.args = list(family = poisson(link = "identity")), 
              formula = y~x -1, aes(), color ="#252525", size = 1.5, se = TRUE, fill = "#cccccc") +
  geom_text(data =c(inc.prev.upd$loc.prev[inc.prev.upd$ci5 == "CI5" & inc.prev.upd$age.grp == "25-34"]), aes(colour=cntry[,1]), alpha=0, x=1, y=1)

### by country
sh <- c(0:24, 1, 4, 8, 9)
ggplot(inc.prev.upd, aes(x = prev, y = IR)) +
  theme_classic() +
  geom_point(aes(color = loc.prev, shape = loc.prev), size = 2) +
  scale_shape_manual(values = sh) +
  guides(color = guide_legend(ncol = 1),
         shape = guide_legend(ncol = 1)) +
  #geom_text(aes(label = cid), hjust = 1, vjust = 1, size = 3) +
  facet_wrap(~age.grp, labeller = labeller(age.grp = labels), scales = "free_x") +
  theme(strip.text.x = element_text(hjust = -2, size = 12)) +
  labs(title = "International Correlation of hrHPV and Cervical Cancer",
       x = "hrHPV prevalence (%)",
       y = "Cervical Cancer Incidence (per 100 000 women)",
       color = "Location",
       shape = "Location") +
  ylim(0, 155) +
  #xlim(0, 50) +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 24),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 18))+
  #geom_smooth(method = "glm", method.args = list(family = "poisson"(link = "identity")), formula = y~x-1, aes(), color ="black", size = 1, se = TRUE)
  geom_smooth(method = "glm", method.args = list(family = poisson(link = "identity")), 
              formula = y~x -1, aes(), color ="#252525", size = 1.5, se = TRUE, fill = "#cccccc")
