## prev. in 5y age groups
pooled.data <- read_dta("I:/Projects/International Correlation of HPV and Cervical Cancer/codes and documents/data/prevalence data for R codes/HPVPREV_POOL_V29-1.dta")

### organize the data
# missing values
sel.paper0.rm <-  which(pooled.data$sel_paper0 == 0)
pooled.data <- pooled.data[-sel.paper0.rm, ]
# delete the rows in all colums of the women that have not been selected


### select high risk types ###
Hrisk <- c("ahpv16", "ahpv18", "ahpv31","ahpv33","ahpv35","ahpv39","ahpv45","ahpv51","ahpv52","ahpv56","ahpv58","ahpv59","ahpv68", "ahpv73", "ahpv82") # omitted apvhrx for NA reason
# age groups [15 - 90) in 5 year age groups, only keeping high risk types, id of women and their age.
pooled.hrisk <-  pooled.data[, c("sgcentre", "sgid", Hrisk, "sga3")]
# eventually remove age > 64 here? (as Delphine in her paper. analysis first?!)

### select age intervals ###
pooled.hrisk$age.grp <- factor(cut(pooled.hrisk$sga3, seq(15, 80, 5), right = FALSE), labels = c("P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8", "P9", "P10", "P11", "P12", "P13")) # if change, also change filter 2 rows below!
# age groups cannot be names as intervals as then they cannot be called in glm! (adapt when changing age groups!)
# right = FALSE: interval is open on the right (does not include upper endpoint) as in the cv5xi


###select ages #
pooled.hrisk <- pooled.hrisk %>%
  filter(sga3 >= 15 &  sga3 < 80) # omit women in younger/older age groups. !!! also change above & in Uganda & Costa Rica!!!!
pooled.hrisk <- pooled.hrisk %>%
  mutate(hpvpos = rowSums(pooled.hrisk[, Hrisk])) %>% # number of different hpv infections
  mutate(hpvsino = ifelse(hpvpos > 0, 1, 0)) %>%# factor if hpv  positive or negative for high risk types. NA now as 0 !?
  mutate(hpvsino = factor(hpvsino, levels = c(0,1), labels = c("neg", "pos")))

### gather in one table #
m <- matrix(data = NA, nrow = length(loc), ncol = length(levels(pooled.hrisk$age.grp)) + 5)
prevalence <- data.frame(m)
colnames(prevalence) <- c("cid", "sgcentre", "loc", "Year", "n", levels(pooled.hrisk$age.grp))
prevalence$loc <- inc$loc
prevalence$cid <- inc$cid
prevalence$sgcentre <- inc$sgcentre





################################# I. calculation of prevalence per country#####################################

##### I. algeria ####
alg.hrisk <- pooled.hrisk[pooled.hrisk$sgcentre == 44, ] # still individual data
alg.table <- table(alg.hrisk$age.grp, alg.hrisk$hpvsino) 
alg.table <- as.data.frame.matrix(alg.table)

# as.data.frame treats pos, neg as variables like age and therefore creates one long column containing both 
# other option: spread() with tidyr, but then column names have to be redefined
alg.table <- alg.table %>%
  mutate("cid" = 1) %>%
  mutate("loc" = "alg") %>%
  mutate("age.grp" = levels(alg.hrisk$age.grp)) %>%
  mutate("prev" = round((pos * 100/ (neg + pos)), 1)) %>%
  mutate("se" = round(1.96 * sqrt(prev * (100 - prev) / ((neg + pos)*100)), 1)) %>% # prevalence s.e. is calculated as binomial
  mutate("ci" = paste(prev - se, prev + se, sep = "-")) # as characters


prevalence[prevalence$cid == 1, 6:(dim(prevalence)[2])] <- alg.table$prev 
prevalence[prevalence$cid == 1, "n"] <- dim(alg.hrisk)[1]
prevalence[prevalence$cid == 1, "sgcentre"] <- 44
prevalence[prevalence$cid == 1, "Year"] <- "2007-2008"


#### I. uganda #### 
## extra script as not in pooled data. Not population wide, only  < 25y ##

uga.data <- read_dta("file:///I:/Projects/International Correlation of HPV and Cervical Cancer/codes and documents/data/prevalence data for R codes/Uganda data/girls-baseline-part-quest-clin-lab-sample-hpvres-fup-cyto-updbasefupoct2007-subtypes.dta")

uga.hrisk <- uga.data[, c("HPVNUM", "AGE", "select_paper_baseline", "h16", "h18", "h31","h33","h35","h39","h45","h51","h52","h56","h58","h59","h68_73", "h82")]
# select_paper_baseline =1 means women included. total n = 1275 (see variable-description.doc)
head(uga.hrisk) 

includedwomen <-  which(uga.hrisk$select_paper_baseline == 1)
uga.hrisk <- uga.hrisk[includedwomen, ]
dim(uga.hrisk)

uga.hrisk$age.grp <- cut(uga.hrisk$AGE, seq(15, 80, 5), right = FALSE)

str(uga.hrisk) # hpv pos/neg coded as numeric 1/0 
uga.hrisk[is.na(uga.hrisk)] <- 0 # mathmatical functions do not work with NA so transform to 0
uga.hrisk <- uga.hrisk %>%
  mutate(hpvpos = rowSums(uga.hrisk[,  c("h16", "h18", "h31","h33","h35","h39","h45","h51","h52","h56","h58","h59","h68_73", "h82")])) %>% # number of different hpv infections
  mutate(hpvsino = ifelse(hpvpos > 0, 1, 0)) %>% # factor if hpv  positive or negative for high risk types. 
  mutate(hpvsino = factor(hpvsino, levels = c(0,1), labels = c("neg", "pos")))

uga.table <- table(uga.hrisk$age.grp, uga.hrisk$hpvsino)
uga.table <- as.data.frame.matrix(uga.table) # as.data.frame treats pos, neg as variables like age and therefore creates one long column containing both 
# other option: spread() with tidyr, but then column names have to be redefined
uga.table <- uga.table %>%
  mutate("cid" = 2) %>%
  mutate("age.grp" = levels(uga.hrisk$age.grp)) %>%
  mutate("prev" = round((pos * 100/ (neg + pos)), 1)) %>%
  mutate("se" = round(1.96 * sqrt(prev * (100 - prev) / ((neg + pos)*100)), 1)) %>% # prevalence s.e. is calculated as binomial
  mutate("ci" = paste(prev - se, prev + se, sep = "-")) # as characters
head(uga.hrisk)

uga.table
prevalence[prevalence$cid == 2, 6:(dim(prevalence)[2])] <- uga.table$prev
prevalence[prevalence$cid == 2, "n"] <- dim(uga.hrisk)[1]
prevalence[prevalence$cid == 2, "sgcentre"] <- 100
prevalence[prevalence$cid == 2, "Year"] <- "2002-2004"



#### I. costa rica ####
pooled.data <- read_dta("I:/Projects/International Correlation of HPV and Cervical Cancer/codes and documents/data/prevalence data for R codes/HPVPREV_POOL_V29-1.dta")

# pooled.data %>%
#group_by(sgcentre) %>%
#summarise(n=n())%>%
#knitr::kable()
# in new loaded data set there is costa rica included

cori.hrisk <-  pooled.data[pooled.data$sgcentre == 19, c("sgcentre", "sgid", Hrisk, "sga3", "sel_paper0")]
cori.hrisk$age.grp <- cut(cori.hrisk$sga3, seq(15, 80, 5), right = FALSE)

cori.hrisk <- cori.hrisk %>%
  filter(sga3 >= 15 &  sga3 < 80)

cori.hrisk <- cori.hrisk %>%
  mutate(hpvpos = rowSums(cori.hrisk[, Hrisk])) %>% # number of different hpv infections
  mutate(hpvsino = ifelse(hpvpos > 0, 1, 0)) %>%# factor if hpv  positive or negative for high risk types. NA now as 0 !?
  mutate(hpvsino = factor(hpvsino, levels = c(0,1), labels = c("neg", "pos")))
cori.table <- table(cori.hrisk$age.grp, cori.hrisk$hpvsino)
cori.table <- as.data.frame.matrix(cori.table) # as.data.frame treats pos, neg as variables like age and therefore creates one long column containing both 
# other option: spread() with tidyr, but then column names have to be redefined#
cori.table <- cori.table %>%
  mutate("cid" = 5) %>%
  mutate("loc" = "cori") %>%
  mutate("age.grp" = levels(cori.hrisk$age.grp)) %>%
  mutate("prev" = round((pos * 100/ (neg + pos)), 1)) %>%
  mutate("se" = round(1.96 * sqrt(prev * (100 - prev) / ((neg + pos)*100)), 1)) %>% # prevalence s.e. is calculated as binomial
  mutate("ci" = paste(prev - se, prev + se, sep = "-")) # as characters


cori.table
# # write.xlsx(cori.table, file = "hpv.prevalence.xlsx")
prevalence[prevalence$cid == 5, 6:(dim(prevalence)[2])] <- cori.table$prev
prevalence[prevalence$cid == 5, "n"] <- dim(cori.hrisk)[1]
prevalence[prevalence$cid == 5, "sgcentre"] <- 19
prevalence[prevalence$cid == 5, "Year"] <- "1993-1994"




#### I. bucamaranga, colombia (col) ####

col.hrisk <- pooled.hrisk[pooled.hrisk$sgcentre == 12, ] # important to use pooled.hrisk as because of costa rica data.pooled is complete again (not used women included again)
col.table <- table(col.hrisk$age.grp, col.hrisk$hpvsino)
col.table <- as.data.frame.matrix(col.table) # as.data.frame treats pos, neg as variables like age and therefore creates one long column containing both 
# other option: spread() with tidyr, but then column names have to be redefined
col.table <- col.table %>%
  mutate("cid" = 6) %>%
  mutate("loc" = "col") %>%
  mutate("age.grp" = levels(pooled.hrisk$age.grp)) %>%
  mutate("prev" = round((pos * 100/ (neg + pos)), 1)) %>%
  mutate("se" = round(1.96 * sqrt(prev * (100 - prev) / ((neg + pos)*100)), 1)) %>% # prevalence s.e. is calculated as binomial
  mutate("ci" = paste(prev - se, prev + se, sep = "-")) # as characters
col.table
# write.xlsx(col.table, file = "hpv.prevalence.xlsx") 
prevalence[prevalence$cid == 6, 6:(dim(prevalence)[2])] <- col.table$prev
prevalence[prevalence$cid == 6, "sgcentre"] <- 12
prevalence[prevalence$cid == 6, "n"] <- dim(col.hrisk)[1]
prevalence[prevalence$cid == 6, "Year"] <- "1993-1995"




#### I. shenyang, china (chin)####

chin.hrisk <- pooled.hrisk[pooled.hrisk$sgcentre == 23, ]
chin.table <- table(chin.hrisk$age.grp, chin.hrisk$hpvsino)
chin.table <- as.data.frame.matrix(chin.table) # as.data.frame treats pos, neg as variables like age and therefore creates one long column containing both 
# other option: spread() with tidyr, but then column names have to be redefined
chin.table <- chin.table %>%
  mutate("cid" = 8) %>%
  mutate("loc" = "chin") %>%
  mutate("age.grp" = levels(pooled.hrisk$age.grp)) %>%
  mutate("prev" = round((pos * 100/ (neg + pos)), 1)) %>%
  mutate("se" = round(1.96 * sqrt(prev * (100 - prev) / ((neg + pos)*100)), 1)) %>% # prevalence s.e. is calculated as binomial
  mutate("ci" = paste(prev - se, prev + se, sep = "-")) # as characters

chin.table
# write.xlsx(chin.table, file = "hpv.prevalence.xlsx") 
prevalence[prevalence$cid == 8, 6:(dim(prevalence)[2])] <- chin.table$prev
prevalence[prevalence$cid == 8, "n"] <- dim(chin.hrisk)[1]
prevalence[prevalence$cid == 8, "sgcentre"] <- 23
prevalence[prevalence$cid == 8, "Year"] <- "2005"



#### I. india ####

ind.hrisk <- pooled.hrisk[pooled.hrisk$sgcentre == 18, ]
ind.table <- table(ind.hrisk$age.grp, ind.hrisk$hpvsino)
ind.table <- as.data.frame.matrix(ind.table) # as.data.frame treats pos, neg as variables like age and therefore creates one long column containing both 
# other option: spread() with tidyr, but then column names have to be redefined
ind.table <- ind.table %>%
  mutate("cid" = 9) %>%
  mutate("loc" = "ind") %>%
  mutate("age.grp" = levels(pooled.hrisk$age.grp)) %>%
  mutate("prev" = round((pos * 100/ (neg + pos)), 1)) %>%
  mutate("se" = round(1.96 * sqrt(prev * (100 - prev) / ((neg + pos)*100)), 1)) %>% # prevalence s.e. is calculated as binomial
  mutate("ci" = paste(prev - se, prev + se, sep = "-")) # as characters

ind.table
# write.xlsx(ind.table, file = "hpv.prevalence.xlsx") 
prevalence[prevalence$cid == 9, 6:(dim(prevalence)[2])] <- ind.table$prev
prevalence[prevalence$cid == 9, "n"] <- dim(ind.hrisk)[1]
prevalence[prevalence$cid == 9, "sgcentre"] <- 18
prevalence[prevalence$cid == 9, "Year"] <- "2004"



#### I. iran ####
iran.hrisk <- pooled.hrisk[pooled.hrisk$sgcentre == 61, ]
iran.table <- table(iran.hrisk$age.grp, iran.hrisk$hpvsino)
iran.table <- as.data.frame.matrix(iran.table) # as.data.frame treats pos, neg as variables like age and therefore creates one long column containing both 
# other option: spread() with tidyr, but then column names have to be redefined
iran.table <- iran.table %>%
  mutate("cid" = 10) %>%
  mutate("loc" = "iran") %>%
  mutate("age.grp" = levels(pooled.hrisk$age.grp)) %>%
  mutate("prev" = round((pos * 100/ (neg + pos)), 1)) %>%
  mutate("se" = round(1.96 * sqrt(prev * (100 - prev) / ((neg + pos)*100)), 1)) %>% # prevalence s.e. is calculated as binomial
  mutate("ci" = paste(prev - se, prev + se, sep = "-")) # as characters

iran.table

# write.xlsx(iran.table, file = "hpv.prevalence.xlsx")
prevalence[prevalence$cid == 10, 6:(dim(prevalence)[2])] <- iran.table$prev
prevalence[prevalence$cid == 10, "n"] <- dim(iran.hrisk)[1]
prevalence[prevalence$cid == 10, "sgcentre"] <- 61
prevalence[prevalence$cid == 10, "Year"] <- "2013-2014"

#### I. south korea ####
soko.hrisk <- pooled.hrisk[pooled.hrisk$sgcentre == 15, ]
n.soko <- dim(soko.hrisk)[1]
soko.table <- table(soko.hrisk$age.grp, soko.hrisk$hpvsino)
soko.table <- as.data.frame.matrix(soko.table)

soko.table <- soko.table %>%
  mutate("cid" = 12) %>%
  mutate("loc" = "soko") %>%
  mutate("age.grp" = levels(pooled.hrisk$age.grp)) %>%
  mutate("prev" = round((pos * 100/ (neg + pos)), 1)) %>%
  mutate("se" = round(1.96 * sqrt(prev * (100 - prev) / ((neg + pos)*100)), 1)) %>% # prevalence s.e. is calculated as binomial
  mutate("ci" = paste(prev - se, prev + se, sep = "-")) # as characters


soko.table
prevalence[prevalence$cid == 12, 6:(dim(prevalence)[2])] <- soko.table$prev
prevalence[prevalence$cid == 12, "n"] <- dim(soko.hrisk)[1]
prevalence[prevalence$cid == 12, "sgcentre"] <- 15
prevalence[prevalence$cid == 12, "Year"] <- "1999-2000"


#### I. ho chi minh, vietnam (viet1)####

viet1.hrisk <- pooled.hrisk[pooled.hrisk$sgcentre == 2, ]
n.viet1 <- dim(viet1.hrisk)[1]
viet1.table <- table(viet1.hrisk$age.grp, viet1.hrisk$hpvsino)
viet1.table <- as.data.frame.matrix(viet1.table)

viet1.table <- viet1.table %>%
  mutate("cid" = 13) %>%
  mutate("loc" = "viet1") %>%
  mutate("age.grp" = levels(pooled.hrisk$age.grp)) %>%
  mutate("prev" = round((pos * 100/ (neg + pos)), 1)) %>%
  mutate("se" = round(1.96 * sqrt(prev * (100 - prev) / ((neg + pos)*100)), 1)) %>% # prevalence s.e. is calculated as binomial
  mutate("ci" = paste(prev - se, prev + se, sep = "-")) # as characters


viet1.table
# write.xlsx(viet1.table, file = "hpv.prevalence.xlsx")
prevalence[prevalence$cid == 13, 6:(dim(prevalence)[2])] <- viet1.table$prev
prevalence[prevalence$cid == 13, "n"] <- dim(viet1.hrisk)[1]
prevalence[prevalence$cid == 13, "sgcentre"] <- 2
prevalence[prevalence$cid == 13, "Year"] <- "1997"


#### I. lampang, thailand (thai1) ####

thai1.hrisk <- pooled.hrisk[pooled.hrisk$sgcentre == 7, ]
n.thai1 <- dim(thai1.hrisk)[1]
thai1.table <- table(thai1.hrisk$age.grp, thai1.hrisk$hpvsino)
thai1.table <- as.data.frame.matrix(thai1.table)

thai1.table <- thai1.table %>%
  mutate("cid" = 15) %>%
  mutate("loc" = "thai1") %>%
  mutate("age.grp" = levels(pooled.hrisk$age.grp)) %>%
  mutate("prev" = round((pos * 100/ (neg + pos)), 1)) %>%
  mutate("se" = round(1.96 * sqrt(prev * (100 - prev) / ((neg + pos)*100)), 1)) %>% # prevalence s.e. is calculated as binomial
  mutate("ci" = paste(prev - se, prev + se, sep = "-")) # as characters


thai1.table
# write.xlsx(thai1.table, file = "hpv.prevalence.xlsx")
prevalence[prevalence$cid == 15, 6:(dim(prevalence)[2])] <- thai1.table$prev
prevalence[prevalence$cid == 15, "n"] <- dim(thai1.hrisk)[1]
prevalence[prevalence$cid == 15, "sgcentre"] <- 7
prevalence[prevalence$cid == 15, "Year"] <- "1997-1998"

#### I. entre dos rios province, argentina(arg) ####

arg.hrisk <- pooled.hrisk[pooled.hrisk$sgcentre == 9, ]
n.arg <- dim(arg.hrisk)[1]
arg.table <- table(arg.hrisk$age.grp, arg.hrisk$hpvsino)
arg.table <- as.data.frame.matrix(arg.table)

arg.table <- arg.table %>%
  mutate("cid" = 3) %>%
  mutate("loc" = "arg") %>%
  mutate("age.grp" = levels(pooled.hrisk$age.grp)) %>%
  mutate("prev" = round((pos * 100/ (neg + pos)), 1)) %>%
  mutate("se" = round(1.96 * sqrt(prev * (100 - prev) / ((neg + pos)*100)), 1)) %>% # prevalence s.e. is calculated as binomial
  mutate("ci" = paste(prev - se, prev + se, sep = "-")) # as characters


arg.table
# write.xlsx(arg.table, file = "hpv.prevalence.xlsx")
prevalence[prevalence$cid == 3, 6:(dim(prevalence)[2])] <- arg.table$prev
prevalence[prevalence$cid == 3, "n"] <- dim(arg.hrisk)[1]
prevalence[prevalence$cid == 3, "sgcentre"] <- 9
prevalence[prevalence$cid == 3, "Year"] <- "1998"

#### I. songkla, thailand (thai2) ####

thai2.hrisk <- pooled.hrisk[pooled.hrisk$sgcentre == 14, ]
n.thai2 <- dim(thai2.hrisk)[1]
thai2.table <- table(thai2.hrisk$age.grp, thai2.hrisk$hpvsino)
thai2.table <- as.data.frame.matrix(thai2.table)

thai2.table <- thai2.table %>%
  mutate("cid" = 16) %>%
  mutate("loc" = "thai2") %>%
  mutate("age.grp" = levels(pooled.hrisk$age.grp)) %>%
  mutate("prev" = round((pos * 100/ (neg + pos)), 1)) %>%
  mutate("se" = round(1.96 * sqrt(prev * (100 - prev) / ((neg + pos)*100)), 1)) %>% # prevalence s.e. is calculated as binomial
  mutate("ci" = paste(prev - se, prev + se, sep = "-")) # as characters


thai2.table
# write.xlsx(thai2.table, file = "hpv.prevalence.xlsx")
prevalence[prevalence$cid == 16, 6:(dim(prevalence)[2])] <- thai2.table$prev
prevalence[prevalence$cid == 16, "n"] <- dim(thai2.hrisk)[1]
prevalence[prevalence$cid == 16, "sgcentre"] <- 14
prevalence[prevalence$cid == 16, "Year"] <- "1997-1999"

#### I. spain ####
####two incidences, however only one prevalence. therefore prev(spain1) = prev(spain2) = prev(spain)

spain.hrisk <- pooled.hrisk[pooled.hrisk$sgcentre == 3, ]
n.spain <- dim(spain.hrisk)[1]
spain.table <- table(spain.hrisk$age.grp, spain.hrisk$hpvsino)
spain.table <- as.data.frame.matrix(spain.table)

spain.table <- spain.table %>%
  mutate("cid" = 17) %>%
  mutate("loc" = "spain") %>%
  mutate("age.grp" = levels(pooled.hrisk$age.grp)) %>%
  mutate("prev" = round((pos * 100/ (neg + pos)), 1)) %>%
  mutate("se" = round(1.96 * sqrt(prev * (100 - prev) / ((neg + pos)*100)), 1)) %>% # prevalence s.e. is calculated as binomial
  mutate("ci" = paste(prev - se, prev + se, sep = "-")) # as characters


spain.table
# write.xlsx(spain.table, file = "hpv.prevalence.xlsx")
prevalence[prevalence$cid == 17, 6:(dim(prevalence)[2])] <- spain.table$prev
prevalence[prevalence$cid == 18, 6:(dim(prevalence)[2])] <- spain.table$prev
prevalence[prevalence$cid == 17, "n"] <- dim(spain.hrisk)[1]
prevalence[prevalence$cid == 18, "n"] <- dim(spain.hrisk)[1]
prevalence[prevalence$cid == 17, "sgcentre"] <- 3
prevalence[prevalence$cid == 18, "sgcentre"] <- 3
prevalence[prevalence$cid == 17, "Year"] <- "1998"
prevalence[prevalence$cid == 18, "Year"] <- "1998"


#### I. santiago, antofagasta, chile ####

chile.hrisk <- pooled.hrisk[pooled.hrisk$sgcentre == 16, ]
n.chile <- dim(chile.hrisk)[1]
chile.table <- table(chile.hrisk$age.grp, chile.hrisk$hpvsino)
chile.table <- as.data.frame.matrix(chile.table)

chile.table <- chile.table %>%
  mutate("cid" = 4) %>%
  mutate("loc" = "chile") %>%
  mutate("age.grp" = levels(pooled.hrisk$age.grp)) %>%
  mutate("prev" = round((pos * 100/ (neg + pos)), 1)) %>%
  mutate("se" = round(1.96 * sqrt(prev * (100 - prev) / ((neg + pos)*100)), 1)) %>% # prevalence s.e. is calculated as binomial
  mutate("ci" = paste(prev - se, prev + se, sep = "-")) # as characters


chile.table
# write.xlsx(chile.table, file = "hpv.prevalence.xlsx")
prevalence[prevalence$cid == 4, 6:(dim(prevalence)[2])] <- chile.table$prev
prevalence[prevalence$cid == 4, "n"] <- dim(chile.hrisk)[1]
prevalence[prevalence$cid == 4, "sgcentre"] <- 16
prevalence[prevalence$cid == 4, "Year"] <- "2001" #2001 orig. study, follow-up 2006 wth age.grps >70



#### I. torino, italy (ital) ####

ital.hrisk <- pooled.hrisk[pooled.hrisk$sgcentre == 83, ]
n.ital <- dim(ital.hrisk)[1]
ital.table <- table(ital.hrisk$age.grp, ital.hrisk$hpvsino)
ital.table <- as.data.frame.matrix(ital.table)

ital.table <- ital.table %>%
  mutate("cid" = 20) %>%
  mutate("loc" = "ital") %>%
  mutate("age.grp" = levels(pooled.hrisk$age.grp)) %>%
  mutate("prev" = round((pos * 100/ (neg + pos)), 1)) %>%
  mutate("se" = round(1.96 * sqrt(prev * (100 - prev) / ((neg + pos)*100)), 1)) %>% # prevalence s.e. is calculated as binomial
  mutate("ci" = paste(prev - se, prev + se, sep = "-")) # as characters


ital.table
# write.xlsx(ital.table, file = "hpv.prevalence.xlsx")
prevalence[prevalence$cid == 20, 6:(dim(prevalence)[2])] <- ital.table$prev
prevalence[prevalence$cid == 20, "n"] <- dim(ital.hrisk)[1]
prevalence[prevalence$cid == 20, "sgcentre"] <- 83
prevalence[prevalence$cid == 20, "Year"] <- "2002"

#### I. Amsterdam, the Netherlands (neth) ####

neth.hrisk <- pooled.hrisk[pooled.hrisk$sgcentre == 4, ]
n.neth <- dim(neth.hrisk)[1]
neth.table <- table(neth.hrisk$age.grp, neth.hrisk$hpvsino)
neth.table <- as.data.frame.matrix(neth.table)

neth.table <- neth.table %>%
  mutate("cid" = 19) %>%
  mutate("loc" = "neth") %>%
  mutate("age.grp" = levels(pooled.hrisk$age.grp)) %>%
  mutate("prev" = round((pos * 100/ (neg + pos)), 1)) %>%
  mutate("se" = round(1.96 * sqrt(prev * (100 - prev) / ((neg + pos)*100)), 1)) %>% # prevalence s.e. is calculated as binomial
  mutate("ci" = paste(prev - se, prev + se, sep = "-")) # as characters


neth.table
# write.xlsx(neth.table, file = "hpv.prevalence.xlsx")
prevalence[prevalence$cid == 19, 6:(dim(prevalence)[2])] <- neth.table$prev
prevalence[prevalence$cid == 19, "n"] <- dim(neth.hrisk)[1]
prevalence[prevalence$cid == 19, "sgcentre"] <- 4
prevalence[prevalence$cid == 19, "Year"] <- "1995-1998"



#### I. Warsawa, Poland (pol) ####

pol.hrisk <- pooled.hrisk[pooled.hrisk$sgcentre == 41, ]
n.pol <- dim(pol.hrisk)[1]
pol.table <- table(pol.hrisk$age.grp, pol.hrisk$hpvsino)
pol.table <- as.data.frame.matrix(pol.table)

pol.table <- pol.table %>%
  mutate("cid" = 22) %>%
  mutate("loc" = "pol") %>%
  mutate("age.grp" = levels(pooled.hrisk$age.grp)) %>%
  mutate("prev" = round((pos * 100/ (neg + pos)), 1)) %>%
  mutate("se" = round(1.96 * sqrt(prev * (100 - prev) / ((neg + pos)*100)), 1)) %>% # prevalence s.e. is calculated as binomial
  mutate("ci" = paste(prev - se, prev + se, sep = "-")) # as characters

pol.table
# write.xlsx(pol.table, file = "hpv.prevalence.xlsx")
prevalence[prevalence$cid == 22, 6:(dim(prevalence)[2])] <- pol.table$prev
prevalence[prevalence$cid == 22, "n"] <- dim(pol.hrisk)[1]
prevalence[prevalence$cid == 22, "sgcentre"] <- 41
prevalence[prevalence$cid == 22, "Year"] <- "2006"






############################################## I. pooled prevalence table ################################################

# remove NaN
prevalence[prevalence == "NaN"] <- NA
# remove 0
prevalence[prevalence == 0] <- NA
prevalence

prev.model <- prevalence %>%
  gather(., "entry.age", "prev", "P1":"P13") %>%
  separate(Year, into = paste0("Year", c(0, ""))) 
prev.model$entry.age <- as.numeric(factor(prev.model$entry.age, 
                                          levels = c("P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8", "P9", "P10", "P11", "P12", "P13"),
                                          labels = c("(15,20]", "(20,25]", "(25,30]", "(30,35]", "(35,40]", "(40,45]", "(45,50]", "(50,55]",
                                                     "(55,60]", "(60,65]", "(65,70]", "(70,75]", "(75,80]")))
prev.model$Year <- as.integer(prev.model$Year)
str(prev.model)

#### mortality ####

mortality.lexis <- mortality %>%
  tidyr::gather(., "age.grp", "mort.rate", 3:15) %>%
  mutate(lex.dur = 1) %>%
  mutate(entry.age = as.numeric(factor(age.grp, levels = c("M15_19", "M20_24", "M25_29", "M30_34", "M35_39", "M40_44", "M45_49", "M50_54", 
                                                           "M55_59", "M60_64", "M65_69", "M70_74", "M75_79"), labels = levels(cori$age.grp)))) %>%
  mutate(death = mort.rate*10^(-5)) %>%
  mutate(year = as.numeric(Year))


####
####
####data frame to calculate inc in hpv positive women####
hpv.inc <- inc.ci5.all %>%
  tidyr::gather(., "age.grp", "inc.rate", 2:14) %>%
  filter(loc == "Costa Rica") %>%
  mutate(lex.dur = 1) %>%
  mutate(entry.age = as.numeric(factor(age.grp, levels = c("R15_19", "R20_24", "R25_29", "R30_34", "R35_39", "R40_44", "R45_49", "R50_54", 
                                                           "R55_59", "R60_64", "R65_69", "R70_74", "R75_79"), labels = levels(cori$age.grp)))) %>%
  mutate(icc = inc.rate*10^(-5)) %>%
  mutate(year = as.numeric(Year)) %>%
  select(-lex.dur, -year, -icc) %>%
  full_join(., prev.model, by = c("cid", "entry.age", "loc", "Year")) %>%
  select(-sgcentre, -Year0, - age.grp) %>%
  full_join(., mortality.lexis, by = c("cid", "entry.age", "Year")) %>%
  select(-Location, - lex.dur, - death, - year) %>% # Midperiod is for mortality
  mutate(ih = NA)
# hpv.inc <- na.omit(hpv.inc, cols=c("inc.rate"), invert = FALSE)
hpv.inc <- hpv.inc[order(hpv.inc$cid, hpv.inc$entry.age, hpv.inc$Year),]
head(hpv.inc)

#### function to calculate ih (incidence in hpv positive women) ####
## does not work yet for general data frame
for(i in 1:(nrow(hpv.inc)-1)){
  if(is.na(hpv.inc$prev[i+1])){
  #if(hpv.inc$cid[i] == hpv.inc$cid[i+1]){ # same location (in location sorted by age and year, so no need for extra conditions that could include NAs)
  hpv.inc$prev[i+1] <- 100* (hpv.inc$prev[i]/100 - hpv.inc$inc.rate[i]/10^5 - hpv.inc$mort[i]/10^5 * (hpv.inc$prev[i])/100) /
    (1 - hpv.inc$inc.rate[i]/10^5 - hpv.inc$mort[i]/10^5)
  }
  #}
}

for(i in 1:nrow(hpv.inc)){
    hpv.inc$ih[i] <- hpv.inc$inc.rate[i] / hpv.inc$prev[i] * 10^2 # as 10^(-5)/10^(-2) = 10^(-3). 
}
tail(hpv.inc)
hpv.inc[hpv.inc$loc == "Costa Rica", ]

head(hpv.inc)
hpv.inc$Year[1] +1
