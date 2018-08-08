install.packages("mstate")
library(mstate)
data(ebmt3)
head(ebmt3)
 # define transition matrices: which matrices are possible?
 # the vectors define the positions (in rows) of the possible transitions, ordered by numbers.
tmat <- transMat(x = list(c(2, 3), c(3), c()), names = c("Tx", "PR", "RelDeath"))
tmat
 # times in years
ebmt3$prtime <- ebmt3$prtime/365.25
ebmt3$rfstime <- ebmt3$rfstime/365.25
 # create a long format data frame: each transition has an own row:
covs <- c("dissub", "age", "drmatch", "tcd", "prtime")
msbmt <- msprep(time = c(NA, "prtime", "rfstime"), # a character vector containing column names containing times at which states are visited. length (#states) (or a matrix of length n * S)
                status = c(NA, "prstat", "rfsstat"), # # a character vector containing columns with 1/0 whether state was visted
                data = ebmt3, 
                trans = tmat, # transition matrix
                keep = covs) #  containing covariate(s) that need to be retained in the output dataset, of length n (number of individuals)

head(msbmt)
 # seperate data frame  with expanded transition sprecific covariates
expcovs <- expand.covs(msbmt, covs[2:3], append = FALSE)
head(expcovs)
 # add columns with axpanded covariated using short names
msbmt <- expand.covs(msbmt, covs, append = TRUE, longnames = FALSE)
head(msbmt) # object of class msdata
 # creating a cox model assuming markov model, assuming different effects of covariants for different transitions.
 # no proportionality assumptions: strata(trans) - seperate hazard for different values of trans
 # as data is in long format the delayed entries are specified according to status
c1 <- coxph(Surv(Tstart, Tstop, status) ~ dissub1.1 + dissub2.1 +
              + age1.1 + age2.1 + drmatch.1 + tcd.1 + dissub1.2 + dissub2.2 +
              + age1.2 + age2.2 + drmatch.2 + tcd.2 + dissub1.3 + dissub2.3 +
              + age1.3 + age2.3 + drmatch.3 + tcd.3 + strata(trans), 
            data = msbmt,
            method = "breslow")
 # assumption: Transitions 2 and 3 are proportional, so they are consideres as one strata (distinguishable from transition 1 because they have the same endoint death -> to)
msbmt$pr <- 0
msbmt$pr[msbmt$trans == 3] <- 1 # new column specifying whether there has been a platelet recovery in strata to death (trans 2 & 3)
c2 <- coxph(Surv(Tstart, Tstop, status) ~ dissub1.1 + dissub2.1 +
                + age1.1 + age2.1 + drmatch.1 + tcd.1 + dissub1.2 + dissub2.2 +
                + age1.2 + age2.2 + drmatch.2 + tcd.2 + dissub1.3 + dissub2.3 +
                + age1.3 + age2.3 + drmatch.3 + tcd.3 + pr + strata(to), 
            data = msbmt,
            method = "breslow")
c2
cox.zph(c2) # test for the proportional hazards assumption of a Cox Regression: evidence of non-proportionality of the baseline transition intensities of transitions 2 (p < 0.05).
 # proportional execpt for the disease subtypes

 # including the arrival time in state 2 as covariate in model (prtime.3 as can logically only influence transition 2 -> 3)
c3 <- coxph(Surv(Tstart, Tstop, status) ~ dissub1.1 + dissub2.1 +
              + age1.1 + age2.1 + drmatch.1 + tcd.1 + dissub1.2 + dissub2.2 +
              + age1.2 + age2.2 + drmatch.2 + tcd.2 + dissub1.3 + dissub2.3 +
              + age1.3 + age2.3 + drmatch.3 + tcd.3 + pr + prtime.3 + strata(to),
            data = msbmt, 
            method = "breslow")
c3 # time of platelet relapse effect on transtion rate 2 -> 3 is not significant 

 # to do a clock-reset model one would use msbmt$time instead of Tstart, as there is one row with one time for each transition anyway

 # Prediction
 # estimating the baseline hazards
newd <- data.frame(dissub = rep(0, 3), age = rep(0, 3), drmatch = rep(0, 3), tcd = rep(0, 3), trans = 1:3)
newd$dissub <- factor(newd$dissub, levels = 0:2, labels = levels(ebmt3$dissub))
newd$age <- factor(newd$age, levels = 0:2, labels = levels(ebmt3$age))
newd$drmatch <- factor(newd$drmatch, levels = 0:1, labels = levels(ebmt3$drmatch))
newd$tcd <- factor(newd$tcd, levels = 0:1, labels = levels(ebmt3$tcd)) # a patient with aml, <= 20 J, no gender missmatch, no tcd
attr(newd, "trans") <- tmat
class(newd) <- c("msdata", "data.frame")  # as epamd.covs expects object of class msdata
newd <- expand.covs(newd, covs[1:4], longnames = FALSE)
newd$strata = 1:3
newd # newdata data frame needs to have a column
 # strata specifying to which stratum in the coxph object each transition belongs (so coxph = msdata + strata)

msf1 <- msfit(c1, newdata = newd, trans = tmat) # estimate of the baseline cumulative hazard for the "stratifed hazards" model
summary(msf1)
 # Haz = cum hazard for one transition for one patient

 # cox proportional hazards model
newd$strata = c(1, 2, 2)
newd$pr <- c(0, 0, 1)
msf2 <- msfit(c2, newdata = newd, trans = tmat)
summary(msf2)

 # compare grafically:
par(mfrow = c(1, 2))
plot(msf1, cols = rep(1, 3), lwd = 2, lty = 1:3, xlab = "Years since transplant",
       ylab = "Stratified baseline hazards", legend.pos = c(2, 0.9))
plot(msf2, cols = rep(1, 3), lwd = 2, lty = 1:3, xlab = "Years since transplant",
       ylab = "Proportional baseline hazards", legend.pos = c(2, 0.9))
par(mfrow = c(1, 1))
