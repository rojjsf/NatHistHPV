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

 # estimating the cumulative probability from the cumulative incidences 
 # Aalen-Johansen-Estimator using probtrans
pt <- probtrans(msf2, predt = 0) # predt specifies starting time s = 0 (= forward prediction)
 # probtrans object are lists with predicted transitions probablities from state [i]

tmat2 <- transMat(x = list(c(2, 4), c(3), c(), c())) # mnow seperating transition 1->3 and 2->3
tmat2
msf2$trans <- tmat2
pt <- probtrans(msf2, predt = 0)
summary(pt, from = 1)

plot(pt, ord = c(2, 3, 4, 1), lwd = 2, xlab = "Years since transplant",
     ylab = "Prediction probabilities", cex = 0.75, legend = c("Alive in remission, no PR",
                                                                 "Alive in remission, PR", "Relapse or death after PR",
                                                                 "Relapse or death without PR"))
 # special plot method for probtrans objects (stacked  in order ord = c())

 # fixed horizon: survival probablity knowing that person suvived until that moment


 # competing risks
data(aidssi)
si <- aidssi # Just a shorter name
head(si)
table(si$status)

tmat <- trans.comprisk(2, names = c("event-free", "AIDS", "SI"))
tmat
 # AFTER trans.comprisk, prepare long data frame
si$stat1 <- as.numeric(si$status == 1) # TRUE -> 1
si$stat2 <- as.numeric(si$status == 2)
silong <- msprep(time = c(NA, "time", "time"), 
                 status = c(NA, "stat1", "stat2"), 
                 data = si, 
                 keep = "ccr5", 
                 trans = tmat) # a different transition matrix for competing causes
silong <- expand.covs(silong, "ccr5") # dummy covariates (transition or cause-specific covariates) vor cox regression later
silong[1:8, ] # if ww covariate is always 0

 # survfit: . 
 # Predicted curves from a coxph model have one row for each stratum in the Cox model fit and one column for each specified covariate set. 
 # Curves from a multi-state model have one row for each stratum and a column for each state, 
 # the strata correspond to predictors on the right hand side of the equation
 #  --> net KAPLAN MAIER

 # Cuminc: nonparametric Cumulative Incidence functions. Surv = failure-FREE Interval
 # ?? Aalen-Johnson (probtrans in mstate) for competing risks?
ci <- Cuminc(time = si$time, status = si$status)
ci <- Cuminc(time = "time", status = "status", data = aidssi)
head(ci)

idx0 <- (ci$time < 13)
plot(c(0, ci$time[idx0], 13), c(1, 1 - ci$CI.1[idx0], min(1 - ci$CI.1[idx0])), 
       type = "s", xlim = c(0, 13), ylim = c(0, 1), 
       xlab = "Years from HIV infection", ylab = "Probability", lwd = 2)
lines(c(0, ci$time[idx0], 13), c(0, ci$CI.2[idx0], max(ci$CI.2[idx0])),
        type = "s", lwd = 2)
text(8, 0.77, adj = 0, "AIDS")
text(8, 0.275, adj = 0, "SI")
