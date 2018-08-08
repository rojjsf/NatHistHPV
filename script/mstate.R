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
c1
?coxph
