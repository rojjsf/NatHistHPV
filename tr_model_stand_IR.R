library(haven)
library(foreign)
library(Epi)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lcmm)
library(stringr)


# load icc age-standardized incidence rates (world) in year of study 
stIR <- read.csv("C:/Users/schultefrohlinder/Documents/R/NatHistHPV/data sets/standard_IR.csv", header = TRUE)
head(stIR)

# model including stIR as covariate
#(run tr_model_screen.R)
Dpop <- mort.long %>%
  full_join(., prvl.long, by = c("cid", "age", "Year", "sgcentre")) %>% 
  full_join(., inc.long, by = c("cid", "age", "Year")) %>%
  full_join(., pop.long, by = c("cid", "age", "Year"))%>%
  select(-stY) %>%
  full_join(., info[, c("sgcentre", "stY", "cid", "Location", "cov")], by = c("cid", "sgcentre"))

Dpop <- Dpop[order(Dpop$cid, Dpop$Year, Dpop$age),] 
Dpop <- Dpop[which(!is.na(Dpop$IR)), ]
Dpop <- Dpop[which(!is.na(Dpop$sgcentre)), ]




####`9. calculate Nicc, Npos, YsncS ####

for (i in 1:nrow(Dpop)){
  Dpop$PopFemale[i] <- Dpop$PopFemale[i] * 1000 # exact number of women per year and age group and country
  Dpop$Nicc[i] <- round(Dpop$PopFemale[i] * Dpop$IR[i] *10^(-5), 1)# number of cancer cases per age and year
  Dpop$Npos[i] <- round(Dpop$prev[i] * Dpop$PopFemale[i], 1)  # number of hpv positive women in Year of hpv study and age group
  
}

for(i in 1:(nrow(Dpop))){
  if(!is.na(Dpop$Npos[i])) {
    a <- Dpop$age[i]
    y <- Dpop$Year[i]
    loc <- Dpop$cid[i]
    Dpop$Npos[Dpop$age == (a+1) & Dpop$Year == (y+1) & Dpop$cid == loc] <- round(Dpop$Npos[i] - Dpop$Npos[i]*Dpop$MR[i]*10^(-5) - Dpop$Nicc[i], 1)
    #Dpop$prev[Dpop$Age == (a+1) & Dpop$Year == (y+1) & Dpop$cid == loc] <- round(Dpop$Npos[i] *100 / Dpop$PopFemale[i], 2)
  }
}

Dpop <- Dpop[which(!is.na(Dpop$Npos)), ]

#### loop start ####
ggout<- list()
DpCout <- list()
lcmm_out <- list("tr_lcmm", "tr_bic", "tr_pred", "tr_prob", "tr_conv", "tr_lambda")
Dp_out <- list()

for(d in c(9)){
  cat("\n ageint: ", d) # locating error
  #browser()
  for(f in c(20, 30, 40)) {
    cat("\n age.beg: ", f)
    mina <- f 
    maxa <- sum(f, d)
    
    Dp <- data.frame(matrix(nrow = 0, ncol = 18)) # Dp = Data population (as nested in of Dpop)
    colnames(Dp) <- colnames(Dpop)
    
    for(j in info$cid){
      sist <- (2012 - as.numeric(info$stY[info$cid == j])) # nb of years since prevalence Study
      if(info$stY[info$cid == j] < 2012){ # excluding Iran (in 2014)
        # cat("\n cid: ", j) # locating error
        for(i in 0:sist){
          a <- (mina+i):(maxa+i) # moving age group by one year
          y <- as.numeric(Dpop$stY) + i # i years added to year of prev. Study
          Dp <- rbind(Dp, Dpop[Dpop$age %in% a & Dpop$Year==y & Dpop$cid== j, ])
        }
      }
    }
    Dp <- Dp[!is.na(Dp$IR), ]
    
    
    # Years since Prevalence Study:
    Dp <- Dp %>%
      mutate(YsncS = as.numeric(Dp$Year) - as.numeric(Dp$stY)) %>%
    filter(YsncS <= 14)# model fitting only with data for 11 years as afterwards to few countries

     
    
    
    Dp_out[[stringr::str_c("Dp", mina, "_", maxa)]] <- Dp
    #data frame DpC: cumulative rates for full cohort per country, year (not over full time period) ####
    # Sum all cases and women of all ages (in previously selected age group) in each Year and country (location, cid)
    DpC <- Dp %>%
      ungroup() %>%
      group_by(Year, YsncS, cid, Location, cov) %>% 
      summarise(Npos = sum(Npos), 
                Nicc = sum(Nicc), 
                prev = sum(Npos)*100/sum(PopFemale),
                IR = sum(Nicc)*100000/sum(PopFemale),
                logIR = log(sum(Nicc)/sum(PopFemale)),
                se = sqrt(sum(Nicc))/sum(Npos), # standard error of rates = rate^2/cases = sqrt(cases)/personyears (Esteve p. 52)
                logRate = log(sum(Nicc)/sum(Npos)), 
                selog = sqrt(1/sum(Npos))) %>% # var(log(k/m)) = 1/(Rate*pyears); se(log(k/m)) = sqrt(1/k). Esteve p. 53
      filter(Location != "Uganda") %>%
      filter(Location != "Viet Nam")   
    
    DpC <- merge(DpC, stIR[, -c(3, 5, 6, 7)])
    DpC$Location <- as.factor(DpC$Location)
    DpC$cid <- as.factor(DpC$cid)
    DpC[which(DpC$Nicc <= 0 | DpC$Npos <= 0 ), c("Nicc", "Npos", "logRate", "se")] <- NA # to avoid -Inf when Nicc = 0
    
    ## screening coverage
    #DpC$cov <- factor(DpC$cov, levels = c(2, 1, 3), labels = c("medium", "high", "low"))
    #DpC$cov <- factor(DpC$cov, levels = c(1, 2, 3), labels = c("high", "medium",  "low"))
    DpC$cov <- factor(DpC$cov, levels = c(3, 1, 2), labels = c("low", "high", "medium"))
    #DpC$cov <- factor(DpC$cov, levels = c(1, 2), labels = c("yes", "no"))
    DpCout[[stringr::str_c("DpC", mina, "_", maxa)]] <- DpC # to have all age cohort for mixed model
    

    
    
    
    # latent class mixed model
    # #no random effects
    lcmm_out$tr_lcmm[[stringr::str_c(mina, "_", maxa, "_0")]] <-
      tr_lcmm_0 <- lcmm(logRate ~ YsncS,  subject = "Location",  ng = 1, data = DpC, link = "splines")
    lcmm_out$tr_lcmm[[stringr::str_c(mina, "_", maxa, "_stIR_cov_0")]] <-
      tr_lcmm_stIR_cov_0 <- lcmm(logRate ~ YsncS*stIR + cov, subject = "Location",  ng = 1, data = DpC, link = "splines")
    lcmm_out$tr_lcmm[[stringr::str_c(mina, "_", maxa, "_stIR_0_int")]] <-
      tr_lcmm_stIR_0_int <- lcmm(logRate ~ YsncS*stIR,  subject = "Location",  ng = 1, data = DpC, link = "splines")
    lcmm_out$tr_lcmm[[stringr::str_c(mina, "_", maxa, "_stIR_0")]] <-
      tr_lcmm_stIR_0 <- lcmm(logRate ~ YsncS+stIR, subject = "Location",  ng = 1, data = DpC, link = "splines")
    lcmm_out$tr_lcmm[[stringr::str_c(mina, "_", maxa, "_cov_0")]] <-
      tr_lcmm_cov_0 <- lcmm(logRate ~ YsncS*cov,  subject = "Location",  ng = 1, data = DpC, link = "4-equi-splines")
    lcmm_out$tr_lcmm[[stringr::str_c(mina, "_", maxa, "_stIR_cov_0_int")]] <-
      tr_lcmm_stIR_cov_0_int <- lcmm(logRate ~ YsncS*stIR + YsncS*cov,  subject = "Location",  ng = 1, data = DpC, link = "splines")
    lcmm_out$tr_lcmm[[stringr::str_c(mina, "_", maxa, "_int_0")]] <-
      tr_lcmm_int_0 <- lcmm(logRate ~ YsncS*cov,  subject = "Location",  ng = 1, data = DpC, link = "splines")
    lcmm_out$tr_lcmm[[stringr::str_c(mina, "_", maxa, "_year_0")]] <-
      tr_lcmm_year_0 <- lcmm(logRate ~ YsncS*Year + YsncS*stIR,  subject = "Location",  ng = 1, data = DpC, link = "splines")
    lcmm_out$tr_lcmm[[stringr::str_c(mina, "_", maxa, "_all_0")]] <-
      tr_lcmm_all_0 <- lcmm(logRate ~ YsncS*cov*stIR,  subject = "Location",  ng = 1, data = DpC, link = "splines")
    


    # #random effects on intercept only
    lcmm_out$tr_lcmm[[stringr::str_c(mina, "_", maxa, "_1")]] <-
      tr_lcmm_1 <- lcmm(logRate ~ YsncS, random = ~ 1, subject = "Location",  ng = 1, data = DpC, link = "splines")
    lcmm_out$tr_lcmm[[stringr::str_c(mina, "_", maxa, "_stIR_cov_1")]] <-
      tr_lcmm_stIR_cov_1 <- lcmm(logRate ~ YsncS*stIR + cov, random = ~ 1, subject = "Location",  ng = 1, data = DpC, link = "splines")
    lcmm_out$tr_lcmm[[stringr::str_c(mina, "_", maxa, "_stIR_1_int")]] <-
      tr_lcmm_stIR_1_int <- lcmm(logRate ~ YsncS*stIR, random = ~ 1, subject = "Location",  ng = 1, data = DpC, link = "splines")
    lcmm_out$tr_lcmm[[stringr::str_c(mina, "_", maxa, "_stIR_1")]] <-
      tr_lcmm_stIR_1 <- lcmm(logRate ~ YsncS+stIR, random = ~ 1, subject = "Location",  ng = 1, data = DpC, link = "splines")
    lcmm_out$tr_lcmm[[stringr::str_c(mina, "_", maxa, "_cov_1")]] <-
      tr_lcmm_cov_1 <- lcmm(logRate ~ YsncS*cov, random = ~ 1, subject = "Location",  ng = 1, data = DpC, link = "splines")
    lcmm_out$tr_lcmm[[stringr::str_c(mina, "_", maxa, "_stIR_cov_1_int")]] <-
      tr_lcmm_stIR_cov_1_int <- lcmm(logRate ~ YsncS*stIR + YsncS*cov,  random = ~ 1, subject = "Location",  ng = 1, data = DpC, link = "splines")
    lcmm_out$tr_lcmm[[stringr::str_c(mina, "_", maxa, "_all_1")]] <-
      tr_lcmm_all_1 <- lcmm(logRate ~ YsncS*cov*stIR,  subject = "Location", random = ~1, ng = 1, data = DpC, link = "splines")
    
    
    # #random effects on intercept and slope
    lcmm_out$tr_lcmm[[stringr::str_c(mina, "_", maxa)]] <-
      tr_lcmm <- lcmm(logRate ~ YsncS, random = ~ YsncS , subject = "Location",  ng = 1, data = DpC, link = "splines")
    lcmm_out$tr_lcmm[[stringr::str_c(mina, "_", maxa, "_stIR_cov")]] <-
      tr_lcmm_stIR_cov <- lcmm(logRate ~ YsncS*stIR + cov, random = ~ YsncS, subject = "Location",  ng = 1, data = DpC, link = "splines")
    lcmm_out$tr_lcmm[[stringr::str_c(mina, "_", maxa, "_stIR")]] <-
      tr_lcmm_stIR <- lcmm(logRate ~ YsncS+stIR, random = ~ YsncS, subject = "Location",  ng = 1, data = DpC, link = "splines")
    lcmm_out$tr_lcmm[[stringr::str_c(mina, "_", maxa, "_stIR_int")]] <-
      tr_lcmm_stIR_int <- lcmm(logRate ~ YsncS*stIR, random = ~ YsncS, subject = "Location",  ng = 1, data = DpC, link = "splines")
    lcmm_out$tr_lcmm[[stringr::str_c(mina, "_", maxa, "_cov")]] <-
      tr_lcmm_cov <- lcmm(logRate ~ YsncS*cov, random = ~ YsncS, subject = "Location",  ng = 1, data = DpC, link = "splines")
    lcmm_out$tr_lcmm[[stringr::str_c(mina, "_", maxa, "_int")]] <-
      tr_lcmm_int <- lcmm(logRate ~ YsncS*cov,  random = ~ YsncS, subject = "Location",  ng = 1, data = DpC, link = "splines")
    lcmm_out$tr_lcmm[[stringr::str_c(mina, "_", maxa, "_stIR_cov_int")]] <-
      tr_lcmm_stIR_cov_int <- lcmm(logRate ~ YsncS*stIR + YsncS*cov,  random = ~YsncS, subject = "Location",  ng = 1, data = DpC, link = "splines")
    lcmm_out$tr_lcmm[[stringr::str_c(mina, "_", maxa, "_all")]] <-
      tr_lcmm_all <- lcmm(logRate ~ YsncS*cov*stIR,  subject = "Location", random = ~YsncS, ng = 1, data = DpC, link = "splines")
    


    # # bic
    lcmm_out$tr_bic[[stringr::str_c(mina, "_", maxa, "_0")]] <-
      bic <- round(tr_lcmm_0$BIC, 2)
    lcmm_out$tr_bic[[stringr::str_c(mina, "_", maxa, "_stIR_cov_0")]] <-
      bic <- round(tr_lcmm_stIR_cov_0$BIC, 2)
    lcmm_out$tr_bic[[stringr::str_c(mina, "_", maxa, "stIR_0_int")]] <-
      bic <- round(tr_lcmm_stIR_0_int$BIC, 2)
    lcmm_out$tr_bic[[stringr::str_c(mina, "_", maxa, "stIR_0")]] <-
      bic <- round(tr_lcmm_stIR_0$BIC, 2)
    lcmm_out$tr_bic[[stringr::str_c(mina, "_", maxa, "_cov_0")]] <-
      bic <- round(tr_lcmm_cov_0$BIC, 2)
    lcmm_out$tr_bic[[stringr::str_c(mina, "_", maxa, "_stIR_cov_0_int")]] <-
      bic <- round(tr_lcmm_stIR_cov_0_int$BIC, 2)
    lcmm_out$tr_bic[[stringr::str_c(mina, "_", maxa, "_int_0")]] <-
      bic <- round(tr_lcmm_int_0$BIC, 2)
    lcmm_out$tr_bic[[stringr::str_c(mina, "_", maxa, "_year_0")]] <-
      bic <- round(tr_lcmm_year_0$BIC, 2)
    lcmm_out$tr_bic[[stringr::str_c(mina, "_", maxa, "_all_0")]] <-
      bic <- round(tr_lcmm_all_0$BIC, 2)

    lcmm_out$tr_bic[[stringr::str_c(mina, "_", maxa, "_1")]] <-
      bic <- round(tr_lcmm_1$BIC, 2)
    lcmm_out$tr_bic[[stringr::str_c(mina, "_", maxa, "_stIR_cov_1")]] <-
      bic <- round(tr_lcmm_stIR_cov_1$BIC, 2)
    lcmm_out$tr_bic[[stringr::str_c(mina, "_", maxa, "stIR_1_int")]] <-
      bic <- round(tr_lcmm_stIR_1_int$BIC, 2)
    lcmm_out$tr_bic[[stringr::str_c(mina, "_", maxa, "stIR_1")]] <-
      bic <- round(tr_lcmm_stIR_1$BIC, 2)
    lcmm_out$tr_bic[[stringr::str_c(mina, "_", maxa, "_cov_1")]] <-
      bic <- round(tr_lcmm_cov_1$BIC, 2)
    lcmm_out$tr_bic[[stringr::str_c(mina, "_", maxa, "_stIR_cov_1_int")]] <-
      bic <- round(tr_lcmm_stIR_cov_1_int$BIC, 2)
    lcmm_out$tr_bic[[stringr::str_c(mina, "_", maxa, "_all_1")]] <-
      bic <- round(tr_lcmm_all_1$BIC, 2)

    lcmm_out$tr_bic[[stringr::str_c(mina, "_", maxa)]] <-
      bic <- round(tr_lcmm$BIC, 2)
    lcmm_out$tr_bic[[stringr::str_c(mina, "_", maxa, "_stIR_cov")]] <-
      bic <- round(tr_lcmm_stIR_cov$BIC, 2)
    lcmm_out$tr_bic[[stringr::str_c(mina, "_", maxa, "_stIR_int")]] <-
      bic <- round(tr_lcmm_stIR_int$BIC, 2)
    lcmm_out$tr_bic[[stringr::str_c(mina, "_", maxa, "_stIR")]] <-
      bic <- round(tr_lcmm_stIR$BIC, 2)
    lcmm_out$tr_bic[[stringr::str_c(mina, "_", maxa, "_cov")]] <-
      bic <- round(tr_lcmm_cov$BIC, 2)
    lcmm_out$tr_bic[[stringr::str_c(mina, "_", maxa, "_int")]] <-
      bic <- round(tr_lcmm_int$BIC, 2)
    lcmm_out$tr_bic[[stringr::str_c(mina, "_", maxa, "_stIR_cov_int")]] <-
      bic <- round(tr_lcmm_stIR_cov_int$BIC, 2)
    lcmm_out$tr_bic[[stringr::str_c(mina, "_", maxa, "_all")]] <-
      bic <- round(tr_lcmm_all$BIC, 2)
    
  }
}



  summary(lcmm_out$tr_lcmm$`20_29_all_0`)
as.data.frame(lcmm_out$tr_bic)
as.data.frame(lcmm_out$tr_lcmm$`30_39_all_0`$best)



head(DpCout$DpC20_29)

##plot facets for both coverage groups with respective models

nd_y <- data.frame(YsncS = 0:18, 
                   cov = "yes", 
                   stIR = mean(stIR$stIR[stIR$cid %in% info$cid[info$cov == 1]]))
nd_n <- data.frame(YsncS = 0:18, 
                   cov = "no", 
                   stIR = mean(stIR$stIR[stIR$cid %in% info$cid[info$cov == 2]]))
DpC20 <- DpCout$DpC20_29 %>%
  mutate(ag = "ag1") 
pred20 <- rbind(data.frame("logr" = predictY(lcmm_out$tr_lcmm$`20_29_stIR_cov`, 
                                             nd_y, methInteg = 1, draws = TRUE)$pred[,1],
                          "YsncS" = c(0:18), "ag" = "ag1", "cov" = "yes"),
               data.frame("logr" = predictY(lcmm_out$tr_lcmm$`20_29_stIR_cov`, nd_n, methInteg = 1, draws = TRUE)$pred[,1],
                          "YsncS" = c(0:18), "ag" = "ag1", "cov" = "no"))
DpC20 <- merge(DpC20, pred20)

DpC30 <- DpCout$DpC30_39 %>%
  mutate(ag = "ag2")
pred30 <- rbind(data.frame("logr" = predictY(lcmm_out$tr_lcmm$`30_39_stIR_cov`, nd_y, methInteg = 1, draws = TRUE)$pred[,1],
                           "YsncS" = c(0:18), "ag" = "ag2", "cov" = "yes"),
                data.frame("logr" = predictY(lcmm_out$tr_lcmm$`30_39_stIR_cov`, nd_n, methInteg = 1, draws = TRUE)$pred[,1],
                           "YsncS" = c(0:18), "ag" = "ag2", "cov" = "no"))
DpC30 <- merge(DpC30, pred30)

DpC40 <- DpCout$DpC40_49 %>%
  mutate(ag = "ag3")
pred40 <- rbind(data.frame("logr" = predictY(lcmm_out$tr_lcmm$`40_49_stIR_cov`, nd_y, methInteg = 1, draws = TRUE)$pred[,1],
                           "YsncS" = c(0:18), "ag" = "ag3", "cov" = "yes"),
                data.frame("logr" = predictY(lcmm_out$tr_lcmm$`40_49_stIR_cov`, nd_n, methInteg = 1, draws = TRUE)$pred[,1],
                           "YsncS" = c(0:18), "ag" = "ag3", "cov" = "no"))
DpC40 <- merge(DpC40, pred40)
DpC_df <- rbind(DpC20, DpC30, DpC40)
labels <- c(`ag1` = "20-29", `ag2` = "30-39", `ag3` = "40-49")


ggplot(DpC_df, aes(x = YsncS, y = logRate)) +
  theme_minimal() +
  ggtitle("Transition rate from HPV to cervical cancer by age and screening coverage") +
  geom_line(aes(col = Location), size = 0.75) +
  scale_color_manual(values = col) +
  geom_line(aes(x = YsncS, y =  logr), col = "black", size = 1.7, linetype = 1) +
  facet_wrap(~ cov + ag, labeller = labeller(ag = labels), nrow = 2, ncol = 3) +
  theme(strip.text.x = element_text(size = 12, face = "bold")) +
  #geom_ribbon(aes(x = YsncS, ymin = logr_lo, ymax = logr_hi), fill = 'azure4', alpha = 0.4)+
  labs(y = "log Incidence Rate in hrHPV+ women", 
       x = "Years since HPV detection") +
  ylim(-10.2, -2.5)  +
  xlim(0, 12) +
  theme(title = element_text(size = 18, face = "bold"),
        legend.title = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 14),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 12),
        legend.key.height = unit(0.8, "cm"),
        legend.key.width = unit(1.2, "cm"))  

