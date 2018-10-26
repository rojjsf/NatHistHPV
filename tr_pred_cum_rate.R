ggout <- list()
DpCout <- list()
lcmm_out <- list("tr_lcmm", "tr_bic", "tr_pred", "tr_prob", "tr_conv", "tr_lambda")
Dp_out <- list()
tr_pp <- list() 

for(d in c(4, 9)){
  cat("\n ageint: ", d) # locating error
  #browser()
  for(f in c(20, 25, 30)) {
    
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
      mutate(YsncS = as.numeric(Dp$Year) - as.numeric(Dp$stY)) 
    Dp_out[[stringr::str_c("Dp", mina, "_", maxa)]] <- Dp
    
    #data frame DpC: cumulative rates for full cohort per country, year (not over full time period) ####
    # Sum all cases and women of all ages (in previously selected age group) in each Year and country (location, cid)
    DpC <- Dp %>%
      ungroup() %>%
      group_by(YsncS, cid, Location) %>% 
      summarise(Npos = sum(Npos), 
                Nicc = sum(Nicc), 
                se = sqrt(sum(Nicc))/sum(Npos), # standard error of rates = rate^2/cases = sqrt(cases)/personyears (Esteve p. 52)
                logRate = log(sum(Nicc)/sum(Npos)), 
                selog = sqrt(1/sum(Npos))) %>% # var(log(k/m)) = 1/(Rate*pyears); se(log(k/m)) = sqrt(1/k). Esteve p. 53
      filter(Location != "Uganda") %>%
      filter(Location != "Viet Nam")         
    DpC$Location <- as.factor(DpC$Location)
    DpC$cid <- as.factor(DpC$cid)
    DpC[which(DpC$logRate < (-20)), "logRate"] <- NA # to avoid -Inf when Nicc = 0
    DpCout[[stringr::str_c("DpC", mina, "_", maxa)]] <- DpC # to have all age cohort for mixed model
    
    
    #### 11.Plot ####
    
    col <- c('#d8b365', '#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#67001f', '#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#003c30','#b15928', '#dd1c77', '#1c9099')
    DpCplot <- ggplot2::ggplot(DpC, aes(x = YsncS, y = logRate)) + 
      geom_point(aes(color = Location)) + 
      geom_line(aes(color = Location)) +
      scale_color_manual(values = col) +
      ggtitle(stringr::str_c("cohort of HPV+ women ", mina, "-", maxa, "y")) +
      ylab("cervical cancer inidence Rate in HPV+ women") +
      xlab("Years since HPV detection") +
      ylim(-10.5, -3) +
      geom_linerange(aes(x = YsncS, ymin = logRate - selog, ymax = logRate + selog, color = Location)) + # add standard error bars
      theme_bw()
    ggout$DpC_plot[[stringr::str_c(mina, "_", maxa)]] <- DpCplot
    
    
    
    #DpC_5y_grid <- gridExtra::grid.arrange(ggout$DpCplot20_24, ggout$DpCplot25_29, ggout$DpCplot30_34, ggout$DpCplot35_39)
    #DpC_10y_grid <- gridExtra::grid.arrange(ggout$DpCplot20_29, ggout$DpCplot25_34, ggout$DpCplot30_39, ggout$DpCplot35_44)
    #DpC_15y_grid <- gridExtra::grid.arrange(ggout$DpCplot20_34, ggout$DpCplot25_39, ggout$DpCplot30_44, ggout$DpCplot35_49)
    
    
    
    #col <- c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#67001f', '#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#003c30','#b15928', '#dd1c77', '#1c9099')
    #DpCplot <- ggplot2::ggplot(DpC, aes(x = YsncS, y = Nicc * 100000/Npos)) + 
    #  geom_point(aes(color = Location)) + 
    #  geom_line(aes(color = Location)) +
    #  scale_color_manual(values = col) +
    #  ggtitle(stringr::str_c("Yearly ICC Incidence Rate in birth cohort of women ", mina, "-", maxa, "y and HPV + at study entry")) +
    #  ylab("ICC incidence rate per 100 000") +
    #  xlab("Years since Prevalence Study") +
    #  geom_linerange(aes(x = (YsncS), ymin = (Nicc/Npos - se)*100000, ymax = (Nicc/Npos + se)*100000, color = Location)) + # add standard error bars
    #  theme_bw()
    #DpCplot
    
    
    
    
    #### 12. Model ####
    
    # ln(Nicc/Npos) = alpha + beta*YsncS
    # beta is rate of change 
    nd <- data.frame(YsncS = 0:15)
    
    # latent class mixed model
    # par(mfrow = c(2, 4))
    # select age group from loop
    
    lcmm_out$tr_lcmm[[stringr::str_c(mina, "_", maxa)]] <- 
      tr_lcmm <- lcmm(logRate ~ YsncS, random = ~ YsncS, subject = "Location",  ng = 1, data = DpC, link = "splines")
    
    # bic
    lcmm_out$tr_bic[[stringr::str_c(mina, "_", maxa)]] <- 
      bic <- round(tr_lcmm$BIC, 2)
    
    # predictions
    lcmm_out$tr_pred[[stringr::str_c(mina, "_", maxa)]] <- 
      tr_pred <- predictY(tr_lcmm, nd, methInteg = 1, draws = TRUE)
    
    # groups
    lcmm_out$tr_prob[[stringr::str_c(mina, "_", maxa)]] <- tr_lcmm$pprob
    
    # convergence criteria
    lcmm_out$tr_conv[[stringr::str_c(mina, "_", maxa)]] <- tr_lcmm$conv
    
    ## slope
    lcmm_out$tr_lambda[[stringr::str_c(mina, "_", maxa)]] <- # 1 gamma per year
      tr_lambda <- data.frame("YsncS" = tr_pred$times[2:dim(nd)[1],]-0.5, 
                              "lambda" = diff(tr_pred$pred[, 1]))
    
    m_logr <- data.frame("logr" = tr_pred$pred[, 1], 
                         "logr_lo" = tr_pred$pred[, 2],
                         "logr_hi" = tr_pred$pred[, 3],
                         "YsncS" = tr_pred$times$YsncS)
    DpC <- DpC %>%
      full_join(., m_logr, by = "YsncS")
    
    col <- c('#d8b365', '#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#67001f', '#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#003c30','#b15928', '#dd1c77', '#1c9099')
    
    ggout$tr_plot[[stringr::str_c(mina, "_", maxa)]] <- 
      ggplot2::ggplot(DpC, aes(x = YsncS, y = logRate)) + 
      #geom_point(aes(color = Location)) + 
      geom_ribbon(aes(x = YsncS, ymin = logr_lo, ymax = logr_hi), fill = 'azure3', alpha = 0.4)+
      geom_line(aes(color = Location), size = 0.75) +
      scale_color_manual(values = col) +
      ggtitle(stringr::str_c(mina, "_", maxa, "y")) +
      ylab("log Incidence Rate in hrHPV+ women") +
      xlab("Years since HPV detection") +
      ylim(-10.5, -3)  +
      # geom_linerange(aes(x = YsncS, ymin = logRate - selog, ymax = logRate + selog, color = Location)) + # add standard error bars
      theme_bw() + 
      geom_line(aes(x = YsncS, y =  logr), col = "black", size = 1.7, linetype = 1)
    
    
  
    
    #### Simulation
    
    # cumulative incidence as happend in whole fem. pop.
    cum_dat <- Dp %>% 
      ungroup() %>%
      group_by(sgcentre, cid, Location, stY) %>%
      arrange(YsncS) %>%
      summarise(cumR = last(cumsum(Nicc)/cumsum(PopFemale[YsncS == 0]))) 
    
    cum_dat <- cum_dat[which(cum_dat$Location != "Viet Nam"), ]
    cum_dat <- cum_dat[which(cum_dat$Location != "Uganda"), ]
    cum_dat <- cum_dat[which(cum_dat$Location != "Italy"), ]
    
    for(k in seq_along(cum_dat$cid)) {
      c <- as.numeric(cum_dat$cid[k])
      st <- as.numeric(cum_dat$stY[cum_dat$cid ==c])
      sg <- as.numeric(cum_dat$sgcentre[cum_dat$cid ==c])
      cat("\n cid: ", c)   
      # Pre-Lexis object
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
      
      
      #### Incidence rates ####
      Dpop$Rate <-  Dpop$Nicc/Dpop$Npos
      Dpop$YsncS <- as.numeric(Dpop$Year) - as.numeric(Dpop$stY)
      lex_dat <- Dpop %>%  
        filter(YsncS >=0 & !is.na(Rate)) %>%  
        mutate(lex.dur = 1) %>%
        mutate(calender = as.numeric(Year)) %>%
        mutate(mort.rate = MR*10^(-5))
      head(lex_dat)
      
      # mortality 
      mr <- function(x){
        for(a in 1:nrow(x)){
          id <- x[a, "age"] 
          y <- x[a, "calender"] #name of a time scale  
          return(lex_dat[lex_dat$age==id & lex_dat$calender == y & lex_dat$cid == c, "mort.rate"]) # two time scales with same units
        }
      }
      
      # incidence rates from predicted model
      ir <- function(x){
        for(b in 1:nrow(x)){
          if(x[b, "lex.Cst"] == "hpv"){
            y <- as.numeric(x[b, "calender"]) - st
            return(exp(tr_pred$pred[tr_pred$times$YsncS == y, 1]))
          }
        }
      }
      
      ##Transition object
      Tr <- list("hpv" = list("icc" = ir,
                              "death" = mr),
                 "free" = list("death" = mr))
      
      ## Lexis simulation - only for one country
      hpvSim <- simLexis(Tr, PreLex, t.range = 20, N = 20)
      
      ## simulate cohort
      nSt <- nState(hpvSim,
                    at=seq(0, (2012-st), 1), from= st, time.scale="calender")
    
      ## plot
      pp <- as.data.frame(pState( nSt, perm=c(3, 2, 1)))
      pp$time <- c(st:2012)
      tr_pp$pp[[stringr::str_c("c_", c, "_", mina, "-", maxa)]] <- pp["2012", "icc"]
      tr_pp$cum[[stringr::str_c("c_", c, "_", mina, "-", maxa)]] <- cum_point <- cum_dat$cumR[cum_dat$cid==c]
      tr_pp$loc[[stringr::str_c("c_", c, "_", mina, "-", maxa)]] <- cum_dat$Location[cum_dat$cid ==c]
      tr_pp$ag[[stringr::str_c("c_", c, "_", mina, "-", maxa)]] <- stringr::str_c(mina, "_", maxa)
    }
  }
}

  
  ### comparision
cum_prob <- as.data.frame(t(bind_rows(tr_pp$cum, tr_pp$pp))) # transpose: switch rows and columns
cum_prob$id <- row.names(cum_prob)
tr_l <- as.data.frame(tr_pp$loc)
tr_l$id <- row.names(tr_l)
tr_ag <- as.data.frame(tr_pp$ag)
tr_ag$id <- row.names(tr_ag)
cum_prob <- cum_prob %>%
  full_join(., tr_l, by = "id") %>%
  full_join(., tr_ag, by = "id")
colnames(cum_prob) <- c("cum", "pred", "id", "Location", "AgeGroup")
cum_prob$AgeGroup <- factor(cum_prob$AgeGroup, levels = c("20_24", "25_29", "30_34", "20_29", "25_34", "30_39"),  
                                                          labels = c("20-24", "25-29", "30-34", "20-29", "25-34", "30-39"))

col <- c('#d8b365', '#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#67001f', '#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#003c30','#b15928', '#dd1c77', '#1c9099')

ggplot(cum_prob, aes(pred, cum)) +
  theme_minimal() +
  scale_color_manual(values = col) +
  geom_line(aes(color = Location), size = 1.3) +
  geom_point(aes(shape = AgeGroup), size = 2.1) +
  xlim(0, 0.02) +
  ylim(0, 0.01) +
  labs(title = "Comparing actual vs. predicted cervical cancer cumulative incidence by country and age group", 
       subtitle = "Actual: CI5 rates, UN population. Predicted: using estimated rates in hrHPV+ women.",
       x = "predicted cumulative incidence probablity",
       y = "actual cumulative incidence probablity") 

ggplot(cum_prob, aes(pred, cum)) +
  theme_minimal() +
  scale_color_manual(values = col) +
  geom_smooth(aes(color = AgeGroup), size = 1, se = FALSE) +
  geom_point(aes(color= AgeGroup), size = 1.5) +
  xlim(0, 0.02) +
  ylim(0, 0.01) +
  labs(title = "Comparing actual vs. predicted cervical cancer cumulative incidence by age group", 
       subtitle = "Actual: CI5 rates, UN population. Predicted: using estimated rates in hrHPV+ women.",
       x = "predicted cumulative incidence probablity",
       y = "actual cumulative incidence probablity") 

