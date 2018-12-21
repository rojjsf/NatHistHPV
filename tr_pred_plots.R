cori_pp <- tr_pp$pp_pred$`c_5_25-34` %>%
  mutate(time = c(1994:2012))
cori_cum <- data.frame("icc" = tr_pp$pp_cum$`c_5_25-34`["2012" , "icc"], "time" = 2012)
ggplot(cori_pp, aes(x = time, y = icc)) + 
  theme_classic() +
  geom_line(size = 1.2) +
  geom_point(aes(x = cori_cum$time, y = cori_cum$icc), color = "red", size = 3) +
  labs(title = "Women in Costa Rica, 25-34y in 1993", 
       sub = "Point: Cumulative incidence using CI5 data",
       y = "Predicted cumulative incidence of cervical cancer", 
       x = "Time[Year]") +
  theme(plot.title = element_text(size = 20, face = "bold"))




ital_pp <- tr_pp$pp_pred$`c_20_25-34` %>%
   mutate(time = c(2002:2012))
ital_cum <- data.frame("icc" = tr_pp$pp_cum$`c_20_25-34`["2012" , "icc"], "time" = 2012)
ggplot(ital_pp, aes(x = time, y = icc)) + 
  theme_classic() +
  geom_line(size = 1.2) +
  geom_point(aes(x = ital_cum$time, y = ital_cum$icc), color = "red", size = 3) +
  labs(title = "Women in Italy, 25-34y in 2002", 
       sub = "Point: Cumulative incidence using CI5 data",
       y = "Predicted cumulative incidence of cervical cancer", 
       x = "Time[Year]") +
  theme(plot.title = element_text(size = 20, face = "bold"))

thai_pp <- tr_pp$pp_pred$`c_16_25-34` %>%
  mutate(time = c(2000:2012))
thai_cum <- data.frame("icc" = tr_pp$pp_cum$`c_16_25-34`["2012" , "icc"], "time" = 2012)
ggplot(thai_pp, aes(x = time, y = icc)) + 
  theme_classic() +
  geom_line(size = 1.2) +
  geom_point(aes(x = thai_cum$time, y = thai_cum$icc), color = "red", size = 3) +
  labs(title = "Women in Thailand, 25-34y in 2002", 
       sub = "Point: Cumulative incidence using CI5 data",
       y = "Predicted cumulative incidence of cervical cancer", 
       x = "Time[Year]") +
  theme(plot.title = element_text(size = 20, face = "bold"))

