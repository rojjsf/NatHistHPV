library(RColorBrewer)
display.brewer.all()
#ag <- c("[20,25)", "[25,30)", "[30,35)", "[35,40)", "[40,45)", "[45,50)")
ag <- c("[25,30)", "[30,35)")
rl <- 1000
ver2 <- hpv.inc.long %>%
  mutate(age = as.numeric(c(10 + as.numeric(hpv.inc.long$age.grp)*5 + 0:4))) %>%
  filter(ih >= 0 & ih < rl)%>%
  filter(age.grp %in% ag) 
ver3 <- Dp %>%
  ungroup() %>%
  group_by(Year, Location, cid, age.grp) %>% 
  summarise(Npos = sum(Npos), Nicc = sum(Nicc), PopFemale = sum(PopFemale)) %>%
  #filter(Location != "Viet Nam" & Location != "Uganda" & cid != 17 & cid != 18) %>%
  mutate(rateh = Nicc*100000/Npos) %>%
  filter(rateh >= 0 & rateh<= rl)%>%
  filter(age.grp %in% ag) #%>%
  #filter(Year %in% 2010:2012)
ver3[ver3$cid == 15, "Location"] <- "Thailand.L"
ver3[ver3$cid == 16, "Location"] <- "Thailand.S"
ver3$age.grp <- factor(ver3$age.grp, levels = levels(ver2$age.grp))
ver3$cid <- as.numeric(ver3$cid)
#ver3 <- ver3[-is.na(ver3$Location),]
test23 <- ver2 %>%
  full_join(., ver3, by = c("cid", "Year", "age.grp"))
test23$location <- as.factor(test23$Location)
ggplot(test23, aes(x = ih, y = rateh)) +
  geom_point(aes(color = Location, shape = age.grp)) +
  scale_color_manual(values = col) +
  #scale_shape_manual(values = c(1, 2, 8, 15:22)) +
  ggtitle(stringr::str_c("comparing calculation of transition rates without/with count data ", ag)) 
#coord_cartesian(xlim = c(0, rl), ylim = c(0, rl))

col <- c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#ffff99','#b15928',"#1a1a1a", "#5ab4ac", "#dd1c77")
ggplot(test23, aes(x = ih, y = Nicc/Npos*100000)) +
  geom_point(aes(color = Location, shape = age.grp)) +
  #scale_shape_manual(values = c(1, 2, 8, 15:22)) +
  scale_color_manual(values = col) +
  ggtitle(stringr::str_c("comparing calculation of transition rates without/with count data")) +
  facet_wrap(~Location, scales = 'free')
test23[300:350, ]
hpv.inc[hpv.inc$cid == 8, ]

hist(ver2[ver2$ih > 0, "ih"])
head(test23)
head(ver2)
head(Dp)


 ## comparing rates and Nicc/Npos
col <- c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#ffff99','#b15928',"#1a1a1a")
ggplot(test23, aes(x = inc.rate, y = Nicc/PopFemale*100000)) +
  geom_point(aes(color = Location, shape = age.grp, na.rm = FALSE)) +
  #scale_shape_manual(values = c(1, 2, 8, 15:22)) +
  scale_color_manual(values = col) +
  ggtitle(stringr::str_c("comparing calculation of incidence rates and Nicc/PopFemale ")) +
  coord_cartesian(xlim = c(0, rl), ylim = c(0, rl))

ggplot(test23, aes(x = inc.rate, y = Nicc/PopFemale*100000)) +
  geom_point(aes(color = age.grp, shape = age.grp)) +
  #scale_shape_manual(values = c(1, 2, 8, 15:22)) +
  scale_color_manual(values = col) +
  facet_wrap(~Location, scales = 'free') +
  ggtitle(stringr::str_c("comparing calculation of incidence rates and Nicc/PopFemale ")) 
  # coord_cartesian(xlim = c(0, rl), ylim = c(0, rl))



str(test23)
