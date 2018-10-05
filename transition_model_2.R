 #### 1. info table ####
 
info <- data.frame(matrix(nrow = 18, ncol = 0))
info <- info %>%
  mutate("cid" = c(1,  2,  5,  6,  8,  9, 10, 12, 13, 15,  3, 16, 17, 18,  4, 20, 19, 22), # assigned by Rosa
         "sgcentre" = c(44, 100, 19, 12, 23, 18, 61, 15,  2,  7,  9, 14,  3,  3, 16, 83,  4, 41), # defined by IARC prevalence studies
         "stY" = c("2007-2008", "2002-2004", "1993-1994", "1993-1995", "2005", "2004", "2013-2014", "1999-2000", "1997", "1997-1998", 
                    "1998", "1990-2000", "1998", "1998", "2001", "2002", "1995-1998", "2006"),
         "loc.prev" = c("Algeria, Setif","Uganda, Kyadondo County", "Costa Rica",                
                        "Colombia, Bucaramanga","China, Shenyang","India, Dindigul, Ambilikkai",                  
                        "Iran (Islamic Republic of), Golestan Province", "Republic of Korea, Busan",   "Viet Nam, Ho Chi Minh City",                   
                        "Thailand, Lampang","Argentina, Entre Rios Province", "Thailand, Songkhla",        
                        "Spain, Tarragona", "Spain, Girona", "Chile, Region of Antofagasta",                 
                        "Italy, Turin", "The Netherlands", "Poland, Poznan"),  # Prevalence STudy location
         "country" =  c("Algeria", "Uganda", "Costa Rica", "Colombia", "China", "India", "Iran (Islamic Republic of)",  "Republic of Korea", "Viet Nam", "Thailand", # for mortality, population
                        "Argentina", "Thailand", "Spain", "Spain", "Chile", "Italy", "Netherlands", "Poland"),
         "Location" = c("Algeria", "Uganda", "Costa Rica", "Colombia", "China", "India", "Iran (Islamic Republic of)",  "Republic of Korea", "Viet Nam", "Thailand (Lampang)", # for graph
                        "Argentina", "Thailand (Songkhla)", "Spain (Terragona)", "Spain (Girona)", "Chile", "Italy", "Netherlands", "Poland")) %>%
  separate(stY, into = paste0("stY", c(0, "")))
info$stY[info$cid %in% c(8, 9, 13, 3, 17, 18, 4, 20, 22)] <- c(2005, 2004, 1997, 1998, 2000, 2000, 2003, 2002, 2006) # filling NA if only one study year
info
#### 2. select age group, country ####
mina <- 25
maxa <- 30

#### 3. mortality ####
 # load from mortality_rates.UN


#### 4. population ####

#### 5. incidence ####

#### 6. prevalence ####

####7. joint table ####

#### 8. calculate Nicc, Npos, YsncS ####

#### 9. Plot ####
#### 10. Model ####
