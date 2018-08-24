library(haven)
library(Epi)
library(dplyr)
library(xlsx)
library(writexl)
library(tidyr)

#### cervical cancer mortality #### 

Morticd10_part1 <- read.csv("I:/Projects/International Correlation of HPV and Cervical Cancer/codes and documents/data/mortality data for R codes/Morticd10_part1")
Morticd10_part2 <- read.csv("I:/Projects/International Correlation of HPV and Cervical Cancer/codes and documents/data/mortality data for R codes/Morticd10_part2")
pop <- read.csv("I:/Projects/International Correlation of HPV and Cervical Cancer/codes and documents/data/mortality data for R codes/pop")

countries <- c(1010, # Algeria
               1190, # Guinea
               1220, # Kenya
               1340, # Nigeria
               1370, # Rwanda
               1430, # South Africa| general mort available
               1530, # Uganda 
               2020, # Argentina| general mort available
               2090, # Canada| general mort available
               2120, # Chile| general mort available
               2130, # Colombia| general mort available
               2140, # Costa Rica| general mort available
               2310, # Mexico| general mort available
               2460, # Uruguay| general mort available
               3027, # Bhutan
               3068, # China
               3100, # India
               3130, # Iran
               3260, # Mongolia
               3280, # Nepal
               3290, # Pakistan
               3325, # Korea| general mort available
               3380, # Thailand| general mort available
               3408, # Viet Nam
               4055, # Estonia| general mort available
               4084, # Georgia| general mort available
               4180, # Italy| general mort available
               4188, # Lithuania| general mort available
               4210, # netherlands| general mort available
               4230, # Poland| general mort available
               4280, # Spain| general mort available
               4308, # United Kingdom| general mort available
               5020, # Australia| general mort available
               5070, # Fiji| general mort available
               5207 # Vanuatu
)

gen.countries <-  c("South Africa", # country names for which there is general mortality data available
                                "Argentina",
                                "Canada",
                                "Chile",
                                "Colombia",
                                "Costa Rica",
                                "Mexico",
                                "Uruguay",
                                "Korea",
                                "Thailand",
                                "Estonia",
                                "Georgia",
                                "Italy",
                                "Lithuania",
                                "Netherlands",
                                "Poland",
                                "Spain",
                                "United Kingdom",
                                "Australia",
                                "Fiji")

country.names <- c("Algeria"
                   , "Guinea"
                   , "Kenya"
                   , "Nigeria"
                   , "Rwanda"
                   , "South Africa"# general mort available
                   , "Uganda "
                   , "Argentina"# general mort available
                   , "Canada"# general mort available
                   , "Chile"# general mort available
                   , "Colombia"# general mort available
                   , "Costa Rica"# general mort available
                   , "Mexico"# general mort available
                   , "Uruguay"# general mort available
                   , "Bhutan"
                   , "China"
                   , "India"
                   , "Iran"
                   , "Mongolia"
                   , "Nepal"
                   , "Pakistan"
                   , "Korea"# general mort available
                   , "Thailand"# general mort available
                   , "Viet Nam"
                   , "Estonia"# general mort available
                   , "Georgia"# general mort available
                   , "Italy"# general mort available
                   , "Lithuania"# general mort available
                   , "Netherlands"# general mort available
                   , "Poland"# general mort available
                   , "Spain"# general mort available
                   , "United Kingdom"# general mort available
                   , "Australia"# general mort available
                   , "Fiji"# general mort available
                   , "Vanuatu"
                   )

info.mort <- data.frame(country.names, "Country" = countries)

c53.icd10_part1 <- Morticd10_part1 %>%
  filter(Cause %in% c(1037, "C53")) %>% #death due to cervical cancer
  #filter(Sex == 2) %>% # female deaths only
  #select(-IM_Deaths1, -IM_Deaths2, -IM_Deaths3, -IM_Deaths4) %>% # infant deaths
  #filter(Year %in% 1993:2018) %>% # only years for which we have the prev & inc data
  filter(Country %in% countries)
c53.icd10_part1 %>%
  group_by(Country) %>%
  summarise(n=n())%>%
  knitr::kable() # nb of years with mortality count per country
# countries available: South Africa, Korea, Thailand, Estonia, Georgia, Lithuania
stat.table(Country, contents = count(), data = c53.icd10_part1 )

c53.icd10_part2 <- Morticd10_part2 %>%
  filter(Cause %in% c("C53")) %>%
  filter(Sex == 2) %>%
  select(-IM_Deaths1, -IM_Deaths2, -IM_Deaths3, -IM_Deaths4) %>%
  filter(Year %in% 1993:2018) %>%
  filter(Country %in% countries) 
c53.icd10_part2 %>%
  group_by(Country) %>%
  summarise(n=n())%>%
  knitr::kable()
# countries available: South Africa, Iran, Korea, Thailand, Estonia

pop.fem <- pop %>%
  filter(Sex == 2) %>%
  select(-Lb) %>% # live births
  filter(Country %in% countries )


head(c53.icd10_part1)
which()

#
#
#### mortality all causes #### 
#
#

Morticd10_part1 %>%
  filter(Sex == 2) %>% 
  filter(Year %in% 1993:2018) %>% # only years for which we have the prev & inc data
  filter(Country %in% countries) %>%
  filter(Cause != "C53") %>%
  group_by(Country) %>%
  summarise(n=n())%>%
  knitr::kable()

all.icd10_part1 <- Morticd10_part1 %>%
  filter(Sex == 2) %>% 
  filter(Year %in% 1993:2018) %>% # only years for which we have the prev & inc data
  filter(Country %in% countries) %>%
  filter(Cause != "C53") 

all.icd10_part2 <- Morticd10_part2 %>%
  filter(Sex == 2) %>% 
  filter(Year %in% 1993:2018) %>% # only years for which we have the prev & inc data
  filter(Country %in% countries) %>%
  filter(Cause != "C53") 

all.icd10 <- rbind(all.icd10_part1, all.icd10_part2)

all.icd10 <- all.icd10%>%  
  group_by(Country, Year) %>%
  summarise("death20_24" = sum(Deaths10), "death25_29" = sum(Deaths11), "death30_34" = sum(Deaths12), "death35_39" = sum(Deaths13), "death40_44" = sum(Deaths14), 
          "death45_49" = sum(Deaths15), "death50_54" = sum(Deaths16), "death55_59" = sum(Deaths17), "death60_64" = sum(Deaths18), "death65_69" = sum(Deaths19), 
          "death70_74" = sum(Deaths20), "death75_79" = sum(Deaths21), "death80_84" = sum(Deaths22), "death85" = sum(Deaths23))

head(all.icd10)


pop.all <- pop %>%
  filter(Sex == 2) %>%
  # select(-Lb, - Pop1, - Pop2, -Pop3, -Pop4, -Pop5, -Pop6, -Pop7, -Pop8, -Pop9) %>% # live births and < age 20 excluded
  filter(Country %in% countries) %>%
  filter(Year %in% 1993:2018)
pop.all <- pop.all %>%
  transmute(Country = Country,
            Year = Year,
            SubDiv = SubDiv, # category of data
            "pop20_24" = Pop10, "pop25_29" = Pop11, "pop30_34" = Pop12, "pop35_39" = Pop13, "pop40_44" = Pop14, 
            "pop45_49" = Pop15, "pop50_54" = Pop16, "pop55_59" = Pop17, 
            "pop60_64" = Pop18, "pop65_69" = Pop19, "pop70_74" = Pop20, "pop75_79" = Pop21, "pop80_84" = Pop22, "pop85" = Pop23
            )

pop.all %>%
   group_by(Country) %>%
   summarise(n=n())%>%
   knitr::kable()

mort.all <- merge(all.icd10, pop.all, by = c("Country", "Year")) # all.x or all.y ?? no use if only death or pop info...
mort.all <- merge(info.mort, mort.all, by = "Country")
mort.all <- mort.all %>%
  mutate("mort20_24" = round(death20_24 * 100000/pop20_24, 2),
         "mort25_29" = round(death25_29 * 100000/pop25_29, 2),
         "mort30_34" = round(death30_34 * 100000/pop30_34, 2),
         "mort35_39" = round(death35_39 * 100000/pop35_39, 2),
         "mort40_44" = round(death40_44 * 100000/pop40_44, 2),
         "mort45_49" = round(death45_49 * 100000/pop45_49, 2),
         "mort50_54" = round(death50_54 * 100000/pop50_54, 2),
         "mort55_59" = round(death55_59 * 100000/pop55_59, 2),
         "mort60_64" = round(death60_64 * 100000/pop60_64, 2),
         "mort65_69" = round(death65_69 * 100000/pop65_69, 2),
         "mort70_74" = round(death70_74 * 100000/pop70_74, 2),
         "mort75_79" = round(death75_79 * 100000/pop75_79, 2),
         "mort80_84" = round(death80_84 * 100000/pop80_84, 2),
         "mort85" = round(death85 * 100000/pop85, 2))
  
head(mort.all)
mort.all %>%
  group_by(country.names) %>%
  summarise(n=n())%>%
  knitr::kable()
