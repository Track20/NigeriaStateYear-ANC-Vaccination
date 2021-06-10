# Subnational Nigeria Vaccine, ANC, SBA, and Facility Delivery by Year
# Kristin Bietsch, PhD
# Senior Demographer, Avenir Health
# June 10, 2021

library(dplyr)
library(tidyr)
library(haven)
library(stringr)
library(questionr)
library(ggplot2)
library(cowplot)
library(ggpubr)
library(pdftools)
library(lemon)
setwd("C:/Users/KristinBietsch/files/Track20/Nigeria")


# DHS Data
# Birth recode
dhs_br <- read_dta("C:/Users/KristinBietsch/files/DHS Data/Nigeria/NGBR7AFL.DTA")
dhs_br13 <- read_dta("C:/Users/KristinBietsch/files/DHS Data/Nigeria/NGBR6AFL.DTA")

# Child Recode
dhs_kr <- read_dta("C:/Users/KristinBietsch/files/DHS Data/Nigeria/NGKR7AFL.DTA")
dhs_kr13 <- read_dta("C:/Users/KristinBietsch/files/DHS Data/Nigeria/NGKR6AFL.DTA")


# ANC data from DHS
dhs_br_clean <- dhs_br %>% mutate(anc1 = case_when(m14==0 ~ 0,
                                                   m14>=1 & m14<=90 ~ 1),
                                  anc4= case_when(m14<4 ~ 0,
                                                  m14>=4 & m14<=90 ~ 1),
                                  mid_preg_year = floor((b3-4)/12 + 1900) ) %>%
  mutate(Region=case_when(sstate==230 ~ "Ekiti",
                          sstate==40 ~ "Jigawa",
                          sstate==110 ~ "Kaduna",
                          sstate==100 ~ "Kano",
                          sstate==30 ~ "Katsina",
                          sstate==120 ~ "Kebbi",
                          sstate==360 ~ "Lagos",
                          sstate==370 ~ "Ogun",
                          sstate==240 ~ "Ondo",
                          sstate==220 ~ "Osun",
                          sstate==210 ~ "Oyo",
                          sstate==10 ~ "Sokoto",
                          sstate==20 ~ "Zamfara")) %>% 
  filter(!is.na(Region)) %>%
  filter(mid_preg_year>2013 & mid_preg_year<2018) %>%
  mutate(Region_Year=paste(Region, mid_preg_year, sep="_"),
         sampleweights=v005/100000) 

anc1_dhs <-    as.data.frame(prop.table(wtd.table( dhs_br_clean$Region_Year, dhs_br_clean$anc1, weights=dhs_br_clean$sampleweights ),1)) %>% filter(Var2==1) %>% select(-Var2) %>% rename(ANC1=Freq, Region_Year=Var1)
anc4_dhs <-    as.data.frame(prop.table(wtd.table( dhs_br_clean$Region_Year, dhs_br_clean$anc4, weights=dhs_br_clean$sampleweights ),1)) %>% filter(Var2==1) %>% select(-Var2) %>% rename(ANC4=Freq, Region_Year=Var1)
n_dhs <- as.data.frame(table(dhs_br_clean$Region_Year)) %>% rename(N=Freq, Region_Year=Var1)

anc_dhs <- full_join(anc1_dhs, anc4_dhs, by="Region_Year") %>% full_join( n_dhs,  by="Region_Year") %>% separate(Region_Year,  c("Region", "Year"), sep="_") %>% rename(DHSBirths=N)


fac_birth <-  dhs_br %>% mutate(fac_deliv = case_when(m15>=21 & m15<=36 ~ 1, m15<20 ~ 0, m15>40 ~ 0),
                                public_deliv = case_when(m15>=21 & m15<=29 ~ 1, m15<20 ~ 0, m15>30 ~ 0)) %>%
  mutate(sampleweights=v005/100000,
         birth_year=b2) %>%
  mutate(Region=case_when(sstate==230 ~ "Ekiti",
                          sstate==40 ~ "Jigawa",
                          sstate==110 ~ "Kaduna",
                          sstate==100 ~ "Kano",
                          sstate==30 ~ "Katsina",
                          sstate==120 ~ "Kebbi",
                          sstate==360 ~ "Lagos",
                          sstate==370 ~ "Ogun",
                          sstate==240 ~ "Ondo",
                          sstate==220 ~ "Osun",
                          sstate==210 ~ "Oyo",
                          sstate==10 ~ "Sokoto",
                          sstate==20 ~ "Zamfara")) %>% 
  filter(!is.na(Region)) %>%
  filter(birth_year>2013 & birth_year<2018) %>%
  mutate(Region_Year=paste(Region, birth_year, sep="_")) 


fac_birth_dhs <-    as.data.frame(prop.table(wtd.table( fac_birth$Region_Year, fac_birth$fac_deliv, weights=fac_birth$sampleweights ),1)) %>% filter(Var2==1) %>% select(-Var2) %>% rename(fac_deliv=Freq, Region_Year=Var1)
pubfac_birth_dhs <-    as.data.frame(prop.table(wtd.table( fac_birth$Region_Year, fac_birth$public_deliv, weights=fac_birth$sampleweights ),1)) %>% filter(Var2==1) %>% select(-Var2) %>% rename(public_deliv=Freq, Region_Year=Var1)

fac_dhs <- full_join(fac_birth_dhs, pubfac_birth_dhs, by="Region_Year")  %>% separate(Region_Year,  c("Region", "Year"), sep="_") 





dhs_br_clean13 <- dhs_br13 %>% mutate(anc1 = case_when(m14==0 ~ 0,
                                                       m14>=1 & m14<=90 ~ 1),
                                      anc4= case_when(m14<4 ~ 0,
                                                      m14>=4 & m14<=90 ~ 1),
                                      mid_preg_year = floor((b3-4)/12 + 1900) ) %>%
  mutate(Region=case_when(sstate==230 ~ "Ekiti",
                          sstate==40 ~ "Jigawa",
                          sstate==110 ~ "Kaduna",
                          sstate==100 ~ "Kano",
                          sstate==30 ~ "Katsina",
                          sstate==120 ~ "Kebbi",
                          sstate==360 ~ "Lagos",
                          sstate==370 ~ "Ogun",
                          sstate==240 ~ "Ondo",
                          sstate==220 ~ "Osun",
                          sstate==210 ~ "Oyo",
                          sstate==10 ~ "Sokoto",
                          sstate==20 ~ "Zamfara")) %>% 
  filter(!is.na(Region)) %>%
  filter(mid_preg_year>2008 & mid_preg_year<2013) %>%
  mutate(Region_Year=paste(Region, mid_preg_year, sep="_"),
         sampleweights=v005/100000) 

anc1_dhs13 <-    as.data.frame(prop.table(wtd.table( dhs_br_clean13$Region_Year, dhs_br_clean13$anc1, weights=dhs_br_clean13$sampleweights ),1))  %>% filter(Var2==1) %>% select(-Var2) %>% rename(ANC1=Freq, Region_Year=Var1)
anc4_dhs13 <-    as.data.frame(prop.table(wtd.table( dhs_br_clean13$Region_Year, dhs_br_clean13$anc4, weights=dhs_br_clean13$sampleweights ),1)) %>% filter(Var2==1) %>% select(-Var2) %>% rename(ANC4=Freq, Region_Year=Var1)
n_dhs13 <- as.data.frame(table(dhs_br_clean13$Region_Year)) %>% rename(N=Freq, Region_Year=Var1)

anc_dhs13 <- full_join(anc1_dhs13, anc4_dhs13, by="Region_Year") %>% full_join( n_dhs13,  by="Region_Year") %>% separate(Region_Year,  c("Region", "Year"), sep="_") %>% rename(DHSBirths=N)

anc_dhs_full <- bind_rows(anc_dhs, anc_dhs13) %>% mutate(Year=as.numeric(as.character(Year)))


###

fac_birth13 <-  dhs_br13 %>% mutate(fac_deliv = case_when(m15>=21 & m15<=36 ~ 1, m15<20 ~ 0, m15>40 ~ 0),
                                    public_deliv = case_when(m15>=21 & m15<=29 ~ 1, m15<20 ~ 0, m15>30 ~ 0)) %>%
  mutate(sampleweights=v005/100000,
         birth_year=b2) %>%
  mutate(Region=case_when(sstate==230 ~ "Ekiti",
                          sstate==40 ~ "Jigawa",
                          sstate==110 ~ "Kaduna",
                          sstate==100 ~ "Kano",
                          sstate==30 ~ "Katsina",
                          sstate==120 ~ "Kebbi",
                          sstate==360 ~ "Lagos",
                          sstate==370 ~ "Ogun",
                          sstate==240 ~ "Ondo",
                          sstate==220 ~ "Osun",
                          sstate==210 ~ "Oyo",
                          sstate==10 ~ "Sokoto",
                          sstate==20 ~ "Zamfara")) %>% 
  filter(!is.na(Region)) %>%
  filter(birth_year>2008 & birth_year<2013) %>%
  mutate(Region_Year=paste(Region, birth_year, sep="_")) 


fac_birth_dhs13 <-    as.data.frame(prop.table(wtd.table( fac_birth13$Region_Year, fac_birth13$fac_deliv, weights=fac_birth13$sampleweights ),1)) %>% filter(Var2==1) %>% select(-Var2) %>% rename(fac_deliv=Freq, Region_Year=Var1)
pubfac_birth_dhs13 <-    as.data.frame(prop.table(wtd.table( fac_birth13$Region_Year, fac_birth13$public_deliv, weights=fac_birth13$sampleweights ),1)) %>% filter(Var2==1) %>% select(-Var2) %>% rename(public_deliv=Freq, Region_Year=Var1)

fac_dhs13 <- full_join(fac_birth_dhs13, pubfac_birth_dhs13, by="Region_Year")  %>% separate(Region_Year,  c("Region", "Year"), sep="_") 

fac_dhs_full <- bind_rows(fac_dhs, fac_dhs13) %>% mutate(Year=as.numeric(as.character(Year))) %>% arrange(Region, Year)

##########################################################33
# SBA from DHS

dhs_sba2018 <- dhs_br %>% mutate(period=60,
                                 age=b19) %>%
  filter(age<period) %>%
  mutate(sba=case_when(m3a==1 | m3b==1 | m3c==1 ~ 1,
                       m3a==0 & m3b==0 & m3c==0 ~ 0)) %>%
  mutate(sampleweights=v005/100000,
         birth_year=b2) %>%
  mutate(Region=case_when(sstate==230 ~ "Ekiti",
                          sstate==40 ~ "Jigawa",
                          sstate==110 ~ "Kaduna",
                          sstate==100 ~ "Kano",
                          sstate==30 ~ "Katsina",
                          sstate==120 ~ "Kebbi",
                          sstate==360 ~ "Lagos",
                          sstate==370 ~ "Ogun",
                          sstate==240 ~ "Ondo",
                          sstate==220 ~ "Osun",
                          sstate==210 ~ "Oyo",
                          sstate==10 ~ "Sokoto",
                          sstate==20 ~ "Zamfara")) %>% 
  filter(!is.na(Region)) %>%
  filter(birth_year>2013 & birth_year<2018) %>%
  mutate(Region_Year=paste(Region, birth_year, sep="_")) 

dhs_sba2013 <- dhs_br13 %>% mutate(period=60,
                                   age=v008 - b3) %>%
  filter(age<period) %>%
  mutate(sba=case_when(m3a==1 | m3b==1 | m3c==1 ~ 1,
                       m3a!=1 & m3b!=1 & m3c!=1 ~ 0)) %>%
  mutate(sampleweights=v005/100000,
         birth_year=b2) %>%
  mutate(Region=case_when(sstate==230 ~ "Ekiti",
                          sstate==40 ~ "Jigawa",
                          sstate==110 ~ "Kaduna",
                          sstate==100 ~ "Kano",
                          sstate==30 ~ "Katsina",
                          sstate==120 ~ "Kebbi",
                          sstate==360 ~ "Lagos",
                          sstate==370 ~ "Ogun",
                          sstate==240 ~ "Ondo",
                          sstate==220 ~ "Osun",
                          sstate==210 ~ "Oyo",
                          sstate==10 ~ "Sokoto",
                          sstate==20 ~ "Zamfara")) %>% 
  filter(!is.na(Region)) %>%
  filter(b2>2008 & b2<2013) %>%
  mutate(Region_Year=paste(Region, birth_year, sep="_")) 

sba_dhs2018 <-    as.data.frame(prop.table(wtd.table( dhs_sba2018$Region_Year, dhs_sba2018$sba, weights=dhs_sba2018$sampleweights ),1)) %>% filter(Var2==1) %>% select(-Var2) %>% rename(SBA=Freq, Region_Year=Var1)  %>% separate(Region_Year,  c("Region", "Year"), sep="_") 
sba_dhs2013 <-    as.data.frame(prop.table(wtd.table( dhs_sba2013$Region_Year, dhs_sba2013$sba, weights=dhs_sba2013$sampleweights ),1)) %>% filter(Var2==1) %>% select(-Var2) %>% rename(SBA=Freq, Region_Year=Var1) %>% separate(Region_Year,  c("Region", "Year"), sep="_") 


sba_dhs_full <- bind_rows(sba_dhs2018, sba_dhs2013) %>% mutate(Year=as.numeric(as.character(Year))) %>% arrange(Region, Year) %>% rename(SBA_DHS=SBA)



##########################################################33
# Vaccine data from DHS


dhs_kr_clean <- dhs_kr %>% mutate(age = v008- b3) %>% filter(age>=12 & age<=35)  %>% filter(b5==1) %>% 
  mutate( year_birth = b2 ,
          year_1month = floor(b2 + ((b1 + 1)/12)),
          year_2month = floor(b2 + ((b1 + 2)/12)),
          year_3month = floor(b2 + ((b1 + 3)/12)),
          year_4month = floor(b2 + ((b1 + 4)/12)),
          year_6month = floor(b2 + ((b1 + 6)/12)),
          year_9month = floor(b2 + ((b1 + 9)/12)),
          year_11month = floor(b2 + ((b1 + 11)/12)),
          year_12month = floor(b2 + ((b1 + 12)/12)),
          year_15month = floor(b2 + ((b1 + 15)/12))) %>%
  mutate(bcg=case_when(h2>=1 & h2<=3 ~ 1, h2==0 | h2==8 ~ 0),
         polio0 = case_when(h0>=1 & h0<=3 ~ 1, h0==0 | h0==8 ~ 0),
         polio1 = case_when(h4>=1 & h4<=3 ~ 1, h4==0 | h4==8 ~ 0),
         polio2 = case_when(h6>=1 & h6<=3 ~ 1, h6==0 | h6==8 ~ 0),
         polio3 = case_when(h8>=1 & h8<=3 ~ 1, h8==0 | h8==8 ~ 0),
         polio_sum= polio1 + polio2 + polio3,
         polio_sum1 = case_when(polio_sum>=1 ~ 1 , polio_sum<1 ~ 0),
         polio_sum2 = case_when(polio_sum>=2 ~ 1 , polio_sum<2 ~ 0),
         polio_sum3 = case_when(polio_sum>=3 ~ 1 , polio_sum<3 ~ 0),
         measles1 = case_when(h9>=1 & h9<=3 ~ 1, h9==0 | h9==8 ~ 0),
         measles2 = case_when(h9a>=1 & h9a<=3 ~ 1, h9a==0 | h9a==8 ~ 0),
         measles_sum= measles1 + measles2 ,
         measles_sum1 = case_when(measles_sum>=1 ~ 1 , measles_sum<1 ~ 0),
         measles_sum2 = case_when(measles_sum>=2 ~ 1 , measles_sum<2 ~ 0),
         pneumo1= case_when(h54>=1 & h54<=3 ~ 1, h54==0 | h54==8 ~ 0), 
         pneumo2= case_when(h55>=1 & h55<=3 ~ 1, h55==0 | h55==8 ~ 0), 
         pneumo3= case_when(h56>=1 & h56<=3 ~ 1, h56==0 | h56==8 ~ 0),
         pneumo_sum= pneumo1 + pneumo2 + pneumo3,
         pneumo_sum1 = case_when(pneumo_sum>=1 ~ 1 , pneumo_sum<1 ~ 0),
         pneumo_sum2 = case_when(pneumo_sum>=2 ~ 1 , pneumo_sum<2 ~ 0),
         pneumo_sum3 = case_when(pneumo_sum>=3 ~ 1 , pneumo_sum<3 ~ 0),
         hepb0= case_when(h50>=1 & h50<=3 ~ 1, h50==0 | h50==8 ~ 0),
         penta1= case_when(h54>=1 & h54<=3 ~ 1, h54==0 | h54==8 ~ 0), 
         penta2= case_when(h55>=1 & h55<=3 ~ 1, h55==0 | h55==8 ~ 0), 
         penta3= case_when(h56>=1 & h56<=3 ~ 1, h56==0 | h56==8 ~ 0),
         penta_sum= penta1 + penta2 + penta3,
         penta_sum1 = case_when(penta_sum>=1 ~ 1 , penta_sum<1 ~ 0),
         penta_sum2 = case_when(penta_sum>=2 ~ 1 , penta_sum<2 ~ 0),
         penta_sum3 = case_when(penta_sum>=3 ~ 1 , penta_sum<3 ~ 0) )  %>%
  mutate(full_vac=case_when(bcg==1 & measles_sum1==1 & penta_sum1==1 & penta_sum2==1 & penta_sum3==1 & polio_sum1==1 & polio_sum2==1 & polio_sum3==1 ~ 1,
                            bcg!=1 | measles_sum1!=1 | penta_sum1!=1 | penta_sum2!=1 | penta_sum3!=1 | polio_sum1!=1 | polio_sum2!=1 | polio_sum3!=1 ~ 0)) %>%
  mutate(Region=case_when(sstate==230 ~ "Ekiti",
                          sstate==40 ~ "Jigawa",
                          sstate==110 ~ "Kaduna",
                          sstate==100 ~ "Kano",
                          sstate==30 ~ "Katsina",
                          sstate==120 ~ "Kebbi",
                          sstate==360 ~ "Lagos",
                          sstate==370 ~ "Ogun",
                          sstate==240 ~ "Ondo",
                          sstate==220 ~ "Osun",
                          sstate==210 ~ "Oyo",
                          sstate==10 ~ "Sokoto",
                          sstate==20 ~ "Zamfara")) %>%
  filter(!is.na(Region)) %>%
  mutate(sampleweights=v005/100000,
         Region_YearBirth=paste(Region, year_birth, sep="_"),
         Region_Year1Month=paste(Region, year_1month, sep="_"),
         Region_Year2Month=paste(Region, year_2month, sep="_"),
         Region_Year3Month=paste(Region, year_3month, sep="_"),
         Region_Year4Month=paste(Region, year_4month, sep="_"),
         Region_Year6Month=paste(Region, year_6month, sep="_"),
         Region_Year9Month=paste(Region, year_9month, sep="_"),
         Region_Year11Month=paste(Region, year_11month, sep="_"),
         Region_Year12Month=paste(Region, year_12month, sep="_"),
         Region_Year15Month=paste(Region, year_15month, sep="_")) 


bcg_dhs <-    as.data.frame(prop.table(wtd.table( dhs_kr_clean$Region_YearBirth, dhs_kr_clean$bcg, weights=dhs_kr_clean$sampleweights ),1)) %>% 
  filter(Var2==1) %>% select(-Var2) %>% rename(BCG=Freq) %>% 
  separate(Var1, c("Region", "YearBirth"), sep="_") %>% filter(YearBirth==2016 | YearBirth==2017) %>% rename(Year=YearBirth)

polio0_dhs <-    as.data.frame(prop.table(wtd.table( dhs_kr_clean$Region_YearBirth, dhs_kr_clean$polio0, weights=dhs_kr_clean$sampleweights ),1)) %>% 
  filter(Var2==1) %>% select(-Var2) %>% rename(polio0=Freq) %>% 
  separate(Var1, c("Region", "YearBirth"), sep="_") %>% filter(YearBirth==2016 | YearBirth==2017) %>% rename(Year=YearBirth)

polio_sum1_dhs <-    as.data.frame(prop.table(wtd.table( dhs_kr_clean$Region_Year1Month, dhs_kr_clean$polio_sum1, weights=dhs_kr_clean$sampleweights ),1)) %>% 
  filter(Var2==1) %>% select(-Var2) %>% rename(polio1=Freq) %>% 
  separate(Var1, c("Region", "Year1Month"), sep="_") %>% filter(Year1Month==2016 | Year1Month==2017) %>% rename(Year=Year1Month)

polio_sum2_dhs <-    as.data.frame(prop.table(wtd.table( dhs_kr_clean$Region_Year2Month, dhs_kr_clean$polio_sum2, weights=dhs_kr_clean$sampleweights ),1)) %>% 
  filter(Var2==1) %>% select(-Var2) %>% rename(polio2=Freq) %>% 
  separate(Var1, c("Region", "Year2Month"), sep="_") %>% filter(Year2Month==2016 | Year2Month==2017) %>% rename(Year=Year2Month)

polio_sum3_dhs <-    as.data.frame(prop.table(wtd.table( dhs_kr_clean$Region_Year2Month, dhs_kr_clean$polio_sum3, weights=dhs_kr_clean$sampleweights ),1)) %>% 
  filter(Var2==1) %>% select(-Var2) %>% rename(polio3=Freq) %>% 
  separate(Var1, c("Region", "Year2Month"), sep="_") %>% filter(Year2Month==2016 | Year2Month==2017) %>% rename(Year=Year2Month)

measles1_dhs <-    as.data.frame(prop.table(wtd.table( dhs_kr_clean$Region_Year9Month, dhs_kr_clean$measles_sum1, weights=dhs_kr_clean$sampleweights ),1)) %>% 
  filter(Var2==1) %>% select(-Var2) %>% rename(measles1=Freq) %>% 
  separate(Var1, c("Region", "Region_Year9Month"), sep="_") %>% filter(Region_Year9Month==2017 | Region_Year9Month==2018) %>% rename(Year=Region_Year9Month)

measles2_dhs <-    as.data.frame(prop.table(wtd.table( dhs_kr_clean$Region_Year15Month, dhs_kr_clean$measles_sum2, weights=dhs_kr_clean$sampleweights ),1)) %>% 
  filter(Var2==1) %>% select(-Var2) %>% rename(measles2=Freq) %>% 
  separate(Var1, c("Region", "Region_Year9Month"), sep="_") %>% filter(Region_Year9Month==2017 | Region_Year9Month==2018) %>% rename(Year=Region_Year9Month)

pneumo_sum1_dhs <-    as.data.frame(prop.table(wtd.table( dhs_kr_clean$Region_Year2Month, dhs_kr_clean$pneumo_sum1, weights=dhs_kr_clean$sampleweights ),1)) %>% 
  filter(Var2==1) %>% select(-Var2) %>% rename(pneumo1=Freq) %>% 
  separate(Var1, c("Region", "Year2Month"), sep="_") %>% filter(Year2Month==2016 | Year2Month==2017) %>% rename(Year=Year2Month)

pneumo_sum2_dhs <-    as.data.frame(prop.table(wtd.table( dhs_kr_clean$Region_Year4Month, dhs_kr_clean$pneumo_sum2, weights=dhs_kr_clean$sampleweights ),1)) %>% 
  filter(Var2==1) %>% select(-Var2) %>% rename(pneumo2=Freq) %>% 
  separate(Var1, c("Region", "Year4Month"), sep="_") %>% filter(Year4Month==2016 | Year4Month==2017) %>% rename(Year=Year4Month)

pneumo_sum3_dhs <-    as.data.frame(prop.table(wtd.table( dhs_kr_clean$Region_Year6Month, dhs_kr_clean$pneumo_sum3, weights=dhs_kr_clean$sampleweights ),1)) %>% 
  filter(Var2==1) %>% select(-Var2) %>% rename(pneumo3=Freq) %>% 
  separate(Var1, c("Region", "Year6Month"), sep="_") %>% filter(Year6Month==2016 | Year6Month==2017) %>% rename(Year=Year6Month)

hepb0_dhs <-    as.data.frame(prop.table(wtd.table( dhs_kr_clean$Region_Year9Month, dhs_kr_clean$hepb0, weights=dhs_kr_clean$sampleweights ),1)) %>% 
  filter(Var2==1) %>% select(-Var2) %>% rename(hepb0=Freq) %>% 
  separate(Var1, c("Region", "Year9Month"), sep="_") %>% filter(Year9Month==2017 | Year9Month==2018) %>% rename(Year=Year9Month)


penta_sum1_dhs <-    as.data.frame(prop.table(wtd.table( dhs_kr_clean$Region_Year1Month, dhs_kr_clean$penta_sum1, weights=dhs_kr_clean$sampleweights ),1)) %>% 
  filter(Var2==1) %>% select(-Var2) %>% rename(penta1=Freq) %>% 
  separate(Var1, c("Region", "Year1Month"), sep="_") %>% filter(Year1Month==2016 | Year1Month==2017) %>% rename(Year=Year1Month)

penta_sum2_dhs <-    as.data.frame(prop.table(wtd.table( dhs_kr_clean$Region_Year2Month, dhs_kr_clean$penta_sum2, weights=dhs_kr_clean$sampleweights ),1)) %>% 
  filter(Var2==1) %>% select(-Var2) %>% rename(penta2=Freq) %>% 
  separate(Var1, c("Region", "Year2Month"), sep="_") %>% filter(Year2Month==2016 | Year2Month==2017) %>% rename(Year=Year2Month)

penta_sum3_dhs <-    as.data.frame(prop.table(wtd.table( dhs_kr_clean$Region_Year3Month, dhs_kr_clean$penta_sum3, weights=dhs_kr_clean$sampleweights ),1)) %>% 
  filter(Var2==1) %>% select(-Var2) %>% rename(penta3=Freq) %>% 
  separate(Var1, c("Region", "Year3Month"), sep="_") %>% filter(Year3Month==2016 | Year3Month==2017) %>% rename(Year=Year3Month)

vacfull_dhs <-    as.data.frame(prop.table(wtd.table( dhs_kr_clean$Region_YearBirth, dhs_kr_clean$full_vac, weights=dhs_kr_clean$sampleweights ),1)) %>% 
  filter(Var2==1) %>% select(-Var2) %>% rename(full_vac=Freq) %>% 
  separate(Var1, c("Region", "YearBirth"), sep="_") %>% filter(YearBirth==2016 | YearBirth==2017) %>% rename(Year=YearBirth)


vac_dhs <- full_join(bcg_dhs, polio0_dhs, by=c("Region", "Year")) %>% 
  full_join(polio_sum1_dhs , by=c("Region", "Year")) %>% 
  full_join(polio_sum2_dhs , by=c("Region", "Year")) %>% 
  full_join( polio_sum3_dhs , by=c("Region", "Year")) %>% 
  full_join( measles1_dhs , by=c("Region", "Year")) %>% 
  full_join( measles2_dhs , by=c("Region", "Year")) %>% 
  full_join( pneumo_sum1_dhs , by=c("Region", "Year")) %>% 
  full_join( pneumo_sum2_dhs , by=c("Region", "Year")) %>% 
  full_join( pneumo_sum3_dhs , by=c("Region", "Year")) %>% 
  full_join( hepb0_dhs , by=c("Region", "Year")) %>% 
  full_join( penta_sum1_dhs , by=c("Region", "Year")) %>% 
  full_join( penta_sum2_dhs , by=c("Region", "Year")) %>% 
  full_join( penta_sum3_dhs , by=c("Region", "Year")) %>% 
  full_join( vacfull_dhs , by=c("Region", "Year")) %>% 
  arrange(Region, Year)


# no penta in 13
# No measles2
# no pneumo
# no hepB0

dhs_kr_clean13 <- dhs_kr13 %>% mutate(age = v008- b3) %>% filter(age>=12 & age<=35)  %>% filter(b5==1) %>% 
  mutate( year_birth = b2 ,
          year_1month = floor(b2 + ((b1 + 1)/12)),
          year_2month = floor(b2 + ((b1 + 2)/12)),
          year_3month = floor(b2 + ((b1 + 3)/12)),
          year_4month = floor(b2 + ((b1 + 4)/12)),
          year_6month = floor(b2 + ((b1 + 6)/12)),
          year_9month = floor(b2 + ((b1 + 9)/12)),
          year_11month = floor(b2 + ((b1 + 11)/12)),
          year_12month = floor(b2 + ((b1 + 12)/12)),
          year_15month = floor(b2 + ((b1 + 15)/12))) %>%
  mutate(bcg=case_when(h2>=1 & h2<=3 ~ 1, h2==0 | h2==8 ~ 0),
         polio0 = case_when(h0>=1 & h0<=3 ~ 1, h0==0 | h0==8 ~ 0),
         polio1 = case_when(h4>=1 & h4<=3 ~ 1, h4==0 | h4==8 ~ 0),
         polio2 = case_when(h6>=1 & h6<=3 ~ 1, h6==0 | h6==8 ~ 0),
         polio3 = case_when(h8>=1 & h8<=3 ~ 1, h8==0 | h8==8 ~ 0),
         polio_sum= polio1 + polio2 + polio3,
         polio_sum1 = case_when(polio_sum>=1 ~ 1 , polio_sum<1 ~ 0),
         polio_sum2 = case_when(polio_sum>=2 ~ 1 , polio_sum<2 ~ 0),
         polio_sum3 = case_when(polio_sum>=3 ~ 1 , polio_sum<3 ~ 0),
         measles1=case_when(h9>=1 & h9<=3 ~ 1, h9==0 | h9==8 ~ 0),
         dpt1= case_when(h3>=1 & h3<=3 ~ 1, h3==0 | h3==8 ~ 0), 
         dpt2= case_when(h5>=1 & h5<=3 ~ 1, h5==0 | h5==8 ~ 0), 
         dpt3= case_when(h7>=1 & h7<=3 ~ 1, h7==0 | h7==8 ~ 0),
         dpt_sum= dpt1 + dpt2 + dpt3,
         dpt_sum1 = case_when(dpt_sum>=1 ~ 1 , dpt_sum<1 ~ 0),
         dpt_sum2 = case_when(dpt_sum>=2 ~ 1 , dpt_sum<2 ~ 0),
         dpt_sum3 = case_when(dpt_sum>=3 ~ 1 , dpt_sum<3 ~ 0))  %>%
  mutate(full_vac=case_when(bcg==1 & measles1==1 & dpt_sum1==1 & dpt_sum2==1 & dpt_sum3==1 & polio_sum1==1 & polio_sum2==1 & polio_sum3==1 ~ 1,
                            bcg!=1 | measles1!=1 | dpt_sum1!=1 | dpt_sum2!=1 | dpt_sum3!=1 | polio_sum1!=1 | polio_sum2!=1 | polio_sum3!=1 ~ 0)) %>%
  mutate(Region=case_when(sstate==230 ~ "Ekiti",
                          sstate==40 ~ "Jigawa",
                          sstate==110 ~ "Kaduna",
                          sstate==100 ~ "Kano",
                          sstate==30 ~ "Katsina",
                          sstate==120 ~ "Kebbi",
                          sstate==360 ~ "Lagos",
                          sstate==370 ~ "Ogun",
                          sstate==240 ~ "Ondo",
                          sstate==220 ~ "Osun",
                          sstate==210 ~ "Oyo",
                          sstate==10 ~ "Sokoto",
                          sstate==20 ~ "Zamfara")) %>%
  filter(!is.na(Region)) %>%
  mutate(sampleweights=v005/100000,
         Region_YearBirth=paste(Region, year_birth, sep="_"),
         Region_Year1Month=paste(Region, year_1month, sep="_"),
         Region_Year2Month=paste(Region, year_2month, sep="_"),
         Region_Year3Month=paste(Region, year_3month, sep="_"),
         Region_Year4Month=paste(Region, year_4month, sep="_"),
         Region_Year6Month=paste(Region, year_6month, sep="_"),
         Region_Year9Month=paste(Region, year_9month, sep="_"),
         Region_Year11Month=paste(Region, year_11month, sep="_"),
         Region_Year12Month=paste(Region, year_12month, sep="_"),
         Region_Year15Month=paste(Region, year_15month, sep="_")) 



bcg_dhs13 <-    as.data.frame(prop.table(wtd.table( dhs_kr_clean13$Region_YearBirth, dhs_kr_clean13$bcg, weights=dhs_kr_clean13$sampleweights ),1)) %>% 
  filter(Var2==1) %>% select(-Var2) %>% rename(BCG=Freq) %>% 
  separate(Var1, c("Region", "YearBirth"), sep="_") %>% filter(YearBirth==2011 | YearBirth==2012) %>% rename(Year=YearBirth)


polio0_dhs13 <-    as.data.frame(prop.table(wtd.table( dhs_kr_clean13$Region_YearBirth, dhs_kr_clean13$polio0, weights=dhs_kr_clean13$sampleweights ),1)) %>% 
  filter(Var2==1) %>% select(-Var2) %>% rename(polio0=Freq) %>% 
  separate(Var1, c("Region", "YearBirth"), sep="_") %>% filter(YearBirth==2011 | YearBirth==2012) %>% rename(Year=YearBirth)

polio_sum1_dhs13 <-    as.data.frame(prop.table(wtd.table( dhs_kr_clean13$Region_Year1Month, dhs_kr_clean13$polio_sum1, weights=dhs_kr_clean13$sampleweights ),1)) %>% 
  filter(Var2==1) %>% select(-Var2) %>% rename(polio1=Freq) %>% 
  separate(Var1, c("Region", "Year1Month"), sep="_") %>% filter(Year1Month==2011 | Year1Month==2012) %>% rename(Year=Year1Month)

polio_sum2_dhs13 <-    as.data.frame(prop.table(wtd.table( dhs_kr_clean13$Region_Year2Month, dhs_kr_clean13$polio_sum2, weights=dhs_kr_clean13$sampleweights ),1)) %>% 
  filter(Var2==1) %>% select(-Var2) %>% rename(polio2=Freq) %>% 
  separate(Var1, c("Region", "Year2Month"), sep="_") %>% filter(Year2Month==2011 | Year2Month==2012) %>% rename(Year=Year2Month)

polio_sum3_dhs13 <-    as.data.frame(prop.table(wtd.table( dhs_kr_clean13$Region_Year2Month, dhs_kr_clean13$polio_sum3, weights=dhs_kr_clean13$sampleweights ),1)) %>% 
  filter(Var2==1) %>% select(-Var2) %>% rename(polio3=Freq) %>% 
  separate(Var1, c("Region", "Year2Month"), sep="_") %>% filter(Year2Month==2011 | Year2Month==2012) %>% rename(Year=Year2Month)

measles1_dhs13 <-    as.data.frame(prop.table(wtd.table( dhs_kr_clean13$Region_Year9Month, dhs_kr_clean13$measles1, weights=dhs_kr_clean13$sampleweights ),1)) %>% 
  filter(Var2==1) %>% select(-Var2) %>% rename(measles1=Freq) %>% 
  separate(Var1, c("Region", "Region_Year9Month"), sep="_") %>% filter(Region_Year9Month==2012 | Region_Year9Month==2013) %>% rename(Year=Region_Year9Month)

dpt_sum1_dhs13 <-    as.data.frame(prop.table(wtd.table( dhs_kr_clean13$Region_Year1Month, dhs_kr_clean13$dpt_sum1, weights=dhs_kr_clean13$sampleweights ),1)) %>% 
  filter(Var2==1) %>% select(-Var2) %>% rename(dpt1=Freq) %>% 
  separate(Var1, c("Region", "Year1Month"), sep="_") %>% filter(Year1Month==2011 | Year1Month==2012) %>% rename(Year=Year1Month)

dpt_sum2_dhs13 <-    as.data.frame(prop.table(wtd.table( dhs_kr_clean13$Region_Year2Month, dhs_kr_clean13$dpt_sum2, weights=dhs_kr_clean13$sampleweights ),1)) %>% 
  filter(Var2==1) %>% select(-Var2) %>% rename(dpt2=Freq) %>% 
  separate(Var1, c("Region", "Year2Month"), sep="_") %>% filter(Year2Month==2011 | Year2Month==2012) %>% rename(Year=Year2Month)

dpt_sum3_dhs13 <-    as.data.frame(prop.table(wtd.table( dhs_kr_clean13$Region_Year3Month, dhs_kr_clean13$dpt_sum3, weights=dhs_kr_clean13$sampleweights ),1)) %>% 
  filter(Var2==1) %>% select(-Var2) %>% rename(dpt3=Freq) %>% 
  separate(Var1, c("Region", "Year3Month"), sep="_") %>% filter(Year3Month==2011 | Year3Month==2012) %>% rename(Year=Year3Month)

vacfull_dhs13 <-    as.data.frame(prop.table(wtd.table( dhs_kr_clean13$Region_YearBirth, dhs_kr_clean13$full_vac, weights=dhs_kr_clean13$sampleweights ),1)) %>% 
  filter(Var2==1) %>% select(-Var2) %>% rename(full_vac=Freq) %>% 
  separate(Var1, c("Region", "YearBirth"), sep="_") %>% filter(YearBirth==2011 | YearBirth==2012) %>% rename(Year=YearBirth)



vac_dhs13 <- full_join(bcg_dhs13, polio0_dhs13, by=c("Region", "Year")) %>% 
  full_join(polio_sum1_dhs13 , by=c("Region", "Year")) %>% 
  full_join(polio_sum2_dhs13 , by=c("Region", "Year")) %>% 
  full_join( polio_sum3_dhs13 , by=c("Region", "Year")) %>% 
  full_join( measles1_dhs13 , by=c("Region", "Year")) %>% 
  full_join( dpt_sum1_dhs13 , by=c("Region", "Year")) %>% 
  full_join( dpt_sum2_dhs13 , by=c("Region", "Year")) %>% 
  full_join( dpt_sum3_dhs13 , by=c("Region", "Year")) %>% 
  full_join( vacfull_dhs13 , by=c("Region", "Year")) %>% 
  arrange(Region, Year)


vac_dhs_full <- bind_rows(vac_dhs, vac_dhs13) %>% arrange(Region, Year) %>%
  rename(BCG_DHS=BCG,
         Polio0_DHS=polio0,
         Polio1_DHS=polio1,
         Polio2_DHS=polio2,
         Polio3_DHS=polio3,
         Measles1_DHS=measles1,
         Measles2_DHS=measles2,
         Pneumo1_DHS=pneumo1,
         Pneumo2_DHS=pneumo2,
         Pneumo3_DHS=pneumo3,
         HepB0_DHS=hepb0,
         Penta1_DHS=penta1,
         Penta2_DHS=penta2,
         Penta3_DHS=penta3,
         DPT1_DHS=dpt1,
         DPT2_DHS=dpt2,
         DPT3_DHS=dpt3,
         FullVac_DHS=full_vac) %>% mutate(Year= as.numeric(as.character(Year)))