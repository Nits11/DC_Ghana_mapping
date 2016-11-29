#Clear the R environment before starting
rm(list=ls(all=TRUE))

#bring all libraries needed
library(foreign)
library(dplyr)
library(stringr)
library(ggplot2)
#######################################################################################
########################          MICS 2011          ##################################
#######################################################################################
#unzip the data (loop allows to reduce unzipping for computation time)
if (!file.exists('Data/Original/MICS/mics4 datasets')){
  unzip("Data/Original/MICS/mics4 datasets.zip")
}

#import data frames (Note: warning message for our dataset is ok, alternative code loses data)
child_dat=read.spss("Data/Original/MICS/mics4 datasets/ch.sav", to.data.frame=TRUE)
hh_dat=read.spss("Data/Original/MICS/mics4 datasets/hh.sav", to.data.frame=TRUE)
gps_dat=read.csv("Data/Original/MICS/MICS4EcoZone_PA.csv")
lab_mal_dat=read.spss("Data/Original/MICS/mics4 datasets/malariaf-13032012merge.sav", to.data.frame=TRUE)


#cleaning the child data with malaria lab
#addded GPS data for each household (Note: not all GPS is available)

child_dat=child_dat %>%
  mutate_each(funs(tolower), AM7) %>%
  mutate(Age_m=12*(UF8Y-AG1Y)+(UF8M-AG1M)) %>%  #calculates the age in months per child
  inner_join(lab_mal_dat) %>%     #adding malaria lab work
  mutate(Micro=FINALa) %>% 
  mutate(Micro=replace(Micro,FINALa==2,0)) %>%   #Convert negative to 0 (binary form)
  mutate(Age_m=replace(Age_m,Age_m < 0,12*AG2)) %>%   #for age_months missing, estimate from their years of age
  select(HH1,HH2, UF1,UF4,UF8D,UF8M,UF8Y,AG2,Age_m,ML1,ML2,HI1,AN3,AN4,AM7,Micro,AM8,AM10) %>% 
  inner_join(gps_dat) %>% 
  filter(!is.na(Long))
  
#there are some ages that didn't have month included. So we will take their age (in years) time 12 (for months)
if(child_dat$Age_m < 0){12*child_dat$AG2}
hist(child_dat$Age_m)

#cleaning the household data
hh_dat=hh_dat %>% 
  select(HH1, HH2, HH6, HH7, HH7A, IR1, hhweight, ethnicity, helevel, windex5)

#join household data to child data
child_dat=child_dat %>% 
  inner_join(hh_dat,by = c("HH1" = "HH1", "HH2" = "HH2"))
