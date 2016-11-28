#Clear the R environment before starting
rm(list=ls(all=TRUE))

#bring all libraries needed
library(foreign)
library(dplyr)
library(stringr)

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

child_dat2=child_dat %>%
  mutate_each(funs(tolower), AM7) %>%
  inner_join(lab_mal_dat) %>%     #adding malaria lab work
  mutate(Age_m=12*(UF8Y-AG1Y)+(UF8M-AG1M)) %>%  #calculates the age in months per child
  mutate(Micro=FINALa) %>% 
  mutate(Micro=replace(Micro,FINALa==2,0)) %>% 
  select(HH1,UF1,UF4,UF8D,UF8M,UF8Y,UF9,Age_m,ML1,ML2,HI1,AN3,AN4A,AN4,AM7,Micro,AM8,AM9,AM10,AM11) %>% 
  inner_join(gps_dat) %>% 
  filter(!is.na(Long))
  
