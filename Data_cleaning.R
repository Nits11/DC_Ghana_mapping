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
  mutate(Sev_an=0) %>% 
  mutate(Sev_an=replace(Sev_an,AM8 < 7,1)) %>% 
  mutate(Age_m=replace(Age_m,Age_m < 0,12*AG2)) %>   #for age_months missing, estimate from their years of age
  select(HH1,HH2,Day=UF8D,Month=UF8M,Year=UF8Y,Age_year=AG2,Age_months=Age_m,fever=ML1,Health_insurance=HI1,Weight=AN3,Height=AN4,AM7,Micro,Sev_an,Hb=AM8,RDT=AM10) %>% 
  inner_join(gps_dat) %>%  
  filter(!is.na(Long))
  
#there are some ages that didn't have month included. So we will take their age (in years) time 12 (for months)
if(child_dat$Age_m < 0){12*child_dat$AG2}
hist(child_dat$Age_m)
# One child needs to be manually changed to 6 months (manual mistake)
child_dat$Age_months[which(child_dat$Age_months==0)]=6

#cleaning the household data
hh_dat=hh_dat %>% 
  select(HH1, HH2, Residence=HH6, Region=HH7, District=HH7A, Sex=HL4, IRS=IR1, hhweight, ethnicity, 
         H_education=helevel, Wealth_q=windex5)

#join household data to child data
child_dat=child_dat %>% 
  inner_join(hh_dat,by = c("HH1" = "HH1", "HH2" = "HH2")) %>% 
  filter(!is.na(Micro))

#Add an RDT positive column
child_dat$RDT_pos=NA
child_dat$RDT_pos[child_dat$RDT=="Positive,falciparum only (PF)"|child_dat$RDT=="Positive, Other species (O,M,V)"|
                    child_dat$RDT=="Positive, both falciparum and OMV"]=1
child_dat$RDT_pos[child_dat$RDT=="Negative"|child_dat$RDT=="Other"]=0
table(child_dat$RDT_pos, useNA = "ifany")


######exploratory analysis#################
######the aim of this to visualise the malaria data to see what it looks like
######we will look at microscopy as the gold standard and check to see any interesting trends against
######a) demographic variable (Age, sex, wealth, head of household education, insurance, ethnicity)
######b) biomarker (Hemoglobin, RDT checks)

#Age in months and microscopy prevalence
ggplot(child_dat, aes(x=Age_months)) +
  stat_smooth(aes(y=RDT_pos, colour="RDT"),method = "loess",size=1.5)+
  stat_smooth(aes(y=Micro, colour="Microscopy"),method = "loess", size=1.5)+
  scale_colour_manual("", 
                      breaks = c("RDT", "Microscopy"),
                      values = c("red", "blue")) +
  xlab("Age in months")+
  ylab("Prevalence")+
  theme_bw()

#######################################################################################
########################          DHS 2014          ##################################
#######################################################################################
####OPENING DHS DATA#####
####NOTE: make sure you have the package foreign installed to open STATA files
####It is also important to read the recode manuals provided by DHS to call appropriate data when neccesary

#############################################################
DHS <- read.dta("Data/Original/MICS/DHS/ghpr71dt/GHPR71FL.DTA") #Note this takes a while
#check data
names(DHS)

#malaria (microscopy) column is SHMALA
table(DHS$shmala)
#malaria (RDT) column
table(DHS$hml35)
#############################################################
#recode the data
DHS$micro <- ifelse(DHS$shmala=="negative",0,ifelse(DHS$shmala=="positive",1,NA))
DHS$RDT <- ifelse(DHS$hml35=="negative",0,ifelse(DHS$hml35=="positive",1,NA))
table(DHS$micro)
table(DHS$RDT)  #check these look the same just binary now as original data

#aggregate to cluster level results
mal=aggregate(cbind(micro,RDT)~hv001,data=DHS,mean)
