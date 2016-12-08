#######################################################################################
########################    DATA CLEANING FILE      ###################################
#######################################################################################
#Clear the R environment before starting
rm(list=ls(all=TRUE))

#install appropriate packages
list.of.packages <- c("foreign", "dplyr", "stringr", "ggplot2", "ggmap")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#bring all libraries needed
library(foreign)
library(dplyr)
library(stringr)
library(ggplot2)
library(ggmap)
 
#######################################################################################
########################          MICS 2011          ##################################
#######################################################################################

########################################
######DATA CLEANING AND ORGANISING######
########################################
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
  inner_join(hh_dat,by = c("HH1" = "HH1", "HH2" = "HH2")) %>% 
  filter(!is.na(Micro)) %>%
  inner_join(gps_dat) %>%  
  filter(!is.na(Long)) %>% 
  #select is acting like a drama-queen, forced it using dplyr::
  dplyr::select(HH1,HH2,Day=UF8D,Month=UF8M,Year=UF8Y,Age_year=AG2,Age_months=Age_m,fever=ML1,Health_insurance=HI1,
         Weight=AN3,Height=AN4, Micro,Sev_an,Hb=AM8,RDT=AM10, Residence=HH6.y, Region=HH7.y, District=HH7A, 
         IRS=IR1, hhweight, ethnicity=ethnicity.y, H_education=helevel, Wealth_q=windex5.y, Long=Long,Lat=Lat) 

#From now you don't need the other databases; in order to not get confused just remove them
rm("hh_dat")
rm("lab_mal_dat")
rm("gps_dat")

#there are some ages that didn't have month included. So we will take their age (in years) time 12 (for months)
child_dat=child_dat %>%
mutate(Age_months=replace(Age_months,Age_months < 0 & !is.na(Age_months),12*Age_year))   #for age_months missing, estimate from their years of age
hist(child_dat$Age_months)

# One child needs to be manually changed to 6 months (manual mistake)
child_dat$Age_months[which(child_dat$Age_months==0)]=6
hist(child_dat$Age_months)

#Also Hemoglobin values >25 are most likely not results convert to NA
#ditto fever=DK should be converted to NA
child_dat$Hb[which(child_dat$Hb>25)]=NA
child_dat$fever=as.character(child_dat$fever)
child_dat$fever[which(child_dat$fever=="DK"|child_dat$fever=="Missing")]=NA
table(child_dat$fever)
table(child_dat$Hb)

#Add an RDT positive column
child_dat$RDT=as.numeric(child_dat$RDT)
child_dat$RDT[which(child_dat$RDT==4|child_dat$RDT==5)]=0
child_dat$RDT[which(child_dat$RDT==1|child_dat$RDT==2|child_dat$RDT==3)]=1
table(child_dat$RDT)

########################################
######    EXPLORATORY ANALYSIS    ######
########################################
######the aim of this to visualise the malaria data to see what it looks like
######we will look at microscopy as the gold standard and check to see any interesting trends against
######RDT will be use ocassionaly to explore the difference in prevalence reported by RDT vs. Microscopy 
######which is usually higher given it is a period prevalence.

#Age in months and microscopy prevalence and RDT
ggplot(child_dat, aes(x=Age_months)) +
  stat_smooth(aes(y=RDT, colour="RDT"),method = "loess",size=1.5)+
  stat_smooth(aes(y=Micro, colour="Microscopy"),method = "loess", size=1.5)+
  xlab("Age in months")+
  ylab("Probabilty of Infection")+
  ggtitle("MICS 2011, probability of infection by Age of child (in months)")+
  theme(panel.background= element_blank(), panel.grid.minor = element_line(colour = "lightgrey"),panel.border = element_blank(),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))+
  labs(colour = "")

#wealth by residence and prevalence
w=child_dat %>% 
  select(Wealth_q, Micro, Residence) %>% 
  mutate(Malaria="Yes") %>% 
  mutate(Malaria=replace(Malaria,Micro==0,"No"))
w$Malaria=as.factor(w$Malaria)


ggplot(w, aes(x=Wealth_q))+
  geom_bar(aes(fill=Malaria))+
  facet_wrap(~ Residence)+
  ggtitle("MICS 2011, children by residence and wealth")+
  #Format axes
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5), labels = c("Poorest", "Poorer", "Middle", "Richer", "Richest")) + 
  scale_y_continuous(name = "Number of Children")+
  scale_fill_manual(name = "Malaria infection", values = c("#fc8d59", "#91bfdb")) +
  theme_minimal(base_size = 12, base_family = "Arial")+
  theme(axis.text.x = element_text(size = 8, angle = 45), 
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        axis.title.x = element_blank()) +
  # Legend formatting
  theme(legend.text = element_text(size = 8),
        legend.title = element_text(size = 10),
        legend.position = "top", 
        legend.direction = "horizontal") +
  
  #Facets formatting
  theme(strip.text.x = element_text(size = 10),
        strip.text.y = element_text(size = 10),
        panel.margin = unit(1, "lines"))


#wealth and education
ggplot(child_dat, aes(x=Wealth_q))+
  geom_bar(aes(fill=H_education))+
  ggtitle("MICS 2011, Education of Head of Household and wealth")+
  #Format axes
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5), labels = c("Poorest", "Poorer", "Middle", "Richer", "Richest")) + 
  scale_y_continuous(name = "Number of Children")+
  scale_fill_manual(name = "Highest Education", values = c("darksalmon", "mediumorchid2", "darkturquoise", "chartreuse4")) +
  theme_minimal(base_size = 12, base_family = "Arial")+
  theme(axis.text.x = element_text(size = 8, angle = 45), 
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        axis.title.x = element_blank()) +
  # Legend formatting
  theme(legend.text = element_text(size = 8),
        legend.title = element_text(size = 10),
        legend.position = "top", 
        legend.direction = "horizontal") +
  
  #Facets formatting
  theme(strip.text.x = element_text(size = 10),
        strip.text.y = element_text(size = 10),
        panel.margin = unit(1, "lines"))

#malaria and hemoglobin
ggplot(child_dat, aes(x=as.factor(Micro),y=Hb))+
  geom_boxplot()+
  theme(panel.background= element_blank(), panel.grid.minor = element_line(colour = "lightgrey"),panel.border = element_blank(),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))+
  scale_x_discrete(name="Malaria infection",breaks = c(0,1), labels = c("No", "Yes")) +
  scale_y_continuous(name = "Hemoglobin level (g/dL)")+
  ggtitle("DHS 2014, Hemoglobin Leevels by malaria infection status")


########################################
###### GIS EXPLORATORY ANALYSIS   ######
########################################
MICS_mal=child_dat %>% 
  group_by(HH1) %>% 
  summarise(Malaria=mean(Micro)*100, RDT=mean(RDT)*100, Long=mean(Long), Lat=mean(Lat))
write.csv(MICS_mal,"Data/GIS/MICS_mal.csv", row.names = F)



#Visualise Map of points by prevalence

world_map <- map_data("world")
Ghana <- subset(world_map, world_map$region=="Ghana")

ggplot() + coord_fixed() +
  xlab("") + ylab("")+ 
  #Make base plot
  geom_polygon(data=Ghana, aes(x=long, y=lat, group=group), 
                                     colour="black", fill="white")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
       panel.background = element_rect(fill = 'white', colour = 'white'), 
       axis.line = element_line(colour = "white"), legend.position="left",
       axis.ticks=element_blank(), axis.text.x=element_blank(),
       axis.text.y=element_blank())+
#adding the prevalence points
  geom_point(data=MICS_mal, 
             aes(x=Long, y=Lat, colour=Malaria, size=Malaria), alpha=0.4)+ 
  scale_colour_gradient(name = "Malaria Prevalence (%)",high = "red", low = "Yellow")+
    scale_size_continuous(name = "Malaria Prevalence (%)", range = c(1,5))+
  guides(color=guide_legend(), size = guide_legend())+
  ggtitle("MICS 2011, Malaria Prevalence Point map")





#######################################################################################
########################          DHS 2014          ##################################
#######################################################################################
####NOTE: important to read the recode manuals provided by DHS to call appropriate data when neccesary

########################################
######DATA CLEANING AND ORGANISING######
########################################
DHS_hh <- read.dta("Data/Original/DHS/ghpr71dt/GHPR71FL.DTA") #Note this takes a while
DHS_ch <- read.dta("Data/Original/DHS/ghkr71dt/GHkR71FL.DTA") #Note this takes a while

#Need to create a key column ID to merge the two together
DHS_ch$ID <- with(DHS_ch, paste0(v001, v002, b16)) #, v003, caseid
DHS_hh$ID <- with(DHS_hh, paste0(hv001, hv002, hvidx))#, hvidx, hhid
#Add GPS coordinate data to it (DHS is already in WGS84 coordinate system with latlong)
DHS_gps=read.csv("Data/Original/DHS/DHS_gps.csv")

DHS_gps=DHS_gps %>% 
  dplyr::select(HH1=DHSCLUST, Long=LONGNUM, Lat=LATNUM)

DHS <- DHS_ch %>% 
  inner_join(DHS_hh, by="ID") %>%
  mutate(Sev_an=0) %>% 
  mutate(Sev_an=replace(Sev_an,hw53<70,1)) %>%
  mutate(Age_year=v007-v010) %>% 
  dplyr::select(HH1=v001, HH2=v002, Day=v016, Month=v006, Year=v007, Age_year,Age_months=hw1, fever=h22,
         Health_Insurance=v481, Weight=hw2, Height=hw3, Micro=shmala, Sev_an, Hb=hw53, RDT=hml35,
         Residence=v025, Region=v024, District=sdist, IRS=hv235, hhweight=v005, ethnicity=v131, 
         H_education=v106, Wealth_q=v190) %>% 
  filter(!is.na(Micro)) 

#NOTES: approx 788 children are dropped suscpiously during this cleaning. However,
#codes used for unique ID are provided by DHS and are standard. I have emailed to resolve 
#specific country issue.

#adding GPS data
DHS=merge(DHS, DHS_gps, by="HH1")

#check data
names(DHS)

#recode the data
DHS$Micro <- ifelse(DHS$Micro=="negative",0,ifelse(DHS$Micro=="positive",1,NA))
DHS$RDT <- ifelse(DHS$RDT=="negative",0,ifelse(DHS$RDT=="positive",1,NA))
table(DHS$Micro, useNA="ifany")
table(DHS$RDT, useNA="ifany")  #check these look the same just binary now as original data
DHS=filter(DHS,!is.na(Micro))

#clean Hb levels
DHS$Hb[which(DHS$Hb>250)]=NA
DHS$Hb=DHS$Hb/10
hist(DHS$Hb)

########################################
######    EXPLORATORY ANALYSIS    ######
########################################
######the aim of this to visualise the malaria data to see what it looks like
######we will look at microscopy as the gold standard and check to see any interesting trends against
######RDT will be use ocassionaly to explore the difference in prevalence reported by RDT vs. Microscopy 
######which is usually higher given it is a period prevalence.

#Age in months and microscopy prevalence and RDT
ggplot(DHS, aes(x=Age_months)) +
  stat_smooth(aes(y=RDT, colour="RDT"),method = "loess",size=1.5)+
  stat_smooth(aes(y=Micro, colour="Microscopy"),method = "loess", size=1.5)+
  xlab("Age in months")+
  ylab("Probabilty of Infection")+
  ggtitle("DHS 2014, probability of infection by Age of child (in months)")+
  theme(panel.background= element_blank(), panel.grid.minor = element_line(colour = "lightgrey"),panel.border = element_blank(),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))+
  labs(colour = "")

#wealth by residence and prevalence
w=DHS %>% 
  dplyr::select(Wealth_q, Micro, Residence) %>% 
  mutate(Malaria="Yes") %>% 
  mutate(Malaria=replace(Malaria,Micro==0,"No"))
w$Malaria=as.factor(w$Malaria)
w$Wealth_q=as.factor(w$Wealth_q)
w$Residence=as.factor(w$Residence)
str(w)

ggplot(w, aes(x=Wealth_q))+
  geom_bar(aes(fill=Malaria))+
  facet_wrap(~ Residence)+
  ggtitle("DHS 2014, children by residence and wealth")+
  #Format axes
  scale_y_continuous(name = "Number of Children")+
  scale_fill_manual(name = "Malaria infection", values = c("#fc8d59", "#91bfdb")) +
  theme_minimal(base_size = 12, base_family = "Arial")+
  theme(axis.text.x = element_text(size = 8, angle = 45), 
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        axis.title.x = element_blank()) +
  # Legend formatting
  theme(legend.text = element_text(size = 8),
        legend.title = element_text(size = 10),
        legend.position = "top", 
        legend.direction = "horizontal") +
  
  #Facets formatting
  theme(strip.text.x = element_text(size = 10),
        strip.text.y = element_text(size = 10),
        panel.margin = unit(1, "lines"))


#wealth and education
ggplot(DHS, aes(x=Wealth_q))+
  geom_bar(aes(fill=H_education))+
  ggtitle("DHS 2014, Education of Head of Household and wealth")+
  #Format axes
  scale_y_continuous(name = "Number of Children")+
  scale_fill_manual(name = "Highest Education", values = c("darksalmon", "mediumorchid2", "darkturquoise", "chartreuse4")) +
  theme_minimal(base_size = 12, base_family = "Arial")+
  theme(axis.text.x = element_text(size = 8, angle = 45), 
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        axis.title.x = element_blank()) +
  # Legend formatting
  theme(legend.text = element_text(size = 8),
        legend.title = element_text(size = 10),
        legend.position = "top", 
        legend.direction = "horizontal") +
  
  #Facets formatting
  theme(strip.text.x = element_text(size = 10),
        strip.text.y = element_text(size = 10),
        panel.margin = unit(1, "lines"))

#malaria and hemoglobin
ggplot(DHS, aes(x=as.factor(Micro),y=Hb))+
  geom_boxplot()+
  theme(panel.background= element_blank(), panel.grid.minor = element_line(colour = "lightgrey"),panel.border = element_blank(),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))+
  scale_x_discrete(name="Malaria infection",breaks = c(0,1), labels = c("No", "Yes")) +
  scale_y_continuous(name = "Hemoglobin level (g/dL)")+
  ggtitle("DHS 2014, Hemoglobin Leevels by malaria infection status")


########################################
###### GIS EXPLORATORY ANALYSIS   ######
########################################
DHS_mal=DHS %>% 
  group_by(HH1) %>% 
  summarise(Malaria=mean(Micro)*100, Long=mean(Long), Lat=mean(Lat)) %>% 
  filter(Long!=0.000000)

write.csv(DHS_mal,"Data/GIS/DHS_mal.csv", row.names = F)


#Visualise Map of points by prevalence

world_map <- map_data("world")
Ghana <- subset(world_map, world_map$region=="Ghana")

ggplot() + coord_fixed() +
  xlab("") + ylab("")+ 
  #Make base plot
  geom_polygon(data=Ghana, aes(x=long, y=lat, group=group), 
               colour="black", fill="white")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill = 'white', colour = 'white'), 
        axis.line = element_line(colour = "white"), legend.position="left",
        axis.ticks=element_blank(), axis.text.x=element_blank(),
        axis.text.y=element_blank())+
  #adding the prevalence points
  geom_point(data=DHS_mal, 
             aes(x=Long, y=Lat, colour=Malaria, size=Malaria), alpha=0.4)+ 
  scale_colour_gradient(name = "Malaria Prevalence (%)",high = "red", low = "Yellow")+
  scale_size_continuous(name = "Malaria Prevalence (%)", range = c(1,5))+
  guides(color=guide_legend(), size = guide_legend())+
  ggtitle("DHS 2014, Malaria Prevalence Point map")
