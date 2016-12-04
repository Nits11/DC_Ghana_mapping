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
  select(HH1,HH2,Day=UF8D,Month=UF8M,Year=UF8Y,Age_year=AG2,Age_months=Age_m,fever=ML1,Health_insurance=HI1,
         Weight=AN3,Height=AN4,AM7,Micro,Sev_an,Hb=AM8,RDT=AM10) %>% #
  inner_join(gps_dat) %>%  
  filter(!is.na(Long))

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
child_dat$fever[which(child_dat$fever=="DK"|child_dat$fever=="Missing")]=NA

#cleaning the household data
names(hh_dat)
hh_dat=hh_dat %>% 
  select(HH1, HH2, Residence=HH6, Region=HH7, District=HH7A, IRS=IR1, hhweight, ethnicity, 
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

#Age in months and microscopy prevalence and RDT
ggplot(child_dat, aes(x=Age_months)) +
  stat_smooth(aes(y=RDT_pos, colour="RDT"),method = "loess",size=1.5)+
  stat_smooth(aes(y=Micro, colour="Microscopy"),method = "loess", size=1.5)+
  xlab("Age in months")+
  ylab("Probabilty of Infection")+
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
  scale_y_continuous(name = "Hemoglobin level (g/dL)")

####################GIS DATA####################
MICS_mal=child_dat %>% 
  group_by(HH1) %>% 
  summarise(Malaria=mean(Micro)*100, RDT=mean(RDT_pos)*100, Long=mean(Long), Lat=mean(Lat))
write.csv(MICS_mal,"Data/GIS/MICS_mal.csv", row.names = F)



#Visualise Map of points by prevalence
library(ggmap)
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
  guides(color=guide_legend(), size = guide_legend())

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

#Add GPS coordinate data to it

