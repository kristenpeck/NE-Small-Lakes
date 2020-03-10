
#This script provides basic analysis of the small lakes data
# Author: Kristen Peck
# Date: 12-Feb-2020


#install.packages(c("FSA","readxl", "ggplot2","dplyr"))

library(readxl)
library(ggplot2)
library(plyr)
library(dplyr)
library(FSA)
library(lubridate)
library(tidyr)

effort <- read_excel("SmallLakesDB.xlsx",sheet = "Effort")
str(effort)
catch <- read_excel("SmallLakesDB.xlsx",sheet = "Catch", na = "NA")
str(catch)
env <- read_excel("SmallLakesDB.xlsx",sheet = "Enviro", na = "NA")
str(env)


effort2019 <- effort %>% 
  filter(year %in% 2019)

catch2019 <- catch %>% 
  mutate(fl.cat = lencat(fl, breaks= c(Sub_stock=0,Stock=200, Quality=400, 
                                       Trophy=550, 1000), use.names=T)) %>% 
  mutate(k = (100000*m)/fl^3) %>% 
  filter(year %in% 2019) %>% 
  filter(sp %in% c("RB","EB"))

catch.effort <- full_join(effort, catch, by= c("lake", "year", "effortid"))

catch.effort2019 <- catch.effort %>% 
  filter(year %in% 2019) %>% 
  filter(sp %in% c("RB","EB"))

#### CPUE ####
 
soaktime <- ddply(effort2019,~lake, summarize, soak.time = sum(efforthr))
tot.catch <- ddply(catch2019,~lake, summarize, tot.catch = length(sp))

CPUE <- soaktime %>% 
  full_join(tot.catch) %>% 
  mutate(cpue = round(tot.catch/soak.time,2))
CPUE


#QA#
unique(catch.effort2019$sp)
unique(catch2019$mat)



#### Pete ####

lk.catch2019 <- catch2019 %>% 
  filter(lake %in% "Pete") %>% 
  filter(sp %in% "RB") %>% 
  filter(!is.na(ageid))
  
lk.catch2019

ggplot(data=lk.catch2019) +
  geom_histogram(aes(x=fl, fill=mat), colour = "black", binwidth= 10)+
  ggtitle(lk.catch2019$lake)

#### Pete env ####
str(env)
lk.env2019 <- env %>% 
  mutate(year = year(date)) %>% 
  filter(year %in% 2019) %>% 
  filter(lake %in% "Pete") %>% 
  gather("var","value",tempdown, -dodown, -conddown)

str(lk.env2019)

ggplot(data=lk.env2019)+
  geom_path(aes(x=value, y=depthdown, colour=var), size=2)+
  scale_y_reverse(name= "Depth (m)", 
                  breaks=seq(min(lk.env2019$depthdown),
                             max(lk.env2019$depthdown),1))+
  scale_x_continuous(name = "",breaks=seq(min(lk.env2019$value, na.rm=T),
                                          max(lk.env2019$value, na.rm=T),1))+
  scale_colour_manual(values=c("black"),
                      name = "",labels = c("Temp. deg C"))+
  ggtitle(lk.env2019$lake)

(ave.cond <- mean(lk.env2019$conddown, na.rm=T))



#### Chunamun ####

lk.catch2019 <- catch2019 %>% 
  filter(lake %in% "Chunamun") %>% 
  filter(sp %in% "RB")

ggplot(data=lk.catch2019) +
  geom_histogram(aes(x=fl, fill=mat), colour = "black", binwidth= 10)+
  ggtitle(lk.catch2019$lake)

#### Chun env ####
str(env)
lk.env2019 <- env %>% 
  mutate(year = year(date)) %>% 
  filter(year %in% 2019) %>% 
  filter(lake %in% "Chunamun") %>% 
  gather("var","value",tempdown, -dodown, -conddown)

str(lk.env2019)

ggplot(data=lk.env2019)+
  geom_path(aes(x=value, y=depthdown, colour=var), size=2)+
  scale_y_reverse(name= "Depth (m)", 
                  breaks=seq(min(lk.env2019$depthdown),
                             max(lk.env2019$depthdown),1))+
  scale_x_continuous(name = "",breaks=seq(min(lk.env2019$value,na.rm=T),
                                          max(lk.env2019$value,na.rm=T),0.2))+
  scale_colour_manual(values=c("black"),
                      name = "",labels = c("Temp. deg C"))+
  ggtitle(lk.env2019$lake)

(ave.cond <- mean(lk.env2019$conddown, na.rm=T))


#### Sundance ####

lk.catch2019 <- catch2019 %>% 
  filter(lake %in% "Sundance") %>% 
  filter(sp %in% "RB")

ggplot(data=lk.catch2019) +
  geom_histogram(aes(x=fl, fill=fl.cat), colour = "black", binwidth= 10)+
  ggtitle(lk.catch2019$lake)

# NOTE: no environmental data taken from Sundance in 2019



#### Quality ####

lk.catch2019 <- catch2019 %>% 
  filter(lake %in% "Quality") %>% 
  filter(sp %in% "RB")
str(lk.catch2019)

table.qual <- summarise(lk.catch2019,
            totcaught = length(sp),
            totmatchecked = length(which(!is.na(mat))),
            totST = length(which(mat %in% "ST")),
            totM = length(which(mat %in% "M")),
            totMT = length(which(mat %in% "MT")),
            totIM = length(which(mat %in% "IM")),
            totSP = length(which(mat %in% "SP")),
            totalfl.checked = length(which(!is.na(fl))),
            totalm.checked = length(which(!is.na(m))),
            aveFL = mean(fl, na.rm=T),
            minFL = min(fl, na.rm=T),
            maxFL = max(fl, na.rm=T),
            avem = mean(m, na.rm=T),
            minm = min(m, na.rm=T),
            maxm = max(m, na.rm=T),
            avek = mean(k, na.rm=T),
            mink = min(k, na.rm=T),
            maxk = max(k, na.rm=T))

table.qual

unique(lk.catch2019$stomach)

#CPUE

lk.effort2019 <- effort %>% 
  filter(year %in% 2019) %>% 
  filter(lake %in% "Quality")
lk.effort2019$efforthr  

(CPUE <- table.qual$totcaught/sum(lk.effort2019$efforthr))
  
  

#length-frequency plot
ggplot(data=lk.catch2019) +
  geom_histogram(aes(x=fl, fill=fl.cat), colour = "black", binwidth= 10)+
  ggtitle(lk.catch2019$lake)

#condition-frequency plot
ggplot(data=lk.catch2019) +
  geom_histogram(aes(x=k, fill=fl.cat), colour = "black", binwidth= 0.05)+
  ggtitle(lk.catch2019$lake)

#length-weight plot
ggplot(data=lk.catch2019) +
  geom_point(aes(x=fl, y=m), colour = "black", size=4)+
  #geom_smooth(aes(x=fl, y=m), method="lm")+
  ggtitle(lk.catch2019$lake)



#### Quality env ####

str(env)
lk.env2019 <- env %>% 
  mutate(year = year(date)) %>% 
  filter(year %in% 2019) %>% 
  filter(lake %in% "Quality") %>% 
  gather("var","value",tempdown, dodown, -conddown)

str(lk.env2019)

qual.env.plot <- ggplot(data=lk.env2019)+
  geom_line(aes(x=value, y=depthdown, colour=var), size=2)+
  scale_y_reverse(name= "Depth (m)", 
                  breaks=seq(min(lk.env2019$depthdown),
                             max(lk.env2019$depthdown),1))+
  scale_x_continuous(name = "",breaks=seq(min(lk.env2019$value),
                                          max(lk.env2019$value),1))+
  scale_colour_manual(values=c("gray40","black"),
                        name = "",labels = c("DO mg/L","Temp. deg C"))+
  ggtitle(paste(lk.env2019$lake, "Lake"))
qual.env.plot
ggsave(qual.env.plot, filename = "qual.env.plot2019.png", dpi = 300)

(ave.cond <- mean(lk.env2019$conddown, na.rm=T))

lk.env2019 %>% 
  filter(depthdown %in% 2) %>% 
  select(conddown)

#### Boulder: ####

lk.catch2019 <- catch2019 %>% 
  filter(lake %in% "Boulder") %>% 
  filter(sp %in% "RB")

ggplot(data=lk.catch2019) +
  geom_histogram(aes(x=fl, fill=fl.cat), colour = "black", binwidth= 10)+
  ggtitle(lk.catch2019$lake)

