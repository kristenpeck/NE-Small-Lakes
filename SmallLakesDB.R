
#This script provides basic analysis of the small lakes data. It is meant to be universal among 
  # lakes, but unique circumstances make that a little tricky. 

# Author: Kristen Peck
# Date created : 12-Feb-2020
# Github home: https://github.com/kristenpeck/NE-Small-Lakes


library(readxl)
library(ggplot2)
library(plyr)
library(dplyr)
library(FSA)
library(lubridate)
library(tidyr)
library(sf)
library(mapview)
library(reshape2)
library(car)
library(nnet)
library(gridExtra)
library(grid)

### Load Effort Data ####

effort <- read_excel("SmallLakesDB-copy.xlsx",sheet = "Effort")
str(effort); names(effort)

TableA1.rawdata <- effort %>% 
  select(Lake=lake, Year=year, Crew=crew, `Effort ID`=effortid, `Net Type`=nettype, `Set Date`=setdate,
         `Lift Date`=liftdate, `Set Time`=settime, `Lift Time`=lifttime, `Temp. (deg C)`=temp)
write.csv(TableA1.rawdata, "TableA1.rawdata.csv", row.names = F)

catch <- read_excel("SmallLakesDB-copy.xlsx",sheet = "Catch", na = "NA", 
                    col_types = c("guess","guess","guess","guess","guess",
                                  "guess","guess","guess","guess","guess",
                                  "guess","numeric","text","text", "guess",
                                  "guess","guess", "guess"))

### Load Catch Data ####

catch <- catch %>% 
  mutate(fl.cat = lencat(fl, breaks= c(Sub_stock=0,Stock=200, Quality=400, 
                                       Trophy=550, 1000), use.names=T)) %>% #PSD from Paul Askeys rapid assessment tool
  mutate(k = (100000*m)/fl^3, 
         mat2 = ifelse(mat == "IM"|mat == "ST"|mat == "af3n", "IM/ST", mat),
         logm = log10(m),logL = log10(fl)) %>% #in cases where IM was confused with ST, combined
  arrange(sp)


catch$catchID <- 1:nrow(catch)

str(catch); names(catch)

TableA2.rawdata <- catch %>% 
  select(lake, year, `Effort ID`=effortid, sp, `FL (mm)`=fl, `M (g)`=m, Stomach=stomach, 
         `Age Structure`=`age structure`, Age=age, `Age ID`=ageid, Comments=comments)
write.csv(TableA2.rawdata, "TableA2.rawdata.csv", row.names = F, na = "")



### Load env. Data ####

env <- read_excel("SmallLakesDB-copy.xlsx",sheet = "Enviro", na = "NA")
str(env)


### CTD profiles ####

yr.select <- c(2020)
lk.select <- c("Wright")


env.selectyr <- env %>% 
  mutate(year = year(date)) %>% 
  filter(year %in% yr.select) %>% 
  gather("var","value",tempdown, dodown, -conddown)

env.selectyrlk <- env %>% 
  mutate(year = year(date)) %>% 
  filter(year %in% yr.select) %>% 
  filter(lake %in% lk.select) %>% 
  gather("var","value",tempdown, dodown, -conddown)

Figure.env <- ggplot(data=env.selectyrlk)+
  geom_path(aes(x=value, y=depthdown, colour=var), size=1.5)+
  scale_y_reverse(name= "Depth (m)", 
                  breaks=seq(min(env.selectyrlk$depthdown, na.rm=T),
                             max(env.selectyrlk$depthdown, na.rm=T), 1))+
  # scale_x_continuous(name = "",breaks=seq(floor(min(env.selectyrlk$value, na.rm=T)),
  #                                         ceiling(max(env.selectyrlk$value, na.rm=T)),1))+
  scale_colour_manual(values=c("black", "gray60"),
                      name = "",labels = c("Diss. Oxygen (mg/L)","Temp. (deg C)"))+
  facet_wrap(~year, ncol=2)+
  theme_bw()
Figure.env

# NOTE: no environmental data taken from:
# 2017 - Moose, Inga, Sundance
# 2018 - all measured!
# 2019 - Sundance



#### Maps ####

#### Maps- Select Year-Effort ####

yr.select <- c(2017)

effort.selectyr <- effort %>% 
  filter(year %in% yr.select)


#need to put start and end coordinates in same columns, then re-project

locations.easting <- effort.selectyr %>% 
  mutate(effortidF = as.factor(effortid)) %>% 
  dplyr::select(lake, nettype, effortidF, starteast, endeast) %>% 
  melt() %>% 
  mutate(startend = ifelse(grepl("start",variable),yes = "start",no="end")) %>% 
  select(lake, nettype, effortidF, startend, UTME=value)

locations.northing <- effort.selectyr %>% 
  mutate(effortidF = as.factor(effortid)) %>% 
  dplyr::select(lake, nettype, effortidF, startnorth, endnorth) %>% 
  melt() %>% 
  mutate(startend = ifelse(grepl("start",variable),yes = "start",no="end")) %>% 
  select(lake, nettype, effortidF, startend, UTMN=value)

(locations <- locations.easting %>% 
    full_join(locations.northing, by=c("lake", "nettype", "effortidF","startend")) %>% 
    filter(!is.na(UTME)))
str(locations)


#no coords for Heart or Stewart, 2018; Inga 2017





# Figure 1 - overview map: ###

locations.lk <- locations %>% 
  filter(startend %in% "start", nettype %in% "RIC7 SGN") %>% 
  st_as_sf(coords = c("UTME", "UTMN"), 
           crs = 3157)

lk.overview.map <- mapview(locations.lk, cex=2, lwd=1, legend=F,map.types="OpenStreetMap") %>% 
  leafem::addStaticLabels(label = locations.lk$lake,
                  noHide = T,
                  direction = 'top',
                  textOnly = TRUE,
                  textsize = "20px")

print(lk.overview.map) 


#### Appendix: gillnets maps: ###

unique(locations.lk$lake)

lk.select <- "Boot"

indiv.nets <- locations %>% 
  filter(lake %in% lk.select)

indiv.nets.st <- st_as_sf(indiv.nets, 
                   coords = c("UTME", "UTMN"), 
                   crs = 3157)

lks.albers <- st_transform(indiv.nets.st, crs=3005)

#### Maps- Select Year-ENV ####

yr.select <- c(2017)

env.selectyr <- env %>% 
  mutate(year = year(date)) %>% 
  filter(year %in% yr.select, lake %in% lk.select) 

env.point.yr <- env.selectyr %>% 
  st_as_sf(coords = c("easting", "northing"), 
           crs = 3157) 



#overview map alternative - did not use because ugly

# library(bcdata)
# 
# bc <- bcdc_query_geodata('7-5m-provinces-and-states-the-atlas-of-canada-base-maps-for-bc') %>%
#   filter(ENGLISH_NAME == 'British Columbia') %>%
#   collect()
# bc
# 
# Peace <- bcdc_query_geodata('WHSE_ADMIN_BOUNDARIES.EADM_WLAP_REGION_BND_AREA_SVW') %>%
#   filter(REGION_NAME == 'Peace') %>%
#   collect()
# Peace
# 
# lks.mapping <- bcdc_query_geodata('cb1e3aba-d3fe-4de1-a2d4-b8b6650fb1f6') %>%
#   filter(AREA_HA > 10000) %>%
#   collect()
# 
# #bcdc_query_geodata('WHSE_BASEMAPPING.FWA_LAKES_POLY')
# lks.individual <- bcdc_query_geodata('cb1e3aba-d3fe-4de1-a2d4-b8b6650fb1f6') %>%
#   filter(INTERSECTS(lks.albers)) %>%
#   collect()
# ggplot()+
#   geom_sf(data=bc, fill="white")+
#   geom_sf(data=Peace, fill="tan")+
#   geom_sf(data=lks.mapping, col="blue")+
#   geom_sf(data=lks.albers, size=2)


mapview(lks.albers)

# net.lines<- lks.albers %>%
#   dplyr::group_by(lake, effortidF) %>%
#   dplyr::summarize() %>%
#   st_cast("LINESTRING") 

net.lines<- lks.albers %>%
  filter(effortidF %in% c("1","2","3")) %>%
  group_by(lake, effortidF, nettype) %>%
  dplyr::summarize() %>%
  st_cast("LINESTRING")

#this is setting the default colours for all mapviews
mapviewOptions(vector.palette = colorRampPalette(c("white", "cornflowerblue", "grey60")))


net.lines.map <- mapview(list(net.lines,lks.albers), 
                         zcol="nettype", 
                         col.lines = c("snow", "grey"),
                         layer.name = "Net Type",
                         legend = list(TRUE, FALSE),
                         lwd=2) + mapview(env.point.yr, layer.name = "Env. station")

print(net.lines.map) #failed to label nets though... not sure why





#### FL frequency, by maturity or species ####

#### Select Year-catch ####
yr.select <- 2020

catch.selectyr <- catch %>% 
  filter(year %in% yr.select) 

effort.selectyr <- effort %>% 
  filter(year %in% yr.select)

catch.effort.selectyr <- full_join(effort.selectyr, catch.selectyr, by=c("lake", "year", "effortid"))

#### Tables - CPUE, demographics ####

str(catch.effort.selectyr)
unique(catch.effort.selectyr$sp)

Table1 <- catch.effort.selectyr %>% 
  filter(sp %in% c("RB","EB","SU","NP","YP")) %>% 
  dplyr::group_by(lake,sp, effortid) %>% 
  dplyr::summarise(`Soak Time (hrs)`=sum(unique(efforthr)), `Net Type`=paste(unique(nettype),collapse=","), 
                   Year=unique(year),`Species`=paste(unique(sp), collapse=","),n = length(unique(catchID)),
                   CPUE = round(length(unique(catchID))/`Soak Time (hrs)`,2),
                   `FL mean` = mean(fl, na.rm=T),
                   `FL range (mm)`=paste0(min(fl, na.rm=T),"-", max(fl, na.rm=T), collapse=","),
                   `m mean` = mean(m, na.rm=T),
                   `m range (g)`=paste0(min(m, na.rm=T), "-",max(m, na.rm=T), collapse=","),
                   `k mean` = mean(k, na.rm=T),
                   `k range`=paste0(round(min(k, na.rm=T),2), "-",round(max(k, na.rm=T),2),collapse=",")) %>% 
  dplyr::arrange(Year)
Table1
names(Table1)


Table1.moose <- Table1 %>% 
  filter(lake %in% "Moose") %>% 
  mutate(`Mean FL (range)`=paste0(round(`FL mean`,2)," (",`FL range (mm)`,")")) %>% 
  mutate(`Mean M (range)`=paste0(round(`m mean`,2)," (",`m range (g)`,")")) %>% 
  mutate(`Mean K (range)` = paste0(round(`k mean`, 2)," (", `k range`, ")")) %>% 
  mutate(`Soak Time (hrs)` = round(`Soak Time (hrs)`, 2)) %>% 
  select(lake, Year, sp,`Net Type`,`Soak Time (hrs)`, n, `Mean FL (range)`,`Mean M (range)`,
         `Mean K (range)`, CPUE)
Table1.moose

Table1.boot <- Table1 %>% 
  filter(lake %in% "Boot") %>% 
  mutate(`Mean FL (range)`=paste0(round(`FL mean`,2)," (",`FL range (mm)`,")")) %>% 
  mutate(`Mean M (range)`=paste0(round(`m mean`,2)," (",`m range (g)`,")")) %>% 
  mutate(`Mean K (range)` = paste0(round(`k mean`, 2)," (", `k range`, ")")) %>% 
  mutate(`Soak Time (hrs)` = round(`Soak Time (hrs)`, 2)) %>% 
  select(lake, Year, sp,`Net Type`,`Soak Time (hrs)`, n, `Mean FL (range)`,`Mean M (range)`,
         `Mean K (range)`, CPUE) %>% 
  arrange(sp,`Net Type`)
Table1.boot

Table1.stony <- Table1 %>% 
  filter(lake %in% "Stony") %>% 
  mutate(`Mean FL (range)`=paste0(round(`FL mean`,2)," (",`FL range (mm)`,")")) %>% 
  mutate(`Mean M (range)`=paste0(round(`m mean`,2)," (",`m range (g)`,")")) %>% 
  mutate(`Mean K (range)` = paste0(round(`k mean`, 2)," (", `k range`, ")")) %>% 
  mutate(`Soak Time (hrs)` = round(`Soak Time (hrs)`, 2)) %>% 
  select(lake, Year, sp,`Net Type`,`Soak Time (hrs)`, n, `Mean FL (range)`,`Mean M (range)`,
         `Mean K (range)`, CPUE)
Table1.stony

Table1.big <- Table1 %>% 
  filter(lake %in% "Big") %>% 
  mutate(`Mean FL (range)`=paste0(round(`FL mean`,2)," (",`FL range (mm)`,")")) %>% 
  mutate(`Mean M (range)`=paste0(round(`m mean`,2)," (",`m range (g)`,")")) %>% 
  mutate(`Mean K (range)` = paste0(round(`k mean`, 2)," (", `k range`, ")")) %>% 
  mutate(`Soak Time (hrs)` = round(`Soak Time (hrs)`, 2)) %>% 
  select(lake, Year, sp,`Net Type`,`Soak Time (hrs)`, n, `Mean FL (range)`,`Mean M (range)`,
         `Mean K (range)`, CPUE)
Table1.big

Table1.wright <- Table1 %>% 
  filter(lake %in% "Wright") %>% 
  mutate(`Mean FL (range)`=paste0(round(`FL mean`,2)," (",`FL range (mm)`,")")) %>% 
  mutate(`Mean M (range)`=paste0(round(`m mean`,2)," (",`m range (g)`,")")) %>% 
  mutate(`Mean K (range)` = paste0(round(`k mean`, 2)," (", `k range`, ")")) %>% 
  mutate(`Soak Time (hrs)` = round(`Soak Time (hrs)`, 2)) %>% 
  select(lake, Year, sp,`Net Type`,`Soak Time (hrs)`, n, `Mean FL (range)`,`Mean M (range)`,
         `Mean K (range)`, CPUE) %>% 
  arrange(`Net Type`)
Table1.wright

write.csv(Table1.wright, "Table1.wright.csv", row.names = F)

# table - demographics

table.fish <- ddply(catch.selectyr, ~(lake), summarize,
                    `#caught` = length(sp),
                    `% not sterile` = 
                      round(length(which(mat %in% c("M","MT","SP")))/
                              length(which(!is.na(mat))),2)*100,
                    #totalfl.checked = length(which(!is.na(fl))),
                    #totalm.checked = length(which(!is.na(m))),
                    `mean FL (mm)` = round(mean(fl, na.rm=T),2),
                    `min FL (mm)` = round(min(fl, na.rm=T),2),
                    `max FL (mm)` = round(max(fl, na.rm=T),2),
                    `mean mass (g)` = round(mean(m, na.rm=T),2),
                    `min mass (g)` = round(min(m, na.rm=T),2),
                    `max mass (g)` = round(max(m, na.rm=T),2),
                    `mean k` = round(mean(k, na.rm=T),2),
                    `min k` = round(min(k, na.rm=T),2),
                    `max k` = round(max(k, na.rm=T),2))

table.fish

#write.csv(table.fish, file = "table.fish.csv",row.names = F)





## ## ## ## ## ## ##
#### HISTOGRAMS ####
## ## ## ## ## ## ##


#### FL frequency, by maturity or species ####

str(catch.selectyr)

Figure.FL.mat <- ggplot(data=catch.selectyr) +
  geom_histogram(aes(x=fl, fill=mat2), colour = "black", binwidth= 50)+
  facet_wrap(~lake)+
  scale_y_continuous(breaks = seq(0,100,10))+
  labs(x="Fork Length (mm)", y="Frequency", fill="Maturity")+
  theme_bw()

Figure.FL.mat #note this puts species together
 

# specfic lake, by species 

unique(catch.selectyr$year)
unique(catch.selectyr$lake)

lk.select = "Big"

catch.selectyrlk <- catch.selectyr %>% 
  filter(lake %in% lk.select) 

unique(catch.selectyrlk$sp)

Figure.FL.sp <- ggplot(data=catch.selectyrlk) +
  geom_histogram(aes(x=fl, fill=sp), colour="black", binwidth= 10)+
  facet_wrap(~sp, ncol=1)+
  scale_x_continuous(breaks = seq(50,max(catch.selectyrlk$fl, na.rm=T),50))+
  #scale_y_continuous(breaks = seq(0,30,5))+
  labs(x="Fork Length (mm)", y="Frequency", fill="Species")+
  theme_bw()

Figure.FL.sp



### FL frequency, compare years ####

lk.select = "Boot"
str(catch)
catch.selectlk <- catch %>% 
  filter(lake %in% lk.select, surveytype %in% "gillnet")

unique(catch.selectlk$sp)

Figure.FL.yr <- ggplot(data=catch.selectlk) +
  geom_histogram(aes(x=fl, fill=sp), colour="black", binwidth= 10)+
  facet_wrap(~year, ncol=1)+
  scale_x_continuous(breaks = seq(100,max(catch.selectlk$fl, na.rm=T),50))+
  #scale_y_continuous(breaks = seq(0,30,5))+
  labs(x="Fork Length (mm)", y="Frequency", fill="Species")+
  theme_bw()+
  theme(legend.position = "bottom")

Figure.FL.yr



#### RB FL condition, by stock category ####

RB.catch.selectyr <- catch.selectyr %>% 
  filter(sp %in% "RB")

ggplot(data=RB.catch.selectyr) +
  geom_histogram(aes(x=fl, fill=fl.cat), colour = "black", binwidth= 30)+
  facet_wrap(~lake)+
  labs(y="Frequency", x="Fork Length (mm)", fill="Category")+
  theme_bw()

#condition-frequency plot
ggplot(data=RB.catch.selectyr) +
  geom_histogram(aes(x=k, fill=fl.cat), colour = "black", binwidth= 0.05)+
  geom_vline(xintercept = 1, linetype="dashed")+
  facet_wrap(~lake)+
  labs(y="Frequency", x="Fulton's k", fill="Category")+
  theme_bw()


#lake specific condition-frequency plot : RB

RB.catch.selectlk <- catch %>% 
  filter(sp %in% "RB", surveytype %in% "gillnet") %>% 
  filter(lake %in% lk.select) 

Figure.K.lk <- ggplot(data=RB.catch.selectlk) +
  geom_histogram(aes(x=k, fill=fl.cat), colour = "black", binwidth= 0.05)+
  geom_vline(xintercept = 1, linetype="dashed")+
  facet_wrap(~year, ncol=1)+
  labs(title = RB.catch.selectlk$lake, y="Frequency", x="Fulton's k", fill="Category")+
  theme_bw()+
  theme(legend.position = "bottom")
Figure.K.lk

unique(RB.catch.selectlk$year)
#

#lake specific condition-frequency plot : EB

EB.catch.selectlk <- catch %>% 
  filter(sp %in% "EB", surveytype %in% "gillnet") %>% 
  filter(lake %in% lk.select) 

Figure.K.lk.EB <- ggplot(data=EB.catch.selectlk) +
  geom_histogram(aes(x=k, fill=fl.cat), colour = "black", binwidth= 0.05)+
  geom_vline(xintercept = 1, linetype="dashed")+
  facet_wrap(~year, ncol=1)+
  labs(title = EB.catch.selectlk$lake, y="Frequency", x="Fulton's k", fill="Category")+
  theme_bw()+
  scale_fill_manual(values=c("purple", "dark orange", "green"))+
  theme(legend.position = "bottom")
Figure.K.lk.EB

unique(EB.catch.selectlk$year)



## ## ## ## ## ## ## ##
#### LENGTH-WEIGHT ####
## ## ## ## ## ## ## ##



#### Lk-specific: length-weight relationships ####
unique(catch.selectyr$year)
unique(catch.selectyr$lake)
lk.select = "Boot"


## RB ##
RB.catch.selectyr.lk <- catch.selectyr %>% 
  filter(lake %in% lk.select) %>% 
  filter(sp %in% "RB")
str(RB.catch.selectyr.lk)


fitflwt.rb <- lm(logm~logL, data=RB.catch.selectyr.lk)
summary(fitflwt.rb)
fitflwt.rb$coeff

residPlot(fitflwt.rb) #check for length-wt outliers in residual plot




### predicted weights RB #### - fill in the blanks


(wt <- RB.catch.selectyr.lk %>%
  filter(!is.na(fl)) %>%
  filter(!is.na(m)) %>%
  mutate(m.pred = as.character("measured"))) 

(no.wt <- RB.catch.selectyr.lk %>% 
  filter(!is.na(fl)) %>% 
  filter(is.na(m)) %>% 
  mutate(m.pred = as.character("predicted"))) 

nrow(no.wt)

pred.logwt <- predict(fitflwt.rb, no.wt, interval ="prediction") #this is predicting individual fish weights, not average
cf <- logbtcf(fitflwt.rb, 10)
back.trans <- cf*10^pred.logwt

no.wt$m <- as.numeric(back.trans[,1])
no.wt$pred.m.lwr <- back.trans[,2]
no.wt$pred.m.upr <- back.trans[,3]

(predRB.catch.selectyr.lk <- wt %>% 
   full_join(no.wt) %>% 
    mutate(logm = log10(m)) %>% 
    arrange(catchID)) 





labelflwt.rb <- paste0("log[10](M) == ", round(fitflwt.rb$coeff[1],2)," + ",
                 round(fitflwt.rb$coeff[2],2)," * log[10](FL)")

plot.FLwt <- ggplot(data=predRB.catch.selectyr.lk) +
  geom_point(aes(x=fl, y=m, shape=m.pred, colour=m.pred), alpha=0.8, size=4)+
  geom_smooth(aes(x=fl, y=m), method="loess")+
  labs(title=paste(predRB.catch.selectyr.lk$lake,"Lake,",predRB.catch.selectyr.lk$year),
       x= "Fork Length (mm)", y="Mass (g)", shape="Mass", colour="Mass")+
  theme_bw()+
  theme(legend.position = "")
plot.FLwt

plot.logFLwt <- ggplot(data=predRB.catch.selectyr.lk) +
  geom_point(aes(x=logL, y=logm, shape=m.pred, colour=m.pred), alpha=0.8, size=4)+
  geom_smooth(aes(x=logL, y=logm), method="lm")+
  annotate(geom = "text", x = min(predRB.catch.selectyr.lk$logL, na.rm=T)+.1, 
           y = max(predRB.catch.selectyr.lk$logm, na.rm=T)-0.2, label = labelflwt.rb, parse=T)+
  labs(x= "log10 Fork Length", y="log10 Mass", shape="Mass", colour="Mass", parse=T)+
  theme_bw()+
  theme(legend.position = "bottom")

plot.logFLwt

stackplots.FLwtRB <- grid.arrange(plot.FLwt, plot.logFLwt, ncol=1)
plot(stackplots.FLwtRB)



## RB - EB compared##

EB.catch.selectyr.lk <- catch.selectyr %>% 
  filter(lake %in% lk.select) %>% 
  filter(sp %in% "EB")
str(EB.catch.selectyr.lk)


fitflwt.eb <- lm(logm~logL, data=EB.catch.selectyr.lk)
summary(fitflwt.eb)
fitflwt.eb$coeff

residPlot(fitflwt.eb) #check for length-wt outliers in residual plot




### predicted weights  EB #### - fill in the blanks

(wt.EB <- EB.catch.selectyr.lk %>%
    filter(!is.na(fl)) %>%
    filter(!is.na(m)) %>%
    mutate(m.pred = as.character("measured"))) 

(no.wt.EB <- EB.catch.selectyr.lk %>% 
    filter(!is.na(fl)) %>% 
    filter(is.na(m)) %>% 
    mutate(m.pred = as.character("predicted"))) 

nrow(no.wt.EB)

pred.logwt <- predict(fitflwt.eb, no.wt.EB, interval ="prediction") #this is predicting individual fish weights, not average
cf <- logbtcf(fitflwt.eb, 10)
back.trans <- cf*10^pred.logwt

no.wt.EB$m <- as.numeric(back.trans[,1])
no.wt.EB$pred.m.lwr <- back.trans[,2]
no.wt.EB$pred.m.upr <- back.trans[,3]

(predEB.catch.selectyr.lk <- wt.EB %>% 
    full_join(no.wt.EB) %>% 
    mutate(logm = log10(m)) %>% 
    arrange(catchID)) 


#figure- compare species:
predALL.catch.selectyr.lk <- rbind(predRB.catch.selectyr.lk,predEB.catch.selectyr.lk)
unique(predALL.catch.selectyr.lk$sp)


labelflwt.rb <- paste0("RB: log[10](M) == ", round(fitflwt.rb$coeff[1],2)," + ",
                       round(fitflwt.rb$coeff[2],2)," * log[10](FL)")
labelflwt.eb <- paste0("EB: log[10](M) == ", round(fitflwt.eb$coeff[1],2)," + ",
                       round(fitflwt.eb$coeff[2],2)," * log[10](FL)")

plot.FLwt <- ggplot(data=predALL.catch.selectyr.lk) +
  geom_point(aes(x=fl, y=m, shape=sp, colour=sp), alpha=0.8, size=4)+
  geom_smooth(aes(x=fl, y=m, col=sp), method="loess")+
  labs(title=paste(predALL.catch.selectyr.lk$lake,"Lake,",predALL.catch.selectyr.lk$year),
       x= "Fork Length (mm)", y="Mass (g)", shape="Species", colour="Species")+
  theme_bw()+
  theme(legend.position = "")
plot.FLwt

plot.logFLwt <- ggplot(data=predALL.catch.selectyr.lk) +
  geom_point(aes(x=logL, y=logm, shape=sp, colour=sp), alpha=0.8, size=4)+
  geom_smooth(aes(x=logL, y=logm, col=sp), method="lm")+
  annotate(geom = "text", x = min(predALL.catch.selectyr.lk$logL, na.rm=T)+0.12, 
            y = max(predALL.catch.selectyr.lk$logm, na.rm=T)-0.2, label = labelflwt.rb, parse=T)+
  annotate(geom = "text", x = min(predALL.catch.selectyr.lk$logL, na.rm=T)+0.12, 
           y = max(predALL.catch.selectyr.lk$logm, na.rm=T)-0.45, label = labelflwt.eb, parse=T)+
  labs(x= "log10 Fork Length", y="log10 Mass", shape="Mass", colour="Mass", parse=T)+
  theme_bw()+
  theme(legend.position = "bottom")

plot.logFLwt

stackplots.FLwtALL <- grid.arrange(plot.FLwt, plot.logFLwt, ncol=1)
plot(stackplots.FLwtALL)




# ### plot length-weight by maturity
# 
# 
# #length-weight plot - most recent year
# plot.FLwt.maturity <- ggplot(data=predRB.catch.selectyr.lk) +
#   geom_point(aes(x=fl, y=m, col=mat2, shape=mat2), alpha=0.8, size=4)+
#   geom_smooth(aes(x=fl, y=m), method="loess")+
#   facet_wrap(~sp)+
#   ggtitle(paste(predRB.catch.selectyr.lk$lake,"Lake,",predRB.catch.selectyr.lk$year))+
#   theme_bw()
# plot.FLwt.maturity
# 
# plot.logFLwt.maturity <- ggplot(data=predRB.catch.selectyr.lk) +
#   geom_point(aes(x=logL, y=logm, col=mat2, shape=mat2), alpha=0.8, size=4)+
#   geom_smooth(aes(x=logL, y=logm), method="lm")+
#   facet_wrap(~sp)+
#   ggtitle(paste(predRB.catch.selectyr.lk$lake,"Lake,",predRB.catch.selectyr.lk$year))+
#   labs(colour="Maturity", shape="Maturity")+
#   theme_bw()
# plot.logFLwt.maturity





# compare slopes between species:


 fit.growth <- lm(logm~logL*sp, data=predALL.catch.selectyr.lk)
 (car::Anova(fit.growth)) #note, if interaction is present then that should be interpreted before year effect
 fit.growth.p <- ifelse(Anova(fit.growth)$Pr[3] < 0.001, "<0.001",Anova(fit.growth)$Pr[3])


#### length-weight compare years ####
str(catch)
unique(catch$lake)

lk.select <- "Boot"


lake.temp <- catch %>% 
  filter(lake %in% lk.select) %>% 
  filter(surveytype %in% "gillnet") %>% 
  arrange(year)

(sampled.yrs <- unique(lake.temp$year))

(yr.select <- sampled.yrs[length(sampled.yrs)])
(prev.yr <- sampled.yrs[length(sampled.yrs)-1])


lk.catch.prev <- catch %>% 
  filter(lake %in% lk.select) %>% 
  filter(year %in% c(prev.yr, yr.select)) %>% 
  mutate(yearF = as.factor(year)) %>% 
  filter(sp %in% c("RB","EB")) %>% 
  filter(fl >= 162 | fl <= 130) %>% #this will make 6-panel and 7-panel nets more equal
      filter(age > 1) %>% ## NOTE: this is only for Boot! not for other lakes
  mutate(logm = log10(m),logL = log10(fl)) %>% 
  arrange(yearF)
unique(lk.catch.prev$yearF)


lk.catch.prev.rb <- catch %>% 
  filter(lake %in% lk.select, sp %in% "RB") %>% 
  filter(year %in% c(prev.yr, yr.select)) %>% 
  mutate(yearF = as.factor(year)) %>% 
  filter(fl >= 162 | fl <= 130) %>% #this will make 6-panel and 7-panel nets more equal
    filter(age > 1) %>% ## NOTE: this is only for Boot! not for other lakes
  mutate(logm = log10(m),logL = log10(fl)) %>% 
  arrange(yearF)

lk.catch.prev.eb <- catch %>% 
  filter(lake %in% lk.select, sp %in% "EB") %>% 
  filter(year %in% c(prev.yr, yr.select)) %>% 
  mutate(yearF = as.factor(year)) %>% 
  filter(fl >= 162 | fl <= 130) %>% #this will make 6-panel and 7-panel nets more equal
    filter(age > 1) %>% ## NOTE: this is only for Boot! not for other lakes
  mutate(logm = log10(m),logL = log10(fl)) %>% 
  arrange(yearF)


#length-weight plot - compare years (generic or for both species)

Figure.FLwt.compare <- ggplot() +
  geom_point(data=lk.catch.prev, aes(x=logL, y=logm, col=yearF, shape=yearF),  alpha=0.4, size=4)+
  geom_smooth(data=lk.catch.prev, aes(x=logL, y=logm, col=yearF), method="lm")+
  scale_colour_manual(name="Year",
                      labels = c(unique(lk.catch.prev$year)[1],unique(lk.catch.prev$year)[2]), 
                      values=c("black","purple"))+
  scale_shape_manual(name="Year",
                      labels = c(unique(lk.catch.prev$year)[1],unique(lk.catch.prev$year)[2]),
                     values=c(17, 19))+
  facet_wrap(~sp)+
  labs(x= "log10 Fork Length", y="log10 Mass")+ #title=lk.catch.prev$lake, 
  theme_bw()
Figure.FLwt.compare

#rainbows only
# Figure.FLwt.compare.rb <- ggplot() +
#   geom_point(data=lk.catch.prev.rb, aes(x=logL, y=logm, col=yearF, shape=yearF),  alpha=0.4, size=4)+
#   geom_smooth(data=lk.catch.prev.rb, aes(x=logL, y=logm, col=yearF), method="lm")+
#   scale_colour_manual(name="Year",
#                       labels = c(unique(lk.catch.prev.rb$year)[1],unique(lk.catch.prev.rb$year)[2]), 
#                       values=c("black","blue"))+
#   scale_shape_manual(name="Year",
#                      labels = c(unique(lk.catch.prev.rb$year)[1],unique(lk.catch.prev.rb$year)[2]),
#                      values=c(17, 19))+
#   labs(x= "log10 Fork Length", y="log10 Mass")+ #title=paste(lk.catch.prev.rb$lake,"RB"), 
#   theme_bw()
# Figure.FLwt.compare.rb
# 
# fit2.rb <- lm(logm~logL*yearF, data=lk.catch.prev.rb)
# car::Anova(fit2.rb)
# (p.flwtyrs.RB <- round(Anova(fit2.rb)[3,4], 3))
# 
# #brookies only
# Figure.FLwt.compare.eb <- ggplot() +
#   geom_point(data=lk.catch.prev.eb, aes(x=logL, y=logm, col=yearF, shape=yearF),  alpha=0.4, size=4)+
#   geom_smooth(data=lk.catch.prev.eb, aes(x=logL, y=logm, col=yearF), method="lm")+
#   scale_colour_manual(name="Year",
#                       labels = c(unique(lk.catch.prev.eb$year)[1],unique(lk.catch.prev.eb$year)[2]), 
#                       values=c("black","blue"))+
#   scale_shape_manual(name="Year",
#                      labels = c(unique(lk.catch.prev.eb$year)[1],unique(lk.catch.prev.eb$year)[2]),
#                      values=c(17, 19))+
#   labs( x= "log10 Fork Length", y="log10 Mass")+ #title=paste(lk.catch.prev$lake, "EB"),
#   theme_bw()
# Figure.FLwt.compare.eb
# 
# EB yrs compare present:
fit2.eb <- lm(logm~logL*yearF, data=lk.catch.prev.eb)
summary(fit2.eb)
car::Anova(fit2.eb) #note, if interaction is present then that should be interpreted before year effect
(p.flwtyrs.EB <-  round(Anova(fit2.eb)[3,4],3))







## ## ## ## ## 
#### AGES ####
## ## ## ## ##



#### length at age ####

lk.select <- "Boot"
yr.select <- c("2017")


lake.temp <- catch %>% 
  dplyr::filter(lake %in% lk.select) %>% 
  dplyr::filter(surveytype %in% c("gillnet","carcass count")) %>% 
  arrange(year)
(sampled.yrs <- unique(lake.temp$year))

(yr.prev <- sampled.yrs[length(sampled.yrs)-2])

lk.catch.prev <- catch %>% 
  dplyr::filter(sp %in% c("RB", "EB"), lake %in% lk.select, surveytype %in% c("gillnet"),
                year %in% c(yr.select, yr.prev)) %>% 
  filter(fl >= 162 | fl <= 130) %>% #this will make 6-panel and 7-panel nets more equal
  mutate(age.num = as.numeric(substr(age,1,1)), yearF=as.character(year)) %>% 
  filter(age.num > 1 | is.na(age.num)) %>% ## NOTE: this is only for Boot! delete for other lakes
  mutate(broodyear = ifelse(!is.na(age.num),year-age.num, NA)) %>% 
  mutate(lcat10=lencat(fl, w=10))
lk.catch.prev


#### Predict ages (ALK):####
 
#### RB Previous year ### # comment out if lake did not have any unmeasured ages to predict
# # # # # # # # # # # # #

tmp <- lk.catch.prev %>%
  dplyr::filter(yearF %in% yr.prev,  sp %in% "RB", !is.na(age.num)) %>%
  select(year, fl, lcat10) %>%
  arrange(lcat10)
min(tmp$lcat10, na.rm=T) # cannot predict beyond this minimum so filter out for predicting ages.
(n.toosmall.prev <- length(which(lk.catch.prev$fl < min(tmp$lcat10, na.rm=T)&
                                   lk.catch.prev$year %in% yr.prev)))

## prev.yr aged sample: ##|fl <= 149  ;  &fl >= 150
RB.ages.lk.catch.prev <- lk.catch.prev %>%
  dplyr::filter(!is.na(age.num)|fl < min(tmp$lcat10, na.rm=T), yearF %in% yr.prev, sp %in% "RB") # note the age prediction will not work if the fl is outside the range of measured fl-ages
## prev.yr unaged sample: ##
RB.noages.lk.catch.prev <- lk.catch.prev %>%
  dplyr::filter(is.na(age.num)&fl > min(tmp$lcat10, na.rm=T), yearF %in% yr.prev, sp %in% "RB") #note that all Boot samples were aged in 2002, so no need for this
nrow(RB.noages.lk.catch.prev) #if  no unaged samples, comment out

## prev.yr compare lencat by age
( alk.freq <- xtabs(~lcat10+age.num,data=RB.ages.lk.catch.prev) )
rowSums(alk.freq)

alk <- prop.table(alk.freq,margin=1)
round(alk,3)

str(RB.noages.lk.catch.prev)
# prev.yr predict individual ages from unaged sample (Isermann+Knight 2005 method)
RB.noages.lk.catch.prev <- alkIndivAge(alk,age.num~fl,data=RB.noages.lk.catch.prev)






# 
# # # # # # # # # # # # 
# #### RB Current year ###
# # # # # # # # # # # # 
# 
## current yr aged sample: ##
tmp <- lk.catch.prev %>%
  dplyr::filter(yearF %in% yr.select,  sp %in% "RB", !is.na(age.num)) %>%
  select(year, fl, lcat10) %>%
  arrange(lcat10)
(lcat10.toosmall.current <- min(tmp$lcat10, na.rm=T)) # cannot predict beyond this minimum so filter out for predicting ages.
(n.toosmall.current <- length(which(lk.catch.prev$fl < min(tmp$lcat10, na.rm=T)& lk.catch.prev$year %in% yr.select)))

RB.ages.lk.catch <- lk.catch.prev %>%
  dplyr::filter(!is.na(age.num)|fl < min(tmp$lcat10, na.rm=T), yearF %in% yr.select,  sp %in% "RB")
## current yr unaged sample: ##
RB.noages.lk.catch <- lk.catch.prev %>%
  dplyr::filter(is.na(age.num)&fl > min(tmp$lcat10, na.rm=T), yearF %in% yr.select,  sp %in% "RB")
nrow(RB.noages.lk.catch) #if none, comment out
## current yr compare lencat by age
( alk.freq <- xtabs(~lcat10+age.num,data=RB.ages.lk.catch) )
rowSums(alk.freq)

alk <- prop.table(alk.freq,margin=1)
round(alk,3)




# current yr predict individual ages from unaged sample:
lk.catch.prev %>%
  dplyr::filter(!is.na(age.num), yearF %in% yr.prev, sp %in% "RB")
RB.noages.lk.catch <- alkIndivAge(alk,age.num~fl,data=RB.noages.lk.catch)

#
RB.all <- RB.ages.lk.catch.prev %>%
  full_join(RB.noages.lk.catch.prev) %>%
  full_join(RB.noages.lk.catch) %>%
  full_join(RB.ages.lk.catch) %>%
  mutate(aged.predict = ifelse(is.na(age), "predicted","measured"))

RB.all



# # # # # # # # # # # 
# #### EB Previous year ### comment out if no EB or not unaged EB
# # # # # # # # # # # # 

tmp <- lk.catch.prev %>%
  dplyr::filter(yearF %in% yr.prev,  sp %in% "EB", !is.na(age.num)) %>%
  select(year, fl, lcat10) %>%
  arrange(lcat10)
min(tmp$lcat10, na.rm=T) # cannot predict beyond this minimum so filter out for predicting ages.
(n.toosmall.prev <- length(which(lk.catch.prev$fl < min(tmp$lcat10, na.rm=T)&
                                   lk.catch.prev$year %in% yr.prev)))

## prev.yr aged sample: ##
EB.ages.lk.catch.prev <- lk.catch.prev %>%
  dplyr::filter(!is.na(age.num)|fl < min(tmp$lcat10, na.rm=T), yearF %in% yr.prev, sp %in% "EB")
## prev.yr unaged sample: ##
EB.noages.lk.catch.prev <- lk.catch.prev %>%
  filter(is.na(age.num)&fl > min(tmp$lcat10, na.rm=T), yearF %in% yr.prev, sp %in% "EB")
nrow(EB.noages.lk.catch.prev)
## prev.yr compare lencat by age
( alk.freq <- xtabs(~lcat10+age.num,data=EB.ages.lk.catch.prev) )
rowSums(alk.freq)

alk <- prop.table(alk.freq,margin=1)
round(alk,3)

# prev.yr predict individual ages from unaged sample:
EB.noages.lk.catch.prev <- alkIndivAge(alk,age.num~fl,data=EB.noages.lk.catch.prev)


# # # # # # # # # # # # #
# # #### EB Current year ###
# # # # # # # # # # # # #
tmp <- lk.catch.prev %>%
  dplyr::filter(yearF %in% yr.select,  sp %in% "EB", !is.na(age.num)) %>%
  select(year, fl, lcat10) %>%
  arrange(lcat10)
(lcat10.toosmall.current <- min(tmp$lcat10, na.rm=T)) # cannot predict beyond this minimum so filter out for predicting ages.
(n.toosmall.current <- length(which(lk.catch.prev$fl < min(tmp$lcat10, na.rm=T)& lk.catch.prev$year %in% yr.select)))

## current yr aged sample: ##
EB.ages.lk.catch <- lk.catch.prev %>%
  filter(!is.na(age.num)|fl < min(tmp$lcat10, na.rm=T), yearF %in% yr.select,  sp %in% "EB")
## current yr unaged sample: ##
EB.noages.lk.catch <- lk.catch.prev %>%
  filter(is.na(age.num)&fl > min(tmp$lcat10, na.rm=T), yearF %in% yr.select,  sp %in% "EB")
nrow(EB.noages.lk.catch)
## current yr compare lencat by age
( alk.freq <- xtabs(~lcat10+age.num,data=EB.ages.lk.catch) )
rowSums(alk.freq)

alk <- prop.table(alk.freq,margin=1)
round(alk,3)

# current yr predict individual ages from unaged sample:
EB.noages.lk.catch <- alkIndivAge(alk,age.num~fl,data=EB.noages.lk.catch)

EB.all <- EB.ages.lk.catch.prev %>%
  full_join(EB.noages.lk.catch.prev) %>%
  full_join(EB.noages.lk.catch) %>%
  full_join(EB.ages.lk.catch) %>%
  mutate(aged.predict = ifelse(is.na(age), "predicted","measured"))
EB.all

# fit3.eb <- lm(logL~age.num*yearF, data=EB.all)
# car::Anova(fit3.eb)
#
# ### Combine RB and EB ###
#
 fish.all <- RB.all %>%
  full_join(EB.all)
# 
# 
# *** alternatively, if do not have two species:
# fish.all <- RB.all
# unique(fish.all$yearF)


# check if signficant difference in length distribution to ages between years
# str(fish.all)
# mod1 <- nnet::multinom(age.num~lcat10,data=fish.all,maxit=500)
# mod2 <- nnet::multinom(age.num~lcat10*yearF,data=fish.all,maxit=500)
# 
# anova(mod1,mod2)



### Von Bert. curves: ####

#RB- previous year:
yr.prev

prev.RB.all <- RB.all %>% 
  dplyr::filter(yearF %in% as.character(yr.prev))
names(prev.RB.all)
( svTyp <- vbStarts(fl~age.num,data=prev.RB.all) )
svTyp <- list(Linf=max(prev.RB.all$fl,na.rm=TRUE),K=0.3,t0=0)

vbTyp <- function(age,Linf,K,t0) Linf*(1-exp(-K*(age-t0)))
fitTyp <- nls(fl~vbTyp(age.num,Linf,K,t0),data=prev.RB.all,start=svTyp)

(prev.RB.all.coeff <- coef(fitTyp))

#confint(fitTyp) #one type of confidence interval...see below for other

residPlot(fitTyp,loess = T) # if this plot makes a funnel shape, see Ogle's IFAR book for alternate method

x.prev <- seq(0,max(prev.RB.all$age.num, na.rm=T),length.out=199)        # ages for prediction
pTyp.prev <- vbTyp(x.prev,Linf=coef(fitTyp)[1], K = coef(fitTyp)[2], t0 = coef(fitTyp)[3])   # predicted lengths

rb.prev.df <- data.frame(x.prev, pTyp.prev)

### Trouble-shooting: ###

#I have found that if you only have 4 ages or fewer in the samples, 
# the parameters are difficult to estimate.

#also if there is a small sample size? I think this is another reason parameters cannot be estimated. 


# RB - current year:

yr.select

curr.RB.all <- RB.all %>% 
  dplyr::filter(yearF %in% (yr.select))
names(curr.RB.all)
( svTyp <- vbStarts(fl~age.num,data=curr.RB.all) )
svTyp <- list(Linf=max(curr.RB.all$fl,na.rm=TRUE),K=0.3,t0=0)

vbTyp <- function(age,Linf,K,t0) Linf*(1-exp(-K*(age-t0)))

fitTyp <- nls(fl~vbTyp(age.num,Linf,K,t0),data=curr.RB.all,start=svTyp)

curr.RB.all.coeff <- coef(fitTyp)

confint(fitTyp) #one type of confidence interval...see below for other

residPlot(fitTyp,loess = T) # if this plot makes a funnel shape, see Ogle's IFAR book for alternate method

x.curr <- seq(0,max(curr.RB.all$age.num, na.rm=T),length.out=199)        # ages for prediction
pTyp.curr <- vbTyp(x.curr,Linf=coef(fitTyp)[1], K = coef(fitTyp)[2], t0 = coef(fitTyp)[3])

rb.curr.df <- data.frame(x.curr, pTyp.curr)




# # # EB- previous year: #note doesn't work for Boot - not enough ages
# yr.prev
# 
# prev.EB.all <- EB.all %>%
#   dplyr::filter(yearF %in% as.character(yr.prev))
# names(prev.EB.all)
# ( svTyp <- vbStarts(fl~age.num,data=prev.EB.all) )
# svTyp <- list(Linf=max(prev.EB.all$fl,na.rm=TRUE),K=0.3,t0=0)
# 
# vbTyp <- function(age,Linf,K,t0) Linf*(1-exp(-K*(age-t0)))
# fitTyp <- nls(fl~vbTyp(age.num,Linf,K,t0),data=prev.EB.all,start=svTyp)
# 
# (prev.EB.all.coeff <- coef(fitTyp))
# 
# #confint(fitTyp) #one type of confidence interval...see below for other
# 
# residPlot(fitTyp,loess = T) # if this plot makes a funnel shape, see Ogle's IFAR book for alternate method
# 
# x.prev <- seq(0,max(prev.EB.all$age.num, na.rm=T),length.out=199)        # ages for prediction
# pTyp.prev <- vbTyp(x.prev,Linf=coef(fitTyp)[1], K = coef(fitTyp)[2], t0 = coef(fitTyp)[3])   # predicted lengths
# 
# 
# # EB - current year:
# 
# yr.select
# 
# curr.EB.all <- EB.all %>%
#   dplyr::filter(yearF %in% (yr.select))
# names(curr.EB.all)
# ( svTyp <- vbStarts(fl~age.num,data=curr.EB.all) )
# svTyp <- list(Linf=max(curr.EB.all$fl,na.rm=TRUE),K=0.3,t0=0)
# 
# vbTyp <- function(age,Linf,K,t0) Linf*(1-exp(-K*(age-t0)))
# 
# fitTyp <- nls(fl~vbTyp(age.num,Linf,K,t0),data=curr.EB.all,start=svTyp)
# 
# curr.EB.all.coeff <- coef(fitTyp)
# 
# confint(fitTyp) #one type of confidence interval...see below for other
# 
# residPlot(fitTyp,loess = T) # if this plot makes a funnel shape, see Ogle's IFAR book for alternate method
# 
# x.curr <- seq(0,max(curr.EB.all$age.num, na.rm=T),length.out=199)        # ages for prediction
# pTyp.curr <- vbTyp(x.curr,Linf=coef(fitTyp)[1], K = coef(fitTyp)[2], t0 = coef(fitTyp)[3])
# #pTyp.curr <- vbTyp(x,Linf=max(curr.EB.all$fl,na.rm=TRUE),K=0.3,t0=0)   # THIS IS BOGUS DATA - added in b/c wanted a line...



#Von B labels:
(label.RBcurrentyr <- paste0(yr.select,": ",round(curr.RB.all.coeff[1],2),
                 "*","(1-exp(-",round(curr.RB.all.coeff[2],2),
                 "*","(age - ",round(curr.RB.all.coeff[3],2),"))"))
(label.RBprevyr <- paste0(yr.prev,": ",round(prev.RB.all.coeff[1],2),
                           "*","(1-exp(-",round(prev.RB.all.coeff[2],2),
                           "*","(age - ",round(prev.RB.all.coeff[3],2),"))"))

#shape=aged.predict     shape="aging", #remove if no samples were predicted 

# plot with von B, measured and predicted ages


fish.ages.plot.RB <- ggplot(RB.all)+
  geom_jitter(data=RB.all, aes(x=age.num, y=fl, col=yearF, shape=aged.predict),width = 0.15,
              size=3, alpha=0.6)+
  #geom_smooth(data=fish.all, aes(x=age.num, y=fl, col=yearF), method="loess")+
  geom_line(data=rb.prev.df, aes(x=x.prev, y=pTyp.prev), col="black", size=1.5)+
  geom_line(data=rb.curr.df, aes(x=x.curr, y=pTyp.curr), col="purple", size=1.5)+
  annotate(geom = "text", x = max(RB.all$age.num, na.rm=T)-1, y = 50, label = label.RBcurrentyr, parse=F)+
  annotate(geom = "text", x = max(RB.all$age.num, na.rm=T)-1, y = 10, label = label.RBprevyr, parse=F)+
  scale_x_continuous(breaks = seq(1, max(RB.all$age.num, na.rm=T),1),
                     minor_breaks = 1)+
  scale_y_continuous(limits = c(0,max(RB.all$fl+25, na.rm=T)), 
                                breaks = seq(0, max(RB.all$fl+50, na.rm=T),100))+
  #facet_wrap(~sp)+
  scale_colour_manual(values=c("black", "purple"))+
  labs(x="Age", y="Fork Length (mm)", colour="Year:", shape="Ages:")+
  theme_bw()+
  theme(legend.position = "bottom")

fish.ages.plot.RB




 #

### MORTALITY ####
# note that this is unlikely to work if there are fewer than 5 ages in the catch

curr.RB.all <- RB.all %>% 
  dplyr::filter(yearF %in% (yr.select)) %>% 
  dplyr::filter(!is.na(age.num)) %>% 
  group_by(age.num) %>% 
  summarize(catch=length(age.num))

ggplot(curr.RB.all)+
  geom_point(aes(x=age.num, y=catch))
#
prev.RB.all <- RB.all %>% 
  dplyr::filter(yearF %in% (prev.yr)) %>% 
  dplyr::filter(!is.na(age.num)) %>% 
  group_by(age.num) %>% 
  summarize(catch=length(age.num))

ggplot(prev.RB.all)+
  geom_point(aes(x=age.num, y=catch))


##
curr.EB.all <- EB.all %>% 
  dplyr::filter(yearF %in% (yr.select)) %>% 
  dplyr::filter(!is.na(age.num)) %>% 
  group_by(age.num) %>% 
  summarize(catch=length(age.num))

ggplot(curr.EB.all)+
  geom_point(aes(x=age.num, y=catch))

#

prev.EB.all <- EB.all %>% 
  dplyr::filter(yearF %in% (prev.yr)) %>% 
  dplyr::filter(!is.na(age.num)) %>% 
  group_by(age.num) %>% 
  summarize(catch=length(age.num))

ggplot(prev.EB.all)+
  geom_point(aes(x=age.num, y=catch))

#






## EXTRAS ####

# #take a look at proportional size distribution again with Gabelhouse rules
# rb.cuts <- psdVal("Rainbow Trout")
# 
# catch.psd <- catch %>% 
#   mutate(fl.cat = lencat(fl, breaks= rb.cuts, use.names=T, drop.levels = T)) %>% 
#   filter(sp %in% "RB") %>% 
#   filter(fl >= rb.cuts["stock"])
# range(catch.psd$fl)
# 
# headtail(catch.psd)
# 
# (psd.freq <- xtabs(~fl.cat, data=catch.psd))
# 
# rb.cuts2 <- c(Sub_stock=0,Stock=200, Quality=400, Trophy=550, Over=1000)
# lk.select <- "Boot"
# yr.select <- c("2017","2018","2019")
# 
# catch.psd.lkyr <- catch %>% 
#   mutate(fl.cat = lencat(fl, breaks= rb.cuts2, use.names=T, drop.levels = T)) %>% 
#   filter(sp %in% "RB") %>% 
#   filter(fl >= rb.cuts["stock"]) %>% 
#   filter(lake %in% lk.select) %>% 
#   filter(year %in% yr.select)
# 
# (psd.freq.lk <- xtabs(~fl.cat, data=catch.psd.lkyr))
# (psd.lk <- prop.table(psd.freq.lk)*100 )
# 
# 

### FFSBC data submission ####

lk.profile <- read_excel("database_uploads_template_v8.2.xlsx",sheet = "Lake_Profile", na = "NA")

str(lk.profile)

str(effort)
str(env)
str(catch)


yr.select <- "2019"

catch.sum <- effort %>% 
  full_join(catch, by = c("lake", "year", "effortid")) %>% 
  dplyr::filter(year %in% yr.select) %>% 
  group_by(year, lake, sp, nettype) %>% 
  dplyr::summarize(n = length(sp))
catch.sum  






