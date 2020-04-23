
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
library(sf)
library(mapview)
library(bcdata)
library(reshape2)
library(car)

effort <- read_excel("SmallLakesDB-copy.xlsx",sheet = "Effort")
str(effort)
catch <- read_excel("SmallLakesDB-copy.xlsx",sheet = "Catch", na = "NA")
str(catch)
env <- read_excel("SmallLakesDB-copy.xlsx",sheet = "Enviro", na = "NA")
str(env)

#### Select Year ####

yr.select <- c(2019)

effort.selectyr <- effort %>% 
  filter(year %in% yr.select)

catch.selectyr <- catch %>% 
  mutate(fl.cat = lencat(fl, breaks= c(Sub_stock=0,Stock=200, Quality=400, 
                                       Trophy=550, 1000), use.names=T)) %>% 
  mutate(k = (100000*m)/fl^3) %>% 
  filter(year %in% yr.select) %>% 
  arrange(sp)

catch.selectyr$catchID <- 1:nrow(catch.selectyr)

catch.effort.selectyr <- full_join(effort.selectyr, catch.selectyr, by=c("lake", "year", "effortid"))

#### Table 1 - net set, CPUE ####

str(catch.effort.selectyr)

#### CPUE ####

# soaktime <- ddply(effort2019,~lake, summarize, soak.time = sum(efforthr))
# tot.catch <- ddply(catch2019,~lake, summarize, tot.catch = length(sp))
# 
# CPUE <- soaktime %>% 
#   full_join(tot.catch) %>% 
#   mutate(cpue = round(tot.catch/soak.time,2))
# CPUE


Table1 <- catch.effort.selectyr %>% 
  filter(sp %in% c("RB","EB")) %>% 
  group_by(lake,nettype) %>% 
  summarise(`Soak Time (hrs)`=unique(efforthr), Year=unique(year),
            `Species`=paste(unique(sp), collapse=","), `# caught` = length(unique(catchID)),
            CPUE = round(length(unique(catchID))/unique(efforthr),2),
            `FL range (mm)`=paste0(min(fl, na.rm=T),"-", max(fl, na.rm=T), collapse=","),
            `m range (g)`=paste0(min(m, na.rm=T), "-",max(m, na.rm=T), collapse=","),
            `k range`=paste0(round(min(k, na.rm=T),2), "-",round(max(k, na.rm=T),2),collapse=",")) %>% 
  arrange(Year)
Table1



# table -old

table.fish <- ddply(catch.selectyr, ~lake, summarize,
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

#### Maps ####



str(effort.selectyr)

locations.easting <- effort.selectyr %>% 
  select(lake, nettype, starteast, endeast) %>% 
  melt() %>% 
  mutate(startend = ifelse(grepl("start",variable),yes = "start",no="end")) %>% 
  select(lake, nettype, startend, UTME=value)

locations.northing <- effort.selectyr %>% 
  select(lake, nettype, startnorth, endnorth) %>% 
  melt() %>% 
  mutate(startend = ifelse(grepl("start",variable),yes = "start",no="end")) %>% 
  select(lake, nettype, startend, UTMN=value)


(locations <- locations.easting %>% 
    full_join(locations.northing, by=c("lake", "nettype", "startend")) %>% 
    filter(!is.na(UTME)))
#no coords for Heart or Stewart

# Figure 1 - overview map: ####

locations.lk <- locations %>% 
  filter(startend %in% "start", nettype %in% "RIC7 SGN") %>% 
  st_as_sf(coords = c("UTME", "UTMN"), 
           crs = 3157)

lk.overview.map <- mapview(locations.lk, cex=2, lwd=1, legend=F,map.types="OpenStreetMap") %>% 
  leafem::addStaticLabels(label = locations.lk$lake,
                  noHide = F,
                  direction = 'top',
                  textOnly = TRUE,
                  textsize = "20px")
  
print(lk.overview.map)



#### Appendix: gillnets locations: ####

indiv.nets <- locations %>% 
  filter(lake %in% c("Boulder","Chunamun","Pete","Quality","Sundance"))

indiv.nets.st <- st_as_sf(indiv.nets, 
                   coords = c("UTME", "UTMN"), 
                   crs = 3157)

lks.albers <- st_transform(indiv.nets.st, crs=3005)


#overviewmap alternative - did not use because ugly

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

net.lines<- lks.albers %>% 
  group_by(lake, nettype) %>% 
  summarize() %>% 
  st_cast("LINESTRING")

# ggplot()+
#   geom_sf(data=bc, fill="white")+
#   geom_sf(data=Peace, fill="tan")+
#   geom_sf(data=lks.mapping, col="blue")+
#   geom_sf(data=lks.albers, size=2)




# Appendix - plot nets at individual lakes

lake.select <- "Quality"


lk.nets.select <- lks.albers %>% 
  filter(lake %in% lake.select)

lk.net.lines.select <- net.lines %>% 
  filter(lake %in% lake.select)

#could not get a good enough basemap so did not use the following:
# lks.outline.select <- bcdc_query_geodata('cb1e3aba-d3fe-4de1-a2d4-b8b6650fb1f6') %>%
#   filter(INTERSECTS(lk.nets.select)) %>%
#   collect()
# 
# background <- st_buffer(st_as_sfc(st_bbox(lks.outline.select),crs=3005),100)
# roads <- bcdc_query_geodata("bb060417-b6e6-4548-b837-f9060d94743e") %>% 
#   filter(INTERSECTS(background)) %>% 
#   collect() %>% 
#   st_crop(background)
# 
# ggplot()+
#   geom_sf(data=roads, colour="tan")+
#   geom_sf(data=lks.outline.select, fill="light blue")+
#   geom_sf(data=lk.net.lines.select, aes(colour=nettype))+
#   geom_sf(data=lk.nets.select, aes(colour=nettype))+
#   ggtitle(paste(lake.select, "Lake"))+
#   scale_colour_grey()


mapview(lk.net.lines.select, zcol="nettype",lwd=2, legend=T,map.types="Esri.WorldImagery",layer.name="Net Type",
        color = c("gray50","white"))


### Figure ENV, facet plots ####

lk.env2019 <- env %>% 
  mutate(year = year(date)) %>% 
  filter(year %in% 2019) %>% 
  gather("var","value",tempdown, dodown, -conddown)


Figure.env <- ggplot(data=lk.env2019)+
  geom_path(aes(x=value, y=depthdown, colour=var), size=1.5)+
  scale_y_reverse(name= "Depth (m)", 
                  breaks=seq(min(lk.env2019$depthdown, na.rm=T),
                             max(lk.env2019$depthdown, na.rm=T),1))+
  scale_x_continuous(name = "",breaks=seq(min(lk.env2019$value, na.rm=T),
                                          max(lk.env2019$value, na.rm=T),1))+
  scale_colour_manual(values=c("black", "gray60"),
                      name = "",labels = c("Diss. Oxygen (mg/L)","Temp. (deg C)"))+
  facet_wrap(~lake, ncol=2)+
  theme_bw()
Figure.env
# NOTE: no environmental data taken from Sundance in 2019


#### FL frequency, by Maturity ####

lk.catch.selectyr <- catch.selectyr %>% 
  filter(!is.na(ageid))
  
lk.catch.selectyr

Figure.FL.mat <- ggplot(data=catch.selectyr) +
  geom_histogram(aes(x=fl, fill=mat), colour = "black", binwidth= 50)+
  facet_wrap(~lake)+
  scale_y_continuous(breaks = seq(0,100,10))+
  labs(x="Fork Length (mm)", y="Frequency", fill="Maturity")+
  theme_bw()
Figure.FL.mat
 
#double-check that IM and ST are not mixed up at Chunamun
#also need to download the photos from Chunamun still




#### FL frequency, by stock type ####
#categories defined at top

stock.catch.selectyr <- catch.selectyr %>% 
  filter(sp %in% "RB")

ggplot(data=stock.catch.selectyr) +
  geom_histogram(aes(x=fl, fill=fl.cat), colour = "black", binwidth= 10)+
  facet_wrap(~lake)+
  labs(y="Frequency", x="Fork Length (mm)", fill="Category")+
  theme_bw()



#### length-weight relationships ####

lk.catch.selectyr <- catch.selectyr %>% 
  filter(lake %in% "Chunamun") %>% 
  filter(sp %in% "RB") %>% 
  mutate(logm = log10(m),logL = log10(fl))
str(lk.catch.selectyr)


#length-frequency plot
ggplot(data=lk.catch.selectyr) +
  geom_histogram(aes(x=fl, fill=fl.cat), colour = "black", binwidth= 10)+
  ggtitle(lk.catch.selectyr$lake)

#condition-frequency plot
ggplot(data=lk.catch.selectyr) +
  geom_histogram(aes(x=k, fill=fl.cat), colour = "black", binwidth= 0.05)+
  ggtitle(lk.catch.selectyr$lake)

fit1 <- lm(logm~logL, data=lk.catch.selectyr)
summary(fit1)

residPlot(fit1)
#predict weights of fish who didn't have a weight

no.wt <- lk.catch.selectyr %>% 
  filter(!is.na(fl)) %>% 
  filter(is.na(m)) %>% 
  select(fl,logL)

pred.logwt <- predict(fit1, no.wt, interval ="prediction")
cf <- logbtcf(fit1, 10)
back.trans <- cf*10^pred.logwt

no.wt$pred.m <- back.trans[,1]
no.wt$pred.m.lwr <- back.trans[,2]
no.wt$pred.m.upr <- back.trans[,3]

lk.catch.selectyr.temp <- lk.catch.selectyr %>% 
  full_join(no.wt)
# note: this is not a beautiful way of doing this, since it duplicates predicted weights 
#   for other weights that were measured but have the same value. 



#length-weight plot - most recent year
ggplot(data=lk.catch.selectyr) +
  geom_point(aes(x=logL, y=logm, col=mat),  size=4)+
  geom_smooth(aes(x=logL, y=logm), method="lm")+
  ggtitle(paste(lk.catch.selectyr$lake,"Lake,",lk.catch.selectyr$year))


#### length-weight compare years for a given lake ####
str(catch)
lake.select <- "Pete"

unique(catch$lake)

lake.temp <- catch %>% 
  filter(lake %in% lake.select) %>% 
  arrange(year)
(sampled.yrs <- unique(lake.temp$year))

sampled.yrs[length(sampled.yrs)-1]


lk.catch.prev <- catch %>% 
  filter(lake %in% lake.select) %>% 
  filter(year %in% c(sampled.yrs[length(sampled.yrs)-1],sampled.yrs[length(sampled.yrs)])) %>% 
  mutate(yearF = as.factor(year)) %>% 
  filter(sp %in% "RB") %>% 
  filter(fl >= 162 | fl <= 130) %>% #this will make 6-panel and 7-panel nets more equal
  mutate(logm = log10(m),logL = log10(fl))
lk.catch.prev

#length-weight plot - compare years

Figure.FLwt.compare <- ggplot() +
  geom_point(data=lk.catch.prev, aes(x=logL, y=logm, col=yearF, shape=yearF),  alpha=0.4, size=4)+
  geom_smooth(data=lk.catch.prev, aes(x=logL, y=logm, col=yearF), method="lm")+
  scale_colour_manual(values=c("black","blue"))+
  labs(title=lk.catch.prev$lake, x= "log10 Fork Length", y="log10 Mass", colour="Year")+
  theme_bw()
Figure.FLwt.compare

fit2 <- lm(logm~logL*yearF, data=lk.catch.prev)

car::Anova(fit2)



















