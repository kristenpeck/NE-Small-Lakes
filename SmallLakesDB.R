
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
catch <- read_excel("SmallLakesDB-copy.xlsx",sheet = "Catch", na = "NA", 
                    col_types = c("guess","guess","guess","guess","guess",
                                  "guess","guess","guess","guess","guess",
                                  "guess","numeric","text","text", "guess",
                                  "guess","guess"))
catch <- catch %>% 
  mutate(fl.cat = lencat(fl, breaks= c(Sub_stock=0,Stock=200, Quality=400, 
                                       Trophy=550, 1000), use.names=T)) %>% #PSD from Paul Askeys rapid assessment tool
  mutate(k = (100000*m)/fl^3, 
         mat2 = ifelse(mat == "IM"|mat == "ST"|mat == "af3n", "IM/ST", mat),
         logm = log10(m),logL = log10(fl)) %>% #in cases where IM was confused with ST, combined
  arrange(sp)
catch$catchID <- 1:nrow(catch)

str(catch)


env <- read_excel("SmallLakesDB-copy.xlsx",sheet = "Enviro", na = "NA")
str(env)

#### Select Year-Effort ####

yr.select <- c(2017)

effort.selectyr <- effort %>% 
  filter(year %in% yr.select)

#### Maps ####


str(effort.selectyr)

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

print(lk.overview.map) #not printing?? *** TO FIX ****



#### Appendix: gillnets maps 2019: ####

unique(locations.lk$lake)

lk.select <- "Moose"

indiv.nets <- locations %>% 
  filter(lake %in% lk.select)

indiv.nets.st <- st_as_sf(indiv.nets, 
                   coords = c("UTME", "UTMN"), 
                   crs = 3157)

lks.albers <- st_transform(indiv.nets.st, crs=3005)


#overview map alternative - did not use because ugly

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
  dplyr::group_by(lake, effortidF) %>% 
  dplyr::summarize() %>% 
  st_cast("LINESTRING") # this suddenly is not working?? To work on later...
    
net.lines<- lks.albers %>% 
  filter(effortidF %in% c("1","2")) %>% 
  group_by(lake, nettype) %>% 
  dplyr::summarize() %>% 
  st_cast("LINESTRING") 
plot(net.lines)

      # ggplot()+
      #   geom_sf(data=bc, fill="white")+
      #   geom_sf(data=Peace, fill="tan")+
      #   geom_sf(data=lks.mapping, col="blue")+
      #   geom_sf(data=lks.albers, size=2)

# Appendix - plot nets at individual lakes

lk.select <- "One Island"

lk.nets.select <- lks.albers %>% 
  filter(lake %in% lk.select)

lk.net.lines.select <- net.lines %>% 
  filter(lake %in% lk.select)

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









### CTD profiles ####

yr.select <- 2017

env.selectyr <- env %>% 
  mutate(year = year(date)) %>% 
  filter(year %in% yr.select) %>% 
  gather("var","value",tempdown, dodown, -conddown)


Figure.env <- ggplot(data=env.selectyr)+
  geom_path(aes(x=value, y=depthdown, colour=var), size=1.5)+
  scale_y_reverse(name= "Depth (m)", 
                  breaks=seq(min(env.selectyr$depthdown, na.rm=T),
                             max(env.selectyr$depthdown, na.rm=T),1))+
  scale_x_continuous(name = "",breaks=seq(min(env.selectyr$value, na.rm=T),
                                          max(env.selectyr$value, na.rm=T),1))+
  scale_colour_manual(values=c("black", "gray60"),
                      name = "",labels = c("Diss. Oxygen (mg/L)","Temp. (deg C)"))+
  facet_wrap(~lake, ncol=2)+
  theme_bw()
Figure.env

# NOTE: no environmental data taken from:
  # 2017 - Moose, Inga, One Island, Sundance
  # 2018 - all measured!
  # 2019 - Sundance













#### Select Year-catch ####
yr.select <- 2017

catch.selectyr <- catch %>% 
  filter(year %in% yr.select) 

effort.selectyr <- effort %>% 
  filter(year %in% yr.select)

catch.effort.selectyr <- full_join(effort.selectyr, catch.selectyr, by=c("lake", "year", "effortid"))



#### Tables - CPUE, demographics ####

str(catch.effort.selectyr)

Table1 <- catch.effort.selectyr %>% 
  filter(sp %in% c("RB","EB")) %>% 
  dplyr::group_by(lake,effortid, nettype) %>% 
  dplyr::summarise(`Soak Time (hrs)`=unique(efforthr), `Net Type`=unique(nettype) , 
                   Year=unique(year),`Species`=paste(unique(sp), collapse=","),`# caught` = length(unique(catchID)),
                   CPUE = round(length(unique(catchID))/unique(efforthr),2),
                   `FL range (mm)`=paste0(min(fl, na.rm=T),"-", max(fl, na.rm=T), collapse=","),
                   `m range (g)`=paste0(min(m, na.rm=T), "-",max(m, na.rm=T), collapse=","),
                   `k range`=paste0(round(min(k, na.rm=T),2), "-",round(max(k, na.rm=T),2),collapse=",")) %>% 
  dplyr::arrange(Year)
Table1


# table - demographics

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

lk.select = "Boot"

catch.selectyrlk <- catch.selectyr %>% 
  filter(lake %in% lk.select) 

unique(catch.selectyrlk$sp)

Figure.FL.sp <- ggplot(data=catch.selectyrlk) +
  geom_histogram(aes(x=fl, fill=sp), colour="black", binwidth= 10)+
  facet_wrap(~sp, ncol=1)+
  scale_y_continuous(breaks = seq(0,30,5))+
  labs(x="Fork Length (mm)", y="Frequency", fill="Species")+
  theme_bw()

Figure.FL.sp

#also need to download the photos from Chunamun still




#### RB FL frequency, by stock category ####

RB.catch.selectyr <- catch.selectyr %>% 
  filter(sp %in% "RB")

ggplot(data=RB.catch.selectyr) +
  geom_histogram(aes(x=fl, fill=fl.cat), colour = "black", binwidth= 20)+
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





#### Lk-specific: length-weight relationships ####
unique(catch.selectyr$year)
unique(catch.selectyr$lake)
lk.select = "Moose"

RB.catch.selectyrlk <- catch.selectyr %>% 
  filter(lake %in% lk.select) %>% 
  filter(sp %in% "RB")
str(RB.catch.selectyrlk)


fit1 <- lm(logm~logL, data=RB.catch.selectyrlk)
summary(fit1)

residPlot(fit1) #check for length-wt outliers in residual plot


### predicted weights ####

no.wt <- RB.catch.selectyrlk %>% 
  filter(!is.na(fl)) %>% 
  filter(is.na(m)) %>% 
  select(fl,logL)

pred.logwt <- predict(fit1, no.wt, interval ="prediction") #this is predicting individual fish weights, not average
cf <- logbtcf(fit1, 10)
back.trans <- cf*10^pred.logwt

no.wt$pred.m <- back.trans[,1]
no.wt$pred.m.lwr <- back.trans[,2]
no.wt$pred.m.upr <- back.trans[,3]

RB.catch.selectyrlk.temp <- RB.catch.selectyrlk %>% 
  full_join(no.wt)
# note: this is not a beautiful way of doing this, since it duplicates predicted weights 
#   for other weights that were measured but have the same value. 



#### Select Year-catch ####
yr.select <- 2017
lk.select = "Boot"

catch.selectyrlk <- catch %>% 
  filter(year %in% yr.select) %>% 
  filter(lake %in% lk.select) %>% 
  filter(sp %in% c("RB","EB"))



#length-weight plot - most recent year
ggplot(data=catch.selectyrlk[which(catch.selectyrlk$mat2 %in% "IM/ST"),]) +
  geom_point(aes(x=fl, y=m, col=mat2, shape=mat2), alpha=0.5, size=4)+
  geom_smooth(aes(x=fl, y=m), method="loess")+
  facet_wrap(~sp)+
  ggtitle(paste(catch.selectyrlk$lake,"Lake,",catch.selectyrlk$year))+
  theme_bw()

ggplot(data=catch.selectyrlk) +
  geom_point(aes(x=logL, y=logm, col=mat2, shape=mat2), size=4)+
  geom_smooth(aes(x=logL, y=logm), method="lm")+
  facet_wrap(~sp)+
  ggtitle(paste(catch.selectyrlk$lake,"Lake,",catch.selectyrlk$year))+
  theme_bw()


#### length-weight compare years ####
str(catch)
unique(catch$lake)

lk.select <- "Boot"


lake.temp <- catch %>% 
  filter(lake %in% lk.select) %>% 
  filter(surveytype %in% "gillnet") %>% 
  arrange(year)
(sampled.yrs <- unique(lake.temp$year))

sampled.yrs[length(sampled.yrs)-1]


lk.catch.prev <- catch %>% 
  filter(lake %in% lk.select) %>% 
  filter(year %in% c(sampled.yrs[length(sampled.yrs)-2],sampled.yrs[length(sampled.yrs)])) %>% 
  mutate(yearF = as.factor(year)) %>% 
  filter(sp %in% c("RB","EB")) %>% 
  filter(fl >= 162 | fl <= 130) %>% #this will make 6-panel and 7-panel nets more equal
  mutate(logm = log10(m),logL = log10(fl)) %>% 
  arrange(yearF)

lk.catch.prev.rb <- catch %>% 
  filter(lake %in% lk.select, sp %in% "RB") %>% 
  filter(year %in% c(sampled.yrs[length(sampled.yrs)-2],sampled.yrs[length(sampled.yrs)])) %>% 
  mutate(yearF = as.factor(year)) %>% 
  filter(fl >= 162 | fl <= 130) %>% #this will make 6-panel and 7-panel nets more equal
  mutate(logm = log10(m),logL = log10(fl)) %>% 
  arrange(yearF)

lk.catch.prev.eb <- catch %>% 
  filter(lake %in% lk.select, sp %in% "EB") %>% 
  filter(year %in% c(sampled.yrs[length(sampled.yrs)-2],sampled.yrs[length(sampled.yrs)])) %>% 
  mutate(yearF = as.factor(year)) %>% 
  filter(fl >= 162 | fl <= 130) %>% #this will make 6-panel and 7-panel nets more equal
  mutate(logm = log10(m),logL = log10(fl)) %>% 
  arrange(yearF)


#length-weight plot - compare years (generic)

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
  labs(title=lk.catch.prev$lake, x= "log10 Fork Length", y="log10 Mass")+
  theme_bw()
Figure.FLwt.compare

#rainbows only
Figure.FLwt.compare.rb <- ggplot() +
  geom_point(data=lk.catch.prev.rb, aes(x=logL, y=logm, col=yearF, shape=yearF),  alpha=0.4, size=4)+
  geom_smooth(data=lk.catch.prev.rb, aes(x=logL, y=logm, col=yearF), method="lm")+
  scale_colour_manual(name="Year",
                      labels = c(unique(lk.catch.prev.rb$year)[1],unique(lk.catch.prev.rb$year)[2]), 
                      values=c("black","blue"))+
  scale_shape_manual(name="Year",
                     labels = c(unique(lk.catch.prev.rb$year)[1],unique(lk.catch.prev.rb$year)[2]),
                     values=c(17, 19))+
  labs(title=paste(lk.catch.prev.rb$lake,"RB"), x= "log10 Fork Length", y="log10 Mass")+
  theme_bw()
Figure.FLwt.compare.rb

fit2.rb <- lm(logm~logL*yearF, data=lk.catch.prev.rb)
car::Anova(fit2.rb)

#brookies only
Figure.FLwt.compare.eb <- ggplot() +
  geom_point(data=lk.catch.prev.eb, aes(x=logL, y=logm, col=yearF, shape=yearF),  alpha=0.4, size=4)+
  geom_smooth(data=lk.catch.prev.eb, aes(x=logL, y=logm, col=yearF), method="lm")+
  scale_colour_manual(name="Year",
                      labels = c(unique(lk.catch.prev.eb$year)[1],unique(lk.catch.prev.eb$year)[2]), 
                      values=c("black","blue"))+
  scale_shape_manual(name="Year",
                     labels = c(unique(lk.catch.prev.eb$year)[1],unique(lk.catch.prev.eb$year)[2]),
                     values=c(17, 19))+
  labs(title=paste(lk.catch.prev$lake, "EB"), x= "log10 Fork Length", y="log10 Mass")+
  theme_bw()
Figure.FLwt.compare.eb


fit2.eb <- lm(logm~logL*yearF, data=lk.catch.prev.eb)

car::Anova(fit2.eb) #note, if interaction is present then that should be interpreted before year effect


# Perhaps next, backtransform and label the slope and exponent on figure?


#Next:

# - length at age
# - von B plot
# 

#### length at age ####

lk.select <- "Boot"
yr.select <- c("2017","2018","2019")


# lake.temp <- catch %>% 
#   filter(lake %in% lk.select) %>% 
#   filter(year %in% yr.select) %>% 
#   arrange(year)
# (sampled.yrs <- unique(lake.temp$year))
# 
# sampled.yrs[length(sampled.yrs)-1]

unique(catch$age)
catch.ages.lk <- catch %>% 
  filter(sp %in% c("RB","EB")) %>% 
  filter(lake %in% lk.select) %>% 
  filter(year %in% yr.select) %>% 
  mutate(age.num = as.numeric(substr(age,1,1))) %>% 
  mutate(broodyear = ifelse(!is.na(age.num),year-age.num, NA))
catch.ages.lk
catch.ages.lk$broodyear



ggplot(data=catch.ages.lk)+
  geom_point(aes(x=age.num, y=fl))+
  geom_smooth(aes(x=age.num, y=fl),method = "lm")+
  ggtitle(catch.ages.lk$lake, catch.ages.lk$year)










#take a look at proportional size dstribution again with Gabelhouse rules
rb.cuts <- psdVal("Rainbow Trout")

catch.psd <- catch %>% 
  mutate(fl.cat = lencat(fl, breaks= rb.cuts, use.names=T, drop.levels = T)) %>% 
  filter(sp %in% "RB") %>% 
  filter(fl >= rb.cuts["stock"])
range(catch.psd$fl)

headtail(catch.psd)

(psd.freq <- xtabs(~fl.cat, data=catch.psd))

rb.cuts2 <- c(Sub_stock=0,Stock=200, Quality=400, Trophy=550, Over=1000)
lk.select <- "Boot"
yr.select <- c("2017","2018","2019")

catch.psd.lkyr <- catch %>% 
  mutate(fl.cat = lencat(fl, breaks= rb.cuts2, use.names=T, drop.levels = T)) %>% 
  filter(sp %in% "RB") %>% 
  filter(fl >= rb.cuts["stock"]) %>% 
  filter(lake %in% lk.select) %>% 
  filter(year %in% yr.select)

(psd.freq.lk <- xtabs(~fl.cat, data=catch.psd.lkyr))
(psd.lk <- prop.table(psd.freq.lk)*100 )















