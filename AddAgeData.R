

# This script is to import the BPAL ageing results into the small lakes database 
#  (excel workbook on regional drive)

library(readxl)
library(plyr)
library(dplyr)
library(lubridate)
library(ggplot2)

# File location of age data *NOTE: this will need to be changed every time we get a new file*

age.BCPAL <- read_excel(path = "//SFP.IDIR.BCGOV/S140/S40023/Environmental Stewardship/Fish/DATA/AGEING/2018-19/2018 Reg7B.Stewart&Heart_BCPAL_agedFeb19.20.xlsx",
                      sheet = "7B.2018Stewrt&Heart.BCPAL.aged")


str(age.BCPAL)
names(age.BCPAL)

names(age.BCPAL)[15:17] <- c("Aged_Brood_Year", "Otolith_Age", "Age_comments")
unique(age.BCPAL$Otolith_Age)

level_key <- c("No Age"=NA,"0+"=0, "1+"=1, "2+"=2, "3+"=3, 
               "4+"=4, "5+"=5, "6+"=6, "7+"=7, "8+"=8,"1"=1,"2"=2,"3"=3,"4"=4,"5"=5,"6"=6,
               "7"=7)

ages <- age.BCPAL %>% 
 # mutate(year = year(Date)) %>% 
  mutate(PAL_age = dplyr::recode(Otolith_Age, !!!level_key)) %>% 
  #select(year, Fish_No, Length_mm, Aged_Brood_Year, Scale_Age, PAL_age, Age_comments) %>% 
  select(year, `sample number`, length, Aged_Brood_Year, Otolith_Age, PAL_age, Age_comments)

ages[, c("Otolith_Age","PAL_age")]


#small lakes db (on drive) (this should remain stable, but note this is just the copy)

effort <- read_excel("SmallLakesDB-copy.xlsx",sheet = "Effort")

catch <- read_excel("SmallLakesDB-copy.xlsx",sheet = "Catch", na = "NA")

str(catch)




# add the year class, age and age comments to the regional db

catch.w.ages  <- catch %>% 
  filter(year %in% 2018) %>% 
  left_join(ages, by = c("year", "ageid" = "sample number")) %>% 
  mutate(broodyear = Aged_Brood_Year, age = PAL_age, agecomment = Age_comments) #%>% 
  #select(-c(Aged_Brood_Year, PAL_age, Age_comments, Length_mm, Scale_Age))



#short QA:

ggplot(data=catch.w.ages)+
  geom_jitter(aes(x=age, y=fl, col=lake),height=0, width=0.2)

ggplot(data=catch.w.ages)+
  geom_boxplot(aes(x=age,group=age, y=fl), width=0.2)+
  geom_jitter(aes(x=age, y=fl, col=lake),height=0, width=0.2)



#write.csv(catch.w.ages, "catch.w.ages.temp.csv",na = "", row.names=F)




### FFSBC data submission catch summary ####
# 
# lk.profile <- read_excel("database_uploads_template_v8.2.xlsx",sheet = "Lake_Profile", na = "NA")
# 
# str(lk.profile)


yr.select <- "2018"

catch.sum <- effort %>% 
  full_join(catch, by = c("lake", "year", "effortid")) %>% 
  dplyr::filter(year %in% yr.select) %>% 
  group_by(year, lake, sp, nettype) %>% 
  dplyr::summarize(n = length(sp))
catch.sum  

#fill out biological information:

catch.effort <- effort %>% 
  full_join(catch, by = c("lake", "year", "effortid"))

FFSBC <- read_excel("database_uploads_template_v8.2.xlsx",sheet = "Biological Data")
names(FFSBC)
names(catch.effort)
str(merged)

merged<- catch.effort %>% 
  filter(year %in% yr.select) %>% 
  mutate(`Assessment ID` = NA, Waterbody_Name=lake,Date=ymd(liftdate),Capture_Method="GN", Fish_No=NA,
         Clip=NA, Species=sp, Strain = NA, Ploidy = NA, Length_mm=fl,Weight_g=m,Condition_Factor=NA,
         Sex=sex, Age=age, Aged_Brood_Year=broodyear, Aged_Method=NA, Maturity=mat, Life_Stage=NA,
         Otolith = ifelse(agestructure %in% "OT","Y",NA), Scale=ifelse(agestructure %in% "SC","Y",NA),
         DNA_ID=NA, Net_ID=netid, Net_Mesh_Size=NA, Stomach_Contents=stomach, 
         Comments=paste0(ageid,";",comments.y))
merged.FFSBC <- merged[,38:62]

write.csv(merged.FFSBC, "Biological_Data_FFSBCtemplate.csv", na = "", row.names = F)





