
# This script is to import the BPAL ageing results into the small lakes database 
#  (excel workbook on regional drive)

library(readxl)
library(plyr)
library(dplyr)
library(lubridate)
library(ggplot2)

# File location of age data *NOTE: this will need to be changed every time we get a new file*

age.BCPAL <- read_excel(path = "//SFP.IDIR.BCGOV/S140/S40023/Environmental Stewardship/Fish/DATA/AGEING/2019-20/Results-2019 Reg7B.SLA.Peck_BCPAL.Aged_Dec15.20.xlsx",
                      sheet = "2019-7B.SLA.BCPAL.Aged.Dec15.20")


str(age.BCPAL)

names(age.BCPAL)[27:29] <- c("Aged_Brood_Year", "Scale_Age", "Age_comments")
unique(age.BCPAL$Scale_Age)

level_key <- c("No Age"=NA,"0+"=0, "1+"=1, "2+"=2, "3+"=3, 
               "4+"=4, "5+"=5, "6+"=6, "7+"=7, "8+"=8)

ages <- age.BCPAL %>% 
  mutate(year = year(Date)) %>% 
  mutate(PAL_age = recode(Scale_Age, !!!level_key)) %>% 
  select(year, Fish_No, Length_mm, Aged_Brood_Year, Scale_Age, PAL_age, Age_comments)



#*note this will have to change slightly if ages are OTs in future

#small lakes db (on drive) (this should remain stable, but note this is just the copy)

catch <- read_excel("SmallLakesDB-copy.xlsx",sheet = "Catch", na = "NA")

str(catch)




# add the year class, age and age comments to the db

catch.w.ages  <- catch %>% 
  filter(year %in% 2019, !is.na(ageid)) %>% 
  left_join(ages, by = c("year", "ageid" = "Fish_No")) %>% 
  mutate(broodyear = Aged_Brood_Year, age = PAL_age, agecomment = Age_comments) %>% 
  select(-c(Aged_Brood_Year, PAL_age, Age_comments, Length_mm, Scale_Age))

#short QA:

ggplot(data=catch.w.ages)+
  geom_jitter(aes(x=age, y=fl, col=lake),height=0, width=0.2)





write.csv(catch.w.ages, "catch.w.ages.temp.csv",na = "", row.names=F)









