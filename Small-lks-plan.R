
# This script is in preparation for the Northeast Small Lakes Plan (2021)

#Author: K. Peck, started 12-Jun-2021

source("SmallLakesDB.R")
ls()




str(catch)
str(effort)
str(env)

unique(catch$lake)



stocked.lks <-  data.frame(lake = c("One Island","Boot","Heart", "Chunamun","Sundance",       
                                    "Moose","Pete","Snow","Boulder","Wright",      
                                    "Stewart","Quality","Inga","Borrow Pit 4", 
                                    "Borrow Pit 2","Borrow Pit 1","Borrow Pit 8"),
                           sp = c("RB+EB","RB+EB","EB","RB","RB",
                                  "RB","RB","RB","RB","RB",
                                  "RB","RB","RB","RB",
                                  "RB","RB","RB"),
                           freq = c(1,1,1,1,1,
                                    1,1,2,1,1,
                                    1,1,1,1,
                                    1,1,1),
                           RB.strain = c("BLACKWATER","BLACKWATER",NA,"BLACKWATER","PENNASK",
                                  "PENNASK","PENNASK","BLACKWATER","PENNASK","BLACKWATER",
                                  "PENNASK","FRASER VALLEY","BLACKWATER","FRASER VALLEY",
                                  NA,"FRASER VALLEY","FRASER VALLEY"),
                           EB.strain = c("AYLMER", "AYLMER", "AYLMER", NA, NA,
                                  NA, NA, NA, NA,NA,
                                  NA, NA, NA, NA,
                                  NA, NA, NA),
                           currently.stocked = c(T,T,T,T,T,
                                                 T,T,T,T,T,
                                                 T,T,T,T,
                                                 F,T,T))

stocked.lks


table.surveys <- catch %>% 
  left_join(stocked.lks) %>% 
  group_by(lake, year) %>% 
  dplyr::summarize(sp = paste(unique(sp), collapse=",")) %>% 
  arrange(lake, year)
table.surveys

unique(catch$lake)

table.recent<- catch %>% 
  left_join(stocked.lks) %>% 
  filter(year %in% c(2017:2020), sp %in% c("RB","EB")) %>% 
  group_by(lake) %>% 
  dplyr::summarize(sp = paste(unique(sp), collapse=","), 
                   RB.strain = unique(RB.strain),
                   EB.strain = unique(EB.strain))  
table.recent

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


