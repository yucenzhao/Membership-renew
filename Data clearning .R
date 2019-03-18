library(ggplot2)
library(lubridate)
library(dplyr)
library(tidyr)
library(stringr)
members_FY=read.csv("members.csv",sep = "|")


#Change unrenew to be 0, base be 1, plus be 2
members_FY$RENEW_IND=factor(members_FY$RENEW_IND,labels = c(1,2,0))


#distribution for three renew type
members_FY %>% 
  group_by(RENEW_IND) %>%
  summarise(num=n()) %>%
  ggplot(aes(x=RENEW_IND,y=num)) +geom_col()
  
  
##Clean data
#change all ? to NA
members_FY[members_FY=="?"]=NA
as.numeric(members_FY$MILES_TO_CLUB)
colnames(members_FY)
#is.factor(members_FY$marital_status_desc)
members_FY$hh_size_desc=str_extract(members_FY$hh_size_desc,"[0-9]")
members_FY$nbr_children_desc=str_extract(members_FY$nbr_children_desc,"[0-9]")

mb=read.csv("mb.csv")
mb$MILES_TO_CLUB=as.factor(mb$MILES_TO_CLUB)
mb$MILES_TO_CLUB=as.numeric(mb$MILES_TO_CLUB)
mb$hhh_age_desc=as.factor(mb$hhh_age_desc)
mb$hhh_age_desc=as.numeric(mb$hhh_age_desc)
mb$hhh_age_desc=ifelse(mb$hhh_age_desc<=34,1,
                       ifelse(mb$hhh_age_desc<=50,2,
                              ifelse(mb$hhh_age_desc<=70,3,4)))

write.csv(mb,file="mb1.csv")


#Build model
sample=sample_n(members_FY,4000)
train=sample[1:2000,]
test=sample[2001:4000,]
model <- glm(RENEW_IND ~nbr_children_desc+marital_status_desc+hh_size_desc,family=binomial(link='logit'),data=train)
summary(model)

