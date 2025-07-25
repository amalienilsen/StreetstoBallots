#Data preperations 

library(weights)
library(Hmisc)
library(ggplot2)
library(car)
library(tidyverse)
library(carData)
library(dplyr)
library(car)
library(psych)
library(stargazer)
library(cshapes)
library(tidyverse)
library(zoo)
library(spacetime)
library(caret)
library(lme4)
library(car)
library(MASS)
library(nlme)
library(multilevel)
library(nnet)
library(multiwayvcov)
library(sandwich)
library(lmtest)
library(mlogit)
library(clusterSEs)
library(nnet)
library(brms)
library(survival)
library(stats)
library(data.table)

#PROTEst DATA ACLED 
#filter, only include protests-events

ACLED1 <- ACLEDjan%>%
  filter(V8=="Protests"|V8=="Riots")%>% #only protests or violent demonstrations
  filter(V9 != "Mob violence") %>%
  mutate(V9 = factor(V9, 
                     levels = c("Peaceful protest", 
                                "Protest with intervention",
                                "Excessive force against protesters",
                                "Violent demonstration")))
data <- ACLED1%>%
  mutate(nonviolent = ifelse(V9 == "Peaceful protest" | V9== "Protest with intervention" |
                               V9 == "Excessive force against protesters", 1, 0),
         violent = ifelse(V9 == "Violent demonstration", 1, 0),
         intervention = ifelse(V13 == "16" & V9 == "Protest with intervention", 1, 0), #mild repression
         excessive_force = ifelse(V13 == "16" & V9 == "Excessive force against protesters", 1, 0), #harsh repression
         repression_viol = ifelse(V13 == "15" & V9=="Violent demonstration", 1, 0),
         prot_hurt_civ=ifelse(V13=="57" & V9=="Violent demonstration",1,0))

acled_days <- ACLEDjan%>%
  mutate(event_date = as.Date(ACLEDjan$V5, "%d %B"))

acled_days$difference<-acled_days$event_date-as.Date("2021-11-24")
#Count data 

districtcount <- data %>%
  group_by(V19) %>% # create count variables, number of unique events per district per cell
  summarize(protest_freq=(sum(nonviolent)+sum(violent)),
            nonviolent_freq = sum(nonviolent), 
            violent_freq = sum(violent),
            interv_freq = sum(intervention),
            exforc_freq = sum(excessive_force),
            repviol_freq = sum(repression_viol),
            prothurtciv_freq=sum(prot_hurt_civ))

newdata<-districtcount[-c(1),] #removing first row bc. unknown location

teargas <- read_excel("teargas.xlsx")

#Changing names from Chinese 
teargas$Column3[teargas$Column3=="中西區"]="Hong Kong - Central and Western"
teargas$Column3[teargas$Column3=="元朗區"]="Hong Kong - Yuen Long"
teargas$Column3[teargas$Column3=="油尖旺區"]="Hong Kong - Yau Tsim Mong"
teargas$Column3[teargas$Column3=="黃大仙區"]="Hong Kong - Wong Tai Sin"
teargas$Column3[teargas$Column3=="灣仔區"]="Hong Kong - Wan Chai"
teargas$Column3[teargas$Column3=="西貢區"]="Hong Kong - Sai Kung"
teargas$Column3[teargas$Column3=="大埔區"]="Hong Kong - Tai Po"
teargas$Column3[teargas$Column3=="荃灣區"]="Hong Kong - Tsuen Wan"
teargas$Column3[teargas$Column3=="深水埗區"]="Hong Kong - Sham Shui Po"
teargas$Column3[teargas$Column3=="屯門區"]="Hong Kong - Tuen Mun"
teargas$Column3[teargas$Column3=="沙田區"]="Hong Kong - Sha Tin"
teargas$Column3[teargas$Column3=="葵青區"]="Hong Kong - Kwai Tsing"
teargas$Column3[teargas$Column3=="觀塘區"]="Hong Kong - Kwun Tong"
teargas$Column3[teargas$Column3=="東區"]="Hong Kong - Eastern"
teargas$Column3[teargas$Column3=="九龍城區"]="Hong Kong - Kowloon City"
teargas$Column3[teargas$Column3=="北區"]="Hong Kong - North"

teargas <- teargas[-c(1,327:354), ] #removing rows with events after 23.11.19(district council election)
teargascount <- teargas%>% group_by(Column3) %>% summarise( n() )
names(teargascount) <- c("V19", "teargascount")
#adding districts with zero obs

vector1<-c("Hong Kong - Southern", 0) 
vector2<-c("Hong Kong - Islands",0)


teargascount2<-rbind(teargascount,vector1,vector2)

newdata<-merge(newdata, teargascount2, by="V19")

colnames(newdata)<-c("V19",
                     "protestcount",
                     "nonviolent",
                     "violent",
                     "intervention",
                     "excessive",
                     "repression_vol"
                     ,"prot_hurt_civ",
                     "teargascount")

#changing name of each district in ACLED in order to combine with HKES
newdata$V19[newdata$V19=="Hong Kong - Central and Western"]="1"
newdata$V19[newdata$V19=="Hong Kong - Eastern"]="2"
newdata$V19[newdata$V19=="Hong Kong - Islands"]="10"
newdata$V19[newdata$V19=="Hong Kong - Kowloon City"]="6"
newdata$V19[newdata$V19=="Hong Kong - Kwai Tsing"]="11"
newdata$V19[newdata$V19=="Hong Kong - Kwun Tong"]="7"
newdata$V19[newdata$V19=="Hong Kong - North"]="12"
newdata$V19[newdata$V19=="Hong Kong - Sai Kung"]="13"
newdata$V19[newdata$V19=="Hong Kong - Sha Tin"]="14"
newdata$V19[newdata$V19=="Hong Kong - Sham Shui Po"]="5"
newdata$V19[newdata$V19=="Hong Kong - Southern"]="3"
newdata$V19[newdata$V19=="Hong Kong - Tai Po"]="15"
newdata$V19[newdata$V19=="Hong Kong - Tsuen Wan"]="16"
newdata$V19[newdata$V19=="Hong Kong - Tuen Mun"]="17"
newdata$V19[newdata$V19=="Hong Kong - Wan Chai"]="4"
newdata$V19[newdata$V19=="Hong Kong - Wong Tai Sin"]="8"
newdata$V19[newdata$V19=="Hong Kong - Yau Tsim Mong"]="9"
newdata$V19[newdata$V19=="Hong Kong - Yuen Long"]="18"

colnames(newdata)<-c("QA4",
                     "protestcount",
                     "nonviolent",
                     "violent",
                     "intervention",
                     "excessive",
                     "repression_vol"
                     ,"prot_hurt_civ",
                     "teargascount")

class(newdata$teargascount)
newdata$teargascount<-as.numeric(newdata$teargascount) 

#HONG KONG ELECTION STUDIES 
#Use survey from 2019 election

library(haven)
hkes<-read_dta("hkesCoreData202010RIKTIG.dta")

election2019 <- hkes%>%
  filter(election==6)

#"dont know"-responses=NA 
election2019$QD4A[election2019$QD4A==85]=NA
election2019$QD12[election2019$QD12==85]=NA
election2019$QE5[election2019$QE5==97]=NA
election2019$QE10[election2019$QE10==97]=NA
election2019$QG4[election2019$QG4==97]=NA
election2019$QI1[election2019$QI1==91|election2019$QI1==97]=NA 
election2019$QJ3[election2019$QJ3==97]=NA
election2019$QL37_3[election2019$QL37_3==97]=0


#weekly use social media to discuss public issues 

election2019$telegram<-ifelse(election2019$QL37_3==3
                              |election2019$QL37_3==4
                              |election2019$QL37_3==5 ,1,0)

#Identity
election2019$hongkonger<-ifelse(election2019$QI1==4|election2019$QI1==3,1,0)

#Immigrant
election2019$immigrant<-ifelse(election2019$QA6==2
                               |election2019$QA6==3,1,0)
#University degree
election2019$education<-ifelse(election2019$QB10==4
                               |election2019$QB10==5,1,0)
#recieve gift from district councilor 
election2019$gift<-ifelse(election2019$QL17_99==1,0,1)
#managers or proffesionals

election2019$higher_service<-ifelse(election2019$QB9==1|
                                      election2019$QB9==2|
                                      election2019$QB9==3,1,0)

election2019$lower_service<-ifelse(election2019$QB9==9,1,0)

election2019$working_class<-ifelse(election2019$QB9==5|
                                     election2019$QB9==6|
                                     election2019$QB9==7|
                                     election2019$QB9==8|
                                     election2019$QB9==10,1,0)

election2019$routine_nonman<-ifelse(election2019$QB9==4,1,0)




election2019$student<-ifelse(election2019$QB9==11,1,0)
election2019$retired<-ifelse(election2019$QB9==12,1,0)
election2019$unemployed<-ifelse(election2019$QB9==13,1,0)


#gender 
election2019$female<-ifelse(election2019$QA1==2,1,0)
election2019$income<-election2019$QB4
table(election2019$QB4)

#combineall
hkall<-merge(election2019,newdata,by="QA4")

#voting for oppositionparties 
election2019$opposition<-ifelse(election2019$QD4A==2|
                                  election2019$QD4A==3|
                                  election2019$QD4A==4,1,0)
#voting for incumbent 
election2019$incumbent<-ifelse(election2019$QD4A==1,1,0)
table(election2019$incumbent)

#
#dataset on only previous regime supporters
regimesupporters <- election2019%>%
  filter(QD12==7|QD12==8|QD12==9)

hkall<-merge(election2019, newdata, by="QA4")
#combine with protest data 

combineregimelive<-merge(regimesupporters,newdata,by="QA4") #district live


#dummy for different vote choices 
combineregimelive$defection<-ifelse(combineregimelive$QD4A==2|
                                      combineregimelive$QD4A==3|
                                      combineregimelive$QD4A==4,1,0)

combineregimelive$loyal<-ifelse(combineregimelive$QD4A==1,1,0)

combineregimelive$disengage<-ifelse(combineregimelive$QD4A==83|
                                      combineregimelive$QD4A==84,
                                    1,0)


#Data on previous "no-voters" 

novoters<-election2019%>%
  filter(QD12==83|QD12==81)


#no-voters 2016 opposition 2019

novoters$opposition<-ifelse(novoters$QD4A == 2|
                              novoters$QD4A==3|
                              novoters$QD4A==4, 1, 0)

novoters$incumbent<-ifelse(novoters$QD4A==1,1,0)

novoters$novote<-ifelse(novoters$QD4A==83|novoters$QD4A==84,1,0)

#merge with protest

novoterslive<-merge(novoters,newdata,by="QA4") #district live


###nnet-package
library(nnet)
library(stargazer)
#models -previous previous "non-voters" 


novoterslive1 <- novoterslive %>%
  
  mutate(categorical_outcome = case_when(incumbent == 1 ~ "Incumbent",
                                         
                                         opposition == 1 ~ "Opposition",
                                         
                                         novote == 1 ~ "Abstain")) %>%
  
  # make "novote" the reference category
  
  mutate(categorical_outcome = fct_relevel(categorical_outcome,
                                           
                                           "Abstain", "Opposition", "Incumbent"))






