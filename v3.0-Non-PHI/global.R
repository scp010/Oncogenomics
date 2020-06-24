library(shiny)
library(lubridate)
library(plyr)
library(dplyr)
library(tidyr)
library(readr)
library(reshape2)
library(ggplot2)
library(DT)

#Load Tableau dump (Clean)
#Load Foundation Medicine (FM)
#Load demographics (demo_DI)
#Load diagnosis (dx_DI)
#.rda files from PHI space

fm=BTTSR_FM_v1
demo=demo_DI
dx=dx_DI


##For Version 2.0
#Clean Clean table
clean=tmp[-c(3,6,8,10:14)]%>%
  drop_na(studyID)%>%
  rename(Specimen.Id=Parent.Spec.Id)
clean$Spec.Type=as.character(clean$Spec.Type)
clean$Spec.Type=ifelse(clean$Spec.Type=="Tissue",ifelse(grepl("VF",clean$Spec.Id,fixed=TRUE),"Tissue-VF", ifelse(grepl("ZFIX",clean$Spec.Id,fixed=TRUE),"Tissue-FX","Tissue-FF")),clean$Spec.Type)
clean$Spec.Type=as.factor(clean$Spec.Type)
clean=clean[,c(7,1:6)]
clean=within(clean,rm("Spec.Id"))%>%
  group_by_all()%>%
  tally()

#Select patients with FM data
#Convert blank to NA, remove NAs
fm[fm==""]=NA
#Take complete row of data, convert to studyID/number of assays table, rename 
fm=fm[complete.cases(fm),]%>%
  group_by(studyID)%>%
  tally()%>%
  rename(Assays=n)

#Use Clean, fm, demo as base tables
totalTable=left_join(clean,fm,by="studyID")%>%
  left_join(demo,by="studyID")%>%
    rename(Count=n)
totalTable$Gender=as.character(totalTable$Gender)
totalTable$Gender[is.na(totalTable$Gender)]="Unknown"
totalTable$Gender=as.factor(totalTable$Gender)
totalTable$Race=as.character(totalTable$Race)
totalTable$Race[is.na(totalTable$Race)]="Unknown (Patient cannot or refuses to declare race)"
totalTable$Race=as.factor(totalTable$Race)
totalTable$Ethnicity=as.character(totalTable$Ethnicity)
totalTable$Ethnicity[is.na(totalTable$Ethnicity)]="Unknown (Patient cannot or refuses to declare race)"
totalTable$Ethnicity=as.factor(totalTable$Ethnicity)
totalTable$Assays[is.na(totalTable$Assays)]=0
totalTable=data.frame(totalTable)


