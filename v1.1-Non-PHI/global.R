library(shiny)
library(plyr)
library(dplyr)
library(tidyr)
library(readr)
library(reshape2)
library(ggplot2)
library(DT)
load("/home/oharismendy/BTTSR/clean_20190620_DI_v2.rda")
load("/home/oharismendy/BTTSR/BTTSR_FM_v1.rda")
load("/home/oharismendy/BTTSR/demo_DI.rda")
load("/home/oharismendy/BTTSR/dx_DI.rda")

#Clean Clean table
clean_20190620_DI_v2=clean_20190620_DI_v2[-c(3,9:12)]%>%
  drop_na(studyID)

#Remove Null from Demographics
demo_DI[demo_DI=="NULL"]=NA

#Select patients with FM data
  #Convert blank to NA, remove NAs
BTTSR_FM_v1[BTTSR_FM_v1==""]=NA
  #Take complete row of data, convert to studyID/number of assays table, rename 
BTTSR_FM_v1=BTTSR_FM_v1[complete.cases(BTTSR_FM_v1),]%>%
  group_by(studyID)%>%
    tally()%>%
      rename(Assays=n)

#Build Patient General Data
  #Choose studyID, Disease History from Clean, create unique pairings
patientGeneral=select(clean_20190620_DI_v2,studyID,Disease.History)%>%
  unique()%>%
  #Add Demographics
    join(demo_DI, by="studyID")%>%
  #Add FM data
      join(BTTSR_FM_v1,by="studyID")
patientGeneral$Assays[is.na(patientGeneral$Assays)]=0

#Build Patient Specimen Data
  #Mutate Clean table into total number of specimen types by studyID
specimenGeneral=select(clean_20190620_DI_v2,studyID,Spec.Type,Spec.Collection.Date)%>%
  group_by(studyID,Spec.Type,Spec.Collection.Date)%>%
    tally()%>%
      dcast(studyID+Spec.Collection.Date~Spec.Type,value.var="n")
  #Build Earliest Date and studyID table
date=select(clean_20190620_DI_v2,studyID,Disease.History,Spec.Collection.Date)%>%
  unique()%>%
    group_by(studyID,Disease.History)%>%
      arrange(Spec.Collection.Date)%>%
        slice(1L)
  #Join Specimen table with date/Surgeon table
specimenGeneral=left_join(specimenGeneral,date,by=c("studyID","Spec.Collection.Date"))
  #Reorganize
specimenGeneral=specimenGeneral[,c(1,10,2:ncol(specimenGeneral))]%>%
  rename(Earliest.Collection=Spec.Collection.Date)
specimenGeneral$Disease.History.1=NULL

#Build Specimen Data Table
  #Mutate Clean table into rows of specimen types with parentID, count, collection Date, anatomic site
specimenData=select(clean_20190620_DI_v2,studyID,Parent.Spec.Id,Spec.Type,Spec.Anatomic.Site,Spec.Collection.Date)%>%
  group_by(studyID,Parent.Spec.Id,Spec.Type,Spec.Anatomic.Site,Spec.Collection.Date)%>%
    tally()%>%
      rename(Count=n)
specimenData=data.frame(specimenData[,c(1,2,3,6,4,5)])

#Build Summary Table
summaryData=select(clean_20190620_DI_v2,studyID,Disease.History)%>%
  unique()%>%
    group_by(Disease.History)%>%
      tally()%>%
        arrange(desc(n))%>%
          na_if("")%>%
            na.omit()

#Clean diagnosis table
dx_DI=dx_DI[complete.cases(dx_DI),]
