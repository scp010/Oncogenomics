library(shiny)
library(lubridate)
library(plyr)
library(dplyr)
library(tidyr)
library(readr)
library(reshape2)
library(ggplot2)
library(DT)
load("/home/oharismendy/BTTSR/clean_20200301_DI.rda")
load("/home/oharismendy/BTTSR/BTTSR_FM_v1.rda")
load("/home/oharismendy/BTTSR/demo_DI.rda")
load("/home/oharismendy/BTTSR/dx_DI.rda")

fm=BTTSR_FM_v1
demo=demo_DI
dx=dx_DI

#Clean Clean table
clean=tmp[-c(3,6,8,10:14)]%>%
  drop_na(studyID)

#Remove Null from Demographics
demo[demo=="NULL"]=NA

#Select patients with FM data
#Convert blank to NA, remove NAs
fm[fm==""]=NA
#Take complete row of data, convert to studyID/number of assays table, rename 
fm=fm[complete.cases(fm),]%>%
  group_by(studyID)%>%
    tally()%>%
      rename(Assays=n)

#Build Patient Demographics Data
  #Choose studyID, Disease History from Clean, create unique pairings
  patientGeneral=select(clean,studyID,Disease.History)%>%
    unique()%>%
    #Add Demographics
      join(demo, by="studyID")%>%
        #Add FM data
        join(fm,by="studyID")
  patientGeneral$Assays[is.na(patientGeneral$Assays)]=0

#Build Specimen Inventory Data
  #Build Tissue Table
  tissueTable=select(clean,studyID,Spec.Type,Spec.Collection.Date,Spec.Id)
  tissueTable=tissueTable[tissueTable$Spec.Type=="Tissue",]%>%
    mutate(tissue.Method=ifelse(grepl("VF",Spec.Id,fixed=TRUE),"Tissue-VF", ifelse(grepl("ZFIX",Spec.Id,fixed=TRUE),"Tissue-FX","Tissue-FF")))%>%
      select(studyID,tissue.Method,Spec.Collection.Date)%>%
        group_by(studyID, tissue.Method,Spec.Collection.Date)%>%
          tally()%>%
            dcast(studyID+Spec.Collection.Date~tissue.Method,value.var="n")

  #Mutate Clean table into total number of specimen types by studyID without Tissue
  specimenTable=select(clean,studyID,Spec.Type,Spec.Collection.Date)
  specimenTable=specimenTable[specimenTable$Spec.Type!="Tissue",]%>%
    group_by(studyID,Spec.Type,Spec.Collection.Date)%>%
      tally()%>%
        dcast(studyID+Spec.Collection.Date~Spec.Type,value.var="n")

  #Merge Tissue and non-tissue tables
  specimenGeneral=left_join(specimenTable,tissueTable,by=c("studyID","Spec.Collection.Date"))

  #Build Earliest Date and studyID table
  diseaseHistory=select(clean,studyID,Disease.History,Spec.Collection.Date)%>%
    unique()%>%
      group_by(studyID,Disease.History)%>%
        arrange(Spec.Collection.Date)

  #Join Specimen table with date/Surgeon table
  specimenGeneral=left_join(specimenGeneral,diseaseHistory,by=c("studyID","Spec.Collection.Date"))
  #Reorganize
  specimenGeneral=specimenGeneral[,c(1,12,2:ncol(specimenGeneral))]%>%
    rename(Earliest.Collection=Spec.Collection.Date)
  specimenGeneral$Disease.History.1=NULL

#Build Specimen Details Table
  #Mutate Clean table into rows of specimen types with parentID, count, collection Date, anatomic site without Tissue
  specimenDetail=select(clean,studyID,Parent.Spec.Id,Spec.Type,Spec.Anatomic.Site,Spec.Collection.Date)
  specimenMinus=specimenDetail[specimenDetail$Spec.Type!="Tissue",]%>%
    group_by(studyID,Parent.Spec.Id,Spec.Type,Spec.Anatomic.Site,Spec.Collection.Date)%>%
      tally()%>%
        rename(Count=n)%>%
          rename(Specimen.Id=Parent.Spec.Id)
  specimenSummary=data.frame(specimenMinus[,c(1,2,3,6,4,5)])

  #Mutate Clean Table into rows of tissue only with parent ID, count, collection date, site, specimenId
  specimenPlus=select(clean,studyID,Parent.Spec.Id,Spec.Id,Spec.Type,Spec.Anatomic.Site,Spec.Collection.Date)
  specimenPlus=specimenPlus[specimenPlus$Spec.Type=="Tissue",]%>%
    mutate(tissue.Method=ifelse(grepl("VF",Spec.Id,fixed=TRUE),"Tissue-VF", ifelse(grepl("ZFIX",Spec.Id,fixed=TRUE),"Tissue-FX","Tissue-FF")))%>%
      select(studyID,Parent.Spec.Id,tissue.Method,Spec.Anatomic.Site,Spec.Collection.Date)%>%
        group_by(studyID,Parent.Spec.Id,tissue.Method,Spec.Anatomic.Site,Spec.Collection.Date)%>%
          tally()%>%
            rename(Count=n)%>%
              rename(Spec.Type=tissue.Method)%>%
                rename(Specimen.Id=Parent.Spec.Id)
  specimenPlus$Spec.Type=as.factor(specimenPlus$Spec.Type)
  specimenPlus=data.frame(specimenPlus[,c(1,2,3,6,4,5)])

  #Merge tissue and non-tissue table
  specimenData=rbind(specimenSummary,specimenPlus)

#Build Summary Table
summaryData=select(clean,studyID,Disease.History)%>%
  unique()%>%
    group_by(Disease.History)%>%
      tally()%>%
        arrange(desc(n))%>%
          na_if("")%>%
            na.omit()%>%
              rename(Count=n)