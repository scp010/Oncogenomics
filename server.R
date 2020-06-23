library(shiny)
library(lubridate)
library(plyr)
library(dplyr)
library(tidyr)
library(readr)
library(reshape2)
library(ggplot2)
library(DT)
shinyServer(function(input, output) {
  #Patient Demographics
  #Filters
  output$generalTable=DT::renderDataTable({
    if(!is.null(input$studyID)){
      patientGeneral=patientGeneral[patientGeneral$studyID%in%input$studyID,]
    }
    if(!is.null(input$diseaseHistory)){
      patientGeneral=patientGeneral[patientGeneral$Disease.History%in%input$diseaseHistory,]
    }
    if(!is.null(input$gender)){
      patientGeneral=patientGeneral[patientGeneral$Gender%in%input$gender,]
    }
    if(!is.null(input$race)){
      patientGeneral=patientGeneral[patientGeneral$Race%in%input$race,]
    }
    if(!is.null(input$ethnicity)){
      patientGeneral=patientGeneral[patientGeneral$Ethnicity%in%input$ethnicity,]
    }
    if(!is.na(input$assays)){
      patientGeneral=patientGeneral[patientGeneral$Assays==input$assays,]
    }
    patientGeneral
  },rownames=FALSE)
  
  #Download Data
  output$downloadData1=downloadHandler(
    filename=function(){
      paste("PatientDemographics.csv",sep="")
    },
    content=function(file){
      if(!is.null(input$studyID)){
        patientGeneral=patientGeneral[patientGeneral$studyID%in%input$studyID,]
      }
      if(!is.null(input$diseaseHistory)){
        patientGeneral=patientGeneral[patientGeneral$Disease.History%in%input$diseaseHistory,]
      }
      if(!is.null(input$gender)){
        patientGeneral=patientGeneral[patientGeneral$Gender%in%input$gender,]
      }
      if(!is.null(input$race)){
        patientGeneral=patientGeneral[patientGeneral$Race%in%input$race,]
      }
      if(!is.null(input$ethnicity)){
        patientGeneral=patientGeneral[patientGeneral$Ethnicity%in%input$ethnicity,]
      }
      if(!is.na(input$assays)){
        patientGeneral=patientGeneral[patientGeneral$Assays==input$assays,]
      }
      patientGeneral
      write.csv(patientGeneral,file,row.names=FALSE)
    }
  )
  #Specimen Inventory 
  #Filters
  output$specimenTable=DT::renderDataTable({
    if(!is.null(input$studyID)){
      specimenGeneral=specimenGeneral[specimenGeneral$studyID%in%input$studyID,]
    }
    if(!is.null(input$diseaseHistory)){
      specimenGeneral=specimenGeneral[specimenGeneral$Disease.History%in%input$diseaseHistory,]
    }
    if(input$dateRange==1){
      specimenGeneral=filter(specimenGeneral,specimenGeneral$Earliest.Collection>=min(input$collectionDate)&specimenGeneral$Earliest.Collection<=max(input$collectionDate))
    }else if(input$dateRange==2){
      specimenGeneral=filter(specimenGeneral,specimenGeneral$Earliest.Collection>=(max(specimenGeneral$Earliest.Collection) %m-% months(6))&specimenGeneral$Earliest.Collection<=max(input$collectionDate))
    }else if(input$dateRange==3){
      specimenGeneral=filter(specimenGeneral,specimenGeneral$Earliest.Collection>=(max(specimenGeneral$Earliest.Collection) %m-% months(12))&specimenGeneral$Earliest.Collection<=max(input$collectionDate))
    }
  },rownames=FALSE)
  
  #Download Data
  output$downloadData2=downloadHandler(
    filename=function(){
      paste("SpecimenInventory.csv",sep="")
    },
    content=function(file){
      if(!is.null(input$studyID)){
        specimenGeneral=specimenGeneral[specimenGeneral$studyID%in%input$studyID,]
      }
      if(!is.null(input$diseaseHistory)){
        specimenGeneral=specimenGeneral[specimenGeneral$Disease.History%in%input$diseaseHistory,]
      }
      if(input$dateRange==1){
        specimenGeneral=filter(specimenGeneral,specimenGeneral$Earliest.Collection>=min(input$collectionDate)&specimenGeneral$Earliest.Collection<=max(input$collectionDate))
      }else if(input$dateRange==2){
        specimenGeneral=filter(specimenGeneral,specimenGeneral$Earliest.Collection>=(max(specimenGeneral$Earliest.Collection) %m-% months(6))&specimenGeneral$Earliest.Collection<=max(input$collectionDate))
      }else if(input$dateRange==3){
        specimenGeneral=filter(specimenGeneral,specimenGeneral$Earliest.Collection>=(max(specimenGeneral$Earliest.Collection) %m-% months(12))&specimenGeneral$Earliest.Collection<=max(input$collectionDate))
      }
      write.csv(specimenGeneral,file,row.names=FALSE)
    }
  )
  
  #Specimen Details 
  #Filters
  output$dataTable=DT::renderDataTable({
    if(!is.null(input$studyID)){
      specimenData=specimenData[specimenData$studyID%in%input$studyID,]
    }
    if(!is.null(input$specType)){
      specimenData=specimenData[specimenData$Spec.Type%in%input$specType,]
    }
    if(!is.na(input$specCount)){
      specimenData=specimenData[specimenData$Count>=input$specCount,]
    }
    if(!is.null(input$parentID)){
      specimenData=specimenData[specimenData$Parent.Spec.Id%in%input$parentID,]
    }
    if(!is.null(input$anatomicSite)){
      specimenData=specimenData[specimenData$Spec.Anatomic.Site%in%input$anatomicSite,]
    }
    if(input$dateRange==1){
      specimenData=filter(specimenData,specimenData$Spec.Collection.Date>=min(input$collectionDate)&specimenData$Spec.Collection.Date<=max(input$collectionDate))
    }else if(input$dateRange==2){
      specimenData=filter(specimenData,specimenData$Spec.Collection.Date>=(max(specimenData$Spec.Collection.Date) %m-% months(6))&specimenData$Spec.Collection.Date<=max(input$collectionDate))
    }else if(input$dateRange==3){
      specimenData=filter(specimenData,specimenData$Spec.Collection.Date>=(max(specimenData$Spec.Collection.Date) %m-% months(12))&specimenData$Spec.Collection.Date<=max(input$collectionDate))
    }
  },rownames=FALSE)
  
  #Download Data
  output$downloadData3=downloadHandler(
    filename=function(){
      paste("SpecimenDetails.csv",sep="")
    },
    content=function(file){
      if(!is.null(input$studyID)){
        specimenData=specimenData[specimenData$studyID%in%input$studyID,]
      }
      if(!is.null(input$specType)){
        specimenData=specimenData[specimenData$Spec.Type%in%input$specType,]
      }
      if(!is.na(input$specCount)){
        specimenData=specimenData[specimenData$Count>=input$specCount,]
      }
      if(!is.null(input$parentID)){
        specimenData=specimenData[specimenData$Parent.Spec.Id%in%input$parentID,]
      }
      if(!is.null(input$anatomicSite)){
        specimenData=specimenData[specimenData$Spec.Anatomic.Site%in%input$anatomicSite,]
      }
      if(input$dateRange==1){
        specimenData=filter(specimenData,specimenData$Spec.Collection.Date>=min(input$collectionDate)&specimenData$Spec.Collection.Date<=max(input$collectionDate))
      }else if(input$dateRange==2){
        specimenData=filter(specimenData,specimenData$Spec.Collection.Date>=(max(specimenData$Spec.Collection.Date) %m-% months(6))&specimenData$Spec.Collection.Date<=max(input$collectionDate))
      }else if(input$dateRange==3){
        specimenData=filter(specimenData,specimenData$Spec.Collection.Date>=(max(specimenData$Spec.Collection.Date) %m-% months(12))&specimenData$Spec.Collection.Date<=max(input$collectionDate))
      }
      write.csv(specimenData,file,row.names=FALSE)
    }
  )
  
  #Summary Data 
  #Filters
  output$summaryTable=DT::renderDataTable({
    if(!is.null(input$diseaseHistory)){
      summaryData=summaryData[summaryData$Disease.History%in%input$diseaseHistory,]
    }
    summaryData
  },rownames=FALSE)
  
  #Download Data
  output$downloadData4=downloadHandler(
    filename=function(){
      paste("SummaryData.csv",sep="")
    },
    content=function(file){
      if(!is.null(input$diseaseHistory)){
        summaryData=summaryData[summaryData$Disease.History%in%input$diseaseHistory,]
      }
      write.csv(summaryData,file,row.names=FALSE)
    }
  )
  
  
})
