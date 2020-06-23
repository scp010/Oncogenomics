library(shiny)
library(plyr)
library(dplyr)
library(tidyr)
library(readr)
library(reshape2)
library(ggplot2)
library(DT)
shinyServer(function(input, output) {
  
  #Patient General
  #Filters
  output$generalTable=DT::renderDataTable(DT::datatable({
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
  },rownames=TRUE))
  
  #Download Data
  output$downloadData1=downloadHandler(
    filename=function(){
      paste("PatientGeneral.csv",sep="")
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
  #Patient Specimen 
  #Filters
  output$specimenTable=DT::renderDataTable(DT::datatable({
    if(!is.null(input$studyID)){
      specimenGeneral=specimenGeneral[specimenGeneral$studyID%in%input$studyID,]
    }
    if(!is.null(input$diseaseHistory)){
      specimenGeneral=specimenGeneral[specimenGeneral$Disease.History%in%input$diseaseHistory,]
    }
    specimenGeneral=filter(specimenGeneral,specimenGeneral$Earliest.Collection>=min(input$collectionDate)&specimenGeneral$Earliest.Collection<=max(input$collectionDate))
  }))
  
  #Download Data
  output$downloadData2=downloadHandler(
    filename=function(){
      paste("PatientSpecimen.csv",sep="")
    },
    content=function(file){
      if(!is.null(input$studyID)){
        specimenGeneral=specimenGeneral[specimenGeneral$studyID%in%input$studyID,]
      }
      if(!is.null(input$diseaseHistory)){
        specimenGeneral=specimenGeneral[specimenGeneral$Disease.History%in%input$diseaseHistory,]
      }
      specimenGeneral=filter(specimenGeneral,specimenGeneral$Earliest.Collection>=min(input$collectionDate)&specimenGeneral$Earliest.Collection<=max(input$collectionDate))
      write.csv(specimenGeneral,file,row.names=FALSE)
    }
  )
  
  #Specimen Data 
  #Filters
  output$dataTable=DT::renderDataTable(DT::datatable({
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
    specimenData=filter(specimenData,specimenData$Spec.Collection.Date>=min(input$collectionDate)&specimenData$Spec.Collection.Date<=max(input$collectionDate))
  }))
  
  #Download Data
  output$downloadData3=downloadHandler(
    filename=function(){
      paste("SpecimenData.csv",sep="")
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
      specimenData=filter(specimenData,specimenData$Spec.Collection.Date>=min(input$collectionDate)&specimenData$Spec.Collection.Date<=max(input$collectionDate))
      write.csv(specimenData,file,row.names=FALSE)
    }
  )
  
  #Summary Data 
  #Filters
  output$summaryTable=DT::renderDataTable(DT::datatable({
    if(!is.null(input$diseaseHistory)){
      summaryData=summaryData[summaryData$Disease.History%in%input$diseaseHistory,]
    }
    summaryData
  }))
  
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
