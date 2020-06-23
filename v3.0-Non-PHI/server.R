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
filterTable=reactive({
    totalTable%>%
        #Multiple Table Filters
        filter(if(!is.null(input$studyID)) studyID%in%input$studyID else studyID==studyID)%>%
        filter(if(!is.null(input$diseaseHistory)) Disease.History%in%input$diseaseHistory else Disease.History==Disease.History)%>%
        filter(if(input$dateRange==1) Spec.Collection.Date>=min(input$collectionDate)&Spec.Collection.Date<=max(input$collectionDate) 
               else if(input$dateRange==2) Spec.Collection.Date>=(max(input$collectionDate) %m-% months(6))&Spec.Collection.Date<=max(input$collectionDate) 
               else if(input$dateRange==3) Spec.Collection.Date>=(max(input$collectionDate) %m-% months(12))&Spec.Collection.Date<=max(input$collectionDate))%>%
        filter(if(!is.null(input$specType)) Spec.Type%in%input$specType else Spec.Type==Spec.Type)%>%
        filter(if(is.na(input$specCount)) Count==Count else Count>=input$specCount)%>%
        
        #Patient Demographic Filters
        filter(if(!is.null(input$gender)) Gender%in%input$gender else Gender==Gender)%>%
        filter(if(!is.null(input$race)) Race%in%input$race else Race==Race)%>%
        filter(if(!is.null(input$ethnicity)) Ethnicity%in%input$ethnicity else Ethnicity==Ethnicity)%>%
        filter(if(is.na(input$assays)) Assays==Assays else Assays==input$assays)%>%

        #Specimen Inventory Filters
        filter(if(!is.null(input$anatomicSite)) Spec.Anatomic.Site%in%input$anatomicSite else Spec.Anatomic.Site==Spec.Anatomic.Site)%>%
        filter(if(!is.null(input$parentID)) Specimen.Id%in%input$parentID else Specimen.Id==Specimen.Id)
    })

#Patient Demographics
    #Table
output$generalTable=DT::renderDataTable({
    patientDemographics=select(filterTable(),studyID,Disease.History,Gender,Race,Ethnicity,Assays)%>%
        unique()
},rownames=FALSE)

    #Download 
output$downloadData1=downloadHandler(
    filename=function(){
        paste("PatientDemographics.csv",sep="")
    },
    content=function(file){
        patientDemographics=select(filterTable(),studyID,Disease.History,Gender,Race,Ethnicity,Assays)%>%
            unique()
        write.csv(patientDemographics,file,row.names=FALSE)
    }
)

#Specimen Inventory 
    #Table
output$specimenTable=DT::renderDataTable({
    inventoryTable=select(filterTable(),studyID,Disease.History,Spec.Type,Count)%>%
        unique()%>%
        dcast(studyID+Disease.History~Spec.Type,fun.aggregate=sum,value.var="Count")
    inventoryTable[inventoryTable==0]=NA
    inventoryTable
},rownames=FALSE)
    #Download
output$downloadData2=downloadHandler(
    filename=function(){
        paste("SpecimenInventory.csv",sep="")
    },
    content=function(file){
        inventoryTable=select(filterTable(),studyID,Disease.History,Spec.Type,Count)%>%
            unique()%>%
            dcast(studyID+Disease.History~Spec.Type,fun.aggregate=sum,value.var="Count")
        inventoryTable[inventoryTable==0]=NA
        inventoryTable
        write.csv(inventoryTable,file,row.names=FALSE)
    }
)
    
#Specimen Details 
    #Table
output$dataTable=DT::renderDataTable({
    detailsTable=select(filterTable(),studyID,Specimen.Id,Spec.Type,Count,Spec.Anatomic.Site,Spec.Collection.Date)%>%
        unique()
},rownames=FALSE)
    #Download
output$downloadData3=downloadHandler(
    filename=function(){
        paste("SpecimenDetails.csv",sep="")
    },
    content=function(file){
        detailsTable=select(filterTable(),studyID,Specimen.Id,Spec.Type,Count,Spec.Anatomic.Site,Spec.Collection.Date)%>%
            unique()
        write.csv(detailsTable,file,row.names=FALSE)
    }
)
#Summary Data 
    #Table
output$summaryTable=DT::renderDataTable({
    summaryData=select(filterTable(),studyID,Disease.History)%>%
        unique()%>%
            group_by(Disease.History)%>%
                tally()%>%
                    arrange(desc(n))%>%
                        na_if("")%>%
                            na.omit()%>%
                                rename(Count=n)
},rownames=FALSE)
    #Download
#Download Data
output$downloadData4=downloadHandler(
    filename=function(){
        paste("SummaryData.csv",sep="")
    },
    content=function(file){
        summaryData=select(filterTable(),studyID,Disease.History)%>%
            unique()%>%
                group_by(Disease.History)%>%
                    tally()%>%
                        arrange(desc(n))%>%
                            na_if("")%>%
                                na.omit()%>%
                                    rename(Count=n)
        write.csv(summaryData,file,row.names=FALSE)
    }
)

}
)
    

