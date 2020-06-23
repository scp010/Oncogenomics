library(shiny)
library(lubridate)
library(plyr)
library(dplyr)
library(tidyr)
library(readr)
library(reshape2)
library(ggplot2)
library(DT)
shinyUI(fluidPage(
  titlePanel("BTTSR De-ID Data"),
  sidebarPanel(
    #Global Filters
    h3("Global Table Filters"),
    helpText("Filter Default settings set to include ALL"),
    selectInput("studyID",label="Study ID", choices=unique(clean$studyID),selected=NULL,multiple=TRUE),
    selectInput("diseaseHistory",label="Disease History", choices=unique(clean$Disease.History),selected=NULL,multiple=TRUE),
    radioButtons("dateRange",label="Time Range", choices=list("Full range"=1,"Last six months"=2,"Last year"=3),selected=1),
    dateRangeInput("collectionDate",label="Collection Date",start=min(clean$Spec.Collection.Date),end=max(clean$Spec.Collection.Date)),
    br(),
    h3("Local Table Filters"),
    #Local Page Filters
    #Patient General Data
    conditionalPanel(condition="input.panel==1",
                     checkboxGroupInput("gender",label="Gender",choices=levels(demo$Gender),selected=0),
                     selectInput("race",label="Race",choices=levels(demo$Race),selected=NULL,multiple=TRUE),
                     selectInput("ethnicity",label="Ethnicity",choices=levels(demo$Ethnicity),selected=NULL,multiple=TRUE),
                     numericInput("assays",label="Number of Assays",value=NA)
    ),
    #Patient Specimen Data
    conditionalPanel(condition="input.panel==2"),
    #Specimen Data Table
    conditionalPanel(condition="input.panel==3",
                     helpText("Specimen Type Filter set to OR"),
                     checkboxGroupInput("specType",label="Specimen Type",choices=c("Buffy Coat","PBMC","Plasma","Saliva","Serum","Tissue-FF","Tissue-FX","Tissue-VF","Urine"),selected=NULL),
                     numericInput("specCount",label="Specimen Count",value=NA),
                     selectInput("parentID",label="Specimen ID",choices=unique(clean$Parent.Spec.Id),selected=NULL,multiple=TRUE),
                     selectInput("anatomicSite",label="Specimen Anatomic Site",choices=levels(clean$Spec.Anatomic.Site),selected=NULL,multiple=TRUE)
    )
  ),
  mainPanel(
    tabsetPanel(
      id="panel",
      tabPanel("Patient Demographics", value=1, DT::dataTableOutput("generalTable"), downloadButton("downloadData1","Download Patient General Data")),
      tabPanel("Specimen Inventory", value=2, DT::dataTableOutput("specimenTable"), downloadButton("downloadData2","Download Patient Specimen Data")),
      tabPanel("Specimen Details", value=3, DT::dataTableOutput("dataTable"), downloadButton("downloadData3","Download Specimen Data")),
      tabPanel("Summary Data", value=4, DT::dataTableOutput("summaryTable"),downloadButton("downloadData4","Download Summary Data"))
    )
  )
)
)
