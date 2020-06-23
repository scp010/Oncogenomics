library(shiny)
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
        selectInput("studyID",label="Study ID", choices=unique(clean_20190620_DI_v2$studyID),selected=NULL,multiple=TRUE),
        selectInput("diseaseHistory",label="Disease History", choices=unique(clean_20190620_DI_v2$Disease.History),selected=NULL,multiple=TRUE),
        dateRangeInput("collectionDate",label="Collection Date",start=min(clean_20190620_DI_v2$Spec.Collection.Date),end=max(clean_20190620_DI_v2$Spec.Collection.Date)),
        br(),
        h3("Local Table Filters"),
        #Local Page Filters
        #Patient General Data
        conditionalPanel(condition="input.panel==1",
                         checkboxGroupInput("gender",label="Gender",choices=levels(demo_DI$Gender),selected=NULL),
                         selectInput("race",label="Race",choices=levels(demo_DI$Race),selected=NULL,multiple=TRUE),
                         selectInput("ethnicity",label="Ethnicity",choices=levels(demo_DI$Ethnicity),selected=NULL,multiple=TRUE),
                         numericInput("assays",label="Number of Assays",value=NA)
        ),
        #Patient Specimen Data
        conditionalPanel(condition="input.panel==2",
                         selectInput("surgeon",label="Surgeon",choices=levels(clean_20190620_DI_v2$Surgeon),selected=NULL,multiple=TRUE)
        ),
        #Specimen Data Table
        conditionalPanel(condition="input.panel==3",
                         checkboxGroupInput("specType",label="Specimen Type",choices=levels(clean_20190620_DI_v2$Spec.Type),selected=NULL),
                         numericInput("specCount",label="Specimen Count",value=NA),
                         selectInput("parentID",label="Parent Specimen ID",choices=unique(clean_20190620_DI_v2$Parent.Spec.Id),selected=NULL,multiple=TRUE),
                         selectInput("anatomicSite",label="Specimen Anatomic Site",choices=levels(clean_20190620_DI_v2$Spec.Anatomic.Site),selected=NULL,multiple=TRUE)
        )
    ),
    mainPanel(
        tabsetPanel(
            id="panel",
            tabPanel("Patient General Data", value=1, DT::dataTableOutput("generalTable")),
            tabPanel("Patient Specimen Data", value=2, DT::dataTableOutput("specimenTable")),
            tabPanel("Specimen Data Table", value=3, DT::dataTableOutput("dataTable")),
            tabPanel("Summary Data", value=4, DT::dataTableOutput("summaryTable"))
        )
    )
)
)
