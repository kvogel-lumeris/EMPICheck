if (!requireNamespace('htmlwidgets') || packageVersion('htmlwidgets') <= '0.3.2')
  devtools::install_github('ramnathv/htmlwidgets')
# install DT
if (!require("DT")) devtools::install_github("rstudio/DT")
# if (!require("purrr")) devtools::install_github("hadley/purrr")

#library(XLConnect)
library(RODBC)
library(data.table)
library(sqldf)
library(tidyr)
library (plyr)
library(dplyr)
library(knitr)
library(pander)
library(purrr)
library(ggplot2)
library(chron)
library(date)
library(zoo)
library(lubridate)
library(reshape)
library(shiny)
library(shinyBS)
# library(DT)

##get common functions
wd <-getwd()
setwd('..')
source('./SharedDataScripts/CommonFunctions.R')
setwd(wd)
# remove(wd)

ConsEnv <-"Prod"
ConsClientDB <-"Implementation"

Address <- "SAS-SQL-C01-V02.prodapp.domain,50002"
# Address <- "SAS-SQL-C01-V02.PRODAPP.DOMAIN/CONSULT02"

sQuery <-"SELECT * FROM tTestServers"
SERVERS <- fnQueryDatabaseGeneric(Address,ConsClientDB,sQuery)

sQuery <- "SELECT * FROM tTestClients WHERE Enabled = 1"
CLIENTS <- fnQueryDatabaseGeneric(Address,ConsClientDB,sQuery)

Today <- as.Date(Sys.Date())

rm(ConsEnv)
rm(ConsClientDB)
rm(Address)




######
ui <- fluidPage(
  titlePanel("EMPI Check"),
  sidebarLayout(
    sidebarPanel(
      # selectInput("match",
      #             label="Select Match on criteria",choices=list("Demographics"=1,"MBR_ID"=2,"SYS_MBR_SK"=3),                  
      #             selected=1),
      uiOutput("clientOutput"),
      uiOutput("odsenvOutput"),
      uiOutput("ipaasenvOutput"),
      uiOutput("entityOutput")
      ),
      mainPanel(
        ######Render Markdown
        htmlOutput('markdown')
        ######
        ######Test POP_MBR
        # tableOutput('results')
        ######
        ######Test Input Variables
        # textOutput('match'),
        # textOutput('client'),
        # textOutput('odsenv'),
        # textOutput('ipaasenv'),
        # textOutput('entity')
        ######
      )
    )
  )


######

server <- function(input, output, session) {
  
  output$clientOutput <- renderUI({
    selectInput("clientInput", "Client",
      sort(unique(CLIENTS$Abbrev)))
  })
  
  reactive({
    if (is.null(input$clientInput) || is.na(input$clientInput)) {
      return(NULL)
    }  
    
    ClientID <<- CLIENTS %>%
      filter(Abbrev == input$clientInput) %>%
      select(ClientID) %>%
      unique()
    
  })
  
  
  
  output$odsenvOutput <- renderUI({
    selectInput("odsenvInput", "ODS Environment",
      sort(unique(SERVERS$Name)))
  })
  
  reactive({
    if (is.null(input$odsenvInput) || is.na(input$odsenvInput)) {
      return(NULL)
    }   
    
    
    ODSAddress <<- SERVERS %>%
      filter(Type == "ODS") %>%
      filter(Name == input$odsenvInput) %>%
      select(Path) %>%
      unique() 
 
    sQuery <-"SELECT name AS DBName FROM sys.databases"
    
    fnQueryDatabaseGeneric(ODSAddress,'master',sQuery)

    
  })
  
  
  output$ipaasenvOutput <- renderUI({
    selectInput("ipaasenvInput", "iPaaS Environment",
      sort(unique(SERVERS$Name)))
  })
  
  ENTITIES <<- reactive({
    if (is.null(input$ipaasenvInput) || is.na(input$ipaasenvInput)) {
      return(NULL)
    } 
    
    IPAASAddress <<- SERVERS %>%
      filter(Type == "IPAAS") %>%
      filter(Name == input$ipaasenvInput) %>%
      select(Path) %>%
      unique() 
    
    # IPAASAddress <<- "SAS-SQL-S01-V02.PRODAPP.DOMAIN,50004"
    
    sQuery <- "SELECT * FROM IPAAS.IFENT001MT"
   fnQueryDatabaseGeneric(IPAASAddress, "IPAAS", sQuery)
    
  })
  
  
  output$entityOutput <- renderUI({
    selectInput("entityInput", "Entity ID",
      sort(unique(ENTITIES()$ENTITY_NM)))
  })
  
  reactive({
    if (is.null(input$entityInput) || is.na(input$entityInput)) {
      return(NULL)
    } 
    
    EntityID <<- ENTITIES() %>%
      filter(ENTITY_NM == input$entityInput) %>%
      select(ENTITY_ID) %>%
      unique() 

  })
  
  ######Render Markdown
  output$markdown <- renderUI({

    rmarkdown::render("EMPICheck.Rmd")

  })
  ######
 ######Test Input Variables 
  # output$match <- renderText({
  #   local(input$matchInput)
  # })
  # 
  # output$client <- renderText({
  #   local(input$clientInput)
  # })
  # 
  # output$odsenv <- renderText({
  #   local(input$odsenvInput)
  # })
  # 
  # output$ipaasenv <- renderText({
  #   local(input$ipaasenvInput)
  # })
  # 
  # output$entity <- renderText({
  #   local(input$entityInput)
  # })
  
  ######
  ######Test POP_MBR
  # output$results <- renderTable({
  # 
  #   POP()%>%
  #       select(EMPI, PrimaryGroupingLevel, PrimaryGroupingLabel) %>%
  #       group_by(EMPI) %>%
  #       summarise(
  #         PrimaryGroupingLevel = max(PrimaryGroupingLevel),
  #         PrimaryGroupingLabel = max(PrimaryGroupingLabel)
  #       ) %>%
  #       head()
  # })
  ######
}

shinyApp(ui = ui, server = server)
