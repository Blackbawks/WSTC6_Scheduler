
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#


library(shiny)
library(xtable)
library(readxl)
require(tidyverse)
#library(stringr)
#library(data.table)

X <- readRDS("data/WSTC6_Abstracts.rds")
Y <- readRDS("data/WSTC6_Overview.rds")

## Change these
YEAR <- 2020
MONTH <- 5
MONTHSTR <- "May"
PLENARIES <- c('Tom Hart','Mercedes Santos','Jennifer Provencher')
###

Yyrs <- rep(YEAR,nrow(Y))
Ymns <- rep(MONTH,nrow(Y))
Ydys <- sapply(1:nrow(Y),function(x) as.numeric(strsplit(as.character(Y$Date[x]),' ')[[1]][2]))
Ysthrs <- sapply(1:nrow(Y),function(x) as.numeric(strsplit(as.character(Y$Start[x]),':')[[1]][1]))
Ystmins <- sapply(1:nrow(Y),function(x) as.numeric(strsplit(as.character(Y$Start[x]),':')[[1]][2]))
Yenhrs <- sapply(1:nrow(Y),function(x) as.numeric(strsplit(as.character(Y$End[x]),':')[[1]][1]))
Yenmins <- sapply(1:nrow(Y),function(x) as.numeric(strsplit(as.character(Y$End[x]),':')[[1]][2]))
Ysecs <- rep(0,nrow(Y))

Y$StartGMT <- ISOdate(Yyrs,Ymns,Ydys,Ysthrs,Ystmins,Ysecs)
Y$EndGMT <- ISOdate(Yyrs,Ymns,Ydys,Yenhrs,Yenmins,Ysecs)
Y$Start <- paste(Y$Date, Y$Start,sep=', ')
Y$End <- paste(Y$Date, Y$End,sep=', ')

#X <- enc2utf8(X)

X$ids <- paste(as.character(X$Last),', ',as.character(X$First),sep='')
X$Affiliation <- as.character(X$Affiliation)
X$Country <- as.character(X$Country)
X$First <- as.character(X$First)
X$Last <- as.character(X$Last)
X$Title <- as.character(X$Title)
X$Abstract <- as.character(X$Abstract)
X$Session <- as.character(X$Session)
X$Date <- as.character(format(X$Date,"%b %d"))



yrs <- rep(YEAR,nrow(X))
mns <- rep(MONTH,nrow(X))
dys <- sapply(1:nrow(X),function(x) as.numeric(strsplit(as.character(X$Date[x]),' ')[[1]][2]))
hrs <- as.numeric(format(X$Time,"%H"))
mins <- as.numeric(format(X$Time,"%M"))
secs <- rep(0,nrow(X))

X$GMTTime <- ISOdate(yrs,mns,dys,hrs,mins,secs)
X$fullname <- paste(X$First, X$Last, sep=" ")




shinyServer(function(input, output, session) {

  
  observeEvent(input$filter, {
    if(input$filter == 'Country'){
      shinyjs::enable('Country')
      shinyjs::disable('Affiliation')
      shinyjs::disable('Session')
      shinyjs::disable('Keyword')
      
    }else if(input$filter == 'Affiliation'){
      shinyjs::enable('Affiliation')
      shinyjs::disable('Country')
      shinyjs::disable('Session')
      shinyjs::disable('Keyword')
      
    }else if(input$filter == 'Session'){
      shinyjs::enable('Session')
      shinyjs::disable('Country')
      shinyjs::disable("Affiliation")
      shinyjs::disable('Keyword')
      
    }else if(input$filter == 'Keyword'){
      shinyjs::enable('Keyword')
      shinyjs::disable('Country')
      shinyjs::disable("Affiliation")
      shinyjs::disable('Session')
      
    }else if(input$filter == 'Plenaries'){
      shinyjs::disable('Session')
      shinyjs::disable('Country')
      shinyjs::disable("Affiliation")
      shinyjs::disable("Keyword")
      
    }else if(input$filter == 'Show all'){
      shinyjs::disable("Country")
      shinyjs::disable("Affiliation")
      shinyjs::disable("Session")
      shinyjs::disable("Keyword")
      #shinyjs::disable('Plenaries')
      #updateSelectInput(session, "Presenter",
                        #choices = c(X$ids)
      #)
    }
    
  })
  
  
  
  observe({
    if(input$filter == 'Country'){
      y <- input$Country
      
      values <- X[which(X$Country == y),]
      values <- values[order(values$Last),]
      updateSelectInput(session, "Presenter",
                        choices = c(values$ids)
      )
    }else if(input$filter == 'Affiliation'){
      y <- input$Affiliation
      
      values <- X[which(X$Affiliation == y),]
      values <- values[order(values$Last),]
      updateSelectInput(session, "Presenter",
                        choices = c(values$ids)
      )
    }else if(input$filter == 'Session'){
      y <- input$Session
      
      values <- X[which(X$Session == y),]
      values <- values[order(values$Last),]
      updateSelectInput(session, "Presenter",
                        choices = c(values$ids)
      )
    }else if(input$filter == 'Keyword'){
      y <- input$Keyword
      
      
      values <- X[grep(y,X$Keywords), ]
      
      #values <- X[X$Keywords %like% y, ]
      values <- values[order(values$Last),]
      updateSelectInput(session, "Presenter",
                        choices = c(values$ids)
      )
    }else if(input$filter == 'Plenaries'){
      y <- input$Session
      
      values <- X[which(X$fullname %in% PLENARIES),]
      values <- values[order(values$Last),]
      updateSelectInput(session, "Presenter",
                        choices = c(values$ids)
      )
    }else if(input$filter == 'Show all'){
      values <- X[order(X$Last),]
      updateSelectInput(session, "Presenter",
                        choices = c(values$ids))
    }
    
  })
  
  
  observe({

      Dat <- X[which(X$ids == input$Presenter),]
      TZ <- as.numeric(input$TimeZone) * -1
      if(TZ > 0){
        TZ <- paste('+',as.character(TZ),sep='')
      }else if(TZ < 0){
        TZ <- as.character(TZ)
      }
      
      newTZ <- paste('Etc/GMT',TZ,sep='')
      newTIMEs <- format(Dat$GMTTime,tz=newTZ,usetz=TRUE)
      
      TT <- round(strptime(as.character(newTIMEs),format="%Y-%m-%d %H:%M:%S"),units='min')
      days <- paste(MONTHSTR,as.character(TT$mday),sep=' ')
      TIME <- paste(format(TT, "%H:%M"),sep="")
      output$CurrentTimeZone <- renderUI({
        mess <- paste('<h1>Viewing time zone: <strong>UTC ',input$TimeZone,'</strong></h1>',sep='')
        HTML(mess)
        
      })
      output$Time <- renderTable({
        if(nrow(Dat) > 1){
          Dat$lab <- c(1:nrow(Dat))
          Tab <- data.frame(Label = Dat$lab, 'DAY' = days, 'TIME' = TIME, check.names=FALSE) #"Local Timezone" = Dat$LOCTm,"Offset From UTC" = Dat$Timezone,
        }else{
          Tab <- data.frame('DAY' = days, 'TIME' = TIME, check.names=FALSE)
        }
      })

    ## have to multiply by -1 to fix the backwards tz thing... 
    
  })
  
  
  
  
  
  
  
  output$Person <- renderTable({
    Dat <- X[which(X$ids == input$Presenter),][1,]
    Tab <- data.frame(First=Dat$First, Last=Dat$Last, Email = Dat$Email, Country = Dat$Country)
    xtable(Tab)
  })
  
  output$Info <- renderTable({
    Dat <- X[which(X$ids == input$Presenter),]
    if(nrow(Dat) > 1){
      Dat$lab <- c(1:nrow(Dat))
      Tab <- data.frame(Label = Dat$lab, Handle = Dat$Handle, Affiliation = Dat$Affiliation)
    }else{
      Tab <- data.frame(Handle = Dat$Handle, Affiliation = Dat$Affiliation)
    }
  })
  
  output$Time <- renderTable({
    Dat <- X[which(X$ids == input$Presenter),]

    if(nrow(Dat) > 1){
      Dat$lab <- c(1:nrow(Dat))
      Tab <- data.frame(Label = Dat$lab, 'DAY (UTC)' = Dat$Date, 'TIME (UTC)' = Dat$Time, check.names=FALSE) #"Local Timezone" = Dat$LOCTm,"Offset From UTC" = Dat$Timezone,
    }else{
      Tab <- data.frame('DAY (UTC)' = Dat$Date, 'TIME (UTC)' = Dat$Time, check.names=FALSE)
    }
    
    
  })
  
  
  
  output$TitleDat <- renderUI({
    Dat <- X[which(X$ids == input$Presenter),]
    
    if(nrow(Dat) > 1){
      Dat$lab <- c(1:nrow(Dat))
      TITLEhtml <- paste('<p><strong>',Dat$lab,'</strong>',Dat$Title,'</p>')
    }else{
      TITLEhtml <- paste('<p>',Dat$Title,'</p>')
    }
    HTML(TITLEhtml)
  })
  
  
  output$CurrentTimeZone <- renderUI({
    mess <- paste('<h4>Viewing time zone: UTC ',input$TimeZone,'</h4>',sep='')
    HTML(mess)
    
  })
  
  observe({
    output$CurrentTimeZone2 <- renderUI({
      mess <- paste('<h1>Viewing time zone:<strong> UTC ',input$TimeZone2,'</strong></h1>',sep='')
      HTML(mess)
      
    })
  })
  
  
  
  
  
  output$AbstractDat <- renderUI({
    Dat <- X[which(X$ids == input$Presenter),]
    
    if(nrow(Dat) > 1){
      Dat$lab <- c(1:nrow(Dat))
      Abshtml <- paste('<p><strong>',Dat$lab,'</strong>',Dat$Abstract,'</p>')
    }else{
      Abshtml <- paste('<p>',Dat$Abstract,'</p>')
    }
    HTML(Abshtml)

  })
  
  
  ########## For the schedule
  
  observeEvent(input$filter2, {
    if(input$filter2 == 'Day'){
      shinyjs::enable('Day')
      shinyjs::disable('Session2')
      
    }else if(input$filter2 == 'Session'){
      shinyjs::enable('Session2')
      shinyjs::disable('Day')
      
    }else if(input$filter2 == 'Show Overview'){
      shinyjs::disable('Session2')
      shinyjs::disable("Day")
    }
  })
  
  
  observeEvent(input$TimeZone2,{
    
    TZ <- as.numeric(input$TimeZone2) * -1
    if(TZ > 0){
      TZ <- paste('+',as.character(TZ),sep='')
    }else if(TZ < 0){
      TZ <- as.character(TZ)
    }
    
    newTZ <- paste('Etc/GMT',TZ,sep='')
    
    newTIMEs <- format(X$GMTTime,tz=newTZ,usetz=TRUE)
    TT <- round(strptime(as.character(newTIMEs),format="%Y-%m-%d %H:%M:%S"),units='min')
    days <- paste(MONTHSTR,as.character(TT$mday),sep=' ')
    TIME <- paste(format(TT, "%H:%M"),sep="")
    
 
    Ystarts <- format(Y$StartGMT,tz=newTZ,usetz=TRUE)
    Yends <- format(Y$EndGMT,tz=newTZ,usetz=TRUE)
    YstartTT <- round(strptime(as.character(Ystarts),format="%Y-%m-%d %H:%M:%S"),units='min')
    YendTT <- round(strptime(as.character(Yends),format="%Y-%m-%d %H:%M:%S"),units='min')
    Ystdays <- paste(MONTHSTR,as.character(YstartTT$mday),sep=' ')
    Yendays <- paste(MONTHSTR,as.character(YendTT$mday),sep=' ')
    TIMEst <- paste(format(YstartTT, "%H:%M"),sep="")
    TIMEen <- paste(format(YendTT, "%H:%M"),sep="")
    
    
    output$Schedule <- renderTable(striped = TRUE,hover=TRUE, expr= {
      Dat <- data.frame(Name = X$fullname, Handle = X$Handle, Day = days, Time = TIME,Session = X$Session, Title = X$Title, check.names=FALSE)
      Dat <- tbl_df(Dat) %>% dplyr::filter(!is.na(Title))
      
      if(input$filter2 == 'Session'){
        Dat <- tbl_df(Dat)
        Dat <- Dat %>% dplyr::filter(Session == input$Session2)
        Dat <- data.frame(Dat)
      }else if(input$filter2 == "Day"){
        Dat <- tbl_df(Dat)
        Dat <- Dat %>% dplyr::filter(Day == input$Day)
        Dat <- data.frame(Dat)
      }else if(input$filter2 == "Show Overview"){
        Overdat <- Y %>% select(-Date,-StartGMT,-EndGMT)
        Overdat$Talks <- as.character(Overdat$Talks)
        Overdat$Start <- paste(Ystdays, TIMEst,sep=', ')
        Overdat$End <- paste(Yendays, TIMEen,sep=', ')
        Dat <- data.frame(Overdat)
      }
      
    })
    
    
    
  })
  
  
  observeEvent(input$info, {
    updateTabsetPanel(session = session, inputId = "tabs", selected = "information")
  })
  
  
  
  
  output$Schedule <- renderTable(striped = TRUE,hover=TRUE, expr= {
    Dat <- data.frame(Name = X$fullname, Handle = X$Handle, Day = X$Date, UTC = X$Time,Session = X$Session, Title = X$Title, check.names=FALSE)
    Dat <- tbl_df(Dat) %>% dplyr::filter(!is.na(Title))
    
    Overdat <- Y #%>% select(-Keywords)
    
    if(input$filter2 == 'Session'){
      Dat <- tbl_df(Dat)
      Dat <- Dat %>% dplyr::ilter(Session == input$Session2)
      Dat <- data.frame(Dat)
    }else if(input$filter2 == "Day"){
      Dat <- tbl_df(Dat)
      Dat <- Dat %>% dplyr::filter(Day == input$Day)
      Dat <- data.frame(Dat)
    }else if(input$filter2 == "Show Overview"){
      Overdat$Talks <- as.character(Overdat$Talks)
      Dat <- data.frame(Overdat)
    }
    
  })

})
