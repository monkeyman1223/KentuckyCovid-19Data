library(shiny)
library(ggplot2)
library(shinythemes)
library(rvest)
library(methods)
library(httr)
library(mdsr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
library(plotly)
library(DT)

UScounties <-read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
USstates <-read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")

KentuckyCounties <- UScounties %>%
  filter(state == "Kentucky") %>% 
  select(date,county,cases,deaths) %>% 
  mutate(date = ymd(date))

KentuckyState <- USstates %>%
  filter(state == "Kentucky") %>% 
  select(date,cases,deaths) %>% 
  mutate(date = ymd(date))



Kentuckycountyplot <- function(County,variable){ 
  if (variable == "cases"){
    Kentuckycases <- KentuckyCounties %>% 
      filter(KentuckyCounties$county == County) %>% 
      ggplot(aes(x= date, y= cases))+
      geom_point()+
      geom_smooth(method= "loess",formula = y ~ x,se = FALSE,aes(color="#FFA500"))+
      scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1)))))+
      theme(panel.background = element_rect(fill = "#0099cc", colour = "#0099cc",
                                            size = 0.5, linetype = "solid"),legend.position = "none")+
      labs(x= "Date",y = "Number Of Cases",title = County)
    return(ggplotly(Kentuckycases, tooltip = c("date","cases")) %>% 
             layout(xaxis=list(fixedrange=TRUE)) %>%
             layout(yaxis=list(fixedrange=TRUE)) %>% 
             plotly::config(displayModeBar = FALSE) %>% 
             style(hoverinfo = "skip", traces = 2)
    )}
  
  else {
    Kentuckydeaths <- KentuckyCounties %>% 
      filter(KentuckyCounties$county == County) %>% 
      ggplot(aes(x= date, y= deaths))+
      geom_point()+
      geom_smooth(method= "loess",formula = y ~ x, se = FALSE,aes(color="#FFA500"))+
      scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1)))))+
      theme(panel.background = element_rect(fill = "#0099cc", colour = "#0099cc",
                                            size = 0.5, linetype = "solid"),legend.position = "none")+
      labs(x= "Date",y = "Number Of Deaths",title = County)
    return(ggplotly(Kentuckydeaths, tooltip = c("date","deaths")) %>% 
             layout(xaxis=list(fixedrange=TRUE)) %>%
             layout(yaxis=list(fixedrange=TRUE)) %>% 
             plotly::config(displayModeBar = FALSE)%>% 
             style(hoverinfo = "skip", traces = 2)
    )}}

kentuckystateplot <- function(variable){ 
  if (variable == "cases"){
    Kentuckystatecases <- KentuckyState %>% 
      ggplot(aes(x= date, y= cases))+
      geom_point()+
      geom_smooth(method= "loess",formula = y ~ x,se = FALSE,aes(color="#FFA500"))+
      theme(panel.background = element_rect(fill = "#0099cc", colour = "#0099cc",
                                            size = 0.5, linetype = "solid"),legend.position = "none")+
      labs(x= "Date",y = "Number Of Cases",title = "Kentucky")
    return(ggplotly(Kentuckystatecases, tooltip = c("date","cases")) %>% 
             layout(xaxis=list(fixedrange=TRUE)) %>%
             layout(yaxis=list(fixedrange=TRUE)) %>% 
             plotly::config(displayModeBar = FALSE) %>% 
             style(hoverinfo = "skip", traces = 2)
    )}
  
  else {
    Kentuckystatedeaths <- KentuckyState %>% 
      ggplot(aes(x= date, y= deaths))+
      geom_point()+
      geom_smooth(method= "loess",formula = y ~ x, se = FALSE,aes(color="#FFA500"))+
      theme(panel.background = element_rect(fill = "#0099cc", colour = "#0099cc",
                                            size = 0.5, linetype = "solid"),legend.position = "none")+
      labs(x= "Date",y = "Number Of Deaths",title = "Kentucky")
    return(ggplotly(Kentuckystatedeaths, tooltip = c("date","deaths")) %>% 
             layout(xaxis=list(fixedrange=TRUE)) %>%
             layout(yaxis=list(fixedrange=TRUE)) %>% 
             plotly::config(displayModeBar = FALSE)%>% 
             style(hoverinfo = "skip", traces = 2)
    )}}

KentuckyCountydata <- function(County){ 
  UScounties %>%
    filter(state == "Kentucky") %>% 
    select(date,county,cases,deaths) %>% 
    filter(KentuckyCounties$county == County) %>% 
    mutate(date = ymd(date)) %>% 
    arrange(desc(date))
}

KentuckyStatedata <- function(){ 
  USstates %>%
    filter(state == "Kentucky") %>%
    select(date,cases,deaths) %>% 
    mutate(date = ymd(date)) %>% 
    arrange(date, .by_group = TRUE) %>%
    mutate('New Cases' = cases - lag(cases, default = first(cases))) %>% 
    mutate('New Deaths' = deaths - lag(deaths, default = first(deaths))) %>% 
    arrange(desc(date))
}


shinyServer(function(input, output) {
 output$distPlot1 <- renderPlotly({
    input$go
    kentuckystateplot(isolate(input$variable))
  })
 
 output$data1 <- DT::renderDataTable({
   input$go
   isolate(KentuckyStatedata())
 },options = list(pageLength = 10, dom = 'tip'),rownames=FALSE, style = "bootstrap")

 output$distPlot2 <- renderPlotly({
    input$go
    Kentuckycountyplot(isolate(input$County),isolate(input$variable))
  })
  
  output$data2 <- DT::renderDataTable({
    input$go
    KentuckyCountydata(isolate(input$County))
    },options = list(pageLength = 10, dom = 'tip'),rownames=FALSE,style = "bootstrap")
  
})
