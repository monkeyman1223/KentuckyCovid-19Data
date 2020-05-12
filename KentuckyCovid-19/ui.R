

library(shiny)
library(ggplot2)
library(shinythemes)
library(plotly)
library(DT)
library(methods)
library(httr)
library(mdsr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
library(rvest)

UScounties <-read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")

KentuckyCounties <- UScounties %>%
  filter(state == "Kentucky") %>% 
  select(date,county,cases,deaths) %>% 
  mutate(date = ymd(date))

CountyNames <-
  KentuckyCounties %>% 
  select(county,cases) %>% 
  mutate(score = rank(cases,ties.method = "first")) %>% 
  group_by(county) %>% 
  summarise(Place =sum(score)) %>% 
  select(county)

shinyUI(fluidPage(
  theme = shinytheme("darkly"),
  tags$head(tags$style("div.dataTables_scrollHead span {color: black;}")),
  titlePanel("Kentucky Covid-19"),
  

  sidebarLayout(
    sidebarPanel(
    selectInput("County", "Pick your County",
                CountyNames
), 

selectInput("variable", "What would you like to see?",
            list("cases","deaths")),
tags$head(
  tags$style(HTML('#go{background-color:orange}'))
),
            actionButton(
              inputId = "go",
              label = "Show Results"
            )
),

    mainPanel(
      tags$style(HTML("
                    .dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter, .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_processing, .dataTables_wrapper .dataTables_paginate {
                    color: #ffffff;
                    }

                    thead {
                    color: #ffffff;
                    }

                     tbody {
                    color: #ffffff;
                    }

                   "
                      
                      
      )),
      tabsetPanel(
        tabPanel("Kentucky", plotlyOutput("distPlot1"),DT::dataTableOutput("data1")),
        tabPanel("Selected County", plotlyOutput("distPlot2"),DT::dataTableOutput("data2"))
        )
      )
    )
  )
)