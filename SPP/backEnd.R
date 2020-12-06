library(shinydashboard)
library(shiny)
library(stringr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(plotly)
library(data.table)
library(lubridate)
library(RCurl)
require(plotly)
library(tidyr)
library('ggplot2')
library('forecast')
library('tseries')
source("helperMethods.R")
library(shinycssloaders)

# Options for Spinner
options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=2)





df = getForecastVsActual()
gendf = getGenMix()





options(shiny.maxRequestSize = 30 * 1024 ^ 2)

server <- function(input, output, session) {
  ########### RT Update of SPP operational Data ####################
  
  addWindPenetrationPlot <- function(){
    
    ForecastVsActual <- reactive({
      invalidateLater(60000, session)
      getForecastVsActual() %>%
        filter(between(as_date(df$Interval), input$date[1], input$date[2]) &
                 df$variable %in% input$Category) %>%
        filter(between(Hour, input$Hour[1], input$Hour[2]))
      
    })
    
    output$line <- renderPlotly({
      validate(need(nrow(ForecastVsActual()) > 0, "Data insufficient for plot"))
      p1 <- ForecastVsActual() %>%
        ggplot(aes(
          x = Interval,
          y = value,
          col = variable,
          text = paste(
            "</br>Date: ",
            Interval,
            "</br>Value: ",
            value,
            "</br>Category: ",
            variable
          )
        )) + geom_point() + geom_line() + theme_minimal() + labs(y = "Wind Penetration (%)", x = "Datetime", col = "Category")
      
      ggplotly(p1, tooltip = c("text"))
      
      
      
    })
  }
  
  addGenMixBarPlot <- function(){
    genMixData <- reactive({
      invalidateLater(60000, session)
      
      # functionThatGetsData()
      getGenMix()
      
    })
    output$bar <- renderPlotly({
      validate(need(nrow(genMixData()) > 0, "Data insufficient for plot"))
      p1 <-
        ggplot(genMixData(),
               aes(
                 x = variable,
                 y = value,
                 fill = variable,
                 text = paste("</br>Resource: ", variable, "</br>Energy (MWh): ", value)
               )) +
        geom_bar(position = "dodge", stat = "identity")+theme_classic() + labs(y = "Energy (MWh)", x = "Resources", fill = "Resources") +
        theme_minimal()
      ggplotly(p1, tooltip = c("text"))
      
    })
  }
  

  
  ############# Static Line Chart #################################
  
  addWindPenetrationPlot()
  
  ############# Static Bar Chart #################################
  
  addGenMixBarPlot()
  
  
}
