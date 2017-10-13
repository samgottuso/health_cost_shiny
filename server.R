
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(dplyr)
library(reshape2)
library(ggplot2)
library(ggthemes)

shinyServer(function(input, output) {
  output$vis <- renderText({
    raceSelected <- input$raceSelect
    ageSelected <- input$ageSelect
    genderSelected <- input$genderSelect
    popSelected <- input$popSelect
    paste(raceSelected, ",", ageSelected, ",", genderSelected, ",", popSelected)
  })
  staticDat <- ROI_final
  output$cost_per_patient <- renderPlot({
  myrows = c("Cost_without_intervention","Cost_with_intervention")
  tempData <- staticDat[myrows,]
  colnames(tempData) <- c ("2016", "2017", "2018","2019", "2020")
  tempData <- rbind(colnames(tempData), tempData)
  rownames(tempData)[1]<-"Year"
  tempData <- t(tempData)
  tempData <- as.data.frame(tempData)
  tempData$Cost_without_intervention <- as.numeric(as.character(tempData$Cost_without_intervention))/1000000.0
  tempData$Cost_with_intervention <- as.numeric(as.character(tempData$Cost_with_intervention))/1000000.0
  tempData <- melt(tempData[,c('Year','Cost_without_intervention','Cost_with_intervention')],id.vars = 1)
  ggplot(data = tempData, aes(x = Year, y = value)) + geom_bar(aes(fill = variable),stat = "identity", position = "dodge") +
   xlab("Year") + ylab("dollars in millions") + theme_fivethirtyeight() + ggtitle("Diabetes Treatment Costs")
  })
})
