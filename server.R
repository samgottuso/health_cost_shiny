
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
  
  
  
  
  #renderplot
  output$cost_per_patient <- renderPlot({
    
    #Data setup-- is there a way to do this outside of the renderplot function so that multiple plots can reference it?
    low_IFG_no_intervention<-case_analysis(low_IFG,input$ageSelect,input$raceSelect,input$genderSelect,'high','no',34)
    low_IFG_intervention<-case_analysis(low_IFG,input$ageSelect,input$raceSelect,input$genderSelect,'high','yes',34)
    high_IFG_no_intervention<-case_analysis(high_IFG,input$ageSelect,input$raceSelect,input$genderSelect,'high','no',34)
    high_IFG_intervention<-case_analysis(high_IFG,input$ageSelect,input$raceSelect,input$genderSelect, 'high','yes',34)
    IGT_no_intervention<-case_analysis(IGT,input$ageSelect,input$raceSelect,input$genderSelect,'high','no',34)
    IGT_intervention<-case_analysis(IGT,input$ageSelect,input$raceSelect,input$genderSelect,'high','yes',34)
    IFG_IGT_no_intervention<-case_analysis(IFG_IGT,input$ageSelect,input$raceSelect,input$genderSelect,'high','no',34)
    IFG_IGT_intervention<-case_analysis(IFG_IGT,input$ageSelect,input$raceSelect,input$genderSelect,'high','yes',34)
    
    
    pre_diabetic_no_intervention<-(low_IFG_no_intervention+high_IFG_no_intervention+IGT_no_intervention+IFG_IGT_no_intervention)[c(1,4,5),]
    
    pre_diabetic_intervention<-(low_IFG_intervention+high_IFG_intervention+IGT_intervention+IFG_IGT_intervention)[c(1,4,6,7),]
    
    ROI_final<-ROI_table(pre_diabetic_no_intervention,pre_diabetic_intervention,.4,.03,10000,150)
    rownames(ROI_final) <- str_replace_all(rownames(ROI_final),"\\s+","_")
    
  myrows = c("Cost_without_intervention","Cost_with_intervention")
  tempData <- ROI_final
  tempData<-tempData[myrows,]
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

