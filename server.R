
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
library(scales)
# 3 blue, one gray, one light green, bright green, red
 myPalette <- c ("#253746", "#0a5157", "#87af9a", "#0ea6b5", "#56565b", "#73aa4f", "#d54728")
shinyServer(function(input, output) {
  output$vis <- renderText({
    raceSelected <- input$raceSelect
    ageSelected <- input$ageSelect
    genderSelected <- input$genderSelect
    popSelected <- input$popSelect
    paste(raceSelected, ",", ageSelected, ",", genderSelected, ",", popSelected)
  })
  #reactive function means that anytime ROI_final changes, the data will be updated. access with ROI_final()
  ROI_final <- reactive ({
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
    ROI_final
  })
  
  
  #renderplot cost per patient
  output$cost_per_patient <- renderPlot({
  myrows = c("Cost_without_intervention","Cost_with_intervention")
  tempData <- ROI_final()
  tempData<-tempData[myrows,]
  colnames(tempData) <- c ("2016", "2017", "2018","2019", "2020")
  tempData <- rbind(colnames(tempData), tempData)
  rownames(tempData)[1]<-"Year"
  tempData <- t(tempData)
  tempData <- as.data.frame(tempData)
  tempData$Cost_without_intervention <- round(as.numeric(as.character(tempData$Cost_without_intervention)), 0)
  tempData$Cost_with_intervention <- round(as.numeric(as.character(tempData$Cost_with_intervention)), 0)
  tempData <- melt(tempData[,c('Year','Cost_without_intervention','Cost_with_intervention')],id.vars = 1)
     ggplot(data = tempData, aes(x = Year, y = value)) +
     geom_bar(aes(fill = variable),stat = "identity", position = "dodge")+
     scale_y_continuous(labels=dollar_format(prefix="$")) + 
     ggtitle("Diabetes Treatment Costs") + 
     theme(legend.title=element_blank()) + 
     labs(x = "Year", y = "Cost")+
     scale_fill_manual(values=c(myPalette[3], myPalette[4]))
  })
  
  #Anual Spend
  output$annual_spend <- renderPlot({
    staticDat <- ROI_final()
    myrows = c("Annual_Intervention_Spending")
    tempData <- staticDat[myrows,]
    colnames(tempData) <- c ("2016", "2017", "2018","2019", "2020")
    tempData <- rbind(colnames(tempData), tempData)
    rownames(tempData)[1]<-"Year"
    tempData <- t(tempData)
    tempData <- as.data.frame(tempData)
    tempData$Annual_Intervention_Spending <- round(as.numeric(as.character(tempData$Annual_Intervention_Spending)), 0)
    ggplot(data = tempData, aes(x = Year, y = Annual_Intervention_Spending))+
      geom_bar(stat = 'identity', position = 'dodge', fill = myPalette[1])+
      scale_y_continuous(labels=dollar_format(prefix="$"))+
      ggtitle("Annual Intervention Spending")+
      labs(x = "Year", y = "Spending")
  })
  
  #Case Avoidance
  output$cases_avoided_per_year<-renderPlot({
    ROI_df<-ROI_final()
    ROI_df<-ROI_df[c(1,2),]
    ROI_df<-t(ROI_df)
    ROI_df<-as.data.frame(ROI_df)
    ROI_df<-cbind(ROI_df,c("2016", "2017", "2018","2019", "2020"))
    colnames(ROI_df)<-c("Cases Avoided with Intervention","Incurred Case Avoidance",'Year')
    ROI_df_long<-melt(ROI_df,id.vars=("Year"))
    colnames(ROI_df_long)<-c("Year","Category",'Value')
    
    Spending_df<-ROI_final()
    Spending_df<-Spending_df[c(9,10),]
    #Not sure how to describe it, but it works! Try to figure out how to divide by number of patients maybe?
    Spending_df<-mapply(function(x,y) x/y,Spending_df,10000)
    Spending_df<-t(Spending_df)
    Spending_df<-as.data.frame(Spending_df)
    Spending_df<-cbind(Spending_df,c("2016", "2017", "2018","2019", "2020"))
    colnames(Spending_df)<-c("Annual Spending","Cumulative Spending","Year")
    Spending_df_long<-melt(Spending_df,id.vars="Year")
    colnames(Spending_df_long)<-c("Year","Spending Category","Spending")
    
    
    
    plot<-ggplot()
    plot<-plot+geom_bar(data = ROI_df_long, aes(Year,y=Value,fill=Category),stat = 'identity',position = 'dodge')+
      labs(y="Number of Cases")+ggtitle("Cases Avoided by Year with Cost")+
      scale_fill_manual(values=c(myPalette[4], myPalette[5]))
    
    plot<-plot+geom_line(data=Spending_df_long,aes(Year,Spending,colour=Spending_df_long$`Spending Category`, group=Spending_df_long$`Spending Category`),stat = 'identity')+
      labs(colour = "Spending Category")+
      scale_color_manual(values=c(myPalette[3], myPalette[6]))
    
    plot
  })
  
  
})

