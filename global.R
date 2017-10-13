###Healthcost in R shiny, a first attempt. 

library(shiny)
library(dplyr)
library(stringr)


###reading in the engine tables as CSVs-- will eventually need to do this as RData and save it to the app
diabetes_engine<-read.csv('~/Health_cost_shiny/diabetes_engine.csv',row.names = 1)

###Pre-made DFs--- different populations based on either Baltimore or King County
bmore_hispanic_male<-c(7136,2581,617)
bmore_white_male<-c(32524,23217,16118)
bmore_black_male<-c(48728,50085,24806)
bmore_asian_male<-c(4120,1188,553)
bmore_hispanic_female<-c(5353,1824,614)
bmore_white_female<-c(33886,21421,20388)
bmore_black_female<-c(58231,60778,38939)
bmore_asian_female<-c(4415,1215,707)

bmore_DF<-cbind.data.frame(bmore_hispanic_male,bmore_white_male,bmore_black_male,bmore_asian_male,bmore_hispanic_female,bmore_white_female,bmore_black_female,bmore_asian_female)
colnames(bmore_DF)<-c("Hispanic Male","White Male","Black Male","Asian Male","Hispanic Female","White Female","Black Female","Asian Female")
rownames(bmore_DF)<-c("20-39","40-59","60+")

KC_hispanic_male<-c(86085,54076,24091)
KC_white_male<-c(147324,102038,77782)
KC_black_male<-c(109342,97695,50953)
KC_asian_male<-c(41807,36916,16303)
KC_hispanic_female<-c(81337,61774,35580)
KC_white_female<-c(147654,102984,105808)
KC_black_female<-c(138645,138595,86475)
KC_asian_female<-c(46692,37741,17626)

KC_DF<-cbind.data.frame(KC_hispanic_male,KC_white_male,KC_black_male,KC_asian_male,KC_hispanic_female,KC_white_female,KC_black_female,KC_asian_female)
colnames(KC_DF)<-c("Hispanic Male","White Male","Black Male","Asian Male","Hispanic Female","White Female","Black Female","Asian Female")
rownames(KC_DF)<-c("20-39","40-59","60+")

#Eventually will have to write a function that takes user inputs to create a custom population or imports from SAS/CSV (csv should be fairly easy if it's in this format...)
#Possible Grace ask--- interactive formula that takes a total population and the % of minorities of each (slider input?) and then spits out a working DF in our format

###Function that takes the initial population and gives a huge list which can be sorted out to make seperate DFs, using diabetes engine... if hypertension engine is in the same format then just modify this to accept disease type as a argument
healthcost_values_diabetes<-function(population,pop_estimate){
  ##creating the tables that are on the Engine-Diabetes tab
  #doing the multiplication and assigning
  
  ##First way of doing it
  # if(pop_estimate=="base"){
  #   for(i in 1:8){
  # column_i<-c()
  #     for(j in 1:3){
  #   column_i<-append(column_i,(population[j,i]*diabetes_engine[((i*3)-(3-j)),1])) #omg that took me forever to figure out but it should create all of our columns for all the seperate tables
  #     }
  # calculated_df[,i]<-(column_i)
  #   }
  
  ##New Way
  if(pop_estimate=="base"){
    pop_estimate_list<-c()
    for(i in 1:8){
      for(j in 1:3){
        pop_estimate_list<-append(pop_estimate_list,(population[j,i]*diabetes_engine[((i*3)-(3-j)),1]))
        pop_estimate_list<-append(pop_estimate_list,(population[j,i]*diabetes_engine[((i*3)-(3-j)),2]))
        pop_estimate_list<-append(pop_estimate_list,(population[j,i]*diabetes_engine[((i*3)-(3-j)),3]))
        pop_estimate_list<-append(pop_estimate_list,(population[j,i]*diabetes_engine[((i*3)-(3-j)),4]))
      }
    }
  }else if(pop_estimate=="high"){
    pop_estimate_list<-c()
    for(i in 1:8){
      for(j in 1:3){
        pop_estimate_list<-append(pop_estimate_list,(population[j,i]*diabetes_engine[((i*3)-(3-j)),5]))
        pop_estimate_list<-append(pop_estimate_list,(population[j,i]*diabetes_engine[((i*3)-(3-j)),6]))
        pop_estimate_list<-append(pop_estimate_list,(population[j,i]*diabetes_engine[((i*3)-(3-j)),7]))
        pop_estimate_list<-append(pop_estimate_list,(population[j,i]*diabetes_engine[((i*3)-(3-j)),8]))
      }
    }
  }else if(pop_estimate=="low"){
    pop_estimate_list<-c()
    for(i in 1:8){
      for(j in 1:3){
        pop_estimate_list<-append(pop_estimate_list,(population[j,i]*diabetes_engine[((i*3)-(3-j)),9]))
        pop_estimate_list<-append(pop_estimate_list,(population[j,i]*diabetes_engine[((i*3)-(3-j)),10]))
        pop_estimate_list<-append(pop_estimate_list,(population[j,i]*diabetes_engine[((i*3)-(3-j)),11]))
        pop_estimate_list<-append(pop_estimate_list,(population[j,i]*diabetes_engine[((i*3)-(3-j)),12]))
      }
    }
  }else
    pop_estimate_list<-("Please Select a population estimate")
  return(pop_estimate_list)
}



##Create all the different DFs (Low_IGF, High_IGF, IGT, IGF+IGT) first as a giant list (in healthcost_values_diabetes) and then use value_DF_Creator to turn them into seperate DFs---- 

value_list<-healthcost_values_diabetes(bmore_DF,'high')

value_DF_creator<-function(list,number_table){
  paired_list<-c()
  for(k in seq(number_table,96,4)){
    paired_list<-append(paired_list,list[k])
  }
  out_DF<-data.frame(matrix(data = paired_list,nrow = 3,ncol = 8))
  return(out_DF)
}

low_IFG<-value_DF_creator(value_list,1)
colnames(low_IFG)<-c("Hispanic Male","White Male","Black Male","Asian Male","Hispanic Female","White Female","Black Female","Asian Female")
high_IFG<-value_DF_creator(value_list,2)
colnames(high_IFG)<-c("Hispanic Male","White Male","Black Male","Asian Male","Hispanic Female","White Female","Black Female","Asian Female")
IGT<-value_DF_creator(value_list,3)
colnames(IGT)<-c("Hispanic Male","White Male","Black Male","Asian Male","Hispanic Female","White Female","Black Female","Asian Female")
IFG_IGT<-value_DF_creator(value_list,4)
colnames(IFG_IGT)<-c("Hispanic Male","White Male","Black Male","Asian Male","Hispanic Female","White Female","Black Female","Asian Female")

pre_diabetic_population<-low_IFG+high_IFG+IGT+IFG_IGT

##take those DFS to create the case analysis tables

##So on the input tab it only graphs the avoidance/ROI for the selected demographics-- so I would only have to build out the case analysis for those... or build out the full case analysis and then subset based on selected inputs
#Create a function that takes each of our tables (low IGF, High IGF etc) and converts it into a case analysis table
#Y1-Y4 with and without intervention? #prediabetic population, BMI trajectory, progression rate, normoglycemic, annual normo due to intervention, normo to diabetes progression rate, diabetes incdience, diabetes pervalence

##rate to figure out progression based on table and BMI ######HIS TABLES ONLY LOOK AT THE 2 DIGIT NUMBER, DOESN'T DO THE FULL MATH--- IS THIS RIGHT????
progression_rate_func<-function(measure_table,BMI){
  if(measure_table=="low_IFG"){
    progression_rate<-(.0044*(exp(1)^(.0359*BMI)))
  }else if(measure_table=="high_IFG"){
    progression_rate<-(.0178*(exp(1)^(.0355*BMI)))
  }else if(measure_table=="IFG_IGT"){
    progression_rate<-(.026*(exp(1)^(.0354*BMI)))
  }else if(measure_table=='IGT'){
    if(BMI<24){
      progression_rate<-.0262
    }else if(BMI>=  25 & BMI < 30){
      progression_rate<-.0463
    }else{
      progression_rate<-.0730
    }
  }
  return(progression_rate)
}


##going to have to use this like the value_DF_creator function to create each table (both w and w/o intervention) and then combine them into the pre-diabetic tables
case_analysis<-function(measure_table,age,race,gender,pop_estimate,intervention,BMI=34){
  #subsetting by race
  if(race=='all races'){
    measure_table_race<-measure_table
  }##This isn't the most efficient way to do it, but with my current column names it is how we have to
  else if(race=='hispanic'){
    measure_table_race<-measure_table[,c(1,5)]
  }else if(race=='white'){
    measure_table_race<-measure_table[,c(2,6)]
  }else if(race=='black'){
    measure_table_race<-measure_table[,c(3,7)]
  }else if(race=='asian'){
    measure_table_race<-measure_table[,c(4,8)]
  }
  #subsetting by gender
  if(gender=='both genders'){
    measure_table_gender<-measure_table_race
  }else if(gender=='male'){
    measure_table_gender<-measure_table_race[,1]
  }else if(gender=='female'){
    measure_table_gender<-measure_table_race[,2]
  }
  
  #subsetting by age
  if(age=='all ages'){
    measure_table_final<-measure_table_gender
  }else if(age=='20-39'){
    measure_table_final<-measure_table_gender[1,]
  }else if(age=='40-59'){
    measure_table_final<-measure_table_gender[2,]
  }else if(age=="60+"){
    measure_table_final<-measure_table_gender[3,]
  }
  
  #okay so now we have the table that we need to work with in order to create our case analysis table
  
  #might have to do outside?  
  
  progression_rate<-progression_rate_func(deparse(substitute(measure_table)),BMI)
  
  
  #to get the pre_diabetic table I actually need to do each of the other tables first and then combine in the final... so once the function is complete we'll have to create a w/o and a w/ table for each and then combine to make a prediabetic w/o and w/
  
  
  #Eventually this BMI list will have to be re-written to be a function, but if we just use the standard 34 than it's fine
  BMI_list<-c(33,32,31.0,30,29)
  
  
  ##works up to here
  
  
  if(intervention=='yes'){
    #Intervention
    ##Things I need for this table... PreDiabetic population, BMI Trajectory, progression rate,annual normoglycemic population, normoglycemic to diabetetes progression rate, diabetes incidence, diabetes prevalence
    ##I don't think this is exactly right, but might give me something close enough to work with for now
    pre_diabetic_population<-sum(measure_table_final)
    normoglycemic_population<-pre_diabetic_population*.05
    BMI<-BMI_list[1]
    ##have to figure out how to change the BMI and progression rate for each year as it decreases... he uses an input system and a table... I might cheat a little here and just make that list
    progression_rate<-progression_rate_func(deparse(substitute(measure_table)),BMI)
    normoglycemic_rate<-progression_rate*.44
    diabetes_incidence<-((pre_diabetic_population-normoglycemic_population)*progression_rate)+(normoglycemic_population*normoglycemic_rate)
    diabetes_prevalence<-diabetes_incidence
    year_1<-c(pre_diabetic_population,BMI,progression_rate,normoglycemic_population,normoglycemic_rate,diabetes_incidence,diabetes_prevalence)
    for(i in 2:5){
      pre_diabetic_population<-(pre_diabetic_population*(1.01))-diabetes_incidence-normoglycemic_population
      normoglycemic_population<-pre_diabetic_population*.05
      BMI<-BMI_list[i]
      progression_rate<-progression_rate_func(deparse(substitute(measure_table)),BMI)
      normoglycemic_rate<-progression_rate*.44
      diabetes_incidence<-((pre_diabetic_population-normoglycemic_population)*progression_rate)+(normoglycemic_population*normoglycemic_rate)
      diabetes_prevalence<-diabetes_prevalence+diabetes_incidence
      assign(paste('year',i,sep = '_'),value=c(pre_diabetic_population,BMI,progression_rate,normoglycemic_population,normoglycemic_rate,diabetes_incidence,diabetes_prevalence))  
    }
    output_table<-as.data.frame(matrix(c(year_1,year_2,year_3,year_4,year_5),ncol=5))
    colnames(output_table)<-c('year 1','year 2','year 3','year 4','year 5')
    row.names(output_table)<-c('Pre Diabetic Population','BMI','Progression Rate','Normoglycemic Population','Normoglycemic Rate','Diabetes Incidence','Diabetes Prevalence')
  }else if(intervention=='no'){
    #non_intervention
    ##Things I need for this table... PreDiabetic population, BMI Trajectory, progression rate, diabetes incidence, diabetes prevalence
    pre_diabetic_population<-sum(measure_table_final)
    progression_rate<-progression_rate_func(deparse(substitute(measure_table)),BMI)
    diabetes_incidence<-pre_diabetic_population*progression_rate
    diabetes_prevalence<-diabetes_incidence
    year_1<-c(pre_diabetic_population,BMI,progression_rate,diabetes_incidence,diabetes_prevalence)
    for(i in 2:5){
      pre_diabetic_population<-(pre_diabetic_population*(1.01)-diabetes_incidence)
      diabetes_incidence<-pre_diabetic_population*progression_rate
      diabetes_prevalence<-diabetes_prevalence+diabetes_incidence
      assign(paste('year',i,sep = '_'),value=c(pre_diabetic_population,BMI,progression_rate,diabetes_incidence,diabetes_prevalence))
    }
    
    output_table<-as.data.frame(matrix(c(year_1,year_2,year_3,year_4,year_5),ncol=5))
    colnames(output_table)<-c('year 1','year 2','year 3','year 4','year 5')
    row.names(output_table)<-c('Pre Diabetic Population','BMI','Progression Rate','Diabetes Incidence','Diabetes Prevalence')
  }
  
  return(output_table)
  
  
}


low_IFG_no_intervention<-case_analysis(low_IFG,'all ages','all races','both genders','high','no',34)
low_IFG_intervention<-case_analysis(low_IFG,'all ages','all races','both genders','high','yes',34)
high_IFG_no_intervention<-case_analysis(high_IFG,'all ages','all races','both genders','high','no',34)
high_IFG_intervention<-case_analysis(high_IFG,'all ages','all races','both genders','high','yes',34)
IGT_no_intervention<-case_analysis(IGT,'all ages','all races','both genders','high','no',34)
IGT_intervention<-case_analysis(IGT,'all ages','all races','both genders','high','yes',34)
IFG_IGT_no_intervention<-case_analysis(IFG_IGT,'all ages','all races','both genders','high','no',34)
IFG_IGT_intervention<-case_analysis(IFG_IGT,'all ages','all races','both genders','high','yes',34)

pre_diabetic_no_intervention<-(low_IFG_no_intervention+high_IFG_no_intervention+IGT_no_intervention+IFG_IGT_no_intervention)[c(1,4,5),]

pre_diabetic_intervention<-(low_IFG_intervention+high_IFG_intervention+IGT_intervention+IFG_IGT_intervention)[c(1,4,6,7),]


#Take information from all of these tables and put it into the final ROI table

#What I need... Diabetic Case Avoidance, Commitment adjustusted cases avoidance and cases, healthcare inflation (INPUT EVENTUALLY), cost of treatment (input),total cost of treatment w/,cost of  
ROI_table<-function(pre_diabetic_no_intervention,pre_diabetic_intervention,program_attrition,healthcare_inflation,treatment_cost,intervention_cost){
  diabetic_case_avoidance<-pre_diabetic_no_intervention[3,1]-pre_diabetic_intervention[4,1]
  commitment_adjusted_incurred<-diabetic_case_avoidance*(1-program_attrition)
  commitment_adjusted<-pre_diabetic_intervention[4,1]+(diabetic_case_avoidance-commitment_adjusted_incurred)
  healthcare_inflation=healthcare_inflation
  treatment_cost=treatment_cost
  cost_no_intervention<-treatment_cost*pre_diabetic_no_intervention[3,1]
  cost_intervention<-treatment_cost*pre_diabetic_intervention[4,1]
  cost_avoidance<-commitment_adjusted_incurred*treatment_cost
  #WHY DIVIDE THE PRE-DIABETIC POPULATION BY 5?
  annual_intervention<-intervention_cost*pre_diabetic_intervention[1,1]
  cumulative_intervention<-annual_intervention
  cumulative_ROI<-(cost_avoidance/cumulative_intervention)
  #do I need to subtract 1?
  year_1<-c(diabetic_case_avoidance,commitment_adjusted_incurred,commitment_adjusted,healthcare_inflation,treatment_cost,cost_no_intervention,cost_intervention,cost_avoidance,annual_intervention,cumulative_intervention,cumulative_ROI)
  for(i in 2:5){
    diabetic_case_avoidance<-pre_diabetic_no_intervention[3,i]-pre_diabetic_intervention[4,i]
    commitment_adjusted_incurred<-diabetic_case_avoidance*(1-program_attrition)
    commitment_adjusted<-pre_diabetic_intervention[4,i]+(diabetic_case_avoidance-commitment_adjusted_incurred)
    healthcare_inflation<-healthcare_inflation
    treatment_cost<-treatment_cost+(treatment_cost*healthcare_inflation)
    cost_no_intervention<-treatment_cost*pre_diabetic_no_intervention[3,i]
    cost_intervention<-treatment_cost*pre_diabetic_intervention[4,i]
    cost_avoidance<-commitment_adjusted_incurred*treatment_cost
    #Healthcost currently isn't changing year to year... but it probably should right?
    annual_intervention<-intervention_cost*pre_diabetic_intervention[1,i]
    cumulative_intervention<-cumulative_intervention+annual_intervention
    cumulative_ROI<-(cost_avoidance/cumulative_intervention)
    assign(paste('year',i,sep = '_'),value=c(diabetic_case_avoidance,commitment_adjusted_incurred,commitment_adjusted,healthcare_inflation,treatment_cost,cost_no_intervention,cost_intervention,cost_avoidance,annual_intervention,cumulative_intervention,cumulative_ROI))
    
  }
  output_table<-as.data.frame(matrix(c(year_1,year_2,year_3,year_4,year_5),ncol=5))
  colnames(output_table)<-c('year 1','year 2','year 3','year 4','year 5')
  rownames(output_table)<-c('Case Avoidance','Incurred Case Avoidance','Commitment Adjusted Cases','HealthCare Inflation','Cost per Patient per Year','Cost without intervention','Cost with intervention','Cost Avoidance (incurred)','Annual Intervention Spending','Cummulative Intervention Spending','Cumulative ROI')
  
  return(output_table)
}

ROI_final<-ROI_table(pre_diabetic_no_intervention,pre_diabetic_intervention,.4,.03,10000,150)
rownames(ROI_final) <- str_replace_all(rownames(ROI_final),"\\s+","_")
