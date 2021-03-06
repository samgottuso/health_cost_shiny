###Healthcost in R shiny, a first attempt. 

library(shiny)
library(dplyr)
library(stringr)
library(shinyWidgets)


###reading in the engine tables as CSVs-- will eventually need to do this as RData and save it to the app
diabetes_engine<-read.csv('diabetes_engine.csv',row.names = 1)

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


###Function that takes the initial population and gives a huge list which can be sorted out to make seperate DFs, using diabetes engine... if hypertension engine is in the same format then just modify this to accept disease type as a argument
healthcost_values_diabetes<-function(population_select,pop_estimate,custom_pop_df){
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

  
  if(population_select=="Baltimore"){
    population<-bmore_DF
  }else if(population_select=="Kings County"){
    population<-KC_DF
  }else if(population_select=="Custom"){
    population<-custom_pop_df
  }
  
  ##New Way
  if(pop_estimate=="Base"){
    pop_estimate_list<-c()
    for(i in 1:8){
      for(j in 1:3){
        pop_estimate_list<-append(pop_estimate_list,(population[j,i]*diabetes_engine[((i*3)-(3-j)),1]))
        pop_estimate_list<-append(pop_estimate_list,(population[j,i]*diabetes_engine[((i*3)-(3-j)),2]))
        pop_estimate_list<-append(pop_estimate_list,(population[j,i]*diabetes_engine[((i*3)-(3-j)),3]))
        pop_estimate_list<-append(pop_estimate_list,(population[j,i]*diabetes_engine[((i*3)-(3-j)),4]))
      }
    }
  }else if(pop_estimate=="High"){
    pop_estimate_list<-c()
    for(i in 1:8){
      for(j in 1:3){
        pop_estimate_list<-append(pop_estimate_list,(population[j,i]*diabetes_engine[((i*3)-(3-j)),5]))
        pop_estimate_list<-append(pop_estimate_list,(population[j,i]*diabetes_engine[((i*3)-(3-j)),6]))
        pop_estimate_list<-append(pop_estimate_list,(population[j,i]*diabetes_engine[((i*3)-(3-j)),7]))
        pop_estimate_list<-append(pop_estimate_list,(population[j,i]*diabetes_engine[((i*3)-(3-j)),8]))
      }
    }
  }else if(pop_estimate=="Low"){
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

value_list_dev<-healthcost_values_diabetes('Baltimore','High')

value_DF_creator<-function(list,number_table){
  paired_list<-c()
  for(k in seq(number_table,96,4)){
    paired_list<-append(paired_list,list[k])
  }
  out_DF<-data.frame(matrix(data = paired_list,nrow = 3,ncol = 8))
  return(out_DF)
}

low_IFG<-value_DF_creator(value_list_dev,1)
colnames(low_IFG)<-c("Hispanic Male","White Male","Black Male","Asian Male","Hispanic Female","White Female","Black Female","Asian Female")
high_IFG<-value_DF_creator(value_list_dev,2)
colnames(high_IFG)<-c("Hispanic Male","White Male","Black Male","Asian Male","Hispanic Female","White Female","Black Female","Asian Female")
IGT<-value_DF_creator(value_list_dev,3)
colnames(IGT)<-c("Hispanic Male","White Male","Black Male","Asian Male","Hispanic Female","White Female","Black Female","Asian Female")
IFG_IGT<-value_DF_creator(value_list_dev,4)
colnames(IFG_IGT)<-c("Hispanic Male","White Male","Black Male","Asian Male","Hispanic Female","White Female","Black Female","Asian Female")

pre_diabetic_population_dev<-low_IFG+high_IFG+IGT+IFG_IGT

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
  if(race=='All Races'){
    measure_table_race<-measure_table
  }##This isn't the most efficient way to do it, but with my current column names it is how we have to
  else if(race=='Hispanic'){
    measure_table_race<-measure_table[,c(1,5)]
  }else if(race=='White'){
    measure_table_race<-measure_table[,c(2,6)]
  }else if(race=='Black'){
    measure_table_race<-measure_table[,c(3,7)]
  }else if(race=='Asian'){
    measure_table_race<-measure_table[,c(4,8)]
  }
  #subsetting by gender
  if(gender=='Both Genders'){
    measure_table_gender<-measure_table_race
  }else if(gender=='Male'){
    measure_table_gender<-measure_table_race[,1]
  }else if(gender=='Female'){
    measure_table_gender<-measure_table_race[,2]
  }
  
  #subsetting by age
  if(age=='All Ages'){
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
  #REAL LIST BMI_list<-c(33,32,31.0,30,29)
  #AMMENDED LIST
  BMI_list<-c(32,31,31,30,30)
  
  

  
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

#These are for developing, putting them into server.R for interactives 
##These should match up to the tables on the Engine-Diabetes Tab on the righthand side
low_IFG_no_intervention_dev<-case_analysis(low_IFG,'All Ages','All Races','Both Genders','high','no',34)
low_IFG_intervention_dev<-case_analysis(low_IFG,'All Ages','All Races','Both Genders','high','yes',34)
high_IFG_no_intervention_dev<-case_analysis(high_IFG,'All Ages','All Races','Both Genders','high','no',34)
high_IFG_intervention_dev<-case_analysis(high_IFG,'All Ages','All Races','Both Genders','high','yes',34)
IGT_no_intervention_dev<-case_analysis(IGT,'All Ages','All Races','Both Genders','high','no',34)
IGT_intervention_dev<-case_analysis(IGT,'All Ages','All Races','Both Genders','high','yes',34)
IFG_IGT_no_intervention_dev<-case_analysis(IFG_IGT,'All Ages','All Races','Both Genders','high','no',34)
IFG_IGT_intervention_dev<-case_analysis(IFG_IGT,'All Ages','All Races','Both Genders','high','yes',34)

pre_diabetic_no_intervention_dev<-(low_IFG_no_intervention_dev+high_IFG_no_intervention_dev+IGT_no_intervention_dev+IFG_IGT_no_intervention_dev)[c(1,4,5),]

pre_diabetic_intervention_dev<-(low_IFG_intervention_dev+high_IFG_intervention_dev+IGT_intervention_dev+IFG_IGT_intervention_dev)[c(1,4,6,7),]

#doing rounding at the end
pre_diabetic_no_intervention_dev<-round(pre_diabetic_no_intervention_dev,0)
pre_diabetic_intervention_dev<-round(pre_diabetic_intervention_dev,0)

#Take information from all of these tables and put it into the final ROI table

#What I need... Diabetic Case Avoidance, Commitment adjustusted cases avoidance and cases, healthcare inflation (INPUT EVENTUALLY), cost of treatment (input),total cost of treatment w/,cost of  
ROI_table<-function(pre_diabetic_no_intervention,pre_diabetic_intervention,program_attrition,healthcare_inflation,treatment_cost,intervention_cost){
  diabetic_case_avoidance<-pre_diabetic_no_intervention[3,1]-pre_diabetic_intervention[4,1]
  #Commitment adjusted incurred is the actual amount of people that will get treated--- the diabetic case avoidance is the max IF everyone got treated
  commitment_adjusted_incurred<-diabetic_case_avoidance*(1-program_attrition)
  #Commitment adjusted is the number of people in the community who still have diabetes
  commitment_adjusted<-pre_diabetic_intervention[4,1]+(diabetic_case_avoidance-commitment_adjusted_incurred)
  healthcare_inflation=healthcare_inflation
  treatment_cost=treatment_cost
  cost_no_intervention<-treatment_cost*pre_diabetic_no_intervention[3,1]
  cost_intervention<-treatment_cost*commitment_adjusted
  cost_avoidance<-commitment_adjusted_incurred*treatment_cost
  #WHY DIVIDE THE PRE-DIABETIC POPULATION BY 5?
  #non-dividing by 5 + dynamic population annual_intervention<-intervention_cost*pre_diabetic_intervention[1,1]
  #this is a static population/5
  annual_intervention<-intervention_cost*(pre_diabetic_intervention[1,1]/5)
  cumulative_intervention<-annual_intervention
  cumulative_ROI<-(cost_avoidance/cumulative_intervention)-1
  year_1<-c(diabetic_case_avoidance,commitment_adjusted_incurred,commitment_adjusted,healthcare_inflation,treatment_cost,cost_no_intervention,cost_intervention,cost_avoidance,annual_intervention,cumulative_intervention,cumulative_ROI)
  for(i in 2:5){
    diabetic_case_avoidance<-pre_diabetic_no_intervention[3,i]-pre_diabetic_intervention[4,i]
    commitment_adjusted_incurred<-diabetic_case_avoidance*(1-program_attrition)
    commitment_adjusted<-pre_diabetic_intervention[4,i]+(diabetic_case_avoidance-commitment_adjusted_incurred)
    healthcare_inflation<-healthcare_inflation
    treatment_cost<-treatment_cost+(treatment_cost*healthcare_inflation)
    cost_no_intervention<-treatment_cost*pre_diabetic_no_intervention[3,i]
    cost_intervention<-treatment_cost*commitment_adjusted
    cost_avoidance<-commitment_adjusted_incurred*treatment_cost
    #Healthcost currently isn't changing year to year... but it probably should right?
    ##Healthcost is multiplying the cost as a 5 year cost (so annual figures at 1/5 of the total) but that doesn't make a ton of sense--- it only costs $150 to treat someone for 5 years?
    #Non-dividing by 5 version + dynamic population annual_intervention<-intervention_cost*pre_diabetic_intervention[1,i]
    #this is a static population/5
    annual_intervention<-intervention_cost*(pre_diabetic_intervention[1,1]/5)
    cumulative_intervention<-cumulative_intervention+annual_intervention
    cumulative_ROI<-(cost_avoidance/cumulative_intervention)-1
    assign(paste('year',i,sep = '_'),value=c(diabetic_case_avoidance,commitment_adjusted_incurred,commitment_adjusted,healthcare_inflation,treatment_cost,cost_no_intervention,cost_intervention,cost_avoidance,annual_intervention,cumulative_intervention,cumulative_ROI))
    
  }
  output_table<-as.data.frame(matrix(c(year_1,year_2,year_3,year_4,year_5),ncol=5))
  colnames(output_table)<-c('year 1','year 2','year 3','year 4','year 5')
  rownames(output_table)<-c('Case Avoidance','Incurred Case Avoidance','Commitment Adjusted Cases','HealthCare Inflation','Cost per Patient per Year','Cost without intervention','Cost with intervention','Cost Avoidance (incurred)','Annual Intervention Spending','Cummulative Intervention Spending','Cumulative ROI')
  
  return(output_table)
}

ROI_final_dev<-ROI_table(pre_diabetic_no_intervention_dev,pre_diabetic_intervention_dev,.4,.03,10000,150)
rownames(ROI_final_dev) <- str_replace_all(rownames(ROI_final_dev),"\\s+","_")


##Custom Population Creator


custom_pop<-function(total_population,percent_white,percent_black,percent_asian,percent_hispanic){
  ##assuming that ages follow the same as the general US population (27%,26%,21%) are even and gender is split 50/50
  # 
  if(percent_white+percent_black+percent_asian+percent_hispanic!=100){
      print("Please ensure that populations add up to be equal 100%")
  }else{
  
  ages_distribution<-c(.27,.26,.21)
  hispanic_pop<-total_population*(as.integer(percent_hispanic)/100)
  white_pop<-total_population*(as.integer(percent_white)/100)
  black_pop<-total_population*(as.integer(percent_black)/100)
  asian_pop<-total_population*(as.integer(percent_asian)/100)
  
  hispanic_male<-hispanic_pop*.49
  white_male<-white_pop*.49
  black_male<-black_pop*.49
  asian_male<-asian_pop*.49
  
  hispanic_female<-hispanic_pop*.51
  white_female<-white_pop*.51
  black_female<-black_pop*.51
  asian_female<-asian_pop*.51
  
  custom_hispanic_male<-hispanic_male*ages_distribution
  custom_white_male<-white_male*ages_distribution
  custom_black_male<-black_male*ages_distribution
  custom_asian_male<-asian_male*ages_distribution
  
  custom_hispanic_female<-hispanic_female*ages_distribution
  custom_white_female<-hispanic_female*ages_distribution
  custom_black_female<-black_female*ages_distribution
  custom_asian_female<-asian_female*ages_distribution
  
  custom_pop_df<-cbind.data.frame(custom_hispanic_male,custom_white_male,custom_black_male,custom_asian_male,custom_hispanic_female,custom_white_female,custom_black_female,custom_asian_female)
  custom_pop_df<-round(custom_pop_df,0)
  colnames(custom_pop_df)<-c("Hispanic Male","White Male","Black Male","Asian Male","Hispanic Female","White Female","Black Female","Asian Female")
  rownames(custom_pop_df)<-c("20-39","40-59","60+")
  
  
  return(custom_pop_df)
    
}



}


