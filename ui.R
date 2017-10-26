
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
source('carouselPanel.R')

shinyUI(
  fluidPage(theme = "styles.css",
            navbarPage("Chronic Disease",
                       tabPanel("Engine", value = "Engine",
                                fluidPage(id = "diabetes_panel",
                                          fluidRow(
                                            column(1, 
                                                   dropdownButton(
                                                     fluidRow(
                                                       column(6,
                                                              selectInput(inputId = 'ageSelect', width = '120px', label = "Age", choices = c("20-39","40-59", "60+", "All Ages"),selected = "All Ages")
                                                       ),
                                                       column(6,
                                                              selectInput(inputId = 'raceSelect', width = '120px', label = "Race", choices = c("Hispanic","White", "Black", "Asian","All Races"),selected = "All Races")
                                                       )),
                                                     fluidRow(
                                                       column(6,
                                                              selectInput(inputId = 'genderSelect',width = '120px', label = "Gender", choices = c("Male","Female","Both Genders"),selected="Both Genders")
                                                       ),
                                                       column(6,
                                                              selectInput(inputId = 'popSelect', width = '120px', label = "Pop. Estimate", choices = c("High","Base", "Low"))
                                                       )),

                                                       selectInput(inputId = 'popDFSelect',width = '120px', label = "Population Model", choices = c("Baltimore","Kings County","Custom")),
                                                       circle = TRUE, status = "primary", size = "sm", icon = icon("gear"), width = "300px",
                                                       tooltip = tooltipOptions(title = "Inputs")
                                                     )),
                                            
                                                   
                                        
                                      
                                            column(10,
                                                   wellPanel(id = "carouselPan", height = '300px',
                                                             carouselPanel(auto.advance = F, 
                                                                           plotOutput(outputId = "cases_avoided_per_year",height = "200px"),
                                                                           plotOutput(outputId = "ttl_cost_avoid", height = '200px'),
                                                                           plotOutput(outputId = "roi", height = '200px')
                                                             )))
                                            
                                          ),
                                          fluidRow(
                                            column(6,
                                                   wellPanel(
                                                     plotOutput(outputId = "annual_spend", height = "200px")
                                                     
                                                   )),
                                            column(6,
                                                   wellPanel(
                                                     plotOutput(outputId = "cost_per_patient", height = '200px')
                                                   )))
                                )),
                       
                       tabPanel("New", value = "new",
                                fluidPage( id = "customPage",
                                           fluidRow(
                                             column(1,
                                                    dropdownButton(
                                                      numericInput("enterPop", "Enter total population",value = 0),
                                                      numericInput("whitePop", "Percent White:", min = 0, max = 100, value = 25),
                                                      numericInput("blackPop", "Percent Black:", min = 0, max = 100, value = 25),
                                                      numericInput("asianPop", "Percent Asian:", min = 0, max = 100, value = 25),
                                                      numericInput("hispanicPop", "Percent Hispanic:", min = 0, max = 100, value = 25),
                                                      actionButton("renderPie", "Go"),
                                                      circle = TRUE, status = "primary", size = "sm", icon = icon("gear"), width = "300px",
                                                      tooltip = tooltipOptions(title = "Custom")
                                                      
                                                    )),
                                             column(10,
                                                    wellPanel(
                                                      tableOutput("Custom_Population")
                                                    )
                                             )
                                             
                                           ),
                                           fluidRow(
                                             column(6,
                                                    wellPanel()
                                             ),
                                             column(6,
                                                    wellPanel()
                                             )
                                           )))
            )
  ))      
            