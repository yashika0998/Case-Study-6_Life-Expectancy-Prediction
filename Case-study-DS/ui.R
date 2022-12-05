library(shiny)
library(shinydashboard)
library(rsconnect)
library(shinythemes)
library(readr)
library(dplyr)
library(ggplot2)


lifeExp2 <- readr::read_csv('lifeExp2.csv')

shinyUI(
  dashboardPage(skin = "black",
                dashboardHeader(title = "Explore", titleWidth = 170),
                dashboardSidebar(width = 170,
                                 sidebarMenu(menuItem("About", tabName = "dashboard", icon = icon("book-open")),
                                             menuSubItem("Time-Series 1", tabName = "chart1", icon = icon("chart-line")),
                                             menuSubItem("Time-Series 2", tabName = "chart2", icon = icon("chart-line")),
                                             menuSubItem("Box Plot", tabName = "chart3", icon = icon("square")),
                                             menuSubItem("Bubble Chart", tabName = "chart4", icon = icon("circle")),
                                             menuItem("View Data", tabName = "table", icon = icon("table")),
                                             menuItem("Download Data", tabName = "csvfile", icon = icon("file-download")))
                ),#end of dashboard sidebar
                dashboardBody(fluidPage(theme = shinytheme("cosmo"),
                                        titlePanel("", windowTitle = "Hey There"),
                                        
                                        tabItems(
                                          
                                          tabItem(tabName = "dashboard", fluidRow(h1("Welcome to my R Shiny Life Expectancy Dashboard!"),
                                                                                  mainPanel(column(width=12, 
                                                                                                   "This dashboard is designed based on data set from kaggle, the dataset is cleaned.
                                                                                        "),
                                                                                            box(width = 12,height = 50,column(width =12,"Conclusions made:")),
                                                                                            box(width = 12,column(width =12,"-Life Expectancy increases over time."),
                                                                                                column(width =12,"-Life Expectancy increases with GDP (higher income and better healthcare 
        leads to longer lifespan)"),
                                                                                                column(width =12,"-Life Expectancy is increasing globally."),
                                                                                                column(width =12,"-A sudden drop in life expectancy of a country (refer to Time-Series 1 & Time-Series 2 plots) probably happen")
                                                                                                
                                                                                            )
                                                                                            
                                                                                            
                                                                                  ) #end of mainPanel
                                          ) #end of FluidRow
                                          ), #endofItem dashboard
                                          
                                          tabItem(tabName = "chart1",
                                                  h2(textOutput("oneCountry")),
                                                  fluidRow(
                                                    sidebarLayout(
                                                      sidebarPanel(
                                                        
                                                        #select Input
                                                        selectInput(inputId = "countryInput",
                                                                    label = " Select Country:",
                                                                    choices = sort(unique(lifeExp2$country)),
                                                                    selected = "Afghanistan"),
                                                        
                                                        #slider Input
                                                        sliderInput(inputId = "yearInput",
                                                                    label = "Year",
                                                                    min = 1800,
                                                                    max = 2018,
                                                                    value = 2015, animate = TRUE,
                                                                    sep = "")
                                                        
                                                      ),#end of sidebarPanel
                                                      mainPanel(fluidRow(
                                                        column(width = 12, plotOutput("scatterPlot"))
                                                      ),
                                                      fluidRow(column(width = 12, align ="center",
                                                                      tableOutput("table1")))
                                                      
                                                      ) #end of mainPanel
                                                      
                                                    ) #end of sidebarLayout
                                                  )#end of fluidRow
                                                  
                                          ), #end of tabItem chart1
                                          
                                          tabItem(tabName = "chart2", 
                                                  h2(textOutput("twoCountries")),
                                                  
                                                  fluidRow(
                                                    sidebarLayout(
                                                      sidebarPanel(
                                                        
                                                        #select Input
                                                        selectInput(inputId = "countryInput1",
                                                                    label = "Select First Country:",
                                                                    choices = sort(unique(lifeExp2$country)),
                                                                    selected = "Afghanistan"),
                                                        
                                                        selectInput(inputId = "countryInput2",
                                                                    label = "Select Second Country:",
                                                                    choices = sort(unique(lifeExp2$country)),
                                                                    selected = "Malaysia"),
                                                        #slider Input
                                                        sliderInput(inputId = "yearInput1",
                                                                    label = "Year",
                                                                    min = 1800,
                                                                    max = 2018,
                                                                    value = 2015, animate = TRUE, sep = "")
                                                        
                                                      ),#end of sidebarPanel
                                                      
                                                      mainPanel(fluidRow(
                                                        column(width = 12, plotOutput("scatterPlot1"))
                                                      ),
                                                      fluidRow(column(width = 12, align ="center",
                                                                      tableOutput("table2"),
                                                                      tableOutput("table3")))
                                                      
                                                      ) #end of mainPanel
                                                      
                                                    ) #end of sidebarLayout
                                                  )#end of fluidRow
                                                  
                                                  
                                          ), #end of tabItem chart2
                                          
                                          tabItem(tabName = "chart3",
                                                  fluidRow(h2(textOutput("continentYear")),
                                                           selectInput(inputId = "yearInputContinent",
                                                                       label = "Select Year:",
                                                                       choices = sort(unique(lifeExp2$year)),
                                                                       selected = "2015"),
                                                           plotOutput("boxPlot")
                                                           
                                                  ) #end of FluidRow
                                          ), #end of Item Chart 3
                                          
                                          tabItem(tabName = "chart4",
                                                  fluidRow(h2(textOutput("bubbleYear")),
                                                           sliderInput(inputId = "yearInputBubble",
                                                                       label = "Select Year:",
                                                                       min = min(sort(unique(lifeExp2$year))),
                                                                       max = max(sort(unique(lifeExp2$year))),
                                                                       value = 2015, animate = TRUE, sep = ""),
                                                           plotOutput("bubbleChart")
                                                           
                                                  ) #end of FluidRow 
                                                  
                                          ), #end of Item Chart 4
                                          
                                          tabItem(tabName = "table",
                                                  h3("Data Table : Global Life Expectancy, Population, GDP"),
                                                  
                                                  fluidRow(dataTableOutput("table_list"))
                                                  
                                          ), #endofItem table
                                          
                                          tabItem(tabName = "csvfile",
                                                  titlePanel('File download'),
                                                  sidebarLayout(
                                                    sidebarPanel(
                                                      selectInput("dataset", "Choose a dataset:", 
                                                                  choices = c("lifeExp2")),
                                                      downloadButton('downloadData', 'Download')
                                                    ),
                                                    mainPanel(
                                                      tableOutput('tabledownload')
                                                    )
                                                  )
                                          ) # end of Item csvfile
                                          
                                          
                                        ) #end of tabItems
                                        
                                        
                ) #end of fluidPage
                ) #end of dashboardBody
  ) #end of dashboard Page
) #end of shinyUI



