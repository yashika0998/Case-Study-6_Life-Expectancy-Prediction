library(shiny)
library(shinydashboard)
library(rsconnect)
library(shinythemes)
library(readr)
library(dplyr)
library(ggplot2)

lifeExp2 <- readr::read_csv('lifeExp2.csv')
shinyServer(function(input, output){
  
  output$oneCountry <- renderText({ 
    paste("Life Expectancy vs Time (", input$countryInput,")")
  }) #end of renderText
  
  output$scatterPlot <- renderPlot({
    mutate(lifeExp2, life_expectancy = ifelse(
      is.na(life_expectancy), 0, life_expectancy)) %>% 
      filter(country == input$countryInput, year <= input$yearInput) %>% 
      ggplot(aes(y = life_expectancy , x = year, color = country)) +
      geom_line() + theme_classic() + geom_point(alpha = 0.5, size = 0.8)+
      coord_cartesian(ylim = c(min(lifeExp2$life_expectancy), max(lifeExp2$life_expectancy)), 
                      xlim = c(min(lifeExp2$year), max(lifeExp2$year))) +
      labs(y = "Life Expectancy", x = "Year") + 
      scale_y_continuous(breaks=seq(0, 100, 10)) + 
      scale_x_continuous(breaks=seq(1800, 2020, 10)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      ggtitle("Life Expectancy over time") + 
      theme(plot.title = element_text(hjust = 0.5, size = 20))
  }) #end of renderscatterPlot
  
  output$table1 <- renderTable({ 
    mutate(lifeExp2, life_expectancy = ifelse(
      is.na(life_expectancy), 0, life_expectancy)) %>% 
      filter(year == input$yearInput , country == input$countryInput) %>% 
      select(country, life_expectancy) %>% 
      mutate(year.selected = input$yearInput) %>% 
      summarize(country = input$countryInput, year.selected = ifelse(
        length(year.selected) == 1, year.selected,
        "no data is available"),
        life.expectancy = ifelse(
          length(life_expectancy) == 1,as.character(round(life_expectancy,0)),
          "no data is available")
      )
  }) #end of renderTable
  
  output$twoCountries <- renderText({ 
    paste("Life Expectancy vs Time -", input$countryInput1,"and",input$countryInput2)
  }) #end of renderText
  
  output$scatterPlot1 <- renderPlot({
    mutate(lifeExp2, life_expectancy = ifelse(
      is.na(life_expectancy), 0, life_expectancy)) %>% 
      filter(country == input$countryInput1 | country == input$countryInput2,
             year <= input$yearInput1) %>% 
      ggplot(aes(y = life_expectancy , x = year, color = country)) +
      geom_line() + theme_classic() + geom_point(alpha = 0.5, size = 0.8)+
      coord_cartesian(ylim = c(min(lifeExp2$life_expectancy), max(lifeExp2$life_expectancy)), 
                      xlim = c(min(lifeExp2$year), max(lifeExp2$year))) +
      labs(y = "Life Expectancy", x = "Year") + 
      scale_y_continuous(breaks=seq(0, 100, 10)) + 
      scale_x_continuous(breaks=seq(1800, 2020, 10)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      ggtitle("Life Expectancy over time") + 
      theme(plot.title = element_text(hjust = 0.5, size = 20))
  }) #end of renderscatterPlot1
  
  output$table2 <- renderTable({
    mutate(lifeExp2, life_expectancy = ifelse(
      is.na(life_expectancy), 0, life_expectancy)) %>% 
      filter(year == input$yearInput1 , country == input$countryInput1) %>% 
      select(country, life_expectancy) %>% 
      mutate(year.selected = input$yearInput1) %>% 
      summarize(country = input$countryInput1, year.selected = ifelse(
        length(year.selected) == 1, year.selected,
        "no data is available"),
        life.expectancy = ifelse(
          length(life_expectancy) == 1,as.character(round(life_expectancy,0)),
          "no data is available")
      )
  }) #end of renderTable
  
  output$table3 <- renderTable({ 
    mutate(lifeExp2, life_expectancy = ifelse(
      is.na(life_expectancy), 0, life_expectancy)) %>%  
      filter(year == input$yearInput1 , country == input$countryInput2) %>% 
      select(country, life_expectancy) %>% 
      mutate(year.selected = input$yearInput1) %>% 
      summarize(country = input$countryInput2, year.selected = ifelse(
        length(year.selected) == 1, year.selected,
        "no data is available"),
        life.expectancy = ifelse(
          length(life_expectancy) == 1,as.character(round(life_expectancy,0)),
          "no data is available")
      )
  }) #end of renderTable
  
  output$table_list <- renderDataTable({
    select(lifeExp2, country, year, life_expectancy, pop, gdp) %>% 
      group_by(country, year)
  }) #end of renderDataTable
  
  output$boxPlot <- renderPlot({
    filter(lifeExp2, year == input$yearInputContinent) %>% 
      group_by(continent) %>% 
      #summarize(life_expectancy = round(mean(life_expectancy),0)) %>% 
      ggplot(aes(y = life_expectancy, x = continent, fill = continent)) +
      geom_boxplot() + theme_linedraw() + labs(y = "Life Expectancy", x = "Continent") +
      coord_cartesian(ylim = c(min(lifeExp2$life_expectancy), max(lifeExp2$life_expectancy)))
  }) #end of renderboxPlot
  
  output$continentYear <- renderText({ 
    paste("Life Expectancy vs Continents (Year:", input$yearInputContinent,")")
  }) #end of renderText
  
  output$bubbleYear <- renderText({
    paste("Life Expectancy vs GDP (Year:", input$yearInputBubble,")")
  }) #end of renderText
  
  output$bubbleChart <- renderPlot({
    arrange(lifeExp2,country, year) %>% 
      filter(year == input$yearInputBubble) %>% 
      ggplot(aes(x = gdp, y = life_expectancy)) + 
      geom_point(aes(x = gdp, y = life_expectancy, color = continent, size = pop),
                 alpha = 0.5) + scale_x_log10() +
      geom_text(aes(x = gdp, y = life_expectancy + 3, label = country), color = "grey30",
                data = filter(lifeExp2,year == input$yearInputBubble,
                              pop > 500000000 | 
                                country %in% c("Nigeria", "United States","Malaysia"))) +
      labs(title = "GDP versus life expectancy",
           x = "GDP per capita (log scale)",
           y = "Life expectancy",
           size = "Population (in millions)",
           color = "Continent") + theme_classic() + 
      theme(axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")) +
      scale_x_continuous(labels = scales::comma) + 
      scale_size(range = c(0.1, 10), 
                 breaks = 1000000 * c(250, 500, 750, 1000, 1250),
                 labels = c("250", "500", "750", "1000", "1250")) +
      coord_cartesian(ylim = c(min(lifeExp2$life_expectancy), max(lifeExp2$life_expectancy)),
                      xlim = c(0, 75000))
    
  }) #end of renderBubbleChart
  
  output$tabledownload <- renderTable({
    select(lifeExp2, country, year, life_expectancy, pop, gdp) %>% 
      group_by(country, year)
  }) #end of renderTable
  
  output$downloadData <- downloadHandler(
    
    filename = function(){
      
      paste(input$dataset,".csv", sep = "")
    },
    content = function(file){
      write.csv(input$dataset, file, row.names = FALSE)
    }
  )
})