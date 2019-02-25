
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

# load the required packages
library(shiny)
library(ggplot2)
library(plotrix)
library(reshape2)
library(ggvis)
library(tseries)
library(forecast)
#=================================================== some helper functions ===================================================

# compute populatio density
popDensity <- function(populationTotal){
  pop_density <- populationTotal / 200523
  return(pop_density)
}
# compute the Population growth
popGrowth <- function(naturalIncrease, netMigrants){
  populationGrowth <- naturalIncrease + netMigrants
  return(populationGrowth)
}
# obtain correct column names of the uploaded data 
# this function aims at deleting the appended row names
# when a datafrane is created from an uploaded file
correctNames <- function(inputDataFrame){
  deleteColumn <- grep("(^X$)|(^X\\.)(\\d+)($)", colnames(inputDataFrame), perl = TRUE)
  # ensure that deleteColumn has something
  if(length(deleteColumn) > 0){
    # other data types might apply than character
    row.names(inputDataFrame) <- as.character(inputDataFrame[,grep("^X$",colnames(inputDataFrame))])
    # or introduction of a  new separate column might  be suitable
    inputDataFrame <- inputDataFrame[,-deleteColumn]
    # X might be replaced by different characters, insted of being deleted
    colnames(inputDataFrame) <- gsub("^X", "",colnames(inputDataFrame))
    
    return(inputDataFrame)
  }
}
#=============================================================================================================================

shinyServer(function(input, output,session) {
  
############################################# cleaning and sorting the data ##################################################
  rawPopulationData <- read.csv(file.choose() , header = TRUE)
  # "C:/Users/Administrator/Documents/UgaDemos/population.csv"
  rawPopulationData
  # break the raw population data into short named columns
  year                      <- rawPopulationData$Year
  Births_per_1000_popn       <- rawPopulationData$Births.per.1.000.population
  Deaths_per_1000_popn       <- rawPopulationData$Deaths.per.1.000.population
  Net_Migrants_per_1000_popn <- rawPopulationData$Net.Number.of.Migrants.per.1.000.population
  Rate_of_natural_increase   <- rawPopulationData$Rate.of.natural.increase..percent.
  Growth_rate_percent        <- rawPopulationData$Growth.rate..percent.
  Total_population           <- rawPopulationData$Population
  Births                     <- rawPopulationData$Births
  Deaths                     <- rawPopulationData$Deaths
  Net_Migrants               <- rawPopulationData$Net.Number.of.Migrants
  Natural_increase           <- rawPopulationData$Natural.Increase
  Population_change          <- rawPopulationData$Population.Change
  # store the columns in organised dataframes
  generalPopn_Dynamics              <- data.frame(year,
                                                  Births,
                                                  Deaths,
                                                  Total_population,
                                                  Natural_increase) # this is for general population Dyanamics
  populationDyanamics_per_1000_popn <- data.frame(year,
                                                  Births_per_1000_popn,
                                                  Deaths_per_1000_popn,
                                                  Net_Migrants_per_1000_popn)# this is for population Dynamics per 1000 population
  otherPopulationDynamics           <- data.frame(year,
                                                  Net_Migrants,
                                                  Population_change,
                                                  Growth_rate_percent,
                                                  Rate_of_natural_increase)
     # store the three categories into one data frame
        cleanData <- data.frame(generalPopn_Dynamics,populationDyanamics_per_1000_popn,otherPopulationDynamics)
        # get years as strings
        YEARS <- c("1996","1997","1998","1999","2000","2001","2002","2003","2004","2005","2006",
                   "2007","2008","2009","2010","2011","2012","2013","2014","2015","2016")  
    # this section is for storing piecewise data for statistics section
    # piecewise data
        births_and_DeathsData <- data.frame(year, Births, Deaths) # for births and deaths
        totalPopData <- data.frame(YEARS,Total_population) # for total population
        naturalIncData <- data.frame(YEARS,Natural_increase, Rate_of_natural_increase)# for Natural increase
        migrationsData <- data.frame(YEARS, Net_Migrants, Migration_rate = Net_Migrants_per_1000_popn)# for Migrations
  # end cleaning the data-----------------------------------------------------------------------------------
  
 ############################################# the statistics section #####
    # population density
  populationDensity <- as.integer(popDensity(Total_population)) # get population density as integer
    # population Growth
  calcPopGrowth <- popGrowth(Natural_increase, Net_Migrants)
  
  growthData <- data.frame(YEARS,
                           Natural_increase,
                           Net_Migrants,
                           Growth = calcPopGrowth,
                           Growth_rate = Growth_rate_percent)
  # birth and death rates
  birthDeathRateData <- data.frame(YEARS,
                                   Birth_rate = Births_per_1000_popn,
                                   Death_rate = Deaths_per_1000_popn)
  # end the statistics section------------------------------------------------------------------------------
  
  ############################################ the plots ################
    # population density;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        # for line graph 
      popDensityLine <- ggplot(data.frame(year,populationDensity), aes(year,populationDensity))+
                           geom_line(color = "#00A65A", size = 1)+
                           ggtitle("A line graph showing Uganda's population density (1996 - 2016)")+
                           scale_x_continuous("YEAR", breaks = seq(1996,2016,2))+
                           scale_y_continuous("POPULATION DENSITY\n (people / sq km)", breaks = seq(100,200,10))
        # for bar graph
      popDensityBar  <- ggplot(data.frame(year,populationDensity), aes(year,populationDensity, fill = year))+
                           geom_bar(stat = "identity", fill = "#00A65A")+
                           ggtitle("A bar graph showing Uganda's population density (1996 - 2016)")+
                           scale_x_continuous("YEAR", breaks = seq(1996,2016,2))+
                           scale_y_continuous("POPULATION DENSITY\n (people / sq km)", breaks = seq(20,200,20))
        # first pick the selection
        poplnDensity_selection <- reactive({
          switch(input$dens_selection,
                 "bar graph" =  popDensityBar,
                 "line graph" = popDensityLine
                 )# end switch
        })
        # plot the results of the selection
        output$dens_plot <- renderPlot({
          poplnDensity_selection()
        })
    # end population density;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    #
    # Births and Deaths section;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        # for Births
        birthsBar <- ggplot(births_and_DeathsData, aes(year,Births))+
                        geom_bar(stat = "identity", fill = "#00A65A")+
                        ggtitle("A bar graph showing Uganda's Births (1996 - 2016)")+
                        scale_x_continuous("YEAR", breaks = seq(1996,2016,2))+
                        scale_y_continuous("BIRTHS")
        deathsBar <- ggplot(births_and_DeathsData, aes(year,Deaths))+
                        geom_bar(stat = "identity", fill = "red")+
                        ggtitle("A bar graph showing Uganda's Deaths (1996 - 2016)")+
                        scale_x_continuous("YEAR", breaks = seq(1996,2016,2))+
                        scale_y_continuous("DEATHS", breaks = seq(35000,400000,10000))
        # for line graph 
        birthsDeathsLine <- ggplot(births_and_DeathsData, aes(year))+
                              geom_line(aes(y = births_and_DeathsData$Births),color = "#00A65A", size = 1)+
                              geom_line(aes(y = births_and_DeathsData$Deaths),color = "red", size = 1)+
                              ggtitle("A line graph showing variation of Uganda's\n Births and Deaths with years (1996 - 2016)")+
                              scale_x_continuous("YEAR", breaks = seq(1996,2016,2))+
                              scale_y_continuous("BIRTHS or DEATHS", breaks = seq(300000,2000000,100000))
        # for bar graph
        #
        # combine
        birthsDeathsCombine <- data.frame(YEARS,Births,Deaths)
        # first reshape the data from wide to long
        birthsDeathsCombine.long <- melt(birthsDeathsCombine)
        birthsDeathsCombine.long
        
        birthsDeathsBar  <- ggplot(birthsDeathsCombine.long, aes(birthsDeathsCombine.long$YEAR,value, fill = variable))+
                              geom_bar(stat = "identity", position = "dodge")+
                              ggtitle("A line graph showing variation of Uganda's\n Births and Deaths with years (1996 - 2016)")+
                              scale_x_discrete("YEAR")+
                              scale_y_continuous("BIRTHS or DEATHS", breaks = seq(300000,2000000,100000))+
                              theme(axis.text.x = element_text(angle=60, hjust=1))
        # first pick the selection
        births_deaths_selection <- reactive({
          switch(input$birthsDeaths_selection,
                 "Births" = birthsBar,
                 "Deaths" = deathsBar,
                 "bar graph" =  birthsDeathsBar,
                 "line graph" = birthsDeathsLine
          )# end switch
        })
        # plot the results of the selection
        output$birthsDeaths_plot <- renderPlot({
          births_deaths_selection()
        })
    # end Births and Deaths section;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    #
    #population Growth section;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        # for population Growth bar
        popGrowthBar <- ggplot(growthData, aes(YEARS,growthData$Growth, fill = YEARS))+
                          geom_bar(stat = "identity")+
                          ggtitle("A bar graph showing Uganda's Population Growth (1996 - 2016)")+
                          scale_x_discrete("YEAR")+
                          scale_y_continuous("POPULATION GROWTH", breaks = seq(0,1300000,100000))+
                          theme(plot.title = element_text(hjust = 0.5))
        # for population Growth line
        popGrowthLine <- ggplot(data.frame(year,growthData), aes(year,growthData$Growth))+
                          geom_line(color = "blue", size = 1)+
                          ggtitle("A line graph showing Uganda's Population Growth (1996 - 2016)")+
                          scale_x_continuous("YEAR", breaks = seq(1996,2016,1))+
                          scale_y_continuous("POPULATION GROWTH", breaks = seq(0,1300000,100000))+
                          theme(plot.title = element_text(hjust = 0.5))
        # select
        growthSelection <- reactive({
          switch(input$growthSelection,
                 "Bar graph" = popGrowthBar,
                 "Line graph" = popGrowthLine
                 )# end switch
        })
        # plot the selection
        output$growthPlot <- renderPlot({
          growthSelection()
        })
        # for population Growth rate bar
        popGrowthRateBar <- ggplot(growthData, aes(YEARS,growthData$Growth_rate, color = YEARS))+
                              geom_bar(stat = "identity", fill = "pink")+
                              ggtitle("A Bar graph showing Uganda's Population Growth Rate (1996 - 2016)")+
                              scale_x_discrete("YEAR")+
                              scale_y_continuous("POPULATION GROWTH\n RATE(%)", breaks = seq(0.0,4.0,0.2))+
                              theme(plot.title = element_text(hjust = 0.5))
        # for population Growth rate line
        popGrowthRateLine <- ggplot(growthData, aes(year, growthData$Growth_rate))+
                              geom_line(color = "blue", size = 1)+
                              ggtitle("A line graph showing Uganda's Population Growth Rate (1996 - 2016)")+
                              scale_x_continuous("YEAR", breaks = seq(1996,2016,1))+
                              scale_y_continuous("POPULATION GROWTH\n RATE(%)", breaks = seq(0.0,4.0,0.2))+
                              theme(plot.title = element_text(hjust = 0.5))
        # select
        growthRateSelection <- reactive({
          switch(input$growthRateSelection,
                 "Bar graph" = popGrowthRateBar,
                 "Line graph" = popGrowthRateLine
          )# end switch
        })
        # plot the selection
        output$growthRatePlot <- renderPlot({
          growthRateSelection()
        })
    # end population Growth section;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    #
    #Birth and Death rate section;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        # for birth rate bar 
        birthRateBar <- ggplot(birthDeathRateData, aes(YEARS, birthDeathRateData$Birth_rate, fill = YEARS))+
                          geom_bar(stat = "identity")+
                          ggtitle("A Bar graph showing Uganda's Birth Rate (1996 - 2016)")+
                          scale_x_discrete("YEAR")+
                          scale_y_continuous("BIRTH RATE\n (births per 1000 population)", breaks = seq(0.00,52.00,5.00))+
                          theme(plot.title = element_text(hjust = 0.5))+
                          theme(axis.text.x = element_text(angle=60, hjust=1))
        # for birth rate line
        birthRateLine <- ggplot(birthDeathRateData, aes(year, birthDeathRateData$Birth_rate))+
                          geom_line(color = "blue", size = 1)+
                          ggtitle("A line graph showing Uganda's Birth Rate (1996 - 2016)")+
                          scale_x_continuous("YEAR", breaks = seq(1996,2016,1))+
                          scale_y_continuous("BIRTH RATE\n (births per 1000 population)", breaks = seq(40.00,52.00,1.00))+
                          theme(plot.title = element_text(hjust = 0.5))+
                          theme(axis.text.x = element_text(angle=60, hjust=1))
        # select
        birthRateSelection <- reactive({
          switch(input$birthRateSelection,
                 "Line graph" = birthRateLine,
                 "Bar graph" = birthRateBar
          )# end switch
        })
        # plot the selection
        output$birthRatePlot <- renderPlot({
          birthRateSelection()
        })
        #
        # for death rate bar 
        deathRateBar <- ggplot(birthDeathRateData, aes(YEARS, birthDeathRateData$Death_rate, fill = YEARS))+
                          geom_bar(stat = "identity")+
                          ggtitle("A Bar graph showing Uganda's Death Rate (1996 - 2016)")+
                          scale_x_discrete("YEAR")+
                          scale_y_continuous("DEATH RATE\n (deaths per 1000 population)", breaks = seq(0.0,18.0,2.0))+
                          theme(plot.title = element_text(hjust = 0.5))+
                          theme(axis.text.x = element_text(angle=60, hjust=1))
        # for death rate line
        deathRateLine <- ggplot(birthDeathRateData, aes(year, birthDeathRateData$Death_rate))+
                          geom_line(color = "red", size = 1)+
                          ggtitle("A line graph showing Uganda's Death Rate (1996 - 2016)")+
                          scale_x_continuous("YEAR", breaks = seq(1996,2016,1))+
                          scale_y_continuous("DEATH RATE\n (deaths per 1000 population)", breaks = seq(9.0,18.0,1.0))+
                          theme(plot.title = element_text(hjust = 0.5))+
                          theme(axis.text.x = element_text(angle=60, hjust=1))
        # select
        deathRateSelection <- reactive({
          switch(input$deathRateSelection,
                 "Line graph" = deathRateLine,
                 "Bar graph" = deathRateBar
          )# end switch
        })
        # plot the selection
        output$deathRatePlot <- renderPlot({
          deathRateSelection()
        })
    # end birth and death rate section;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    #
    #Total population section;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        totalPopLine <- ggplot(totalPopData, aes(year,totalPopData$Total_population))+
                          geom_line(color = "red", size = 1)+
                          ggtitle("A line graph showing Uganda's Total population (1996 - 2016)")+
                          scale_x_continuous("YEAR", breaks = seq(1996,2016,1))+
                          scale_y_continuous("TOTAL POPULATION", breaks = seq(20000000,40000000,1500000))+
                          theme(plot.title = element_text(hjust = 0.5))+
                          theme(axis.text.x = element_text(angle=60, hjust=1))
        output$totalPopPlot <- renderPlot({
          totalPopLine
        })
    # end total population section;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    #
    #Natural Increase;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        # for Natural Increase bar
        naturalIncBar <- ggplot(naturalIncData, aes(YEARS,naturalIncData$Natural_increase, fill = YEARS))+
                            geom_bar(stat = "identity")+
                            ggtitle("A bar graph showing Uganda's Natural Increase (1996 - 2016)")+
                            scale_x_discrete("YEAR")+
                            scale_y_continuous("NATURAL INCREASE", breaks = seq(0,1300000,100000))+
                            theme(plot.title = element_text(hjust = 0.5))
        # for Natural Increase line
        naturalIncLine <- ggplot(naturalIncData, aes(year,naturalIncData$Natural_increase))+
                              geom_line(color = "blue", size = 1)+
                              ggtitle("A line graph showing Uganda's Natural Increase (1996 - 2016)")+
                              scale_x_continuous("YEAR", breaks = seq(1996,2016,1))+
                              scale_y_continuous("NATURAL INCREASE", breaks = seq(500000,1300000,100000))+
                              theme(plot.title = element_text(hjust = 0.5))
        # select
        naturalIncSelection <- reactive({
          switch(input$naturalIncSelection,
                 "Bar graph" = naturalIncBar,
                 "Line graph" = naturalIncLine
          )# end switch
        })
        # plot the selection
        output$naturalIncPlot <- renderPlot({
          naturalIncSelection()
        })
        # for Rate of Natural Increase bar
        naturalIncRateBar <- ggplot(naturalIncData, aes(YEARS,naturalIncData$Rate_of_natural_increase, fill = YEARS))+
                                geom_bar(stat = "identity")+
                                ggtitle("A bar graph showing Uganda's Rate of Natural Increase (1996 - 2016)")+
                                scale_x_discrete("YEAR")+
                                scale_y_continuous("RATE OF NATURAL INCREASE\n(%)", breaks = seq(0.00,4.00,0.50))+
                                theme(plot.title = element_text(hjust = 0.5))
        # for Natural Increase line
        naturalIncRateLine <- ggplot(naturalIncData, aes(year,naturalIncData$Rate_of_natural_increase))+
                                geom_line(color = "blue", size = 1)+
                                ggtitle("A line graph showing Uganda's Rate of Natural Increase (1996 - 2016)")+
                                scale_x_continuous("YEAR", breaks = seq(1996,2016,1))+
                                scale_y_continuous("RATE OF NATURAL INCREASE\n(%)", breaks = seq(3.00,4.00,0.01))+
                                theme(plot.title = element_text(hjust = 0.5))
        # select
        naturalIncRateSelection <- reactive({
          switch(input$naturalIncRateSelection,
                 "Bar graph" = naturalIncRateBar,
                 "Line graph" = naturalIncRateLine
          )# end switch
        })
        # plot the selection
        output$naturalIncRatePlot <- renderPlot({
          naturalIncRateSelection()
        })
    # end Natural increase section;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    #
    #Migrations section;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        # for migrations bar
        migrationBar <- ggplot(migrationsData, aes(YEARS,migrationsData$Net_Migrants, fill = YEARS))+
                          geom_bar(stat = "identity")+
                          ggtitle("A bar graph showing Uganda's Net Migrants (1996 - 2016)")+
                          scale_x_discrete("YEAR")+
                          scale_y_continuous("NET MIGRANTS", breaks = seq(-140000,30000,10000))+
                          theme(plot.title = element_text(hjust = 0.5))
        # for migrations line
        migrationLine <- ggplot(migrationsData, aes(year,migrationsData$Net_Migrants))+
                          geom_line(color = "#00A65A", size = 1)+
                          ggtitle("A line graph showing Uganda's Net Migrants (1996 - 2016)")+
                          scale_x_continuous("YEAR", breaks = seq(1996,2016,1))+
                          scale_y_continuous("NET MIGRANTS", breaks = seq(-140000,30000,10000))+
                          theme(plot.title = element_text(hjust = 0.5))
        # select
        migrationsSelection <- reactive({
          switch(input$migrationsSelection,
                 "Bar graph" = migrationBar,
                 "Line graph" = migrationLine
          )# end switch
        })
        # plot the selection
        output$migrationsPlot <- renderPlot({
          migrationsSelection()
        })
        #
        # for migration rate bar
        migrationRateBar <- ggplot(migrationsData, aes(YEARS,migrationsData$Migration_rate, fill = YEARS))+
                              geom_bar(stat = "identity")+
                              ggtitle("A bar graph showing Uganda's Migration Rate (1996 - 2016)")+
                              scale_x_discrete("YEAR")+
                              scale_y_continuous("MIGRATION RATE (%)", breaks = seq(-7.0,2.0,1.0))+
                              theme(plot.title = element_text(hjust = 0.5))
        # for migrations line
        migrationRateLine <- ggplot(migrationsData, aes(year,migrationsData$Migration_rate))+
                                geom_line(color = "#00A65A", size = 1)+
                                ggtitle("A line graph showing Uganda's Migration Rate (1996 - 2016)")+
                                scale_x_continuous("YEAR", breaks = seq(1996,2016,1))+
                                scale_y_continuous("NATURAL INCREASE", breaks = seq(-7.0,2.0,1.0))+
                                theme(plot.title = element_text(hjust = 0.5))
        # select
        migrationRateSelection <- reactive({
          switch(input$migrationRateSelection,
                 "Bar graph" = migrationRateBar,
                 "Line graph" = migrationRateLine
          )# end switch
        })
        # plot the selection
        output$migrationRatePlot <- renderPlot({
          migrationRateSelection()
        })
    # end Migrations section;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  # end plots section---------------------------------------------------------------------------------------
  
############################################## the table outputs #######
        # for general population Dynamics
  names(generalPopn_Dynamics) <- c(
                                    "Year\n",
                                    "Births\n",
                                    "Deaths\n",
                                    "Total\n population",
                                    "Natural\n increase"
    
                                  )
  output$first10g <- renderTable({
    generalPopn_Dynamics[1:11,]
  })
  output$last10g <- renderTable({
    generalPopn_Dynamics[12:21,]
  })
        # for population dyanamics per 1000 population
  names(populationDyanamics_per_1000_popn) <- c(
                                                  "Year\n",
                                                  "Births\n per1000\n population",
                                                  "Deaths\n per1000\n population",
                                                  "Net_Migrants\n per1000\n population"
    
                                                )# end names
  output$first10p <- renderTable({
    populationDyanamics_per_1000_popn[1:11,]
  })
  
        # for population dynamics per 1000 population
  output$last10p <- renderTable({
    populationDyanamics_per_1000_popn[12:21,]
  })
        # for other population Dynamics
  names(otherPopulationDynamics) <- c(
                                        "Year\n",
                                        "Net Number\n of Migrants",
                                        "Population\n change",
                                        "Growth rate\n (%)",
                                        "Rate of\n Natural\n increase"
                                      )# end names
  output$first10o <- renderTable({
    otherPopulationDynamics[1:11,]
  })
  output$last10o <- renderTable({
    otherPopulationDynamics[12:21,]
  })
      # for population Density
  popDensityTable <- data.frame(year,Total_population,populationDensity)
  names(popDensityTable) <- c ("Year\n","Total\n population","Population\n Denisty\n (people per\n sq km)")
  output$dens_table <- renderDataTable({ popDensityTable},
                                 options = list(pageLength = 11)
                                 )
      # for Births and Deaths
  output$birthsDeaths_table <- renderDataTable({births_and_DeathsData},
                                               options = list(pageLength = 11, # show only 11 rows at a time
                                                              autoWidth = TRUE,
                                                              columnDefs = list(list(width = "150px",
                                                                                     targets = "_all")
                                                                                )
                                                              )
                                               )# end data table
      # for population Growth
  output$growthTable <- renderDataTable({growthData},
                                              options = list(pageLength = 10, # show only 10 rows at a time
                                                             columnDefs = list(list(width = "10px",
                                                                                    targets = "_all")
                                                                               )
                                                              )
                                              )# end data table
  # for birth and death rates
  output$birthDeathRateTable <- renderDataTable({birthDeathRateData},
                                                options = list(pageLength = 10, # show only 10 rows at a time
                                                               autoWidth = TRUE,
                                                               columnDefs = list(list(width = "150px",
                                                                                      targets = "_all")
                                                                                  )
                                                              )   
                                              )# end data table
  # for total population
  output$totalPopTable <- renderDataTable({totalPopData},
                                                options = list(pageLength = 11, # show only 11 rows at a time
                                                               autoWidth = TRUE,
                                                               columnDefs = list(list(width = "50px",
                                                                                      targets = "_all")
                                                               )
                                                ) 
                                          )# end data table
  # for Natural Increase
  output$naturalIncTable <- renderDataTable({naturalIncData},
                                              options = list(pageLength = 10, # show only 10 rows at a time
                                                             autoWidth = TRUE,
                                                             columnDefs = list(list(width = "50px",
                                                                                    targets = "_all")
                                                                                )
                                                              )
                                          )# end data table
  # for Migrations
  output$migrationsTable <- renderDataTable({migrationsData},
                                                options = list(pageLength = 10, # show only 10 rows at a time
                                                               autoWidth = TRUE,
                                                               columnDefs = list(list(width = "50px",
                                                                                      targets = "_all")
                                                                                )
                                                               )
                                            )# end data table
  # end table outputs---------------------------------------------------------------------------------------
  
############################################## download section------------------------------------------
        # download for general population characteristics
  output$gdownload <- downloadHandler(
    filename = function(){
      paste(generalPopn_Dynamics, '.csv', sep = '')
    },
    content = function(file){
      write.csv(generalPopn_Dynamics,file)
    }
  )
      # download for population dynamics per 1000 population
  output$pdownload <- downloadHandler(
    filename = function(){
      paste(populationDyanamics_per_1000_popn, '.csv', sep = '')
    },
    content = function(file){
      write.csv(populationDyanamics_per_1000_popn,file)
    }
  )
      # download for other population Dynamics
  output$odownload <- downloadHandler(
    filename = function(){
      paste(otherPopulationDynamics, '.csv', sep = '')
    },
    content = function(file){
      write.csv(otherPopulationDynamics,file)
    }
  )
  # end download section------------------------------------------------------------------------------------
  
############################################## upload data section #######
      # set the file size to be not greater than 10Mb
      options(shiny.maxRequestSize = 10*1024^2)
      # Handle the input file........................................................
      inputFile <- NULL
      inputFileData <- NULL
        # make data frame out of the file uploaded
        inputFileData <- reactive({
                              inputFile <- input$uploadedFile
                              if(is.null(inputFile)){
                                return(NULL)
                              }
                              # verify that the file is less than 10Mb in oder to regulate upload bandwidth
                              if(file.info(inputFile$datapath)$size > (10*1024^2)){
                                output$notify <- renderText("File size is greater than acceptable size of 10Mb.") # reject file and notify the user
                              }
                              # save the data
                              read.csv(inputFile$datapath, header = TRUE) # accept the file
                              # set the column names
                              #inputFileData <- correctNames(inputFileData)  
                          })
        #
        #
        # handle output when file is uploaded
        observeEvent(input$uploadedFile,{
        # ensure that the table is not larger than 8 columns       
        if(ncol(inputFileData()) <= 8){
            #accept to display the table
            output$inputData <- renderDataTable({inputFileData()},
                                                options = list(pageLength = 12, # show only 12 rows at a time
                                                               autoWidth = TRUE,
                                                               columnDefs = list(list(width = "200px",targets = "_all"))
                                                ))
        }else{
            # notify the user that data is saved but dont display the  table
            output$saved <- renderText(
              "Table cannot fit in this area but data is saved,
              go ahead and make plots of your data."
            )
        }
        # do something
        output$doSomething <- renderUI({
          textOutput(outputId = "dosmth")
        })
        output$dosmth <- renderText("Select what to Plot.")
        
        #selects+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        # for x values
        output$xValues <- renderUI({
          # if dataframe is empty return to avoid errors in later functions
          if(is.null(inputFileData())){
            return(NULL)
          }
          else{
            items <- names(inputFileData()) # get the names of the input dataframe
            # create the select input
            selectInput(inputId = "xvalues",
                        label = "Choose value to represent on horizontal axis.",
                        choices = items
            )# end select
          }
        })
        # for y values
        output$yValues <- renderUI({
          # if dataframe is empty return to avoid errors in later functions
          if(is.null(inputFileData())){
            return(NULL)
          }
          else{
            items <- names(inputFileData()) # get the names of the input dataframe
            # create the select input
            selectInput(inputId = "yvalues",
                        label = "Choose value to represent on vertical axis.",
                        choices = items
            )# end select
          }
        })
        # end selects+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        # give labels---------------------------------------------------------
        # for x label
        output$xlabel <- renderUI({
          textInput(inputId = "horizo", label = "Give horizontal axis label.")
        })
        # for y label
        output$ylabel <- renderUI({
          textInput(inputId = "vertic", label = "Give vertical axis.")
        })
        # for title
        output$plotTitle <- renderUI({
          textInput(inputId = "plot_title", label = "Give title to your plot.")
        })
        # end give labels-----------------------------------------------------
        # for plot button
        output$plotButton <- renderUI({
          actionButton(inputId = "plot", label = "Plot")
        })
        
      })# end observeEvents
        #
        #
        #
      # get selections for each axis
          # for x
          xVar <- reactive({
            inputFileData()[[input$xvalues]] 
          })
          # for y
          yVar <- reactive({
            inputFileData()[[input$yvalues]]
          })
      # plot the selection;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        # labels and title
          plottitle <- reactive({input$plot_title}) # title
          x_label <- reactive({input$horizo})# x label
          y_label <- reactive({input$vertic})# y label
          #
          #
          # for line graph
          lineGraph <- reactive({
            ggplot(inputFileData(), aes(xVar(), yVar(),fill = xVar()))+
              geom_line(color = "red", size = 1)+
              ggtitle(plottitle())+
              labs(x = x_label(), y = y_label())+
              theme(axis.text.x = element_text(angle=60, hjust=1))+
              theme(plot.title = element_text(hjust = 0.5))
          })
          # for bar graph
          barGraph <- reactive({
            ggplot(inputFileData(), aes(xVar(), yVar(),fill = xVar()))+
              geom_bar(stat = "identity", fill = "#00A65A")+
              ggtitle(plottitle())+
              labs(x = x_label(), y = y_label())+
              theme(axis.text.x = element_text(angle=60, hjust=1))+
              theme(plot.title = element_text(hjust = 0.5))
          })
          # for box plot
          boxPlotGraph <- reactive({
            ggplot(inputFileData(), aes(xVar(), yVar(),color = xVar()))+
              geom_boxplot()+
              ggtitle(plottitle())+
              labs(x = x_label(), y = y_label())+
              theme(axis.text.x = element_text(angle=60, hjust=1))+
              theme(plot.title = element_text(hjust = 0.5))
          })
          # select what type of plot
          selected_plot <- reactive({
            switch(input$selectedPlot,
                                "bar graph" = barGraph(),
                                "line graph" = lineGraph(),
                                "boxplot" = boxPlotGraph()
                   )# end switch
          })
          # make plot if the button is clicked
          observeEvent(input$plot,{
              # display the what to plot select
              output$whatToPlot <- renderUI({
                selectInput(inputId = "selectedPlot",
                            label = "Show on.",
                            choices = c(
                                        "bar graph",
                                        "line graph",
                                        "boxplot"
                                        )
                            )
              })
              # display plot
              output$displayPlot <- renderPlot({
                selected_plot()
              })
              #display download button and help text
              output$savePlot <- renderText({
                "To save this Plot click the download button. "
              })
              output$downloadPlot <- renderUI({
                downloadButton(outputId = "thePlot", label = "Download Plot")
              })
          })# end observeEvent
          # download the plot and save it on the user's computer
          output$thePlot <- downloadHandler(
            filename = function(){
              paste("plot", '.png', sep = '')
            },
            content = function(file){
              ggsave(file,selected_plot())
            }
          )
      # end plot section;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      #....................................................................................... 
   # end ulpoad data section---------------------------------------------------------------------------------
############################################## predictions section ######
     # for total population;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          totalPopulation_ts <- ts(Total_population, frequency = 1, start=c(1996,2), end=c(2016,2))# create the time serries
          # create a forecast plot object
          totalPopulation_forecast <- autoplot(totalPopulation_ts)+ geom_forecast(h = 5)+
                                        ggtitle("Predictions for the total Population of Uganda, 2017 to 2022")+
                                        labs(x = "YEARS", y = "Total Population")+
                                        scale_x_continuous(breaks = seq(1996,2025,2))+
                                        scale_y_continuous(breaks = seq(15000000,50000000,1600000))+
                                        theme(plot.title = element_text(hjust = 0.5))
          # the plot        
          output$popPrediction <- renderPlot({
            totalPopulation_forecast
          })
          # download
          output$totalPopPredic <- downloadHandler(
            filename = function(){
              paste("total_populationPrediction",'.png', sep = '')
            },
            content = function(file){
              ggsave(file,totalPopulation_forecast)
            }
          )
      # for total births;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          totalBirths_ts <- ts(Births, frequency = 1, start = c(1996,2), end = c(2016,2)) # create time series for Births
          # create forecast plot object
          totalBirths_forecast <- autoplot(totalBirths_ts)+geom_forecast(h = 6)+
                                      ggtitle("Predictions for the total Births of Uganda, 2017 to 2022")+
                                      labs(x = "YEARS", y = "Total Births")+
                                      scale_x_continuous(breaks = seq(1996,2024,2))+
                                      scale_y_continuous(breaks = seq(1000000,2000000,100000))+
                                      theme(plot.title = element_text(hjust = 0.5))
          # the plot
          output$birthsPrediction <- renderPlot({
            totalBirths_forecast
          })
          # download
          output$totalBirthsPredic <- downloadHandler(
            filename = function(){
              paste("births_Prediction",'.png', sep = '')
            },
            content = function(file){
              ggsave(file,totalBirths_forecast)
            }
          )
      # for total deaths;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          totalDeaths_ts <- ts(Deaths, frequency = 1, start = c(1996,2), end = c(2016,2)) # create time series for Deaths
          # create forecast plot object
          totalDeaths_forecast <- autoplot(totalDeaths_ts) + geom_forecast(h = 4)+
                                      ggtitle("Predictions for the total Deaths of Uganda, 2017 to 2022")+
                                      labs(x = "YEARS", y = "Total Deaths")+
                                      scale_x_continuous(breaks = seq(1996,2021,2))+
                                      scale_y_continuous(breaks = seq(300000,500000,10000))+
                                      theme(plot.title = element_text(hjust = 0.5))
          output$deathsthsPrediction <- renderPlot({
            totalDeaths_forecast
          })
          # download
          output$totalDeathsPredic <- downloadHandler(
            filename = function(){
              paste("deaths_Prediction",'.png', sep = '')
            },
            content = function(file){
              ggsave(file,totalDeaths_forecast)
            }
          )
      # for natural Increase;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          naturalIncrease_ts <- ts(Natural_increase, frequency = 1, start = c(1996), end = c(2016))#create time series for natural increase
          # create forecast plot object
          naturalIncrease_forecast <- autoplot(naturalIncrease_ts) + geom_forecast(h = 6)+
                                        ggtitle("Predictions for the Natural Increase")+
                                        labs(x = "YEARS", y = "Natural Increase")+
                                        scale_x_continuous(breaks = seq(1996,2021,2))+
                                        scale_y_continuous(breaks = seq(500000,1500000,100000))+
                                        theme(plot.title = element_text(hjust = 0.5))
          output$naturalIncreasePrediction <- renderPlot({
            naturalIncrease_forecast
          })
            
          
   #end the predictions section------------------------------------------------------------------------------
############################################## faqs section #####
   # for Rate Us;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          # for agreed
          output$agreed <- renderInfoBox({
            infoBox("Rating", paste0(5 + input$agree, " people agree"), color = "teal")
          })
          # for disagreed
          output$disagreed <- renderInfoBox({
            infoBox("Rating", paste0(0 + input$disagree, " people disagree"), color = "red",icon = icon("list"))
          })
   # end Rate Us;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   #
   # for FAQs;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          
          # get the input qn
          inputQn <- reactive({
            input$question
          })
          # display the qn back
          observeEvent(input$askQn,{
            
            output$askedQn <- renderText({
              inputQn()
            })
            #
            output$qn <- renderUI({
              textOutput(outputId = "askedQn")
            })
          })
          
          
   # end FAQs;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          
   # end faqs section----------------------------------------------------------------------------------------
})# end shinyServer------------------------------------------------------------------------------------------
