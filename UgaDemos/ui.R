
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotrix)
library(reshape2)
library(ggvis)
library(tseries)
library(forecast)

############################# Here some are some general Variables #####################################################################################################
# the title variable
logo <- dashboardHeader(title = tags$img(src = "img/logoFinal.png", height=60,width=100))
# the page title
tags$head(
  tags$title("UgaDemos")
  
)
# this is the heading for the tabBox
ugaDemos <- tags$h1(
                style = "color:black;
                         font-weight:bold;
                         text-align:right;
                         margin: 0px 0 8px;
                         text-shadow:-4px 4px 6px green;
	                       padding:0px 0;
                         font-size:250%;
                ",
                "UgaDemos",span(style = "color:red;
                                          font-size:0.4em;
                                          height:1em;
                                          position:relative;
                                          bottom:-2ex;
                                          left:-13ex;
                                        ",
                                "together we analyze")
            )
# system time
timeDate <- Sys.Date()
# the footer
footer <- fluidRow(
  tags$h6(style = "text-align:center;",HTML("&copy;"),"UgaDemos","   ",timeDate)
  
)

##########################################################################################################################
# the User interface
shinyUI(
  fluidPage(
    # head part***************************************************************
    tags$head(
      tags$title("UgaDemos"),
      
      # the styles to format some html elements
      tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"
      ),
      tags$style(
        HTML("
             h4{
                  color:black;
                  font-family:Arial,helvetica;
                  font-weight:bold;
                  padding:4px 0;
                  text-decoration:underline;
             }
             h3{
                  font-size:300%;
                  font-weight:bold;
             }
            .tabs{
                  color:green;
                  text-decoration:none;
            }
            .modi{
                  color:#00A65A;
            }
            .onRight{
                  text-align:right;
                   
            }
            h5{
                font-weight:bold;
            }
            hr{
                border: 1px solid green;
            }
             ")
      )
      
    ), # end head
    # the body part**********************************************************
    tags$body(
      
      dashboardPage( skin = "green", 
                     ##################################### the header section #############################################################################################
                     logo,
                     
                     # the side bar Menu
                     ############################################# the side bar Menu ##########################################################################################
                     dashboardSidebar(
                       sidebarMenu(
                         # home page
                         menuItem("HOME", tabName = "home", icon = icon("th")),
                         #populations page
                         menuItem("POPULATIONS", tabName = "populations", icon = icon("credit-card")),
                         #statistics page
                         menuItem("STATISTICS", tabName = "statistics", icon = icon("bar-chart-o")),
                         #get involved
                         menuItem("GET INVOLVED", tabName = "getInvolved", icon = icon("dashboard")),
                         # predictions page
                         menuItem("PREDICTIONS", tabName = "predictions", icon = icon("calendar")),
                         #FAQs page
                         menuItem("FAQs", tabName = "faqs", icon = icon("list-alt"))
                       )# end sidebar Menu
                     ),# end the side Bar Menu
                     
                     #dashboardBoady
                     #######################################the body section ########################################################################################
                     dashboardBody(
                       tabItems(
                       ################################################## HOME PAGE ############################################
                         tabItem(tabName = "home",
                                 ugaDemos,hr(), # header part
                                 tabBox( id ="ugademos", height = "750px", width = "500px",
                                         tabPanel(title = span(class= "tabs","Overview"),
                                                  column(8,offset = 0,tags$h3("Uganda"),br(),
                                                            tags$h4("Location and size."),
                                                            tags$p("Uganda is located in East Africa and lies across the equator, about 800 kilometres inland from
	                                                                  the Indian Ocean. It lies between 10 29’ South and 40 12’ North latitude, 290 34 East and 350
                                                                    0’ East longitude. The country is landlocked, bordered by Kenya in the East; South Sudan in the
                                                                    North; Democratic Republic of Congo in the West; Tanzania in the South; and Rwanda in South
                                                                    West. It has a total area of 241,551 square kilometers, of which the land area covers 200,523
                                                                    square kilometres."
                                                                   ),
                                                            tags$h4("Administration."),
                                                            tags$p("The country is divided into 111 districts and one capital city. The districts are further subdivided into
                                                                    Counties, Sub counties and Parishes. The role of these local governments is to implement and
                                                                    monitor government programmes at the respective levels. Overtime, the administrative units have
                                                                    been sub-divided with the aim of easing administration and improving the delivery of services."
                                                                   ),
                                                            tags$h4("Geography."),
                                                            tags$p("The country enjoys equatorial climate with plenty of rain and sunshine moderated by the relatively
                                                                    high altitude. In most parts of the country, the mean annual temperatures range from 16 degrees C to
                                                                    30 degrees C. Nevertheless, the Northern and Eastern regions sometimes experience relatively high
                                                                    temperatures exceeding 30 degree C and the South Western region sometimes has temperatures
                                                                    below 16 degrees C.",br(),
                                                                    "The Central, Western and Eastern regions have two rainy seasons, from March to May for the
                                                                    first rains, and the second rains from September to November. The Northern region receives
                                                                    one rainy season from April to October, and the period from November to March has minimal
                                                                    rain. Most of the areas in the country receives between 750 mm and 2,100 mm of rain annually."
                                                                   )
                                                         ),
                                                  # the map area
                                                  column(4,offset = 0,
                                                             tags$h4("Current Districts and Urban Centers "),br(),
                                                             tags$img(src = "img/uganda.png", height = 500, width = 344)
                                                         )
                                                  
                                         ),# end overview
                                         tabPanel(title = span(class = "tabs","About UgaDemos"),
                                                  # image section
                                                  column(4, offset = 0,
                                                         tags$img(src = "img/logoFinal.png", height = 400, width = 300)
                                                         ),#end image section
                                                  column(8, offset = 0,
                                                           tags$h3("About UgaDemos"), br(),
                                                           p("UgaDemos is a web based system which comes up with a thorough analysis of Uganda's population for the past 20 years
                                                              and then visualise the trends in population.The system is able to give supportive information for the change, and 
                                                              finally be able to predict future population trends."
                                                                  ),
                                                           tags$p("Demographic and socio-economic data are useful for planning and evidence-based decision making in any country.
                                                                   Such data are collected through Population Censuses,Demographic and Socio-economic Surveys,Civil Registration 
                                                                   Systems and other Administrative sources. In Uganda, however,the Population and Housing Census remains the
                                                                   main source of demographic data.",br(),strong("reminder:")," although UgaDemos does a thorough population 
                                                                   analysis,it should be remembered that the predictions are not 100% always correct."
                                                                  )
                                                         )# end description section.
                                         )# end about UgaDemos    
                                 ),# end tabBox for home
                                 # the footer
                                 footer
                         ),# end Home*************************************************************************************************************************************
                       ################################################## POPULATIONS PAGE ############################################
                         tabItem(tabName = "populations",
                                 ugaDemos,hr(), # header part
                                 # this is for general population characteristics*********************************
                                 box( height = "auto", width = "500px",
                                       tags$h5("General population characteristics."),
                                       tags$p("Generally,demographic data is characterized by the total number of people including all age groups which is Total population in this case,
                                               Births which are the total number of live children produced in a particular year when the population data was collected. Deaths refers to the total
                                               number of recorded dead people in a give time period for the conuntry or place normally annual basis. Natural increase is the differnce between the 
                                               Births and the Deaths of a certain year."
                                            ),
                                       # first 10 years 1996 to 2005
                                       column(6, offset = 0,
                                              tableOutput("first10g")
                                       ),
                                       # last 10 years 2006 to 2016.
                                       column(6, offset = 0,
                                              tableOutput("last10g")),
                                      # download table
                                      tags$p(" To download this data click the Download button ",
                                               downloadButton(outputId ="gdownload", label = "Download")
                                             )
                                 ),# end box 1
                                 # this is for population dynamics per 1000 population****************************
                                 box(height = "auto", width = "500px",
                                       tags$h5(" Population characteristics per 1000 population."),
                                       tags$p("  This section gives the demographics data per 1000 population of Uganda for the past 20 years, it includes features 
                                                 such as Births per 1000 population; this is the same as Birth rate, calculated by dividing the total borths by the 
                                                 total mid year population then multiply the result by 1,000
                                                 Deaths per 1000 population; this is also called Death rate calculated by dividing the total Deaths by the mid year 
                                                 population then the result is multiplied by 1,000 and Net number of migrants per 1000 population; this is the same
                                                 as Migration rate calculated by dividing the Net number of migrants by the total population then multiply by 1,000."
                                       ),
                                       column(6, offset = 0, 
                                              tableOutput("first10p")),
                                       column(6, offset = 0,
                                              tableOutput("last10p")),
                                      # download table
                                      tags$p(" To download this data click the Download button ",
                                             downloadButton(outputId = "pdownload", label = "Download")
                                             )
                                 ),# end box 2
                                 # this section is for other population characteristics*****************************
                                 box( height = "auto", width = "500px",
                                      tags$h5(" Other population characteristics."),
                                      tags$p(" Demorgraphics data also includes other vital features like Net number of migrants which is the difference between 
                                              immigrants and emigrants, Population change same as the increment or decrement in the total population from one year
                                              to the next"
                                             ),
                                      column(6, offset = 0,
                                             tableOutput("first10o")),
                                      column(6, offset = 0,
                                             tableOutput("last10o"),
                                        # download table
                                        tags$p(" To download this data click the Download button",
                                               downloadButton(outputId = "odownload", label = "Download")
                                               )
                                           )
                                 ),# end box 3
                               # the footer
                                 footer
                         ),# end populations***********************************************************************************************************************************
                       ################################################## STATISTICS PAGE ############################################
                         tabItem(tabName = "statistics",
                                 ugaDemos,hr(), # header part
                                 tabBox(id = "stats", height = "auto", width = "auto",
                                        ################################################## Density
                                         tabPanel(title = span(class = "tabs","Density"),
                                                    tags$p(strong("Population Density: "),
                                                           br(),
                                                           span( style = "text-indent:50px;","
                                                                 is the number of people living in an area, this is expressed as the number of people per 
                                                                 square kilo meter ",strong("(people / sq km)"),". The table and the bar chart below show
                                                                 how Uganda's population density has been changing since 1996 to 2016.")
                                                         ),br(),
                                                    # for table
                                                    column(6, offset = 0,
                                                            dataTableOutput(outputId = "dens_table")
                                                           ),
                                                    # for bar chart
                                                    column(6, offset = 0,
                                                           selectInput(inputId = "dens_selection",
                                                                       label = "Show on",
                                                                       choices = c("bar graph","line graph"),
                                                                       width = "200px" 
                                                                       ),
                                                            br(),
                                                            plotOutput("dens_plot",height = "480px")
                                                           ),tags$br(),
                                                    tags$p("                 . ",tags$br()),
                                                    tags$h5("Observation:"),
                                                    tags$p("Population density is calculated by dividing the total population by the total area. Uganda has a total
                                                            area of ",strong("200,523 sq km"),". The graps ploted above shows that there has been a general increase 
                                                            in the number of people living per square kilo meter since 1996 (100) to 2016 (191)."
                                                           ),
                                                    tags$h5("conclusion:"),
                                                    tags$p(" The large free areas present in Uganda in 1996, this is due to the low population density of 100 people
                                                             per square km, gave the available population chance to produce more as they were sure that there's enough
                                                             space to occupy thus accounting for the general increase in the population density."
                                                           ),
                                                    tags$p(" The general increase in the population density is due to both physical and human factors. The fact that 
                                                             Uganda has most of its areas with fertile soils (loam and alluvium soils) has encouraged Agriculture, 
                                                             this has brought in investors (immigrants) and since there's assurance for food, this has also made births
                                                             to increase which leads to increase in population and population density has to go high as well.",br()," The 
                                                             current population density of Uganda is approximately 207 people per.sq km"
                                                           )
                                                  ),# end Density
                                        ####################################################### Births and Deaths
                                         tabPanel(title = span(class = "tabs","Births\n and\n Deaths"),
                                                    tags$p(strong("Births and Deaths of Uganda (1996 - 20016): "),
                                                           br(),
                                                           span( style = "text-indent:50px;","
                                                                 In this section we discuss the two of the major population determining characteristics i.e. Births and
                                                                 Deaths. ",strong("Births")," reffers to the total number of live children given birth to in a specific year,
                                                                  this feature directly affects population of Uganda because, its derivative called ",strong("Birth rate"),
                                                                 " which is calculated by dividing the total number of Births by the total population multiplied by 1000, 
                                                                 is a major ceriteria while determining how fast the population is growing.",br(),strong("Deaths")," is the
                                                                 total number of recorded dead people in a specified periond of time, usually a year when it comes to demographics.
                                                                 Deaths also have a derivative called ", strong("Death rate")," this is calcualted by dividing the total Deaths by 
                                                                 the total population multiplied by 1000."
                                                                 )
                                                           ),br(),
                                                  # for table
                                                  column(6, offset = 0,
                                                         dataTableOutput(outputId = "birthsDeaths_table")
                                                  ),
                                                  # for chart
                                                  column(6, offset = 0,
                                                         selectInput(inputId = "birthsDeaths_selection",
                                                                     label = "Show",
                                                                     choices = c(
                                                                                 "Births",
                                                                                 "Deaths",
                                                                                 "bar graph",
                                                                                 "line graph"),
                                                                     width = "200px" 
                                                         ),
                                                         br(),
                                                         plotOutput("birthsDeaths_plot",height = "480px")
                                                  ),tags$br(),
                                                  tags$p("                 . ",tags$br()),
                                                  tags$h5("observation"),
                                                  tags$p("The graphics above clearly show that Uganda's births have not gone below 1000000, and there is a general increase in the 
                                                          number of children born from one year to the next. On the other hand, the total deaths have silightly increased and have not
                                                          gone past 400000."),
                                                  tags$h5("conclusion"),
                                                  tags$p(" The increase in Births is due to a number of reasons, and above all is the high fertility rate of Ugandan women of 7.1 
                                                          children per woman in 1996 to 5.7 in 2016. ", strong("Fertility rate, "),"represents the number of 
                                                          children that would be born to a woman if she were to live to the end of her childbearing years and bear children in accordance
                                                          with current age-specific fertility rates. The high school dropouts recorded in Uganda where school children end up getting married 
                                                          at an early age because they don't have anything else to do at that time, getting married at an early age means a high fertility rate.
                                                          ",br(),
                                                          " Also the improvement in health care services which surpresses the infant mortality rate led to the increase in the recorded Births.")
                                                  ),# end Births and Deaths
                                        ################################################# Birth and Death Rate
                                         tabPanel(title = span(class = "tabs","Birth and\n Death Rates"),
                                                  tags$p(strong("Birth Rate:")," this can also be reffered to as Births per 1,000 population which is calculated by dividing the total number of
                                                          of live births by the total population multiplied by 1,000, it is sometimes refferd to as Crude Birth Rate CBR."),
                                                  tags$br("CBR = (Births x 1000) / total population."),
                                                  tags$p(" CBR of 10 - 20 births per 1000 population is considered as low CBR while 40 - 50 births per 1,000 population is considerd high birth 
                                                           rate. We need to take note at this point that both exremes have challenges, high CBR stresses government welfare and family programs.
                                                           On addition to stressing government welfare, educating a growing number of children, and later creating employment for them when they
                                                           reach the working age group constrain the nation's programs and also come in with high population problems like reduced land for 
                                                           agriculture because most of the land is used for housing, and industrialisation in order to create employment, this in the long run 
                                                           reduces food production."),
                                                  tags$p(strong("Death Rate:")," this is the number of deaths per 1000 population, it is also known as the Crude Death Rate which is the total 
                                                           number of deaths to residents in a specified geographic area (country, state, county, etc.) divided by the total population for the 
                                                           same geographic area (for a specified time period, usually a calendar year) and multiplied by 1,000."
                                                         ),
                                                  tags$p(" The table below summerises uganda's Birth  and Death rates from 1996 to 2016."),
                                                  # the table
                                                  dataTableOutput(outputId = "birthDeathRateTable"),
                                                  # the plots
                                                  fluidRow(
                                                    column(6, offset = 0,
                                                               #the plot for birth  rate
                                                               tags$p(style = "text-align:right; align: right;",
                                                                      selectInput(inputId = "birthRateSelection",
                                                                                  label = "Show on..",
                                                                                  choices = c("Line graph","Bar graph"))
                                                               ),
                                                               plotOutput("birthRatePlot" ,height = "300px"),
                                                               tags$br()
                                                           ),# birth rate
                                                    column(6, offset = 0,
                                                               #the plot for death  rate
                                                               tags$p(style = "text-align:right; align: right;",
                                                                      selectInput(inputId = "deathRateSelection",
                                                                                  label = "Show on..",
                                                                                  choices = c("Line graph","Bar graph"))
                                                               ),
                                                               plotOutput("deathRatePlot" ,height = "300px")
                                                           )#death rate
                                                    ),# end fluidRow
                                                  tags$br(),
                                                  tags$h5("Observation."),
                                                  tags$p("From the table above it clearly indicate that Uganda is One of the African countries with a high birth rate in the sub sahara region 
                                                          and highest in East Africa. However, the graphs above clearly show that there was a decline in both the birth rate and the death rate
                                                          of Uganda from 1996 to 2016, from 51.0 births per 1000 population to about 43.4 births per 1000 population for the case of birth rate,
                                                          and from 17.5 deaths per 1000 population to 10.4 deaths per 1000 population. Currently in 2017 Uganda has a birth rate of 42.5 births 
                                                          per 1000 population and a death rate of 9.3 deaths per 1000 population indicating that these two population measures have continued to
                                                          reduce due to some of the reasons explained in the conclusion section below."
                                                         ),
                                                  tags$h5("conclusion."),
                                                  tags$p("Typically, high birth rates are associated with health problems, low life expectancy, low living standards, low social status for women
                                                          and low educational levels. Demographic transition theory postulates that as a country undergoes economic development and social change its
                                                          population growth declines, with birth rates serving as an indicator.",br(),"In a developing country like Uganda, decline in birth rate is 
                                                          as a result of introduction of family planning measures in the nation,the steady decline in birth rates over the past 20 years can be greatly
                                                          attributed to the significant gains in women's freedoms, such as tackling the horror of forced marriage and child marriage, education for 
                                                          women in Uganda and increased socioeconomic opportunities. Women of all economic, social, religious and educational persuasions were choosing 
                                                          to have less children as they were gaining more control over their own reproductive rights. Apart from more children living into their adult years, 
                                                          women are often more ambitious to take up work, education and living their own lives rather than just a life of reproduction. This led to such a 
                                                          decrease in birth rate.",br()," On the other hand, the death rate significantly reduced due to improvement in the health standards, thouugh AIDS had 
                                                         calminated many lives in past decades, the introduction of ARVS gave hope for more life on earth for those who had fallen victims of the so called
                                                         (Mukenenya) interpreted as AIDS, thus reducing the cases of deaths due to this disease. lastly but not the least, the improved Education and research
                                                          patterns also created awareness to the natives of Uganda about the possible ways through which they could lose life, this also reduced the deaths and
                                                         death rate as well."
                                                         )
                                                  
                                                  ),# end Birth rate and Death rate
                                        ################################################ Total Population
                                         tabPanel(title = span(class = "tabs","Total\n Population"),
                                                  tags$p("Uganda's total population has tremendously increased from 1996 to 2016. The graph below can be used to give evidence of the population
                                                          data summerized in the table."
                                                         ),
                                                  tags$br(),
                                                  fluidRow(
                                                      # the table
                                                      column(6, offset = 0,
                                                             div(dataTableOutput(outputId = "totalPopTable"), style = "font-size:95%; width:100%;")
                                                      ),# end table area
                                                      column(6, offset = 0,
                                                             plotOutput(outputId = "totalPopPlot", height = "500px")
                                                      )# end plot area
                                                    ),
                                                  tags$h5("Observation."),
                                                  tags$p(" The graph shows that Uganda's total population rose exponentially  from 20,197,441 people in 1996 to 38,319,241 people in 2016, this 
                                                           indicates an increase of 18.1218 million people ove a period of twenth years."),
                                                  tags$h5("Conclusion."),
                                                  tags$p(" The rapid growth Uganda's total population is attributed to the high birth rates of 40 - 50 births per 1000 population, which are 
                                                           also as a result of high fertility rates of about 7 to 5 children per woman.",br()," Uganda being one of the African nation blessed
                                                           with fertile soils and favourable climate, gave room for high food production and when there's food security in a region, it promoetes
                                                           population growth, according to the 18th century Demographer, ",tags$a(href = "https://en.wikipedia.org/wiki/Thomas_Robert_Malthus",
                                                           "Thomas Robert Malthus "),", postulated that an increase in a nation's food production improved the well-being of the population, 
                                                           but the improvement would be temporary because it led to population growth which in turn restore the original per capita production
                                                           level."
                                                         )
                                                  ),# end Total population
                                        #################################################### Population Growth
                                        tabPanel(title = span(class = "tabs","Population\n Growth"),
                                                   tags$p(" Populatin growth refers to the summation of the Natural Increase and Net migrant in a conuntry at a situpilated time. Natural increase 
                                                            is calculated by subtracting the deaths from the births within the same period of time, on the other hand Net migrants reffers to the 
                                                            difference between immigrants (people moving into the place e.g a country) and emigrants (people leaving the country).
                                                            ",br("Population Growth = Natural Increase + Net migrants = (Births - Deaths) + (immigrants - emigrants)"),
                                                            br()," The table below sumerizes Uganda's population growth, growth rate (%), natural increase and Net number of migrants from 1996 to 
                                                            2016"
                                                          ),
                                                   # the table
                                                   dataTableOutput(outputId = "growthTable"),
                                                   br(),
                                                   #the plot for population growth
                                                   tags$p(style = "text-align:right; align: right;",
                                                          selectInput(inputId = "growthSelection",
                                                                      label = "Show on..",
                                                                      choices = c("Bar graph", "Line graph"))
                                                          ),
                                                  
                                                   plotOutput("growthPlot" ,height = "400px"),
                                                   br(),
                                                   tags$h5("Population Growth rate."),
                                                   tags$p(" Population growth rate is calculated by dividing the population change by the period of time for the change expressed as a percentage, 
                                                            by population change we mean subtracting the population in the previous year from the population in the current year then divide this 
                                                            result by the population in the previous year expressed as a percentage, then divide this result by the time for the change gives the 
                                                            population growth rate"),
                                                 #the plot for population growth rate
                                                 tags$p(style = "text-align:right; align: right;",
                                                        selectInput(inputId = "growthRateSelection",
                                                                    label = "Show on..",
                                                                    choices = c("Bar graph", "Line graph")
                                                                    )
                                                 ),
                                                 plotOutput("growthRatePlot" ,height = "400px"),
                                                 br(),
                                                 tags$h5("Observation."),
                                                 tags$p(" From the two graphs above we see that population grow has a relationship with the growth rate,  there is a general increase in the 
                                                        population growth, however, there's a sharp decrease in population growth from 1996 to 1997, then a sharp rise to 1998, later the growth
                                                        gradually increased to 711728 persons in 2001 and later it rose generally up to 1234263 persons in 2016."
                                                        ),
                                                 tags$h5("conclusion."),
                                                 tags$p(" The increasing population growth rate is due to the highly increasing number of births ")
                                                 ),# end population Growth
                                        ########################################################### Natural increase
                                        tabPanel(title = span(class = "tabs","Natural Increase"),
                                                 tags$p(" In this section we discuss how the Natural Increase and the Rate of Natural Increase of Uganda varied between 1996 to 2016.",
                                                          strong("Natural Increase refers to the difference between the live Births and Deaths during the year for a specified region."),
                                                          ""),br(),
                                                 # the table
                                                 dataTableOutput(outputId = "naturalIncTable"),
                                                 br(),
                                                 #the plot for natural Increase
                                                 tags$p(style = "text-align:right; align: right;",
                                                        selectInput(inputId = "naturalIncSelection",
                                                                    label = "Show on..",
                                                                    choices = c("Bar graph", "Line graph"))
                                                 ),
                                                 
                                                 plotOutput("naturalIncPlot" ,height = "300px"),
                                                 br(),
                                                 tags$h5("Rate of Natural Increase."),
                                                 tags$p(" This is refered to as the  natural increase during a specified time period as a proportion of an area's population at the midpoint
                                                          of the time period. Rates are expressed per 1,000 population. In demographics, Rate of Natural Increase (RNI) is the Crude Bith Rate
                                                          (CBR) minus Crude Death Rate (CDR), this rate excludes population increase from in migration and out migration. When looking at 
                                                          countries, it gives an idea of what position in the Demographic Transition Model, but to find out how much a country is growing, 
                                                          the population growth rate should be observed. The Demographic Transition Theory explains how the rate of natural increase relates to 
                                                          the economic growth.",br()," The graph below shows Uganda's RNI for the past 20 years."
                                                        ),
                                                 #the plot for rate of natural Increase
                                                 tags$p(style = "text-align:right; align: right;",
                                                        selectInput(inputId = "naturalIncRateSelection",
                                                                    label = "Show on..",
                                                                    choices = c("Line graph", "Bar graph"))
                                                        ),
                                                 plotOutput("naturalIncRatePlot" ,height = "300px"),
                                                 br(),
                                                 tags$h5("Observation."),
                                                 tags$p("The graph for Natural Increase shows that there was a steady increase in the natural increase from 1996 to 2016, however on the other 
                                                          hand, the Rate of natural increase declined from 1996 up to 1998, then it rose to 3.34 % in 1999 and generally became constant up to 
                                                        2007 and finally later declined generally to 3.29 % in 2016."),
                                                 tags$h5("conclusion."),
                                                 tags$p(" The high RNI is a characteristic is developing countries, and Uganda being one of them it falls victim of such high RIN of above 2.00%
                                                          , this is because of the high Births rates evidenced through the twenty years analysed.",br(),"The RNI can be shaped by government 
                                                          policy and a country's infrastructure, policies can either encourage an increase in birth rates or discourage an increase in birth rates"
                                                        )
                                                 ),# end natural Increase
                                        ################################################################ Migrations
                                        tabPanel(title = span(class = "tabs","Migrations"),
                                                  tags$p(strong("Net Number of Migrants") ," refers to the difference between Total Immigrants (people coming into the area) and the Total Emigrants
                                                          (people leaving the area), when the number of Immigrants is greater than the number of Emigrants, a positive Net Migration occurs which results
                                                          into a positive Migration rete.",strong("Migration Rate ")," or Rate of Migration also know as the Net Migrants per 1000 population, the Net 
                                                          migration rate is calculated over a one-year period using the mid year population.",br(),"If the net migration rate for a country is 25.2 per 
                                                          1,000 people. This means that for every 1,000 people in that country at the beginning of the year, 25.2 will have immigrated to it by the
                                                          end of the year. This number numerically shows the impact of migration on the country's population and allows for the comparison of that country's
                                                          net migration rate to other country's net migration rate."
                                                         ,br()," The table below summerizes Uganda's Net Migration and Migration Rate."
                                                         ),
                                                 tags$br(),
                                                 # the table
                                                 dataTableOutput("migrationsTable"),
                                                 # the plot for Net Migrations
                                                 selectInput(inputId = "migrationsSelection",
                                                             label = "Show on..",
                                                             choices = c("Bar graph","Line graph")
                                                             ),
                                                 plotOutput("migrationsPlot", height = "350px"),
                                                 tags$h5("Rate of Migration."),
                                                 tags$p("Uganda's Net migration per 1000 population was generally negative, which is a characteristic of countries in the sub Sahara region in Africa.
                                                         The rate of Migration is calculated by dividing the difference between the immigrants and the emigrants by the mid year population then multiply
                                                         the result by 1000 expressed as a percentage. The graph below summerises Uganda's Rate of Migration from 1996 to 2016."
                                                        ),
                                                 # the plot for rate of Migration
                                                 selectInput(inputId = "migrationRateSelection",
                                                             label = "Show on..",
                                                             choices = c("Bar graph","Line graph")
                                                 ),
                                                 plotOutput("migrationRatePlot", height = "350px"),
                                                 tags$h5("Observation"),
                                                 tags$p(" The graphs above show that Uganda's net migrants and the rate of migration are highly related and there is no general trend or pattern that was
                                                          followed, however, there's a sharp decrease in the migration rate between 1996 to 1997 and only two cases of positive rate of migration that is, 
                                                          in 1998 and 2004 and later a nil rate of migration in 2013."),
                                                 tags$h5("Conclusion"),
                                                 tags$p(" Before we conclude we need to understand that a",strong("positive rate"),"  of migration means more people are coming into the country than those
                                                          leaving it, ",strong("negative rate "),"of migration means the opposite of positive rate of migration and ",strong("nil of zero")," rate of migration 
                                                          means people leaving the country are equal to people coming into the country.",br()," The migrations data for Uganda for the past 20 years shows that 
                                                          more people were leaving the country to find settlement elsewhere due to either social, political, environmental or economic factors which kept on 
                                                          the citizens to other countries."
                                                        ),
                                                 tags$p("Social migration is when an individual migrates to have a higher standard of living, to be closer to family or to live in a nation with which they
                                                          identify more. Political migration then is when a person is going in as a refugee to escape war or political persecution. Economical migration is 
                                                          moving to a place where one can aspire to have a career and better job opportunities which end up contributing to better living conditions. Lastly,
                                                          environmental migration is when natural disasters force you to move into a new area.",br()," With all those factors expalined, uganda's negative 
                                                        rate of migration is attributed to mainly social and economic factors since Uganda's employment and standards of living were not at minimum standard
                                                        this encouraged more people to look for opportunities outside Uganda. The few case of positive migration rate evidenced in the 20 years migrations data 
                                                        possibly could be because of the natural disasters and political insatbilities in Uganda's neighbours which forced them to seek refegee in Uganda thus 
                                                        contributing to the more numbers of immigrants than emigrants."
                                                        ),
                                                 tags$p(" Lastly, once we analyze all of the  factors for migration we can get to the idea that the net migration rate can tell us so much about a country. For example,
                                                        if there are a lot of people coming in and few people leaving we can assume it is a wealthy country that keeps evolving and generating more and more 
                                                        opportunities. On the other hand, if not many people are coming in and many are leaving it is easy to assume that there is a chance of violence, low economy,
                                                        or not enough resources to fulfill the existing population.")
                                                 )# end Migrations
                                     ),# end tab box 
                                 # the footer
                                 footer
                         ),# end statistics************************************************************************************************************************************************************
                       ################################################## GET INVOLVED PAGE ############################################
                         tabItem(tabName = "getInvolved",
                                 ugaDemos,hr(), # header part
                                 box(height = "auto", width = "500px",
                                      tags$p("You can get your hands dirty here, click the browse button and upload a csv file not larger than 10 Mb. Make use of the selects that appear alongside 
                                              your uploaded data to choose characteristics to plot on horizonatal and vertical axes, give your graph a title and axis labels by typing in the text boxes
                                              and the click the plot button to make a plot of your selection. Your can download the plot by clicking the Download button below the graph on the right."
                                             ),
                                     fluidRow(
                                       # left colmn
                                       column(4, offset = 0,
                                              # input file
                                              fileInput(inputId = "uploadedFile", label = "Upload Population data.",
                                                        width = "300px",accept = c('csv','.csv')),
                                              tags$br(),
                                              # select inputs for what to plot.
                                              tags$h4(class = "modi" ,uiOutput("doSomething")),
                                              uiOutput("xValues"), # for x values
                                              tags$br(),
                                              uiOutput("yValues"), # for y values
                                              tags$br(),
                                              # give labels to your plot
                                              uiOutput(outputId = "xlabel"),# for x
                                              uiOutput(outputId = "ylabel"),# for y
                                              # give title to your plot
                                              uiOutput(outputId = "plotTitle"),# for title
                                              # the plot button
                                              uiOutput("plotButton")
                                       ),
                                       # right colmn
                                       column(8, offset = 0,
                                              "uploaded content goes here",
                                              tags$h5( style = "color:red;",textOutput("notify")),
                                              tags$h5(style = "color:green;",textOutput("saved")),
                                              dataTableOutput("inputData") # display the data here
                                       )
                                     ),
                                     br(),
                                     #select for what to plot
                                     fluidRow(
                                         column(4, offset = 7,
                                                uiOutput("whatToPlot")#select for what to plot
                                         )
                                     ),
                                     #display the chart here
                                     plotOutput("displayPlot",height = "480px"),
                                     # download the chart
                                     column(4, offset = 7,
                                               uiOutput("savePlot"),#help text
                                               uiOutput("downloadPlot")# button
                                            )
                                     ),# end box
                               # the footer
                               footer
                         ),# end get involved**********************************************************************************************************************************************************
                       ################################################## PREDICTIONS PAGE ############################################
                         tabItem(tabName = "predictions",
                                 ugaDemos,hr(), # header part
                                 tabBox(id = "predictions",height = "auto", width = "500px",
                                       tabPanel(title = span(class = "tabs","Total Population"),
                                                  tags$p("Total population refers to the total number of people or species living in an area determined at a specific time.
                                                          The graph below show the prediction for Uganda's population for six years ahead of 2016 in the blue region.
                                                          "),
                                                  tags$br(),
                                                  # the plot
                                                  plotOutput("popPrediction"),
                                                  tags$p(style = "text-align:right; align: right;",downloadButton(outputId = "totalPopPredic", label = "Download plot")),
                                                  tags$h5("observation."),
                                                  tags$p(" The plot above clearly shows that uganda's population keeps on increasing from 1996 up to 2016, the blue region predicts
                                                           that the population continues to increase. The graph estimates that Uganda's population is about 39 million people in 2017,
                                                           though currently we know that Uganda's population is 40.3 million people, and the above graph shows it will be about 44 million
                                                           people in 2022."
                                                         ),
                                                  tags$h5("conclusion."),
                                                  tags$p(" The population increase shown in the graph above is a good indicator that there's going to be more labour force thats if 
                                                           the dependancy ratio remains as low as 108.71 per cent,",tags$strong("Dependancy ratio")," is the number of dependant people
                                                           , for Uganda (old above 65 years and young below 18 years) as a percentage of working people. However according to Thomas Malthus, a demographer 
                                                           argued that because of the natural human urge to reproduce human population increases geometrically (1, 2, 4, 16, 32, 64, 128, 256, etc.).
                                                           However, food supply, at most, can only increase arithmetically (1, 2, 3, 4, 5, 6, 7, 8, etc.). Therefore, since food is an essential 
                                                           component to human life, population growth in any area or on the planet, if unchecked, would lead to starvation. However, Malthus also argued
                                                           that there are preventative checks and positive checks on the population that slow its growth and keep the population from rising exponentially
                                                           for too long, but still, poverty is inescapable and will continue."
                                                         )
                                       ),# end total population predictions
                                       tabPanel(title = span(class = "tabs","Total Births"),
                                                tags$p(" The prediction for live births of Uganda is shown here, from 2018 to 2023 in the blue region."),
                                                tags$br(),
                                                # the plot
                                                plotOutput("birthsPrediction"),
                                                tags$p(style = "text-align:right; align: right;",downloadButton(outputId = "totalBirthsPredic", label = "Download plot")),
                                                tags$h5("Observation."),
                                                tags$p("The blue region from the graph shows that Uganda's Births keep on increasing steadily."),
                                                tags$h5("Conclusion"),
                                                tags$p("To supress further increase in the number of Births of Uganda due to the shortcoming that may a rise due to high populations, preventative checks 
                                                        that affect the birth rate  include marrying at a later age (moral restraint), abstaining from procreation, and birth control"
                                                       )
                                       ),# end total Births predictions
                                       tabPanel(title = span(class = "tabs","Total Deaths"),
                                                tags$p("Deaths refer to the total number of dead people recorded in a specified period of time mostly a year is used."),
                                                tags$br("Below is the prediction for Uganda's Deaths for five years ahead of 2016."),
                                                # the plot
                                                plotOutput("deathsthsPrediction"),
                                                tags$p(style = "text-align:right; align: right;",downloadButton(outputId = "totalDeathsPredic", label = "Download plot")),
                                                tags$p(strong("observation;")," the predictions show that Ugand's total deaths will continue to rise up to approximately 410,000 people in 2022.")
                                       ),# end total Births predictions
                                       tabPanel(title = span(class = "tabs","Natural Increase"),
                                                tags$h4("Natural Increase"),
                                                tags$p("If you have dealt with Demograohics then probably you know that Deaths and Births are the natural causes of population change.
                                                        Natural increase is calculated as the difference between Births and Deaths for a specific period of time."),
                                                tags$br(),
                                                # the plot
                                                plotOutput("naturalIncreasePrediction"),
                                                tags$p(strong("Observation;")," as long as the births continue to raise more than the deaths, Uganda's Natural increase is also expected to rise, 
                                                       the prediction for births clearly indicated that births will continue to rise therefore, the Natural increase is as well expected to increase.
                                                       The graph above approximates the natural increase to be about 1,450,000 people in 2022.")
                                       )# end total Births predictions
                                      ),# end tabBox
                                 # the footer
                                 footer      
                         ),# end predictions**********************************************************************************************************************************************************
                       ################################################## FAQs PAGE ############################################
                         tabItem(tabName = "faqs",
                                 ugaDemos,hr(), # header part
                                 tabBox(id = "faqs",height = "auto", width = "500px",
                                            tabPanel(title = span(class = "tabs","FAQs"),
                                             tags$p("Hello there, I personally loved the analysis but could there be a possibility of bringing up data concerning Uganda's
                                                     sex ratio, dependency ratio and age categorised data for the past 20 years."
                                                    ),
                                             fluidRow(
                                                column(7, offset = 3,
                                                        tags$p(style = "color:teal;","Currently UgaDemos has not got information concerning above listed population characteristics
                                                               but in the near future we shall provide such data")
                                                       )
                                              ),# end Row
                                            # the Question goes here
                                            uiOutput("qn"),
                                            tags$br(),
                                            tags$h5("Leave a question."),
                                            tags$p(
                                                textAreaInput(inputId = "question",label = "", placeholder = "Ask",width = "400px", height = "100px"),
                                                actionButton(inputId = "askQn", label = "Ask")
                                              )
                                        ),# end faqs
                                        tabPanel(title = span(class = "tabs","Rate Us"),
                                                 tags$p(" UgaDemos has really done a thorough analysis on Uganda's population statistics for the past 20 years, do you agree?"),
                                                 fluidRow(
                                                   column(6, offset = 0,
                                                            tags$br(infoBoxOutput("agreed", width = 6), actionButton("agree", "YES"))
                                                            ),# end agree section
                                                   column(6, offset = 0,
                                                            tags$br(infoBoxOutput("disagreed", width = 6), actionButton("disagree", "NO"))
                                                          )#end disagrees section
                                                 )
                                                 
                                        )# end Rate Us
                                    ),# end tabbox
                                 # the footer
                                 footer  
                         )# end faqs*******************************************************************************************************************************************************************
                       )# end tabItems
                       ##############################################################################################
                     )# end dashboard body
      )# end dashboardPage 
    )# end the body
  )# end fluidPage
)# end the UI