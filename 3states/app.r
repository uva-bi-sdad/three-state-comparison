library(shiny)
library(viridis)
library(scales)
library(dplyr)
library(sf)
library(shinythemes)
library(leaflet)
library(readr)
library(ggplot2)
library(htmlwidgets)
library(htmltools)
library(ggthemes)
library(sf)
library(RColorBrewer)
library(plotly)


#
# Load data ---------------------------------------------

#cm_load data

#data_edu <- read_rds("~/EM/gates/rivanna_data/working/dashboard/data_edu.Rds")
data_edu <- read_rds("data_edu.Rds")


#data_3states <- read_rds("~/EM/gates/src/dashboard/3states/data_3states.rds")
#data_3states <- read_rds("~/EM/gates/src/dashboard/3states/data_3states4.rds")

data_3states <- read_rds("data_3states4.rds")


#
# User interface ---------------------------------------------
#

ui <- fluidPage(theme = shinytheme("cosmo"),
                
                tags$style(type = "text/css",
                           ".recalculating {opacity: 1.0;}"
                ),
                
                tags$head(tags$style(HTML(" .sidebar { font-size: 40%; } "))
                ),
                
                fluidRow(width = 12,
                         align = "center",
                         img(src = "logo.jpg", class = "topimage", width = "40%", style = "display: block; margin-left: auto; margin-right: auto;")
                ),
                
                fluidRow(width = 12, 
                         style = "padding-top:20px; padding-bottom:20px;",
                         column(1),
                         column(10, align = "center", h2(strong("Three State Comparison"))),
                         column(1)
                ), 
                
                fluidRow(width = 12,
                         column(1),
                         column(10, 
                                p(),
                                br(tags$b('This dashboard presents information that is associated with social mobility in three states: Oregon, Iowa and Virginia. The dashboard contains
                                          information that allow comparisons of five factors, characterizing segregation, education, social capital, family structure and income inequality.'), 
                                   (' ')),
                                p()
                         ),
                         column(1)
                ),
                
                
                fluidRow(style = "margin: 6px; padding-top:20px; padding-bottom:20px;",
                         tabsetPanel(
                           tabPanel(title = "Relative Maps and Indicators",
                                    
                                    fluidRow(style = "margin: 6px",
                                             width = 12, 
                                             column(12, align = "left", 
                                                    h3(strong("Comparative Maps")),
                                                    p(),
                                                    p(strong("Select a state and topic using the map selector."), 
                                                      "The resulting maps will display the relative accesibility of remote work, remote education, and telemental health at county-level.",
                                                      "Select the Data and Methods tab to learn more about how we constructed our composite measures."))
                                    ),
                                    
                                    fluidRow(style = "margin: 6px",
                                             column(width = 3,
                                                    h4(strong("Selector"), align = "left"),
                                                    p(), 
                                                    selectInput("whichtopic", "Topic", 
                                                                choices = list("Segregation",
                                                                               "School",
                                                                               "Family Structure",
                                                                               "Social Capital"), 
                                                                selected = "Segregation"),
                                                                # choices = list("Remote education",
                                                                #                "Remote work",
                                                                #                "Telemental health"), 
                                                                # selected = "Remote education"),
                                                    p(),
                                                    selectInput("whichstate", "State", 
                                                                choices = list("Iowa",
                                                                               "Oregon",
                                                                               "Virginia"), 
                                                                selected = "Iowa"),
                                                    p(),
                                                    p("Use the tools above to select your topic and state of interest.
                                                      The selected map will display on the right, and individual indicators associated
                                                      with the selected topic will be come available for further exploration below."),
                                                    p(),
                                                    p("The map may take a moment to load.")
                                             ),
                                             column(width = 9, 
                                                    h4(strong("County Map"), align = "left"),
                                                    p(),
                                                    leafletOutput("mainplot", width = "100%")
                                             )
                                    ),
                                    
                                    fluidRow(style = "margin: 6px",
                                             width = 12, 
                                             column(12, align = "left", 
                                                    h3(strong("Explore Ranking Indicators")),
                                                    p(),
                                                    p(strong("Click on the tabs to explore the individual indicators we used to 
                                                             construct the selected relative accessibility measure."), "Each tab displays a 
                                                      box plot with descriptive statistics and a state map at county level for an individual indicator. 
                                                      The selection of indicators will update depending on the topic selected."))
                                    ),
                                    
                                    conditionalPanel("input.whichtopic == 'School'",
                                                     fluidRow(style = "margin: 6px",
                                                              tabsetPanel(
                                                                tabPanel(title = "High school graduation rate", 
                                                                         br(),
                                                                         h4(strong("Indicator: County-Level proportion of individuals that obtained High School Diploma")),
                                                                         p(),
                                                                         p("The box plot and map below provide a descriptive summary and visualization of data distribution for the selected indicator.
                                                                           Data come from the American Community Survey 2014/18 (5-year) estimates,", 
                                                                           a(href = 'https://factfinder.census.gov/faces/affhelp/jsf/pages/metadata.xhtml?lang=en&type=table&id=table.en.ACS_17_5YR_B28002', 'Table B28002, Presence and types of internet subscriptions in household.', target = "_blank")),
                                                                         column(6, align = "left",
                                                                                p(),
                                                                                strong("County-Level Box Plot"),
                                                                                p(),
                                                                                plotlyOutput("plotly_noint_work", width = "100%"),
                                                                                p()),
                                                                         column(6, align = "left",
                                                                                p(),
                                                                                strong("County-Level Map"),
                                                                                p(),
                                                                                leafletOutput("subplot_noint_work", width = "100%"))
                                                                )
                                                                
                                                                # ,
                                                                # tabPanel(title = "Workers without computer", 
                                                                #          br(),
                                                                #          h4(strong("Indicator: County-Level Percent Population in Labor Force Without a Computer")),
                                                                #          p("The box plot and map below provide a descriptive summary and visualization of data distribution for the selected indicator.
                                                                #            Data come from the American Community Survey 2014/18 (5-year) estimates,", 
                                                                #            a(href = 'https://data.census.gov/cedsci/table?q=computer&tid=ACSDT5Y2018.B28007&t=Telephone,%20Computer,%20and%20Internet%20Access&vintage=2018&d=ACS%205-Year%20Estimates%20Detailed%20Tables', 'Table B28007, Labor force status by presence of a computer and types of internet subscription in household.', target = "_blank")),
                                                                #          column(6, align = "left",
                                                                #                 p(),
                                                                #                 strong("County-Level Box Plot"),
                                                                #                 p(),
                                                                #                 plotlyOutput("plotly_nocomp_work", width = "100%"),
                                                                #                 p()),
                                                                #          column(6, align = "left",
                                                                #                 p(),
                                                                #                 strong("County-Level Map"),
                                                                #                 p(),
                                                                #                 leafletOutput("subplot_nocomp_work", width = "100%"))
                                                                # ),
                                                                # tabPanel(title = "Non-remote workers in remote unfriendly occupations", 
                                                                #          br(),
                                                                #          h4(strong("Indicator: County-Level Percent Population in Labor Force not Working Remotely Employed in Remote Unfriendly Occupations")),
                                                                #          p("The box plot and map below provide a descriptive summary and visualization of data distribution for the selected indicator.
                                                                #            Data come from the American Community Survey 2014/18 (5-year) estimates,", 
                                                                #            a(href = 'https://data.census.gov/cedsci/table?q=B08124%3A%20MEANS%20OF%20TRANSPORTATION%20TO%20WORK%20BY%20OCCUPATION&hidePreview=true&tid=ACSDT5Y2018.B08124', 'Table B08124, Means of transportation to work by occupation.', target = "_blank")),
                                                                #          column(6, align = "left",
                                                                #                 p(),
                                                                #                 strong("County-Level Box Plot"),
                                                                #                 p(),
                                                                #                 plotlyOutput("plotly_occup", width = "100%"),
                                                                #                 p()),
                                                                #          column(6, align = "left",
                                                                #                 p(),
                                                                #                 strong("County-Level Map"),
                                                                #                 p(),
                                                                #                 leafletOutput("subplot_occup", width = "100%"))
                                                                # ),
                                                                # tabPanel(title = "Non-remote workers in remote unfriendly industries", 
                                                                #          br(),
                                                                #          h4(strong("Indicator: County-Level Percent Population in Labor Force not Working Remotely Employed in Remote Unfriendly Industries")),
                                                                #          p("The box plot and map below provide a descriptive summary and visualization of data distribution for the selected indicator.
                                                                #            Data come from the American Community Survey 2014/18 (5-year) estimates,", 
                                                                #            a(href = 'https://data.census.gov/cedsci/table?q=B08126%3A%20MEANS%20OF%20TRANSPORTATION%20TO%20WORK%20BY%20INDUSTRY&hidePreview=true&tid=ACSDT5Y2018.B08126&vintage=2018', 'Table B08126, Means of transportation to work by industry.', target = "_blank")),
                                                                #          column(6, align = "left",
                                                                #                 p(),
                                                                #                 strong("County-Level Box Plot"),
                                                                #                 p(),
                                                                #                 plotlyOutput("plotly_industr", width = "100%"),
                                                                #                 p()),
                                                                #          column(6, align = "left",
                                                                #                 p(),
                                                                #                 strong("County-Level Map"),
                                                                #                 p(),
                                                                #                 leafletOutput("subplot_industr", width = "100%"))
                                                                # )
                                                              )
                                                     )
                                    ),
                                    
                                    conditionalPanel("input.whichtopic == 'Segregation'",
                                    #conditionalPanel("input.whichtopic == 'Remote education'",
                                                     fluidRow(style = "margin: 6px",
                                                              tabsetPanel(
                                                                tabPanel(title = "White Non-Hispanic Population",
                                                                #tabPanel(title = "Households without internet", 
                                                                         br(),
                                                                         h4(strong("Indicator: County-Level Percent of White Non-Hispanic Population")),
                                                                         p("The box plot and map below provide a descriptive summary and visualization of data distribution for the selected indicator.
                                                                           Data come from the American Community Survey 2014/18 (5-year) estimates,", 
                                                                           a(href = 'https://factfinder.census.gov/faces/affhelp/jsf/pages/metadata.xhtml?lang=en&type=table&id=table.en.ACS_17_5YR_B28002', 'Table B28002, Presence and types of internet subscriptions in household.', target = "_blank")),
                                                                         column(6, align = "left",
                                                                                p(),
                                                                                strong("County-Level Box Plot"),
                                                                                p(),
                                                                                plotlyOutput("plotly_noint_edu", width = "100%"),
                                                                                p()),
                                                                         column(6, align = "left",
                                                                                p(),
                                                                                strong("County-Level Map"),
                                                                                p(),
                                                                                leafletOutput("subplot_noint_edu", width = "100%"))
                                                                ),
                                                                tabPanel(title = "Commuters",
                                                                #tabPanel(title = "Youth without computer",
                                                                         br(),
                                                                         h4(strong("Indicator: County-Level proportion of individuals who commute less/more than 15 minutes to work")),
                                                                         p("The box plot and map below provide a descriptive summary and visualization of data distribution for the selected indicator.
                                                                           Data come from the American Community Survey 2014/18 (5-year) estimates,", 
                                                                           a(href = 'https://data.census.gov/cedsci/table?q=computer&tid=ACSDT5Y2018.B28005&t=Telephone,%20Computer,%20and%20Internet%20Access&vintage=2018&d=ACS%205-Year%20Estimates%20Detailed%20Tables', 'Table B28005, Age by presence of a computer and types of internet subscription in household.', target = "_blank")),
                                                                         column(6, align = "left",
                                                                                p(),
                                                                                strong("County-Level Box Plot"),
                                                                                p(),
                                                                                plotlyOutput("plotly_nocomp_edu", width = "100%"),
                                                                                p()),
                                                                         column(6, align = "left",
                                                                                p(),
                                                                                strong("County-Level Map"),
                                                                                p(),
                                                                                leafletOutput("subplot_nocomp_edu", width = "100%"))
                                                                )
                                                                #,
                                                                # tabPanel(title = "Population in K-12", 
                                                                #          br(),
                                                                #          h4(strong("Indicator: County-Level Percent Population Enrolled in K-12")),
                                                                #          p("The box plot and map below provide a descriptive summary and visualization of data distribution for the selected indicator.
                                                                #            Data come from the American Community Survey 2014/18 (5-year) estimates,", 
                                                                #            a(href = 'https://data.census.gov/cedsci/table?q=B14001%3A%20SCHOOL%20ENROLLMENT%20BY%20LEVEL%20OF%20SCHOOL%20FOR%20THE%20POPULATION%203%20YEARS%20AND%20OVER&hidePreview=true&tid=ACSDT5Y2018.B14001&vintage=2018', 'Table B14001,	School enrollment by level of school for the population 3 years and over.', target = "_blank")),
                                                                #          column(6, align = "left",
                                                                #                 p(),
                                                                #                 strong("County-Level Box Plot"),
                                                                #                 p(),
                                                                #                 plotlyOutput("plotly_ink12", width = "100%"),
                                                                #                 p()),
                                                                #          column(6, align = "left",
                                                                #                 p(),
                                                                #                 strong("County-Level Map"),
                                                                #                 p(),
                                                                #                 leafletOutput("subplot_ink12", width = "100%"))
                                                                # )
                                                              )
                                                     )
                                    ),
                                    
                                    conditionalPanel("input.whichtopic == 'Family Structure'",
                                                     fluidRow(style = "margin: 6px",
                                                              tabsetPanel(
                                                                tabPanel(title = "Children living in single-parent households ", 
                                                                         br(),
                                                                         h4(strong("Indicator: County-Level proportion of children living in single-parent households ")),
                                                                         p("The box plot and map below provide a descriptive summary and visualization of data distribution for the selected indicator.
                                                                           Data come from the American Community Survey 2014/18 (5-year) estimates,", 
                                                                           a(href = 'https://factfinder.census.gov/faces/affhelp/jsf/pages/metadata.xhtml?lang=en&type=table&id=table.en.ACS_17_5YR_B28002', 'Table B28002, Presence and types of internet subscriptions in household.', target = "_blank")),
                                                                         column(6, align = "left",
                                                                                p(),
                                                                                strong("County-Level Box Plot"),
                                                                                p(),
                                                                                plotlyOutput("plotly_noint_med", width = "100%"),
                                                                                p()),
                                                                         column(6, align = "left",
                                                                                p(),
                                                                                strong("County-Level Map"),
                                                                                p(),
                                                                                leafletOutput("subplot_noint_med", width = "100%"))
                                                                )
                                                                
                                                       #          #,
                                                       #          tabPanel(title = "Households without computer", 
                                                       #                   br(),
                                                       #                   h4(strong("Indicator: County-Level Percent Households Without a Computer")),
                                                       #                   p("The box plot and map below provide a descriptive summary and visualization of data distribution for the selected indicator.
                                                       #                     Data come from the American Community Survey 2014/18 (5-year) estimates,", 
                                                       #                     a(href = 'https://data.census.gov/cedsci/table?q=computer&tid=ACSDT5Y2018.B28003&t=Telephone,%20Computer,%20and%20Internet%20Access&vintage=2018&d=ACS%205-Year%20Estimates%20Detailed%20Tables', 'Table B28003, Presence of a computer and type of internet subscription in household.', target = "_blank")),
                                                       #                   column(6, align = "left",
                                                       #                          p(),
                                                       #                          strong("County-Level Box Plot"),
                                                       #                          p(),
                                                       #                          plotlyOutput("plotly_nocomp_med", width = "100%"),
                                                       #                          p()),
                                                       #                   column(6, align = "left",
                                                       #                          p(),
                                                       #                          strong("County-Level Map"),
                                                       #                          p(),
                                                       #                          leafletOutput("subplot_nocomp_med", width = "100%"))
                                                       #          ),
                                                       #          tabPanel(title = "Population uninsured", 
                                                       #                   br(),
                                                       #                   h4(strong("Indicator: County-Level Percent Population Without Health Insurance")),
                                                       #                   p("The box plot and map below provide a descriptive summary and visualization of data distribution for the selected indicator.
                                                       #                     Data come from the American Community Survey 2014/18 (5-year) estimates,", 
                                                       #                     a(href = 'https://data.census.gov/cedsci/table?q=coverage&tid=ACSDT5Y2018.B27020&d=ACS%205-Year%20Estimates%20Detailed%20Tables&vintage=2018', 'Table B27020, Health insurance coverage status and type by citizenship status.', target = "_blank")),
                                                       #                   column(6, align = "left",
                                                       #                          p(),
                                                       #                          strong("County-Level Box Plot"),
                                                       #                          p(),
                                                       #                          plotlyOutput("plotly_unins", width = "100%"),
                                                       #                          p()),
                                                       #                   column(6, align = "left",
                                                       #                          p(),
                                                       #                          strong("County-Level Map"),
                                                       #                          p(),
                                                       #                          leafletOutput("subplot_unins", width = "100%"))
                                                       #          ),
                                                       #          tabPanel(title = "Poor mental health days", 
                                                       #                   br(),
                                                       #                   h4(strong("Indicator: County-Level Average Number of Poor Mental Health Days in Past Month")),
                                                       #                   p("The box plot and map below provide a descriptive summary and visualization of data distribution for the selected indicator.
                                                       #                     Data come from the Behavioral Risk Factor Surveillance System 2017 via CountyHealthRankings,", 
                                                       #                     a(href = 'https://www.countyhealthrankings.org/explore-health-rankings/measures-data-sources/county-health-rankings-model/health-outcomes/quality-of-life/poor-mental-health-days', 'Average number of mentally unhealthy days reported in past 30 days (age-adjusted).', target = "_blank")),
                                                       #                   column(6, align = "left",
                                                       #                          p(),
                                                       #                          strong("County-Level Box Plot"),
                                                       #                          p(),
                                                       #                          plotlyOutput("plotly_menthdays", width = "100%"),
                                                       #                          p()),
                                                       #                   column(6, align = "left",
                                                       #                          p(),
                                                       #                          strong("County-Level Map"),
                                                       #                          p(),
                                                       #                          leafletOutput("subplot_poorment", width = "100%"))
                                                       #          ),
                                                       #          tabPanel(title = "Mental health providers", 
                                                       #                   br(),
                                                       #                   h4(strong("Indicator: County-Level Number of Mental Health Providers per 100,000 Population")),
                                                       #                   p("The box plot and map below provide a descriptive summary and visualization of data distribution for the selected indicator.
                                                       # Data come from the CMS National Provider Identification 2019 via CountyHealthRankings,", 
                                                       #                     a(href = 'https://www.countyhealthrankings.org/explore-health-rankings/measures-data-sources/county-health-rankings-model/health-factors/clinical-care/access-to-care/mental-health-providers', 'Number of mental health providers per 100,000 population.', target = "_blank")),
                                                       #                   column(6, align = "left",
                                                       #                          p(),
                                                       #                          strong("County-Level Box Plot"),
                                                       #                          p(),
                                                       #                          plotlyOutput("plotly_menthprov", width = "100%"),
                                                       #                          p()),
                                                       #                   column(6, align = "left",
                                                       #                          p(),
                                                       #                          strong("County-Level Map"),
                                                       #                          p(),
                                                       #                          leafletOutput("subplot_healthprov", width = "100%"))
                                                       #          )
                                                              )
                                                     )
                                    ),
                                    
                                    #########new
                                    conditionalPanel("input.whichtopic == 'Social Capital'",
                                                     fluidRow(style = "margin: 6px",
                                                              tabsetPanel(
                                                                tabPanel(title = "Voter turnout proportion", 
                                                                         br(),
                                                                         h4(strong("Indicator: County-Level proportion of voters")),
                                                                         p(),
                                                                         p("The box plot and map below provide a descriptive summary and visualization of data distribution for the selected indicator.
                                                                           Data come from the American Community Survey 2014/18 (5-year) estimates,", 
                                                                           a(href = 'https://factfinder.census.gov/faces/affhelp/jsf/pages/metadata.xhtml?lang=en&type=table&id=table.en.ACS_17_5YR_B28002', 'Table B28002, Presence and types of internet subscriptions in household.', target = "_blank")),
                                                                         column(6, align = "left",
                                                                                p(),
                                                                                strong("County-Level Box Plot"),
                                                                                p(),
                                                                                plotlyOutput("plotly_voter", width = "100%"),
                                                                                p()),
                                                                         column(6, align = "left",
                                                                                p(),
                                                                                strong("County-Level Map"),
                                                                                p(),
                                                                                leafletOutput("subplot_voter", width = "100%"))
                                                                )
                                                                
                                                                )
                                    )
                                    ),
                                    
                                    
                                    #########end new
                                    
                                    fluidRow(style = "margin: 6px",
                                             width = 12,
                                             column(6, align = "left",
                                                    tags$em("How to interpret this box plot."),
                                                    p("The box plot visualizes data distribution for the selected indicator. The bottom and top horizontal lines extending from the box represent the smallest and largest non-outlier values on the selected indicator.
                                  The bottom of the box represents the lower quartile; 25% of counties have indicator values below this threshold. Similarly, the top of the box represents the upper quartile, with 25% of counties having 
                                  indicator values above this threshold. The body of the box represents the interquartile range, capturing indicator values for 50% of all counties. The horizontal line drawn through the box is the median 
                                  -- the middle quartile or the midpoint value in the distribution. Half of all counties have indicator values below this threshold, and half have indicator values above the threshold.
                                  Dots represent counties with outlier values. Hover over the boxplot to view exact summary values.")),
                                             column(6, align = "left",
                                                    tags$em("How to interpret this map."),
                                                    p("The map visualizes values on the selected indicator at county level. Counties with darker map colors have higher values on the selected indicator. Conversely, counties with lighter colors have lower
                                  values on the selected indicator. To facilitate comparisons and display quintile cut-off points, map colors represent quintile-grouped values. To view the exact indicator value for a particular county, hover over the county on the map. The information
                                  box will display the county name, the indicator value rounded to two decimal points, and the corresponding quintile."))
                                    )
                           ),
                           
                           tabPanel(title = "Data, Measures, and Methods", 
                                    
                                    fluidRow(style = "margin: 6px",
                                             width = 12, 
                                             column(12, align = "left", 
                                                    h3(strong("Data, Measures, and Methods"))
                                             )
                                    ),
                                    
                                    column(4, wellPanel(strong('Remote Work Accessibility'), 
                                                        p(),
                                                        em('Description.'),
                                                        br(), tags$b('The remote work relative accessibility measure highlights counties where residents may have difficulty working remotely if instructed to do so.'),
                                                        ('It conceptualizes four telecommunication infrastructure and employment characteristics as potential barriers, providing
                                                a relative ranking of county telework preparedness.'),
                                                        p(),
                                                        em('How We Measure Remote Work Accessibility.'),
                                                        br('We calculate remote work relative accessibility using information on percent:'),
                                                        tags$li('Households with no broadband internet subscription.'),
                                                        tags$li('Persons in labor force with no computer available.'),
                                                        tags$li('Persons who are not currently working remotely and are employed in telework unfriendly occupations
                                                       (service, natural, construction, maintenance, production, transportation, material moving, and military specific occupations).'),
                                                        tags$li('Persons who are not currently working remotely and are employed in telework unfriendly industries 
                                                       (construction, manufacturing, wholesale, retail, transportation and warehousing, utilities, and government, including armed forces).'),
                                                        br('We compute quintile cut-offs for each indicator. County placement in a higher quintile indicates lower relative accessibility.
                                                  We assign descriptive labels for county relative accessibility based on whether they placed in 4th or 5th quintile a certain number of times. 
                                                  The more times a county places in the 4th or 5th quintile on relevant indicators, the lower its relative remote work accessibility:'),
                                                        tags$li('Very high: 0 indicators.'),
                                                        tags$li('High: 1 indicator.'),
                                                        tags$li('Medium: 2 indicators.'), 
                                                        tags$li('Low: 3 indicators.'),
                                                        tags$li('Very low: all 4 indicators.'),
                                                        p(),
                                                        em('Data source.'),
                                                        p(a(href = 'https://www.census.gov/programs-surveys/acs', 'American Community Survey', target = "_blank"), "2014/18 (5-year) estimates.")
                                    )),                         
                                    column(4, wellPanel(strong('Remote Education Accessibility'), 
                                                        p(),
                                                        em('Description.'),
                                                        br(), tags$b('The remote education relative accessibility measure highlights counties where K-12 students may have difficulty participating in online education.'), 
                                                        ('It considers telecommunication infastructure and K-12 enrollment in providing a relative ranking of county K-12 remote education preparedness.'),
                                                        p(),
                                                        em('How We Measure Remote Education Accessibility.'),
                                                        br('We calculate remote education relative accessibility using information on percent:'),
                                                        tags$li('Households with no internet access subscription.'),
                                                        tags$li('Population under age 18 without a computer.'),
                                                        tags$li('Population enrolled in K-12.'),
                                                        br('We compute quintile cut-offs for each indicator. County placement in a higher quintile indicates lower relative accessibility.
                                                  We assign descriptive labels for county relative accessibility based on whether they placed in 4th or 5th quintile a certain number of times.
                                                  The more times a county places in the 4th or 5th quintile on relevant indicators, the lower its relative remote education accessibility:'),
                                                        tags$li('High: 0 indicators.'),
                                                        tags$li('Medium: 1 indicator.'),
                                                        tags$li('Low: 2 indicators.'),
                                                        tags$li('Very low: all 3 indicators.'),
                                                        p(),
                                                        em('Data source.'),
                                                        p(a(href = 'https://www.census.gov/programs-surveys/acs', 'American Community Survey', target = "_blank"), "2014/18 (5-year) estimates.")
                                    )),
                                    column(4, wellPanel(strong('Telemental Health Accessibility'), 
                                                        p(),
                                                        em('Description.'),
                                                        br(), tags$b('The telemental health relative accessibility measure highlights counties where high need for mental health services is coupled with
                                                            barriers to access.'), ('It considers telecommunication infastructure, health insurance, in-person provider availability, and mental health status
                                                                                    in providing a relative ranking of county K-12 telemental health accessibility.'),
                                                        p(),
                                                        em('How We Measure Telemental Health Accessibility.'),
                                                        br('We calculate telemental health relative accessibility using information on:'),
                                                        tags$li('Percent households without internet access.'),
                                                        tags$li('Percent households with no computer.'),
                                                        tags$li('Average number of poor mental health days in past month.'),
                                                        tags$li('Number of mental health providers per 100,000 population (reverse-coded).'),
                                                        tags$li('Percent population under age 65 without health insurance.'),
                                                        br('We compute quintile cut-offs for each indicator. County placement in a higher quintile indicates lower relative accessibility.
                                                 We assign descriptive labels for county relative accessibility based on whether they placed in 4th or 5th quintile a certain number of times.
                                                 The more times a county places in the 4th or 5th quintile on relevant indicators, the lower its relative telemental health accessibility:'),
                                                        tags$li('Very high: 0 indicators.'),
                                                        tags$li('High: 1 indicator.'),
                                                        tags$li('Medium: 2 or 3 indicators.'),
                                                        tags$li('Low: 4 indicators.'),
                                                        tags$li('Very low: all 5 indicators'),
                                                        p(),
                                                        em('Data source.'),
                                                        p(a(href = 'https://www.census.gov/programs-surveys/acs', 'American Community Survey', target = "_blank"), "2014/18 (5-year) estimates and",
                                                          a(href = 'https://www.countyhealthrankings.org/explore-health-rankings/measures-data-sources', 'County Health Rankings', target = "_blank"), "2019.")
                                    ))
                           ),
                           
                           
                           
                           tabPanel(title = "Acknowledgments and Contact",
                                    
                                    fluidRow(style = "margin: 6px",
                                             width = 12, 
                                             column(12, align = "left", 
                                                    h3(strong("Acknowledgments and Contact"))
                                             )
                                    ),
                                    
                                    column(6, align = "left",
                                           wellPanel(
                                             strong("Acknowledgments"),
                                             br(),
                                             br("TO BE DONE... We used the quintile method described by the", a(href = "https://pcrd.purdue.edu/blog/remote-work-and-the-coronavirus.php", "Purdue Center for Regional Development"), 
                                                "to construct our relative accessibility measures. One of our indicators, remote work accessibility, is also an adaptation of Purdue's remote work metric. 
                                               We depart from their operationalization in using American Community Survey instead of Federal Communications Commission (FCC) data given", a(href = "https://bband.policy-analytics.net", "known issues with FCC Form 477"),
                                                ", as well as in tailoring indicators used to the population in the labor force and to the labor force population that reports not already working from home.")
                                           )
                                    ),
                                    column(6, align = "center",
                                           wellPanel(align = "left",
                                                     strong("Contact"),
                                                     br(),
                                                     br(a( "all the related family")),
                                                     p("Social and Decision Analytics Division, Biocomplexity Institute and Initiative, University of Virginia")
                                           ),
                                           a(href = "https://biocomplexity.virginia.edu/social-decision-analytics", img(src = "uva.png", width = "60%", style = "text-align: center; border: solid 1px white;"), target = "_blank")
                                    )
                           )
                         )
                ),
                
                hr(),
                
                fluidRow(style = "margin: 20px",
                         width = 12, 
                         column(12, align = 'center',
                                tags$small(em('Social and Decision Analytics Division, Biocomplexity Institute and Initiative, University of Virginia')),
                                p(tags$small(em('Last updated: June 2020'))))
                )
                
)


#
# Server ---------------------------------------------
#

server <- function(input, output) {
  
  #
  # Plotly boxplots ------------------------------------------
  # 
  
  # Work: No computer
  output$plotly_nocomp_work <- renderPlotly({
    if (input$whichtopic == "School") {
    #if (input$whichtopic == "Remote work") {
      data <- data_work
      data <- switch(input$whichstate,
                     "Iowa" = data[data$STATEFP == "19", ],
                     "Oregon" = data[data$STATEFP == "41", ],
                     "Virginia" = data[data$STATEFP == "51", ])
      
      plot_ly(y = ~data$nocomputer, 
              x = "Percent population in labor force without a computer",
              showlegend = FALSE,
              hoverinfo = "y",
              type = "box",
              name = "") %>% 
        layout(title = "",
               xaxis = list(title = "",
                            zeroline = FALSE),
               yaxis = list(title = "Percent",
                            zeroline = FALSE,
                            hoverformat = ".2f"))
    }
  })
  
  # Work: No internet
  output$plotly_noint_work <- renderPlotly({
    if (input$whichtopic == "School") {
      #data <- data_work
      data <- data_3states
      data <- switch(input$whichstate,
                     "Iowa" = data[data$STATEFP == "19", ],
                     "Oregon" = data[data$STATEFP == "41", ],
                     "Virginia" = data[data$STATEFP == "51", ])
      
      plot_ly(y = ~data$HS, 
              x = "High school graduation rate",
              showlegend = FALSE,
              #hoverinfo = "y",
              type = "box",
              name = "", 
              text= ~data$county) %>% 
        layout(title = "",
               xaxis = list(title = "",
                            zeroline = FALSE),
               yaxis = list(title = "Percent",
                            zeroline = FALSE,
                            hoverformat = ".2f"))
    }
  })
  
  # Work: Occupations
  output$plotly_occup <- renderPlotly({
    if (input$whichtopic == "School") {
      data <- data_work
      data <- switch(input$whichstate,
                     "Iowa" = data[data$STATEFP == "19", ],
                     "Oregon" = data[data$STATEFP == "41", ],
                     "Virginia" = data[data$STATEFP == "51", ])
      
      plot_ly(y = ~data$occup, 
              x = "Percent non-remote workers in non-remote friendly occupations",
              showlegend = FALSE,
              hoverinfo = "y",
              type = "box",
              name = "") %>% 
        layout(title = "",
               xaxis = list(title = "",
                            zeroline = FALSE),
               yaxis = list(title = "Percent",
                            zeroline = FALSE,
                            hoverformat = ".2f"))
    }
  })
  
  # Work: Industries
  output$plotly_industr <- renderPlotly({
    if (input$whichtopic == "School") {
      data <- data_work
      data <- switch(input$whichstate,
                     "Iowa" = data[data$STATEFP == "19", ],
                     "Oregon" = data[data$STATEFP == "41", ],
                     "Virginia" = data[data$STATEFP == "51", ])
      
      plot_ly(y = ~data$industr, 
              x = "Percent non-remote workers in non-remote friendly industries",
              showlegend = FALSE,
              hoverinfo = "y",
              type = "box",
              name = "") %>% 
        layout(title = "",
               xaxis = list(title = "",
                            zeroline = FALSE),
               yaxis = list(title = "Percent",
                            zeroline = FALSE,
                            hoverformat = ".2f"))
    }
  })
  
  # Med: No internet
  output$plotly_noint_med <- renderPlotly({
    if (input$whichtopic == "Family Structure") {
      #data <- data_med
      data <- data_3states
      data <- switch(input$whichstate,
                     "Iowa" = data[data$STATEFP == "19", ],
                     "Oregon" = data[data$STATEFP == "41", ],
                     "Virginia" = data[data$STATEFP == "51", ])
      
      plot_ly(y = ~data$SP, 
              x = "Percent households without internet access",
              showlegend = FALSE,
              #hoverinfo = "y",
              type = "box",
              name = "",
              text= ~data$county) %>% 
        layout(title = "",
               xaxis = list(title = "",
                            zeroline = FALSE),
               yaxis = list(title = "Percent",
                            zeroline = FALSE,
                            hoverformat = ".2f"))
    }
  })
  
  # Med: No computer
  output$plotly_nocomp_med <- renderPlotly({
    if (input$whichtopic == "Family Structure") {
      data <- data_med
      data <- switch(input$whichstate,
                     "Iowa" = data[data$STATEFP == "19", ],
                     "Oregon" = data[data$STATEFP == "41", ],
                     "Virginia" = data[data$STATEFP == "51", ])
      
      plot_ly(y = ~data$nocomputer, 
              x = "Percent households without a computer",
              showlegend = FALSE,
              hoverinfo = "y",
              type = "box",
              name = "") %>% 
        layout(title = "",
               xaxis = list(title = "",
                            zeroline = FALSE),
               yaxis = list(title = "Percent",
                            zeroline = FALSE,
                            hoverformat = ".2f"))
    }
  })
  
  # Med: Uninsured
  output$plotly_unins <- renderPlotly({
    if (input$whichtopic == "Family Structure") {
      data <- data_med
      data <- switch(input$whichstate,
                     "Iowa" = data[data$STATEFP == "19", ],
                     "Oregon" = data[data$STATEFP == "41", ],
                     "Virginia" = data[data$STATEFP == "51", ])
      
      plot_ly(y = ~data$pct_unins, 
              x = "Percent population without health insurance",
              showlegend = FALSE,
              hoverinfo = "y",
              type = "box",
              name = "") %>% 
        layout(title = "",
               xaxis = list(title = "",
                            zeroline = FALSE),
               yaxis = list(title = "Percent",
                            zeroline = FALSE,
                            hoverformat = ".2f"))
    }
  })
  
  # Med: Mental health days
  output$plotly_menthdays <- renderPlotly({
    if (input$whichtopic == "Family Structure") {
      data <- data_med
      data <- switch(input$whichstate,
                     "Iowa" = data[data$STATEFP == "19", ],
                     "Oregon" = data[data$STATEFP == "41", ],
                     "Virginia" = data[data$STATEFP == "51", ])
      
      plot_ly(y = ~data$avgnum_poormenth, 
              x = "Average number of poor mental health days in past month",
              showlegend = FALSE,
              hoverinfo = "y",
              type = "box",
              name = "") %>% 
        layout(title = "",
               xaxis = list(title = "",
                            zeroline = FALSE),
               yaxis = list(title = "Percent",
                            zeroline = FALSE,
                            hoverformat = ".2f"))
    }
  })
  
  # Med: Providers
  output$plotly_menthprov <- renderPlotly({
    if (input$whichtopic == "Family Structure") {
      data <- data_med
      data <- switch(input$whichstate,
                     "Iowa" = data[data$STATEFP == "19", ],
                     "Oregon" = data[data$STATEFP == "41", ],
                     "Virginia" = data[data$STATEFP == "51", ])
      
      plot_ly(y = ~data$menthprov_per100k, 
              x = "Number of mental health providers per 100,000 population",
              showlegend = FALSE,
              hoverinfo = "y",
              type = "box",
              name = "") %>% 
        layout(title = "",
               xaxis = list(title = "",
                            zeroline = FALSE),
               yaxis = list(title = "Percent",
                            zeroline = FALSE,
                            hoverformat = ".2f"))
    }
  })
  
  # Edu: No internet
  output$plotly_noint_edu <- renderPlotly({
    if (input$whichtopic == "Segregation") {
    #if (input$whichtopic == "Remote education") {
      #data <- data_edu
      data <- data_3states
      data <- switch(input$whichstate,
                     "Iowa" = data[data$STATEFP == "19", ],
                     "Oregon" = data[data$STATEFP == "41", ],
                     "Virginia" = data[data$STATEFP == "51", ])
      
      #plot_ly(y = ~data$nointernet, 
      plot_ly(y = ~data$NHW, 
              x = "Proportion of White population",
              showlegend = FALSE,
              #hoverinfo = "y",
              type = "box",
              name = "",
              text= ~data$county) %>% 
        layout(title = "",
               xaxis = list(title = "",
                            zeroline = FALSE),
               yaxis = list(title = "Percent",
                            zeroline = FALSE,
                            hoverformat = ".2f"))
    }
  })
  
  # Edu: No computer
  output$plotly_nocomp_edu <- renderPlotly({
    if (input$whichtopic == "Segregation") {
    #if (input$whichtopic == "Remote education") {
      #data <- data_edu
      data <- data_3states
      data <- switch(input$whichstate,
                     "Iowa" = data[data$STATEFP == "19", ],
                     "Oregon" = data[data$STATEFP == "41", ],
                     "Virginia" = data[data$STATEFP == "51", ])
      
      plot_ly(y = ~data$CT, 
              x = "Percent of commuters",
              showlegend = FALSE,
              #hoverinfo = "y",
              type = "box",
              name = "",
              text= ~data$county) %>% 
        layout(title = "",
               xaxis = list(title = "",
                            zeroline = FALSE),
               yaxis = list(title = "Percent",
                            zeroline = FALSE,
                            hoverformat = ".2f"))
    }
  })
  
  # In K-12
  output$plotly_ink12 <- renderPlotly({
    if (input$whichtopic == "Segregation") {
    #if (input$whichtopic == "Remote education") {
      data <- data_edu
      data <- switch(input$whichstate,
                     "Iowa" = data[data$STATEFP == "19", ],
                     "Oregon" = data[data$STATEFP == "41", ],
                     "Virginia" = data[data$STATEFP == "51", ])
      
      plot_ly(y = ~data$ink12, 
              x = "Percent population enrolled in K-12",
              showlegend = FALSE,
              hoverinfo = "y",
              type = "box",
              name = "") %>% 
        layout(title = "",
               xaxis = list(title = "",
                            zeroline = FALSE),
               yaxis = list(title = "Percent",
                            zeroline = FALSE,
                            hoverformat = ".2f"))
    }
  })
  
  
  # SocialCapital: Voter turnout
  output$plotly_voter <- renderPlotly({
    if (input$whichtopic == "Social Capital") {
      data <- data_3states
      data <- switch(input$whichstate,
                     "Iowa" = data[data$STATEFP == "19", ],
                     "Oregon" = data[data$STATEFP == "41", ],
                     "Virginia" = data[data$STATEFP == "51", ])
      
      plot_ly(y = ~data$GOTV, 
              x = "Voter turnout proportion",
              showlegend = FALSE,
              #hoverinfo = "y",
              type = "box",
              name = "",
              text= ~data$county) %>% 
        layout(title = "",
               xaxis = list(title = "",
                            zeroline = FALSE),
               yaxis = list(title = "Percent",
                            zeroline = FALSE,
                            hoverformat = ".2f"))
    }
  }) 
  
  
  
  
  
  #
  # Main plots ---------------------------------------------
  #
  
  output$mainplot <- renderLeaflet({
    
    # Main plot: remote education
    if (input$whichtopic == "Segregation") {
    #if (input$whichtopic == "Remote education") {
      data <- data_edu
      data <- switch(input$whichstate,
                     "Iowa" = data[data$STATEFP == "19", ],
                     "Oregon" = data[data$STATEFP == "41", ],
                     "Virginia" = data[data$STATEFP == "51", ])
      
      pal <- colorFactor("Oranges", domain = data$accessibility)
      
      labels <- lapply(
        paste("<strong>County: </strong>",
              data$name,
              "<br />",
              "<strong>Relative accessibility:</strong>",
              data$accessibility,
              "<br />",
              "<strong>Placed in 4th or 5th quintile:</strong>",
              data$scoreTop, " times",
              "<br />",
              "<strong>% Youth (age <18) without internet access:</strong>",
              round(data$nointernet, 2), "(quintile", data$nointernetQuint, ")",
              "<br />",
              "<strong>% Households without a computer:</strong>",
              round(data$nocomputer, 2), "(quintile", data$nocomputerQuint, ")",
              "<br />",
              "<strong>% Population enrolled in K-12:</strong>",
              round(data$ink12, 2), "(quintile", data$ink12Quint, ")"),
        htmltools::HTML
      )
      
      leaflet(data) %>%
        addTiles() %>%
        addPolygons(fillColor = ~pal(data$accessibility), 
                    fillOpacity = 0.8,
                    stroke = TRUE,
                    weight = 0.9,
                    color = "gray",
                    smoothFactor = 0.7,
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft", pal = pal, values = ~data$accessibility,
                  title = "Relative Accessibility", opacity = 1,
                  na.label = "Not Available")
      
      # Main plot: remote work
    } else if (input$whichtopic == "School") {
      data <- data_work
      
      data <- switch(input$whichstate,
                     "Iowa" = data[data$STATEFP == "19", ],
                     "Oregon" = data[data$STATEFP == "41", ],
                     "Virginia" = data[data$STATEFP == "51", ])
      
      pal <- colorFactor("Oranges", domain = data$accessibility)
      
      labels <- lapply(
        paste("<strong>County: </strong>",
              data$name,
              "<br />",
              "<strong>Relative accessibility:</strong>",
              data$accessibility,
              "<br />",
              "<strong>Placed in 4th or 5th quintile:</strong>",
              data$scoreTop, " times",
              "<br />",
              "<strong>% households without internet access:</strong>",
              round(data$nointernet, 2), "(quintile", data$nointernetQuint, ")",
              "<br />",
              "<strong>% population in labor force without a computer:</strong>",
              round(data$nocomputer, 2), "(quintile", data$nocomputerQuint, ")",
              "<br />",
              "<strong>% non-remote workers in non-remote friendly occupations:</strong>",
              round(data$occup, 2), "(quintile", data$occupQuint, ")",
              "<br />",
              "<strong>% non-remote workers in non-remote friendly industries:</strong>",
              round(data$industr, 2), "(quintile", data$industrQuint, ")"),
        htmltools::HTML
      )
      
      leaflet(data) %>%
        addTiles() %>%
        addPolygons(fillColor = ~pal(data$accessibility), 
                    fillOpacity = 0.8,
                    stroke = TRUE,
                    weight = 0.9,
                    color = "gray",
                    smoothFactor = 0.7,
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft", pal = pal, values = ~data$accessibility,
                  title = "Relative Accessibility", opacity = 1,
                  na.label = "Not Available")
      
      # Main plot: Family structure
    } else if (input$whichtopic == "Family Structure") {
      data <- data_med
      data <- switch(input$whichstate,
                     "Iowa" = data[data$STATEFP == "19", ],
                     "Oregon" = data[data$STATEFP == "41", ],
                     "Virginia" = data[data$STATEFP == "51", ])
      
      pal <- colorFactor("Oranges", domain = data$accessibility)
      
      labels <- lapply(
        paste("<strong>County: </strong>",
              data$name,
              "<br />",
              "<strong>Relative accessibility:</strong>",
              data$accessibility,
              "<br />",
              "<strong>Placed in 4th or 5th quintile:</strong>",
              data$scoreTop, " times",
              "<br />",
              "<strong>% households without internet access:</strong>",
              round(data$nointernet, 2), "(quintile", data$nointernetQuint, ")",
              "<br />",
              "<strong>% households without a computer:</strong>",
              round(data$nocomputer, 2), "(quintile", data$nocomputerQuint, ")",
              "<br />",
              "<strong>% population without health insurance:</strong>",
              round(data$pct_unins, 2), "(quintile", data$uninsQuint, ")",
              "<br />",
              "<strong>Number of mental health providers per 100,000:</strong>",
              round(data$menthprov_per100k, 2), "(quintile", data$menthprovQuint, ")",
              "<br />",
              "<strong>Average number of poor mental health days:</strong>",
              round(data$avgnum_poormenth, 2), "(quintile", data$menthdaysQuint, ")"),
        htmltools::HTML
      )
      
      leaflet(data) %>%
        addTiles() %>%
        addPolygons(fillColor = ~pal(data$accessibility), 
                    fillOpacity = 0.8,
                    stroke = TRUE,
                    weight = 0.9,
                    color = "gray",
                    smoothFactor = 0.7,
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft", pal = pal, values = ~data$accessibility,
                  title = "Relative Accessibility", opacity = 1,
                  na.label = "Not Available")
    }  
    
    ####
    else if (input$whichtopic == "Social Capital") {
      data <- data_med
      data <- switch(input$whichstate,
                     "Iowa" = data[data$STATEFP == "19", ],
                     "Oregon" = data[data$STATEFP == "41", ],
                     "Virginia" = data[data$STATEFP == "51", ])
      
      pal <- colorFactor("Oranges", domain = data$accessibility)
      
      labels <- lapply(
        paste("<strong>County: </strong>",
              data$name,
              "<br />",
              "<strong>Relative accessibility:</strong>",
              data$accessibility,
              "<br />",
              "<strong>Placed in 4th or 5th quintile:</strong>",
              data$scoreTop, " times",
              "<br />",
              "<strong>% households without internet access:</strong>",
              round(data$nointernet, 2), "(quintile", data$nointernetQuint, ")",
              "<br />",
              "<strong>% households without a computer:</strong>",
              round(data$nocomputer, 2), "(quintile", data$nocomputerQuint, ")",
              "<br />",
              "<strong>% population without health insurance:</strong>",
              round(data$pct_unins, 2), "(quintile", data$uninsQuint, ")",
              "<br />",
              "<strong>Number of mental health providers per 100,000:</strong>",
              round(data$menthprov_per100k, 2), "(quintile", data$menthprovQuint, ")",
              "<br />",
              "<strong>Average number of poor mental health days:</strong>",
              round(data$avgnum_poormenth, 2), "(quintile", data$menthdaysQuint, ")"),
        htmltools::HTML
      )
      
      leaflet(data) %>%
        addTiles() %>%
        addPolygons(fillColor = ~pal(data$accessibility), 
                    fillOpacity = 0.8,
                    stroke = TRUE,
                    weight = 0.9,
                    color = "gray",
                    smoothFactor = 0.7,
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft", pal = pal, values = ~data$accessibility,
                  title = "Relative Accessibility", opacity = 1,
                  na.label = "Not Available")
    }
    ###
    
  })
  
  #
  # Quintile tables ---------------------------------------------
  #
  
  # Remote education
  output$table_quint_edu <- renderTable({
    if (input$whichtopic == "Segregation") {
    #if (input$whichtopic == "Remote education") {
      data <- data_edu
      data <- switch(input$whichstate,
                     "Iowa" = data[data$STATEFP == "19", ],
                     "Oregon" = data[data$STATEFP == "41", ],
                     "Virginia" = data[data$STATEFP == "51", ])
      
      qnoint <- quantile(data$nointernet, prob = seq(0, 1, 0.2), na.rm = TRUE)
      qnocomp <- quantile(data$nocomputer, prob = seq(0, 1, 0.2), na.rm = TRUE)
      qink12 <- quantile(data$ink12, prob = seq(0, 1, 0.2), na.rm = TRUE)
      
      quintcuts <- bind_rows(qnoint, qnocomp, qink12)
      quintcuts$Indicator <- c("% Households without internet access", "% Youth (age <18) with no computer", "% Population enrolled in K-12")
      quintcuts <- quintcuts %>% select(Indicator, 2:7)
      colnames(quintcuts) <- c("Indicator", "<Q1", "<Q2", "<Q3", "<Q4", "<Q5")
      quintcuts    
    }
  })
  
  # Remote work
  output$table_quint_work <- renderTable({
    if (input$whichtopic == "Remote work") {
      data <- data_work
      data <- switch(input$whichstate,
                     "Iowa" = data[data$STATEFP == "19", ],
                     "Oregon" = data[data$STATEFP == "41", ],
                     "Virginia" = data[data$STATEFP == "51", ])
      
      qnoint <- quantile(data$nointernet, prob = seq(0, 1, 0.2), na.rm = TRUE)
      qnocomp <- quantile(data$nocomputer, prob = seq(0, 1, 0.2), na.rm = TRUE)
      qoccup <- quantile(data$occup, prob = seq(0, 1, 0.2), na.rm = TRUE)
      qind <- quantile(data$industr, prob = seq(0, 1, 0.2), na.rm = TRUE)
      
      quintcuts <- bind_rows(qnoint, qnocomp, qoccup, qind)
      quintcuts$Indicator <- c("% Households without broadband internet", "% Population in labor force without a computer", "% Population in non-remote labor force in non-remote-friendly occupations", "% Population in non-remote labor force in non-remote-friendly industries")
      quintcuts <- quintcuts %>% select(Indicator, 2:7)
      colnames(quintcuts) <- c("Indicator", "<Q1", "<Q2", "<Q3", "<Q4", "<Q5")
      quintcuts
    }
  })
  
  output$table_quint_med <- renderTable({ 
    # Telehealth
    if (input$whichtopic == "Family Structure") {
      data <- data_med
      data <- switch(input$whichstate,
                     "Iowa" = data[data$STATEFP == "19", ],
                     "Oregon" = data[data$STATEFP == "41", ],
                     "Virginia" = data[data$STATEFP == "51", ])
      
      qnoint <- quantile(data$nointernet, prob = seq(0, 1, 0.2), na.rm = TRUE)
      qnocomp <- quantile(data$nocomputer, prob = seq(0, 1, 0.2), na.rm = TRUE)
      qpoorment <- quantile(data$avgnum_poormenth, prob = seq(0, 1, 0.2), na.rm = TRUE)
      qnumprov <- quantile(desc(data$menthprov_per100k), prob = seq(0, 1, 0.2), na.rm = TRUE)
      qnumprov <- abs(qnumprov)
      qunins <- quantile(data$pct_unins, prob = seq(0, 1, 0.2), na.rm = TRUE)
      
      quintcuts <- bind_rows(qnoint, qnocomp, qpoorment, qnumprov, qunins)
      quintcuts$Indicator <- c("% Households without internet access", "% Households without a computer", "Average number of poor mental health days", "Number of mental health providers per 100,000 population", "% Population without health insurance")
      quintcuts <- quintcuts %>% select(Indicator, 2:7)
      colnames(quintcuts) <- c("Indicator", "<Q1", "<Q2", "<Q3", "<Q4", "<Q5")
      quintcuts
    }
  })
  
  #
  # Subplots: No internet ------------------------------------------------
  #
  
  # Education 
  output$subplot_noint_edu <- renderLeaflet({
    if (input$whichtopic == "Segregation") {
     #if (input$whichtopic == "Remote education") {
      #data <- data_edu
      data <- data_3states
      data <- switch(input$whichstate,
                     "Iowa" = data[data$STATEFP == "19", ],
                     "Oregon" = data[data$STATEFP == "41", ],
                     "Virginia" = data[data$STATEFP == "51", ])
      
      pal <- colorQuantile("Blues", domain = data$NHW, probs = seq(0, 1, length = 6), right = TRUE)
      
      labels <- lapply(
        paste("<strong>County: </strong>",
              data$name,
              "<br />",
              "<strong>% White Non Hispanic proportion:</strong>",
              round(data$NHW, 2), 
              "<br />",
              "<strong>Quintile:</strong>",
              data$NHWQuint),
        htmltools::HTML
      )
      
      leaflet(data) %>%
        addTiles() %>%
        addPolygons(fillColor = ~pal(data$NHW), 
                    fillOpacity = 0.8,
                    stroke = TRUE,
                    weight = 0.9,
                    color = "gray",
                    smoothFactor = 0.7,
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft", pal = pal, values = ~data$NHW,
                  title = "Percent<br>(Quintile Group)", opacity = 1,
                  na.label = "Not Available",
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }
  })
  
  # Work
  output$subplot_noint_work <- renderLeaflet({
    if (input$whichtopic == "School") {
      #data <- data_work
      data <- data_3states
      data <- switch(input$whichstate,
                     "Iowa" = data[data$STATEFP == "19", ],
                     "Oregon" = data[data$STATEFP == "41", ],
                     "Virginia" = data[data$STATEFP == "51", ])
      
      pal <- colorQuantile("Blues", domain = data$HS, probs = seq(0, 1, length = 6), right = TRUE)
      
      labels <- lapply(
        paste("<strong>County: </strong>",
              data$name,
              "<br />",
              "<strong>% Households without broadband internet:</strong>",
              round(data$HS, 2), 
              "<br />",
              "<strong>Quintile:</strong>",
              data$HSQuint),
        htmltools::HTML
      )
      
      leaflet(data) %>%
        addTiles() %>%
        addPolygons(fillColor = ~pal(data$HS), 
                    fillOpacity = 0.8,
                    stroke = TRUE,
                    weight = 0.9,
                    color = "gray",
                    smoothFactor = 0.7,
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft", pal = pal, values = ~data$HS,
                  title = "Percent<br>(Quintile Group)", opacity = 1,
                  na.label = "Not Available",
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }
  })
  
  # Single parent families
  output$subplot_noint_med <- renderLeaflet({
    if (input$whichtopic == "Family Structure") {
      #data <- data_med
      data <- data_3states
      data <- switch(input$whichstate,
                     "Iowa" = data[data$STATEFP == "19", ],
                     "Oregon" = data[data$STATEFP == "41", ],
                     "Virginia" = data[data$STATEFP == "51", ])
      
      pal <- colorQuantile("Blues", domain = data$SP, probs = seq(0, 1, length = 6), right = TRUE)
      
      labels <- lapply(
        paste("<strong>County: </strong>",
              data$name,
              "<br />",
              "<strong>% Households without internet access:</strong>",
              round(data$SP, 2), 
              "<br />",
              "<strong>Quintile:</strong>",
              data$SPQuint),
        htmltools::HTML
      )
      
      leaflet(data) %>%
        addTiles() %>%
        addPolygons(fillColor = ~pal(data$SP), 
                    fillOpacity = 0.8,
                    stroke = TRUE,
                    weight = 0.9,
                    color = "gray",
                    smoothFactor = 0.7,
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft", pal = pal, values = ~data$SP,
                  title = "Percent<br>(Quintile Group)", opacity = 1,
                  na.label = "Not Available",
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }
  })
  
  
  
  
  # Social Capital: Voter turnout prop
  output$subplot_voter <- renderLeaflet({
    if (input$whichtopic == "Social Capital") {
      data <- data_3states
      data <- switch(input$whichstate,
                     "Iowa" = data[data$STATEFP == "19", ],
                     "Oregon" = data[data$STATEFP == "41", ],
                     "Virginia" = data[data$STATEFP == "51", ])
      
      pal <- colorQuantile("Blues", domain = data$GOTV, probs = seq(0, 1, length = 6), right = TRUE)
      
      labels <- lapply(
        paste("<strong>County: </strong>",
              data$name,
              "<br />",
              "<strong>% Voter >",
              round(data$GOTV, 2), 
              "<br />",
              "<strong>Quintile:</strong>",
              data$GOTVQuint),
        htmltools::HTML
      )
      
      leaflet(data) %>%
        addTiles() %>%
        addPolygons(fillColor = ~pal(data$GOTV), 
                    fillOpacity = 0.8,
                    stroke = TRUE,
                    weight = 0.9,
                    color = "gray",
                    smoothFactor = 0.7,
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft", pal = pal, values = ~data$GOTV,
                  title = "Percent<br>(Quintile Group)", opacity = 1,
                  na.label = "Not Available",
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }
  })
  
  #
  # Subplots: No computer -----------------------------------------------
  #
  
  # Education
  output$subplot_nocomp_edu <- renderLeaflet({
    if (input$whichtopic == "Segregation") {
    #if (input$whichtopic == "Remote education") {
      #data <- data_edu
      data <- data_3states
      data <- switch(input$whichstate,
                     "Iowa" = data[data$STATEFP == "19", ],
                     "Oregon" = data[data$STATEFP == "41", ],
                     "Virginia" = data[data$STATEFP == "51", ])
      
      pal <- colorQuantile("Blues", domain = data$CT, probs = seq(0, 1, length = 6), right = TRUE)
      
      labels <- lapply(
        paste("<strong>County: </strong>",
              data$name,
              "<br />",
              "<strong>% Households without a computer:</strong>",
              round(data$CT, 2), 
              "<br />",
              "<strong>Quintile:</strong>",
              data$CTQuint),
        htmltools::HTML
      )
      
      leaflet(data) %>%
        addTiles() %>%
        addPolygons(fillColor = ~pal(data$CT), 
                    fillOpacity = 0.8,
                    stroke = TRUE,
                    weight = 0.9,
                    color = "gray",
                    smoothFactor = 0.7,
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft", pal = pal, values = ~data$CT,
                  title = "Percent<br>(Quintile Group)", opacity = 1,
                  na.label = "Not Available",
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }
  })
  
  # Work
  output$subplot_nocomp_work <- renderLeaflet({
    if (input$whichtopic == "Remote work") {
      data <- data_work
      data <- switch(input$whichstate,
                     "Iowa" = data[data$STATEFP == "19", ],
                     "Oregon" = data[data$STATEFP == "41", ],
                     "Virginia" = data[data$STATEFP == "51", ])
      
      pal <- colorQuantile("Blues", domain = data$nocomputer, probs = seq(0, 1, length = 6), right = TRUE)
      
      labels <- lapply(
        paste("<strong>County: </strong>",
              data$name,
              "<br />",
              "<strong>% Households without a computer:</strong>",
              round(data$nocomputer, 2), 
              "<br />",
              "<strong>Quintile:</strong>",
              data$nocomputerQuint),
        htmltools::HTML
      )
      
      leaflet(data) %>%
        addTiles() %>%
        addPolygons(fillColor = ~pal(data$nocomputer), 
                    fillOpacity = 0.8,
                    stroke = TRUE,
                    weight = 0.9,
                    color = "gray",
                    smoothFactor = 0.7,
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft", pal = pal, values = ~data$nocomputer,
                  title = "Percent<br>(Quintile Group)", opacity = 1,
                  na.label = "Not Available",
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }
  })
  
  # Telehealth
  output$subplot_nocomp_med <- renderLeaflet({
    if (input$whichtopic == "Family Structure") {
      data <- data_med
      data <- switch(input$whichstate,
                     "Iowa" = data[data$STATEFP == "19", ],
                     "Oregon" = data[data$STATEFP == "41", ],
                     "Virginia" = data[data$STATEFP == "51", ])
      
      pal <- colorQuantile("Blues", domain = data$nocomputer, probs = seq(0, 1, length = 6), right = TRUE)
      
      labels <- lapply(
        paste("<strong>County: </strong>",
              data$name,
              "<br />",
              "<strong>% Households without a computer:</strong>",
              round(data$nocomputer, 2),
              "<br />",
              "<strong>Quintile:</strong>",
              data$nocomputerQuint),
        htmltools::HTML
      )
      
      leaflet(data) %>%
        addTiles() %>%
        addPolygons(fillColor = ~pal(data$nocomputer), 
                    fillOpacity = 0.8,
                    stroke = TRUE,
                    weight = 0.9,
                    color = "gray",
                    smoothFactor = 0.7,
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft", pal = pal, values = ~data$nocomputer,
                  title = "Percent<br>(Quintile Group)", opacity = 1,
                  na.label = "Not Available",
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }
  })
  
  #
  # Subplots: various -----------------------------------------------
  #
  
  # In K12
  output$subplot_ink12 <- renderLeaflet({
    if (input$whichtopic == "Segregation") {
    #if (input$whichtopic == "Remote education") {
      data <- data_edu
      data <- switch(input$whichstate,
                     "Iowa" = data[data$STATEFP == "19", ],
                     "Oregon" = data[data$STATEFP == "41", ],
                     "Virginia" = data[data$STATEFP == "51", ])
      
      pal <- colorQuantile("Blues", domain = data$ink12, probs = seq(0, 1, length = 6), right = TRUE)
      
      labels <- lapply(
        paste("<strong>County: </strong>",
              data$name,
              "<br />",
              "<strong>% Population enrolled in K-12:</strong>",
              round(data$ink12, 2), 
              "<br />",
              "<strong>Quintile:</strong>",
              data$ink12Quint),
        htmltools::HTML
      )
      
      leaflet(data) %>%
        addTiles() %>%
        addPolygons(fillColor = ~pal(data$ink12), 
                    fillOpacity = 0.8,
                    stroke = TRUE,
                    weight = 0.9,
                    color = "gray",
                    smoothFactor = 0.7,
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft", pal = pal, values = ~data$ink12,
                  title = "Percent<br>(Quintile Group)", opacity = 1,
                  na.label = "Not Available",
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }
  })
  
  # Occupation
  output$subplot_occup <- renderLeaflet({
    if (input$whichtopic == "Remote work") {
      data <- data_work
      data <- switch(input$whichstate,
                     "Iowa" = data[data$STATEFP == "19", ],
                     "Oregon" = data[data$STATEFP == "41", ],
                     "Virginia" = data[data$STATEFP == "51", ])
      
      pal <- colorQuantile("Blues", domain = data$occup, probs = seq(0, 1, length = 6), right = TRUE)
      
      labels <- lapply(
        paste("<strong>County: </strong>",
              data$name,
              "<br />",
              "<strong>% Non-remote workers in non-remote-friendly occupations:</strong>",
              round(data$occup, 2), 
              "<br />",
              "<strong>Quintile:</strong>",
              data$occupQuint),
        htmltools::HTML
      )
      
      leaflet(data) %>%
        addTiles() %>%
        addPolygons(fillColor = ~pal(data$occup), 
                    fillOpacity = 0.8,
                    stroke = TRUE,
                    weight = 0.9,
                    color = "gray",
                    smoothFactor = 0.7,
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft", pal = pal, values = ~data$occup,
                  title = "Percent<br>(Quintile Group)", opacity = 1,
                  na.label = "Not Available",
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }
  })
  
  # Industry
  output$subplot_industr <- renderLeaflet({
    if (input$whichtopic == "Remote work") {
      data <- data_work
      data <- switch(input$whichstate,
                     "Iowa" = data[data$STATEFP == "19", ],
                     "Oregon" = data[data$STATEFP == "41", ],
                     "Virginia" = data[data$STATEFP == "51", ])
      
      pal <- colorQuantile("Blues", domain = data$industr, probs = seq(0, 1, length = 6), right = TRUE)
      
      labels <- lapply(
        paste("<strong>County: </strong>",
              data$name,
              "<br />",
              "<strong>% Non-remote workers in non-remote-friendly industries:</strong>",
              round(data$industr, 2), 
              "<br />",
              "<strong>Quintile:</strong>",
              data$industrQuint),
        htmltools::HTML
      )
      
      leaflet(data) %>%
        addTiles() %>%
        addPolygons(fillColor = ~pal(data$industr), 
                    fillOpacity = 0.8,
                    stroke = TRUE,
                    weight = 0.9,
                    color = "gray",
                    smoothFactor = 0.7,
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft", pal = pal, values = ~data$industr,
                  title = "Percent<br>(Quintile Group)", opacity = 1,
                  na.label = "Not Available",
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }
  })
  
  # Poor mental health days
  output$subplot_poorment <- renderLeaflet({
    if (input$whichtopic == "Family Structure") {
      data <- data_med
      data <- switch(input$whichstate,
                     "Iowa" = data[data$STATEFP == "19", ],
                     "Oregon" = data[data$STATEFP == "41", ],
                     "Virginia" = data[data$STATEFP == "51", ])
      
      pal <- colorQuantile("Blues", domain = data$avgnum_poormenth, probs = seq(0, 1, length = 6), right = TRUE)
      
      labels <- lapply(
        paste("<strong>County: </strong>",
              data$name,
              "<br />",
              "<strong>Average number of poor mental health days in past month:</strong>",
              round(data$avgnum_poormenth, 2), 
              "<br />",
              "<strong>Quintile:</strong>",
              data$menthdaysQuint),
        htmltools::HTML
      )
      
      leaflet(data) %>%
        addTiles() %>%
        addPolygons(fillColor = ~pal(data$avgnum_poormenth), 
                    fillOpacity = 0.8,
                    stroke = TRUE,
                    weight = 0.9,
                    color = "gray",
                    smoothFactor = 0.7,
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft", pal = pal, values = ~data$avgnum_poormenth,
                  title = "Number<br>(Quintile Group)", opacity = 1,
                  na.label = "Not Available",
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }
  })
  
  # Percent uninsured
  output$subplot_unins <- renderLeaflet({
    if (input$whichtopic == "Family Structure") {
      data <- data_med
      data <- switch(input$whichstate,
                     "Iowa" = data[data$STATEFP == "19", ],
                     "Oregon" = data[data$STATEFP == "41", ],
                     "Virginia" = data[data$STATEFP == "51", ])
      
      pal <- colorQuantile("Blues", domain = data$pct_unins, probs = seq(0, 1, length = 6), right = TRUE)
      
      labels <- lapply(
        paste("<strong>County: </strong>",
              data$name,
              "<br />",
              "<strong>% Population without health insurance:</strong>",
              round(data$pct_unins, 2),
              "<br />",
              "<strong>Quintile:</strong>",
              data$uninsQuint),
        htmltools::HTML
      )
      
      leaflet(data) %>%
        addTiles() %>%
        addPolygons(fillColor = ~pal(data$pct_unins), 
                    fillOpacity = 0.8,
                    stroke = TRUE,
                    weight = 0.9,
                    color = "gray",
                    smoothFactor = 0.7,
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft", pal = pal, values = ~data$pct_unins,
                  title = "Percent<br>(Quintile Group)", opacity = 1,
                  na.label = "Not Available",
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }
  })
  
  # Mental health providers
  output$subplot_healthprov <- renderLeaflet({
    if (input$whichtopic == "Family Structure") {
      data <- data_med
      data <- switch(input$whichstate,
                     "Iowa" = data[data$STATEFP == "19", ],
                     "Oregon" = data[data$STATEFP == "41", ],
                     "Virginia" = data[data$STATEFP == "51", ])
      
      pal <- colorQuantile("Blues", domain = data$menthprov_per100k, probs = seq(0, 1, length = 6), right = TRUE, reverse = TRUE)
      
      labels <- lapply(
        paste("<strong>County: </strong>",
              data$name,
              "<br />",
              "<strong>Number of mental health providers per 100,000 population:</strong>",
              round(data$menthprov_per100k, 2), 
              "<br />",
              "<strong>Quintile:</strong>",
              data$menthprovQuint),
        htmltools::HTML
      )
      
      leaflet(data) %>%
        addTiles() %>%
        addPolygons(fillColor = ~pal(data$menthprov_per100k), 
                    fillOpacity = 0.8,
                    stroke = TRUE,
                    weight = 0.9,
                    color = "gray",
                    smoothFactor = 0.7,
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft", pal = pal, values = ~data$menthprov_per100k,
                  title = "Number<br>(Quintile Group)", opacity = 1,
                  na.label = "Not Available",
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }
  })
  
}


#
# App ---------------------------------------------
#

shinyApp(ui = ui, server = server)