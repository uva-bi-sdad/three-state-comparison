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
                           ),
                           
                           
  # Brandon's Historical Part 
                           tabPanel(title = "Historical Background",
                                    
                                    fluidRow(style = "margin: 6px",
                                             width = 12, 
                                             column(12, align = "left", 
                                                    h3(strong("Historical Background"))
                                             )
                                    ),
                                    
                                    fluidRow(style = "margin: 6px",
                                             width = 12, 
                                             column(12, align = "left",
                                                    wellPanel(
                                                      strong("Economic Mobility in IA, OR and VA"),
                                                      br(),
                                                      br("Chetty et al. (2014) identified five core factors correlated with economic mobility. Economic mobility is correlated with several neighborhood characteristics, including:"),
                                                      br("1. income inequality quantified by the Gini coefficient (~ -0.6); and the fraction of households in the middle class defined as the fraction with incomes between the 1st and 3rd quartiles (~ +0.7)"),
                                                      p("2. segregation by race and poverty quantified by the fraction of non-Hispanic whites (~ -0.4); the fraction of individuals in poverty defined as the bottom income quartile (~ -0.5); and fraction of individuals who commute less than 15 minutes to work in a commuter zone (~ +0.6)"),
                                                      p("3. quality of the K-12 school system quantified by output measures such income-adjusted test scores (~ +0.6) and high school graduation rate (~ +0.6)"),
                                                      p("4. social capital quantified by the social capital index, the index is comprised of voter turnout rates; the fraction of people who return their decennial census forms; and various measures of participation in community organizations (~ +0.6)"),
                                                      p("5. family structure quantified by the fraction of children living in single-parent households (~ -0.7); the fraction of adults who are divorced (~ -0.5); and the fraction of adults who are married (~ +0.6)") 
                                                    ))
                                    ),
                                    
                                    fluidRow(style = "margin: 6px",
                                             width = 12, 
                                             column(12, align = "left",
                                                    wellPanel(align = "left",
                                                              strong("Income Inequality in IA, OR and VA"),
                                                              br(),
                                                              p("Economic inequality has been growing in the United States since at least the late-1960s (Pew Research 2019). The average income has increased for Americans between the median and 90th percentile of earners, but wages have stagnated for those in the bottom 50% (Piketty et al. 2017). While the US has the highest levels of income inequality among the G7 countries, there are also a number of geographic and sociodemographic factors that contribute more strongly to existing disparities across various regions within the US (Manduca 2019; Pew Research 2019). For one, low wages have had a disproportionate impact on racial and ethnic minorities (Akee et al. 2020; Chetty et al. 2020; Manduca 2018), inhibiting marginalized, especially black, communities’ capacity to accrue wealth over the course of the 20th century (Rothstein 2017: 153-176). Moreover, the lasting effects of deindustrialization have moved manufacturing jobs away from rural areas to overseas locations, shifting many jobs to the service sector and leaving millions of others without work (Alderson 1999; Kollmeyer 2009). The result has been a geographic divergence where rural areas have experienced “brain drain” of their highly-educated youth who now cluster around major metropolitan areas with jobs in the tech and knowledge industries (Carr and Kefalas 2009; Moretti 2012). This geographic divergence in access to resources shapes social and political cohesion across the country, as residents see themselves as having less and less in common with each other (Beramendi 2012)."),
                                                              p("Overall, our three states have dramatically different economies and in their opportunities for economic mobility. While Iowa is often assumed to be an agricultural state, Iowa’s most prominent industries are manufacturing, healthcare, retail and education (Statistical Atlas 2020). In 2010, Iowa was considered to be one of the most friendly states to do business (CNBC 2010), but the state has seen significant declines in the past decade - likely driven by a depleting workforce and higher cost of business (CNBC 2019). Today, the manufacturing sector consists mainly of processed food, industrial outputs include machinery, electric equipment, chemical products, publishing, and primary metals with companies like Heinz, General Mills, Quaker Oats, Tyson Foods, and ConAgra Foods have large factories in Iowa. Traditionally, Iowa also has a rich business sector, including companies that focus on finance, insurance, healthcare, and energy like wind and ethanol. This diverse economy likely explains why Iowa fared so well on Chetty’s economic mobility atlas in 2010, but it is likely that Iowa’s economic declines and depleting workforce may negatively impact their rankings this time around."),
                                                              p("Unlike Iowa, Virginia has maintained its top seeding as a friendly business state (CNBC 2019), but, when we look more closely, Virginia is really a story of two states - with the northern peak of the state being a center of government, healthcare, finance, and tech with the rest of the state’s economy being predicated on agriculture and the service sector (Statistical Atlas 2020). First off, Virginia has the highest defense spending of any state per capita and contains about 170,000 government jobs, including federal agencies like the NSF, CIA, and DoD (Governing 2019). Northern VA is also seen by many as an emerging “Silicon Valley of the East” (Washington Post 2018), largely because of its proximity to Washington DC and major infrastructural advantages; in fact, the majority of the internet runs through northern VA (Malecki 2002; Richmond Times 2019). On the other hand, southern VA has historical roots in a number of withering industries that include agriculture, manufacturing, coal mining, and railroads. While there are 334,000 ag jobs in Virginia (Weldon Cooper 2017) with the main cash crops include tobacco, apples, grapes, peanuts, tomatoes while the livestock mainly produces turkeys and broilers (USDA 2017), this industry has waned over the years (citation). Moreover, many of the major cities in Virginia, including Blacksburg, Roanoke, Lynchburg, Richmond, and Norfolk have extremely low rates of economic mobility (see The Opportunity Atlas), now depending on educational institutions and the service industry for jobs (Statistical Atlas 2020)."),
                                                              p("While Oregon’s economy was traditionally based on agriculture and forestry, the state has recently shifted more toward manufacturing, service, and technology (Statistical Atlas 2020). Today, Oregon’s agricultural industry is predicated on blueberries, almonds, hazelnuts, wine/grapes, cattle, dairy, sheep, poultry, fishing, as well as the timber and forestry industry (citation). Despite this industry declining (citation), it does not seem that the areas with the lowest economic mobility directly coincide with the areas impacted hardest by loss of private sector jobs in timber (see The Opportunity Atlas). Apart from the agricultural sector, OR is also known for tourism (mainly in Portland), tech (i.e. Silicon Forest), healthcare (Cambia and Kaiser) as well as corporate headquarters for clothing, steel, and manufacturing companies (citation). Importantly, Oregon has no sales tax, has low corporate taxes, and relies mostly on personal income tax - making it more appealing to a number of high-profile businesses (citation).")
                                                    ))
                                    ),
                                    
                                    
                                    #last row 
                                    fluidRow(style = "margin: 6px",
                                             width = 12, 
                                             column(12, align = "left",
                                                    wellPanel(align = "left",
                                                              strong("Racial Segregation in IA, OR and VA"),
                                                              br(),
                                                              br("Segregation refers to the systematic separation of people from different racial groups and socio-economic backgrounds into different neighborhoods. While racial segregation in the US originated with Jim Crow laws implemented in the South, the practice spread to the rest of the US via both local and federal policies as African Americans fled the South during the Great Migration of 1910-1970 (Rothstein 2017). For example, redlining, racially restrictive covenants, urban renewal, economic disinvestment of inner cities, and gentrification have all impacted where communities of color have been able to live and work throughout the 20th century (Charles 2003; Massey and Denton 1993; Rothstein 2017). While some models do suggest that most Asian, Hispanic, and Black residents are willing to integrate, a number of factors tied to housing market discrimination, including resistence from white home owners reinforce the structure of racially segregated neighborhoods across the country (Charles 2003). Segregation hinders economic mobility for black residents, in particular, because these segregated areas also tend to have higher levels of concentrated poverty, leading to higher rates of joblessness, out-of-wedlock births, school dropouts, crime, lower wages, and poorer health outcomes (Cutler & Glaeser 1997; Jargowsky 1996; Krivo & Peterson 1996; Massey & Denton 1993; Williams and Collins 2017; Wilson 1987)."),
                                                              br("As a result of these historical factors, most major cities in the US, including in Iowa, Oregon and Virginia, are segregated in some form today. Iowa and Oregon have relatively similar histories in terms of racial segregation. Although slavery was never legal in either state, both Iowa (90%) and Oregon (87%) are predominantly white (ACS 2019) and have only seen a 2-3% increase in their black populations since 1900 (Source). While racial exclusion laws were repealed over 150 years ago in Iowa and nearly 100 years ago in Oregon (cites), redlining and restrictive covenants restricted black occupants in both states throughout much of the mid-to-late-20th century (Landeck 2019; Mapping Inequality 2018; Smith 2018). More recently, Oregon (+_%) and Iowa (+_%) have seen a rise in the number of Hispanic residents (ACS 2019). In contrast, Virginia’s proximity to the South and its historical role in the Transatlantic Slave Trade means that the state has historically had a higher number of black residents (Kendi 2017). While that total is still 19.9% today (ACS 2019), there has been a ~16% decrease in the block population since 1900 (Source).")
                                                    ), a(href = "https://biocomplexity.virginia.edu/social-decision-analytics", img(src = "uva.png", width = "60%", style = "text-align: center; border: solid 1px white;"), target = "_blank")
                                                    ))
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
