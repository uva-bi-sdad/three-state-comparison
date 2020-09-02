# install.packages("dplyr")
# install.packages("shinythemes")
# install.packages("readr")
# install.packages("ggthemes")
#install.packages("plotly")
#install.packages("reshape")

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
library(reshape)



#install.packages("dplyr")
#
# Load data ---------------------------------------------
#
#previous
# data_med <- read_rds("~/Git/gates/rivanna_data/working/dashboard/data_med.Rds")
# data_work <- read_rds("~/Git/gates/rivanna_data/working/dashboard/data_work.Rds")
# data_edu <- read_rds("~/Git/gates/rivanna_data/working/dashboard/data_edu.Rds")


#cm_load data
# data_med <- read_rds("~/EM/gates/rivanna_data/working/dashboard/data_med.Rds")
# data_work <- read_rds("~/EM/gates/rivanna_data/working/dashboard/data_work.Rds")
#data_edu <- read_rds("~/EM/gates/rivanna_data/working/dashboard/data_edu.Rds")
#data_pol <- read_rds("~/EM/gates/src/dashboard/polcap4/data_pol.rds")
data_pol <- read_rds("data_pol.rds")
#names(data_pol)[1] <- "name"

##add quintiles
# data_pol$votepartQuint <- ntile(data_pol$votepart, 5)
# data_pol$num1000Quint <- ntile(data_pol$num1000, 5)

#data_pol <- data_pol %>% select("name", "X1","state","county","amt",
                                # "num","pop","IRR2010","totalvote","amt1000",
                                # "num1000","votepart","assn2014","votepartQuint","num1000Quint",
                                # "STATEFP","GEOID",  "geometry" )           
                                



# data_pol <- data_edu %>% select("STATEFP","GEOID", "name", "geometry"  )
# names(data_pol)[3] <- "Geographic Area Name"
# 
# pol_cap <- read_csv("~/EM/gates/rivanna_data/pol_cap.csv")
# 
# data_pol <- merge(data_pol, pol_cap, by="Geographic Area Name", all.x=TRUE)
# names(data_pol)[1] <- "name"

#info from organizations
#from https://aese.psu.edu/nercrd/community/social-capital-resources
# social_capital_variables_spreadsheet_for_2014 <- read_csv("~/EM/gates/rivanna_data/social-capital-variables-spreadsheet-for-2014.csv")
# names(social_capital_variables_spreadsheet_for_2014)[2] <- "name"
# library(tidyr)
# social_capital_variables_spreadsheet_for_2014 <- separate(social_capital_variables_spreadsheet_for_2014, col="County", sep = "County" , into = c("county"), remove = FALSE)
# 
# data_pol <- merge(data_pol, social_capital_variables_spreadsheet_for_2014 %>% select(state, name, assn2014), by= c("state", "name") , all.x= TRUE  )

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
                         column(10, align = "center", h2(strong("Political Capital: County Explorer"))),
                         column(1)
                ), 
                
                fluidRow(width = 12,
                         column(1),
                         column(10, 
                                p(),
                                br(tags$b('This dashboard offers a visualization of the political capital in communities.'), 
                                   ('Political capital represents the social infrastructure that connects participation and representation of individuals in the decision 
affecting the community. This dashboard is intended to facilitate comparisons between three states (Oregon, Iowa and Virginia) and its counties, with an additional feature 
to identify the political-legal environment and the extent of participation considering aspects of rurality.  
                                  
                                    The dashboard is intended also to allow extension professionals, students and policy-makers in Iowa, Oregon, and Virginia to help informing decisions and provide a useful resource to
                                     strengthen the vitality of communities.')),
                                p()
                         ),
                         column(1)
                ),
                
                
                
                fluidRow(style = "margin: 6px; padding-top:20px; padding-bottom:20px;",
                         tabsetPanel(
                           tabPanel(title = "Political capitals",
                                    
                                    fluidRow(style = "margin: 6px",
                                             width = 12, 
                                             column(12, align = "left", 
                                                    h3(strong("Participation and representation Maps")),
                                                    p(),
                                                    p(strong("Select a state using the map selector."), 
                                                      "The resulting maps will display the strength of participation and representation at the county-level.",
                                                      "Select the Data and Methods tab to learn more about how we constructed our composite measures."))
                                    ),
                                    
                                   
                                    
                                    
                                    fluidRow(style = "margin: 6px",
                                             column(width = 3,
                                                    h4(strong("Selector"), align = "left"),
                                                    p(),
                                                    selectInput("whichtopic", "Topic",
                                                                choices = list("Political capital"
                                                                               # ,
                                                                               # "Remote work",
                                                                               # "Telemental health"
                                                                               )
                                                                ,
                                                                selected = "Political capital"),
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
                                                    leafletOutput("mainplot2", width = "100%")
                                             )
                                    ),
                    
                                    ### maybe for later use-comparison of 3 states
                                    #new panel 3 states
                                    # fluidRow(style = "margin: 12px",
                                    #          column( width = 4,
                                    #                  h2(strong("Oregon"), align="center"),
                                    #                  p()
                                    #                  
                                    #          )
                                    #          ,
                                    # 
                                    #          column( width = 4,
                                    #                  h2(strong("Iowa"), align="center")
                                    #                ,
                                    #                  p()
                                    #                  # leafletOutput("mainplot", width = "100%")
                                    #          ),
                                    # 
                                    #          column( width = 4,
                                    #                  h2(strong("Virginia"), align="center")
                                    #                  ,
                                    #                  p()
                                    #                  # leafletOutput("mainplot", width = "100%")
                                    #          )
                                    # 
                                    # 
                                    # ),
                                    ############### end 
                                    
                                    
                                    fluidRow(style = "margin: 6px",
                                             width = 12, 
                                             column(12, align = "left", 
                                                    h3(strong("Explore Ranking Indicators")),
                                                    p(),
                                                    p(strong("Click on the tabs to explore the individual indicators that were used to 
                                                             construct the selected Political capital."), "Each tab displays a 
                                                      box plot with descriptive statistics and a state map at county level for an individual indicator. 
                                                      The selection of indicators will update depending on the topic selected."))
                                    ),
                                    
                                    # conditionalPanel("input.whichtopic == 'Remote work'",
                                    #                  fluidRow(style = "margin: 6px",
                                    #                           tabsetPanel(
                                    #                             tabPanel(title = "Households without broadband", 
                                    #                                      br(),
                                    #                                      h4(strong("Indicator: County-Level Percent Households Without Broadband Internet")),
                                    #                                      p(),
                                    #                                      p("The box plot and map below provide a descriptive summary and visualization of data distribution for the selected indicator.
                                    #                                        Data come from the American Community Survey 2014/18 (5-year) estimates,", 
                                    #                                        a(href = 'https://factfinder.census.gov/faces/affhelp/jsf/pages/metadata.xhtml?lang=en&type=table&id=table.en.ACS_17_5YR_B28002', 'Table B28002, Presence and types of internet subscriptions in household.', target = "_blank")),
                                    #                                      column(6, align = "left",
                                    #                                             p(),
                                    #                                             strong("County-Level Box Plot"),
                                    #                                             p(),
                                    #                                             plotlyOutput("plotly_noint_work", width = "100%"),
                                    #                                             p()),
                                    #                                      column(6, align = "left",
                                    #                                             p(),
                                    #                                             strong("County-Level Map"),
                                    #                                             p(),
                                    #                                             leafletOutput("subplot_noint_work", width = "100%"))
                                    #                             ),
                                    #                             tabPanel(title = "Workers without computer", 
                                    #                                      br(),
                                    #                                      h4(strong("Indicator: County-Level Percent Population in Labor Force Without a Computer")),
                                    #                                      p("The box plot and map below provide a descriptive summary and visualization of data distribution for the selected indicator.
                                    #                                        Data come from the American Community Survey 2014/18 (5-year) estimates,", 
                                    #                                        a(href = 'https://data.census.gov/cedsci/table?q=computer&tid=ACSDT5Y2018.B28007&t=Telephone,%20Computer,%20and%20Internet%20Access&vintage=2018&d=ACS%205-Year%20Estimates%20Detailed%20Tables', 'Table B28007, Labor force status by presence of a computer and types of internet subscription in household.', target = "_blank")),
                                    #                                      column(6, align = "left",
                                    #                                             p(),
                                    #                                             strong("County-Level Box Plot"),
                                    #                                             p(),
                                    #                                             plotlyOutput("plotly_nocomp_work", width = "100%"),
                                    #                                             p()),
                                    #                                      column(6, align = "left",
                                    #                                             p(),
                                    #                                             strong("County-Level Map"),
                                    #                                             p(),
                                    #                                             leafletOutput("subplot_nocomp_work", width = "100%"))
                                    #                             ),
                                    #                             tabPanel(title = "Non-remote workers in remote unfriendly occupations", 
                                    #                                      br(),
                                    #                                      h4(strong("Indicator: County-Level Percent Population in Labor Force not Working Remotely Employed in Remote Unfriendly Occupations")),
                                    #                                      p("The box plot and map below provide a descriptive summary and visualization of data distribution for the selected indicator.
                                    #                                        Data come from the American Community Survey 2014/18 (5-year) estimates,", 
                                    #                                        a(href = 'https://data.census.gov/cedsci/table?q=B08124%3A%20MEANS%20OF%20TRANSPORTATION%20TO%20WORK%20BY%20OCCUPATION&hidePreview=true&tid=ACSDT5Y2018.B08124', 'Table B08124, Means of transportation to work by occupation.', target = "_blank")),
                                    #                                      column(6, align = "left",
                                    #                                             p(),
                                    #                                             strong("County-Level Box Plot"),
                                    #                                             p(),
                                    #                                             plotlyOutput("plotly_occup", width = "100%"),
                                    #                                             p()),
                                    #                                      column(6, align = "left",
                                    #                                             p(),
                                    #                                             strong("County-Level Map"),
                                    #                                             p(),
                                    #                                             leafletOutput("subplot_occup", width = "100%"))
                                    #                             ),
                                    #                             tabPanel(title = "Non-remote workers in remote unfriendly industries", 
                                    #                                      br(),
                                    #                                      h4(strong("Indicator: County-Level Percent Population in Labor Force not Working Remotely Employed in Remote Unfriendly Industries")),
                                    #                                      p("The box plot and map below provide a descriptive summary and visualization of data distribution for the selected indicator.
                                    #                                        Data come from the American Community Survey 2014/18 (5-year) estimates,", 
                                    #                                        a(href = 'https://data.census.gov/cedsci/table?q=B08126%3A%20MEANS%20OF%20TRANSPORTATION%20TO%20WORK%20BY%20INDUSTRY&hidePreview=true&tid=ACSDT5Y2018.B08126&vintage=2018', 'Table B08126, Means of transportation to work by industry.', target = "_blank")),
                                    #                                      column(6, align = "left",
                                    #                                             p(),
                                    #                                             strong("County-Level Box Plot"),
                                    #                                             p(),
                                    #                                             plotlyOutput("plotly_industr", width = "100%"),
                                    #                                             p()),
                                    #                                      column(6, align = "left",
                                    #                                             p(),
                                    #                                             strong("County-Level Map"),
                                    #                                             p(),
                                    #                                             leafletOutput("subplot_industr", width = "100%"))
                                    #                             )
                                    #                           )
                                    #                  )
                                    # ),
                                    
                                    conditionalPanel("input.whichtopic == 'Political capital'",
                                                     fluidRow(style = "margin: 6px",
                                                              tabsetPanel(
                                                                tabPanel(title = "Representation - Voting", 
                                                                         br(),
                                                                         h4(strong("Indicator: County-Level proportion of votes")),
                                                                         p("The box plot and map below provide a descriptive summary and visualization of data distribution for the selected indicator."),
                                                                           # Data come from the American Community Survey 2014/18 (5-year) estimates,",
                                                                           # a(href = 'https://factfinder.census.gov/faces/affhelp/jsf/pages/metadata.xhtml?lang=en&type=table&id=table.en.ACS_17_5YR_B28002', 'Table B28002, Presence and types of internet subscriptions in household.', target = "_blank")),
                                                                         column(6, align = "left",
                                                                                p(),
                                                                                strong("County-Level Box Plot"),
                                                                                p(),
                                                                                plotlyOutput("plotly_partic_pol", width = "100%"),
                                                                                p()),
                                                                         column(6, align = "left",
                                                                                p(),
                                                                                strong("County-Level Map"),
                                                                                p(),
                                                                                leafletOutput("subplot_partic_pol", width = "100%"))
                                                                ),
                                                                tabPanel(title = "Participation - Organizations", 
                                                                         br(),
                                                                         h4(strong("Indicator: County-Level Number of organizations per 1000 people. Taken from County-level measure of Social Capital (Rupasingha, Anil and Stephan J. Goetz. 2008)")),
                                                                         p("The box plot and map below provide a descriptive summary and visualization of data distribution for the selected indicator."),
                                                                           # Data come from the American Community Survey 2014/18 (5-year) estimates,", 
                                                                           # a(href = 'https://data.census.gov/cedsci/table?q=computer&tid=ACSDT5Y2018.B28005&t=Telephone,%20Computer,%20and%20Internet%20Access&vintage=2018&d=ACS%205-Year%20Estimates%20Detailed%20Tables', 'Table B28005, Age by presence of a computer and types of internet subscription in household.', target = "_blank")),
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
                                                                ),
                                                                tabPanel(title = "Contributions", 
                                                                         br(),
                                                                         h4(strong("Indicator: County-Level Number of individuals contributing to campaigns.")),
                                                                         p("The box plot and map below provide a descriptive summary and visualization of data distribution for the selected indicator."),
                                                                           # Data come from the American Community Survey 2014/18 (5-year) estimates,", 
                                                                           # a(href = 'https://data.census.gov/cedsci/table?q=B14001%3A%20SCHOOL%20ENROLLMENT%20BY%20LEVEL%20OF%20SCHOOL%20FOR%20THE%20POPULATION%203%20YEARS%20AND%20OVER&hidePreview=true&tid=ACSDT5Y2018.B14001&vintage=2018', 'Table B14001,	School enrollment by level of school for the population 3 years and over.', target = "_blank")),
                                                                         column(6, align = "left",
                                                                                p(),
                                                                                strong("County-Level Box Plot"),
                                                                                p(),
                                                                                plotlyOutput("plotly_ink12", width = "100%"),
                                                                                p()),
                                                                         column(6, align = "left",
                                                                                p(),
                                                                                strong("County-Level Map"),
                                                                                p(),
                                                                                leafletOutput("subplot_ink12", width = "100%"))
                                                                )
                                                              )
                                                     )
                                    ),
                                    
                                    # conditionalPanel("input.whichtopic == 'Telemental health'",
                                    #                  fluidRow(style = "margin: 6px",
                                    #                           tabsetPanel(
                                    #                             tabPanel(title = "Households without internet", 
                                    #                                      br(),
                                    #                                      h4(strong("Indicator: County-Level Percent Households Without Internet Access")),
                                    #                                      p("The box plot and map below provide a descriptive summary and visualization of data distribution for the selected indicator.
                                    #                                        Data come from the American Community Survey 2014/18 (5-year) estimates,", 
                                    #                                        a(href = 'https://factfinder.census.gov/faces/affhelp/jsf/pages/metadata.xhtml?lang=en&type=table&id=table.en.ACS_17_5YR_B28002', 'Table B28002, Presence and types of internet subscriptions in household.', target = "_blank")),
                                    #                                      column(6, align = "left",
                                    #                                             p(),
                                    #                                             strong("County-Level Box Plot"),
                                    #                                             p(),
                                    #                                             plotlyOutput("plotly_noint_med", width = "100%"),
                                    #                                             p()),
                                    #                                      column(6, align = "left",
                                    #                                             p(),
                                    #                                             strong("County-Level Map"),
                                    #                                             p(),
                                    #                                             leafletOutput("subplot_noint_med", width = "100%"))
                                    #                             ),
                                    #                             tabPanel(title = "Households without computer", 
                                    #                                      br(),
                                    #                                      h4(strong("Indicator: County-Level Percent Households Without a Computer")),
                                    #                                      p("The box plot and map below provide a descriptive summary and visualization of data distribution for the selected indicator.
                                    #                                        Data come from the American Community Survey 2014/18 (5-year) estimates,", 
                                    #                                        a(href = 'https://data.census.gov/cedsci/table?q=computer&tid=ACSDT5Y2018.B28003&t=Telephone,%20Computer,%20and%20Internet%20Access&vintage=2018&d=ACS%205-Year%20Estimates%20Detailed%20Tables', 'Table B28003, Presence of a computer and type of internet subscription in household.', target = "_blank")),
                                    #                                      column(6, align = "left",
                                    #                                             p(),
                                    #                                             strong("County-Level Box Plot"),
                                    #                                             p(),
                                    #                                             plotlyOutput("plotly_nocomp_med", width = "100%"),
                                    #                                             p()),
                                    #                                      column(6, align = "left",
                                    #                                             p(),
                                    #                                             strong("County-Level Map"),
                                    #                                             p(),
                                    #                                             leafletOutput("subplot_nocomp_med", width = "100%"))
                                    #                             ),
                                    #                             tabPanel(title = "Population uninsured", 
                                    #                                      br(),
                                    #                                      h4(strong("Indicator: County-Level Percent Population Without Health Insurance")),
                                    #                                      p("The box plot and map below provide a descriptive summary and visualization of data distribution for the selected indicator.
                                    #                                        Data come from the American Community Survey 2014/18 (5-year) estimates,", 
                                    #                                        a(href = 'https://data.census.gov/cedsci/table?q=coverage&tid=ACSDT5Y2018.B27020&d=ACS%205-Year%20Estimates%20Detailed%20Tables&vintage=2018', 'Table B27020, Health insurance coverage status and type by citizenship status.', target = "_blank")),
                                    #                                      column(6, align = "left",
                                    #                                             p(),
                                    #                                             strong("County-Level Box Plot"),
                                    #                                             p(),
                                    #                                             plotlyOutput("plotly_unins", width = "100%"),
                                    #                                             p()),
                                    #                                      column(6, align = "left",
                                    #                                             p(),
                                    #                                             strong("County-Level Map"),
                                    #                                             p(),
                                    #                                             leafletOutput("subplot_unins", width = "100%"))
                                    #                             ),
                                    #                             tabPanel(title = "Poor mental health days", 
                                    #                                      br(),
                                    #                                      h4(strong("Indicator: County-Level Average Number of Poor Mental Health Days in Past Month")),
                                    #                                      p("The box plot and map below provide a descriptive summary and visualization of data distribution for the selected indicator.
                                    #                                        Data come from the Behavioral Risk Factor Surveillance System 2017 via CountyHealthRankings,", 
                                    #                                        a(href = 'https://www.countyhealthrankings.org/explore-health-rankings/measures-data-sources/county-health-rankings-model/health-outcomes/quality-of-life/poor-mental-health-days', 'Average number of mentally unhealthy days reported in past 30 days (age-adjusted).', target = "_blank")),
                                    #                                      column(6, align = "left",
                                    #                                             p(),
                                    #                                             strong("County-Level Box Plot"),
                                    #                                             p(),
                                    #                                             plotlyOutput("plotly_menthdays", width = "100%"),
                                    #                                             p()),
                                    #                                      column(6, align = "left",
                                    #                                             p(),
                                    #                                             strong("County-Level Map"),
                                    #                                             p(),
                                    #                                             leafletOutput("subplot_poorment", width = "100%"))
                                    #                             ),
                                    #                             tabPanel(title = "Mental health providers", 
                                    #                                      br(),
                                    #                                      h4(strong("Indicator: County-Level Number of Mental Health Providers per 100,000 Population")),
                                    #                                      p("The box plot and map below provide a descriptive summary and visualization of data distribution for the selected indicator.
                                    #                    Data come from the CMS National Provider Identification 2019 via CountyHealthRankings,", 
                                    #                                        a(href = 'https://www.countyhealthrankings.org/explore-health-rankings/measures-data-sources/county-health-rankings-model/health-factors/clinical-care/access-to-care/mental-health-providers', 'Number of mental health providers per 100,000 population.', target = "_blank")),
                                    #                                      column(6, align = "left",
                                    #                                             p(),
                                    #                                             strong("County-Level Box Plot"),
                                    #                                             p(),
                                    #                                             plotlyOutput("plotly_menthprov", width = "100%"),
                                    #                                             p()),
                                    #                                      column(6, align = "left",
                                    #                                             p(),
                                    #                                             strong("County-Level Map"),
                                    #                                             p(),
                                    #                                             leafletOutput("subplot_healthprov", width = "100%"))
                                    #                             )
                                    #                           )
                                    #                  )
                                    # ),
                                    
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
                                             width = 8, 
                                             br(),
                                             br(),
                                             strong('Political Capital Definition'), 
                                             p(),
                                             #em('Description.'),
                                             br(), 
                                          
                                             #tags$b('The remote work relative accessibility measure highlights counties where residents may have difficulty working remotely if instructed to do so.'),
                                             ('As defined by Flora, Flora, and Gasteyer in the 5th edition of their book “Rural Communities: Legacy + Change” Political Capital is:  '),
                                             br(),
                                             br(),
                                             ('“… a group’s ability to influence the distribution of resources within a social unit, including helping set the agenda of what resources are available. … 
                                              Political capital consists of organization, connections, voice, and power as citizens turn shared norms and values into standards that are codified into rules, 
                                              regulations, and resources distributions that are enforced.” (page 184)  '),
                                             br(),
                                             br(),
                                             ('Communities can leverage political capital to make change when their norms and values are not reflected in policies that govern them. In keeping with the 
                                              Community Capitals Framework of asset mapping1, we have created a policy asset map for the domains of education, taxation, employment, voting, law enforcement, 
                                              and housing/zoning with a focus on policies that have the potential to impact economic mobility. By identifying those policies that can impede the economic mobility
                                              a community can better strategize for effective change.  ')
                                    ),                         
                                               
                                    fluidRow(style = "margin: 6px",
                                             width = 12, 
                                             column(12, align = "left", 
                                                    h3(strong("Data, Measures, and Methods")),
                                                    ##
                                                    p()
                                             )
                                    ),
                                    
                                    column(4, wellPanel(strong('Representation'), 
                                                        p(),
                                                        em('Description.'),
                                                        br(), tags$b('The representation indicates the extent of political engagement of the community. This meausure intends to capture the desire of political presence in the community 
                                                            through voting and it is reflected by the voter turnout rate.'),
                                                #         ('It conceptualizes four telecommunication infrastructure and employment characteristics as potential barriers, providing
                                                # a relative ranking of county telework preparedness.'),
                                                        p(),
                                                        em('How We Measure Representation'),
                                                        br('Representation is indicated by the voter turnout, given by the proportiont of votes divided by the population:'),
                                                       #  tags$li('Households with no broadband internet subscription.'),
                                                       #  tags$li('Persons in labor force with no computer available.'),
                                                       #  tags$li('Persons who are not currently working remotely and are employed in telework unfriendly occupations
                                                       # (service, natural, construction, maintenance, production, transportation, material moving, and military specific occupations).'),
                                                       #  tags$li('Persons who are not currently working remotely and are employed in telework unfriendly industries 
                                                       # (construction, manufacturing, wholesale, retail, transportation and warehousing, utilities, and government, including armed forces).'),
                                                        br('Quintile cut-offs are also estimated for each indicator. County placement in a higher quintile indicates higher participation. Additionally, a box plot is included to identify the communities '),
                                                        tags$li('Very high: 0 indicators.'),
                                                        # tags$li('High: 1 indicator.'),
                                                        # tags$li('Medium: 2 indicators.'), 
                                                        # tags$li('Low: 3 indicators.'),
                                                        # tags$li('Very low: all 4 indicators.'),
                                                        p(),
                                                        em('Data source.'),
                                                        p(a(href = 'http://uselectionatlas.org', 'Dave Leip’s Atlas of U.S. Presidential Elections.', target = "_blank"), "Accessed through University of Virginia Library.")
                                    )),                         
                                    column(4, wellPanel(strong('Participation'), 
                                                        p(),
                                                        em('Description.'),
                                                        br(), tags$b('The variable for participation tries to acknowledge the institutionalized organization of communities.  '), 
                                                        ('This variables was considered to represent the social fabric of organizations within counties to recognized the level of activism under different levels.  
                                                         For instance, a community with a large variety of organizations such as the Lions Club, the Rotary Club, Boys Scout Association, minority s groups, support for the elderly,
                                                         organizations of volunteers, Red Cross, etc., largely benefit from the activities of its members and it builds a healthy social fabric that is capable to construct civic 
                                                         behavior.'),
                                                        p(),
                                                        em('How We Measure Participation.'),
                                                        br('Ideally, we tried to register hours of participation in activities that are organized by different institutions in communities (counties). We used the number of establishments in 
                                                          organizations captured by the social capital index that is estimated by Penn State University. This variables include a broad range of organizations, including religious, civic, business, 
                                                          political, professional, labor, bowling centers, fitness and recreational sports centers, country clubs and sports clubs.'),
                                                        # tags$li('Households with no internet access subscription.'),
                                                        # tags$li('Population under age 18 without a computer.'),
                                                        # tags$li('Population enrolled in K-12.'),
                                                  #       br('We compute quintile cut-offs for each indicator. County placement in a higher quintile indicates lower relative accessibility.
                                                  # We assign descriptive labels for county relative accessibility based on whether they placed in 4th or 5th quintile a certain number of times.
                                                  # The more times a county places in the 4th or 5th quintile on relevant indicators, the lower its relative remote education accessibility:'),
                                                  #       tags$li('High: 0 indicators.'),
                                                  #       tags$li('Medium: 1 indicator.'),
                                                  #       tags$li('Low: 2 indicators.'),
                                                  #       tags$li('Very low: all 3 indicators.'),
                                                        p(),
                                                        em('Data source.'),
                                                        p(a(href = 'https://aese.psu.edu/nercrd/community/social-capital-resources', 'County-level measure of social capital. ', target = "_blank"), "2014 estimates.")
                                    )),
                                    column(4, wellPanel(strong('Contribution'), 
                                                        p(),
                                                        em('Description.'),
                                                        br(), tags$b('Contribution is a measure of the direct political support for candidates.'), ('It considers the number of individuals that financially contribute to 
                                                                                                                                                    campaigns. In the initial exercise, the measure includes contributors for the 2020 presidential campaign.'),
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
                                    )),
                                    
                                    fluidRow(
                                      style = "margin: 1px",
                                      width = 10 ,align="center",
                                      h3(strong("Index of Political Capital")),
                                      
                                     
                                              # plotlyOutput("plotly_index3d", width = "100%",  height = "auto") ),
                                    plotlyOutput("plotly_index3d", width = "auto" ,  height = "auto") ),
                                    
                                    
                                    fluidRow(
                                      style = "margin: 6px",
                                      width = 12,
                                      
                                      column(6, wellPanel(strong('Index'), 
                                                          p(),
                                                          #em('Description.'),
                                                          br('The index of political capital is comprised by the three indicators mentioned above. 
                                                                       In order to combine the measurements, the index first standarized every variable so that, the data is mapped within a range
                                                                       between 0 and 1, that is, the variable is transformed using the following expression:'),
                                                          #         ('It conceptualizes four telecommunication infrastructure and employment characteristics as potential barriers, providing
                                                          p(
                                                            withMathJax(
                                                              #"$$y_i = \\frac{\\bar{x}_1 - \\bar{x}_2}{\\sqrt{\\frac{s_1^2}{N_1}+\\frac{s_2^2}{N_2}}}$$"
                                                              "$$y_i = \\frac{x_i - \\min{(x_i)}}{\\max{(x_i)}-\\min{(x_i)}}$$"
                                                             
                                                            )),
                                                          p(),
                                                  
                                                           br('where x represents the variable for political capital with participation (i=1), representation (i=2) and contribution (i=3).'),
                                                         #  withMathJax(),
                                                         # helpText('where x represents the variable for political capital with participation, representation or contribution $\\sqrt{2}$ '),
                                                          
                                                           
                                                          p(),
                                                          br('2. With the standarized variable, we proceed to estimate the distance with respect to the origin. 
                                                             This is just the correspondence to the so called euclidian distance.'),
                                                          p(
                                                            withMathJax(
                                                              #"$$t = \\frac{\\bar{x}_1 - \\bar{x}_2}{\\sqrt{\\frac{s_1^2}{N_1}+\\frac{s_2^2}{N_2}}}$$"
                                                              "$$index = \\sqrt{{y}_1^2 + {y}_2^2 +{y}_3^2} $$"
                                                              
                                                            ))
                                                          
                                                        
                                                          
                                      )),                         
                                      
                                      column(6, wellPanel(strong('Interpretation of the index'), 
                                                          p(),
                                                          #em('Description.'),
                                                          br('Every county is reprented by a point in the above figure, according to three different dimensions. 
The index corresponds to the graphical distance between the point (county) and the origin.  That is, the furthest point represents Fairfaix city, VA, where 50.7% of population votes, 
there are 2.9 organizations per every 1000 inhabitants and there is approximately $19 of contributions to politicall purposes per every 1000 people. The index reveals that Fairfax county
possesses a relatively higher political capital, where individuals participate, organize and represent themselves more tha, for instance, Marion County in Oregon, where only 39.6% of the 
population votes, there is only 1 organization per every 1000 people and where the same 1000 people contribute almost $1.45 dollars for political purposes.'),
                                                          br('The index and the variables used are proxies to political participation and representation, and they may be incomplete measures of
                                                             political capital, but they may be a starting point for approximations of power relationships within communities.')
                                                    
                                                          
                                                          
                                      ))                         
                                      
                                      
                                      # column(4, align = "left", 
                                      #        
                                      #          h4(strong("Explanation of the index")),
                                      #          ##
                                      #          p()
                                      # )
                                      
                                      
                                    )
                           ),
                           
                           # tabPanel(title = "More on Connectivity",
                           #          
                           #          fluidRow(style = "margin: 6px",
                           #                   width = 12,
                           #                   column(12,
                           #                          h3(strong("More on Internet Connectivity")),
                           #                          p()
                           #                   )
                           #          ),
                           #          
                           #          fluidRow(style = "margin: 6px",
                           #                   width = 12,
                           #                   column(8,
                           #                          wellPanel(
                           #                            p(),
                           #                            p(tags$b("Internet connectivity is a crucial factor in our relative accessibility measures."), 
                           #                              "Internet at broadband speeds—defined as 25mbps download and 3mbps upload—is still not available 
                           #                               to many Americans. However, estimates differ on how many individuals are without access to broadband and thus are limited in their ability to participate in today’s increasingly online world. 
                           #                               To better understand US broadband coverage and where estimates disagree, our related project examines three publicly available broadband data sources: the Federal Communications Commission (FCC) data, 
                           #                               American Community Survey (ACS) data, and Microsoft (MS) Airband Initiative data. We aim to understand the extent of coverage according to each dataset, to examine discrepancies between the ACS and 
                           #                               Microsoft with FCC data as the source used for policy and funding decision-making, and to address these aims with particular attention to rural areas."),
                           #                            p("We invite you to explore our", a(href = 'https://bband.policy-analytics.net', 'broadband internet data source comparison dashboard.', target = "_blank"), "The dashboard visualizes discrepancies between FCC-reported broadband availability and ACS-reported broadband subscription at census block 
                           #                              group and census tract levels, and the discrepancies between FCC-reported broadband availability and MS-reported broadband usage at the county level. Maps, details about our data sources, and measure descriptions are available on the dashboard website.")
                           #                          )
                           #                   ),
                           #                   column(4,
                           #                          a(href = "http://bband.policy-analytics.net", img(src = "bb1.png", width = "100%", style = "text-align: left; border: solid 1px lightgrey;"), target = "_blank")
                           #                   )
                           #          )
                           # ),
                           
                           
                           
                           tabPanel(title = "Legislation assets of States",
                                    
                                    fluidRow(style = "margin: 6px",
                                             width = 12, 
                                             column(12, align = "left", 
                                                    h3(strong("Domains of Legislation"))
                                             )
                                    ),
                                    fluidRow(style = "margin: 6px",
                                             width = 12, 
                                    column(10, align = "left",
                                           wellPanel(
                                             strong("Introduction"),
                                             br(),
                                             # br("We used the quintile method described by the", a(href = "https://pcrd.purdue.edu/blog/remote-work-and-the-coronavirus.php", "Purdue Center for Regional Development"), 
                                             #    "to construct our relative accessibility measures. One of our indicators, remote work accessibility, is also an adaptation of Purdue's remote work metric. 
                                             #   We depart from their operationalization in using American Community Survey instead of Federal Communications Commission (FCC) data given", a(href = "https://bband.policy-analytics.net", "known issues with FCC Form 477"),
                                             #    ", as well as in tailoring indicators used to the population in the labor force and to the labor force population that reports not already working from home.")
                                             # br("We used the quintile method described by the", a(href = "https://pcrd.purdue.edu/blog/remote-work-and-the-coronavirus.php", "Purdue Center for Regional Development"), 
                                             #    "to construct our relative accessibility measures. One of our indicators, remote work accessibility, is also an adaptation of Purdue's remote work metric. 
                                             #   We depart from their operationalization in using American Community Survey instead of Federal Communications Commission (FCC) data given", a(href = "https://bband.policy-analytics.net", "known issues with FCC Form 477"),
                                             #    ", as well as in tailoring indicators used to the population in the labor force and to the labor force population that reports not already working from home.")
                                             br("Political capital takes various forms of participation and representation of groups and individuals in the community.  This project seeks to summarize several aspects of political capital that largely affect economic 
                                                mobility of communities.  The information focuses on six domains that include the following: "),
                                             tags$li('Education'),
                                             tags$li('Employment'),
                                             tags$li('Policing and criminal justice  '),
                                             tags$li('Housing and zoning '),
                                             tags$li('Voting'),
                                             tags$li('Taxation'),
                                             br("Estimating a quantitative measure of the existence of political capital is a challenging task. We propose the quantification of the identified policy domains into an index. This index consists of sub-domains 
                                                (subject areas) which contain specific questions about existence of policies related to Economic Mobility. For instance, the Law Enforcement and Criminal Justice domain is explained with the following structure: "),
                                             br(),
                                             br(),
                                             strong("Scoring Methods:"),
                                             br(),
                                             br("After an extensive research effort, our team decided to score states with a composite indicator system to measure whether states have policies likely to advance economic mobility. We rooted our composite 
                                                indicator system in dummy variables to make use of qualitative information.  "),
                                             br("If the existence of a state’s policy exhibits potential to advance economic mobility, we assigned a value of 1. If the state lacks a particular law or regulation, we assign 0.  We assumed if a state does 
                                                not have laws or regulations, more variability could occur with respect to mobility and, consequently, improvement of socio-economic advancement may be delayed.  
                                                For instance, Student Discipline is one of the sub-domains identified within political capital of Education. We included multiple questions for Student Discipline, 
                                                such as: “Is there a ban on corporal punishment?” According to Brookings, students subject to corporal punishment performed worse than their peers in non-punitive 
                                                environments. If a state banned corporal punishment, we ranked the state with 1. If they did not ban corporal punishment, we ranked the state with 0.  "),
                                             br("Some subcategories have multiple questions. In order to standardize, we summed the scores across the policy questions within the subcategory by state, then divided by the number of questions in the subcategory. 
                                                A similar approach was used for subdomain scores. Our final index also has a similar approach.   The following table summarizes each domain of political capital and the number of subdomains, subcategories and policy questions.  ")
                                
                                                                        
                                           )
                                    )
                                    )
                                    ,
                                    
                                    ##new plot by domains
                                    fluidRow(
                                      style = "margin: 1px",
                                      width = 8 ,align="center",
                                      h3(strong("Legislation Environment"))
                                      ,

                                      plotlyOutput("plotly_domains", width = "auto" ,  height = "auto") )
                                    #)
                                    #end new sectors
                                    
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
                                             br("We used the quintile method described by the", a(href = "https://pcrd.purdue.edu/blog/remote-work-and-the-coronavirus.php", "Purdue Center for Regional Development"), 
                                                "to construct our relative accessibility measures. One of our indicators, remote work accessibility, is also an adaptation of Purdue's remote work metric. 
                                               We depart from their operationalization in using American Community Survey instead of Federal Communications Commission (FCC) data given", a(href = "https://bband.policy-analytics.net", "known issues with FCC Form 477"),
                                                ", as well as in tailoring indicators used to the population in the labor force and to the labor force population that reports not already working from home.")
                                           )
                                    ),
                                    column(6, align = "center",
                                           wellPanel(align = "left",
                                                     strong("Contact"),
                                                     br(),
                                                     br(a(href = "https://biocomplexity.virginia.edu/teja-pristavec", "All the pol capital family + DSPG")),
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

##################################################################################################################################################################
##################################################################################################################################################################
##################################################################################################################################################################
#
# Server ---------------------------------------------
#

server <- function(input, output) {
  
  #
  # Plotly boxplots ------------------------------------------
  # 
  
  # # Work: No computer
  # output$plotly_nocomp_work <- renderPlotly({
  #   if (input$whichtopic == "Remote work") {
  #     data <- data_work
  #     data <- switch(input$whichstate,
  #                    "Iowa" = data[data$STATEFP == "19", ],
  #                    "Oregon" = data[data$STATEFP == "41", ],
  #                    "Virginia" = data[data$STATEFP == "51", ])
  #     
  #     plot_ly(y = ~data$nocomputer, 
  #             x = "Percent population in labor force without a computer",
  #             showlegend = FALSE,
  #             hoverinfo = "y",
  #             type = "box",
  #             name = "") %>% 
  #       layout(title = "",
  #              xaxis = list(title = "",
  #                           zeroline = FALSE),
  #              yaxis = list(title = "Percent",
  #                           zeroline = FALSE,
  #                           hoverformat = ".2f"))
  #   }
  # })
  # 
  # # Work: No internet
  # output$plotly_noint_work <- renderPlotly({
  #   if (input$whichtopic == "Remote work") {
  #     data <- data_work
  #     data <- switch(input$whichstate,
  #                    "Iowa" = data[data$STATEFP == "19", ],
  #                    "Oregon" = data[data$STATEFP == "41", ],
  #                    "Virginia" = data[data$STATEFP == "51", ])
  #     
  #     plot_ly(y = ~data$nointernet, 
  #             x = "Percent households without broadband internet",
  #             showlegend = FALSE,
  #             hoverinfo = "y",
  #             type = "box",
  #             name = "") %>% 
  #       layout(title = "",
  #              xaxis = list(title = "",
  #                           zeroline = FALSE),
  #              yaxis = list(title = "Percent",
  #                           zeroline = FALSE,
  #                           hoverformat = ".2f"))
  #   }
  # })
  # 
  # # Work: Occupations
  # output$plotly_occup <- renderPlotly({
  #   if (input$whichtopic == "Remote work") {
  #     data <- data_work
  #     data <- switch(input$whichstate,
  #                    "Iowa" = data[data$STATEFP == "19", ],
  #                    "Oregon" = data[data$STATEFP == "41", ],
  #                    "Virginia" = data[data$STATEFP == "51", ])
  #     
  #     plot_ly(y = ~data$occup, 
  #             x = "Percent non-remote workers in non-remote friendly occupations",
  #             showlegend = FALSE,
  #             hoverinfo = "y",
  #             type = "box",
  #             name = "") %>% 
  #       layout(title = "",
  #              xaxis = list(title = "",
  #                           zeroline = FALSE),
  #              yaxis = list(title = "Percent",
  #                           zeroline = FALSE,
  #                           hoverformat = ".2f"))
  #   }
  # })
  # 
  # # Work: Industries
  # output$plotly_industr <- renderPlotly({
  #   if (input$whichtopic == "Remote work") {
  #     data <- data_work
  #     data <- switch(input$whichstate,
  #                    "Iowa" = data[data$STATEFP == "19", ],
  #                    "Oregon" = data[data$STATEFP == "41", ],
  #                    "Virginia" = data[data$STATEFP == "51", ])
  #     
  #     plot_ly(y = ~data$industr, 
  #             x = "Percent non-remote workers in non-remote friendly industries",
  #             showlegend = FALSE,
  #             hoverinfo = "y",
  #             type = "box",
  #             name = "") %>% 
  #       layout(title = "",
  #              xaxis = list(title = "",
  #                           zeroline = FALSE),
  #              yaxis = list(title = "Percent",
  #                           zeroline = FALSE,
  #                           hoverformat = ".2f"))
  #   }
  # })
  # 
  # # Med: No internet
  # output$plotly_noint_med <- renderPlotly({
  #   if (input$whichtopic == "Telemental health") {
  #     data <- data_med
  #     data <- switch(input$whichstate,
  #                    "Iowa" = data[data$STATEFP == "19", ],
  #                    "Oregon" = data[data$STATEFP == "41", ],
  #                    "Virginia" = data[data$STATEFP == "51", ])
  #     
  #     plot_ly(y = ~data$nointernet, 
  #             x = "Percent households without internet access",
  #             showlegend = FALSE,
  #             hoverinfo = "y",
  #             type = "box",
  #             name = "") %>% 
  #       layout(title = "",
  #              xaxis = list(title = "",
  #                           zeroline = FALSE),
  #              yaxis = list(title = "Percent",
  #                           zeroline = FALSE,
  #                           hoverformat = ".2f"))
  #   }
  # })
  # 
  # # Med: No computer
  # output$plotly_nocomp_med <- renderPlotly({
  #   if (input$whichtopic == "Telemental health") {
  #     data <- data_med
  #     data <- switch(input$whichstate,
  #                    "Iowa" = data[data$STATEFP == "19", ],
  #                    "Oregon" = data[data$STATEFP == "41", ],
  #                    "Virginia" = data[data$STATEFP == "51", ])
  #     
  #     plot_ly(y = ~data$nocomputer, 
  #             x = "Percent households without a computer",
  #             showlegend = FALSE,
  #             hoverinfo = "y",
  #             type = "box",
  #             name = "") %>% 
  #       layout(title = "",
  #              xaxis = list(title = "",
  #                           zeroline = FALSE),
  #              yaxis = list(title = "Percent",
  #                           zeroline = FALSE,
  #                           hoverformat = ".2f"))
  #   }
  # })
  # 
  # # Med: Uninsured
  # output$plotly_unins <- renderPlotly({
  #   if (input$whichtopic == "Telemental health") {
  #     data <- data_med
  #     data <- switch(input$whichstate,
  #                    "Iowa" = data[data$STATEFP == "19", ],
  #                    "Oregon" = data[data$STATEFP == "41", ],
  #                    "Virginia" = data[data$STATEFP == "51", ])
  #     
  #     plot_ly(y = ~data$pct_unins, 
  #             x = "Percent population without health insurance",
  #             showlegend = FALSE,
  #             hoverinfo = "y",
  #             type = "box",
  #             name = "") %>% 
  #       layout(title = "",
  #              xaxis = list(title = "",
  #                           zeroline = FALSE),
  #              yaxis = list(title = "Percent",
  #                           zeroline = FALSE,
  #                           hoverformat = ".2f"))
  #   }
  # })
  # 
  # # Med: Mental health days
  # output$plotly_menthdays <- renderPlotly({
  #   if (input$whichtopic == "Telemental health") {
  #     data <- data_med
  #     data <- switch(input$whichstate,
  #                    "Iowa" = data[data$STATEFP == "19", ],
  #                    "Oregon" = data[data$STATEFP == "41", ],
  #                    "Virginia" = data[data$STATEFP == "51", ])
  #     
  #     plot_ly(y = ~data$avgnum_poormenth, 
  #             x = "Average number of poor mental health days in past month",
  #             showlegend = FALSE,
  #             hoverinfo = "y",
  #             type = "box",
  #             name = "") %>% 
  #       layout(title = "",
  #              xaxis = list(title = "",
  #                           zeroline = FALSE),
  #              yaxis = list(title = "Percent",
  #                           zeroline = FALSE,
  #                           hoverformat = ".2f"))
  #   }
  # })
  # 
  # # Med: Providers
  # output$plotly_menthprov <- renderPlotly({
  #   if (input$whichtopic == "Telemental health") {
  #     data <- data_med
  #     data <- switch(input$whichstate,
  #                    "Iowa" = data[data$STATEFP == "19", ],
  #                    "Oregon" = data[data$STATEFP == "41", ],
  #                    "Virginia" = data[data$STATEFP == "51", ])
  #     
  #     plot_ly(y = ~data$menthprov_per100k, 
  #             x = "Number of mental health providers per 100,000 population",
  #             showlegend = FALSE,
  #             hoverinfo = "y",
  #             type = "box",
  #             name = "") %>% 
  #       layout(title = "",
  #              xaxis = list(title = "",
  #                           zeroline = FALSE),
  #              yaxis = list(title = "Percent",
  #                           zeroline = FALSE,
  #                           hoverformat = ".2f"))
  #   }
  # })
  
  # Pol: Participation
  output$plotly_partic_pol <- renderPlotly({
    if (input$whichtopic == "Political capital") {
      #data <- data_edu
      data <- data_pol
### AQUI STOP... A CONSTRUIR LA INFO      
      data <- switch(input$whichstate,
                     "Iowa" = data[data$STATEFP == "19", ],
                     "Oregon" = data[data$STATEFP == "41", ],
                     "Virginia" = data[data$STATEFP == "51", ])
      
      plot_ly(y = ~data$votepart, 
              x = "Voter turnout",
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
  
  # Edu: No computer / Organizations-Associations
  output$plotly_nocomp_edu <- renderPlotly({
    if (input$whichtopic == "Political capital") {
      #data <- data_edu
      data <- data_pol
      data <- switch(input$whichstate,
                     "Iowa" = data[data$STATEFP == "19", ],
                     "Oregon" = data[data$STATEFP == "41", ],
                     "Virginia" = data[data$STATEFP == "51", ])
      
      plot_ly(y = ~data$assn2014, 
              x = "Organizations per 1000 people",
              showlegend = FALSE,
              #hoverinfo = "y",
              type = "box",
              name = "",
              text= ~data$county) %>% 
        layout(title = "",
               xaxis = list(title = "",
                            zeroline = FALSE),
               yaxis = list(title = "Number per 1000 people",
                            zeroline = FALSE,
                            hoverformat = ".2f"))
    }
  })
  
  # In K-12 / contribution (num1000)
  output$plotly_ink12 <- renderPlotly({
    if (input$whichtopic == "Political capital") {
      #data <- data_edu
      data <- data_pol
      data <- switch(input$whichstate,
                     "Iowa" = data[data$STATEFP == "19", ],
                     "Oregon" = data[data$STATEFP == "41", ],
                     "Virginia" = data[data$STATEFP == "51", ])
      
      plot_ly(y = ~data$num1000, 
              x = "Contributors per 1000 people",
              showlegend = FALSE,
              #hoverinfo = "y",
              type = "box",
              name = "",
              text= ~data$county) %>% 
        layout(title = "",
               xaxis = list(title = "",
                            zeroline = FALSE),
               yaxis = list(title = "Dollars per 1000 people",
                            zeroline = FALSE,
                            hoverformat = ".2f"))
    }
  })
  
  #
  # Main plots ---------------------------------------------
  #
  
  output$mainplot <- renderLeaflet({
    
    # Main plot: remote education
    if (input$whichtopic == "Political capital") {
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
    } else if (input$whichtopic == "Political capital") {
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
      
      # Main plot: telehealth
    } else if (input$whichtopic == "Telemental health") {
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
  })
  
  #
  # Quintile tables ---------------------------------------------
  #
  
  # Remote education
  output$table_quint_edu <- renderTable({
    if (input$whichtopic == "Remote education") {
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
    if (input$whichtopic == "Telemental health") {
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
  
  ############################################# MAIN NEW PLOT
  
  output$mainplot2 <- renderLeaflet({
    if (input$whichtopic == "Political capital") {
      #data <- data_edu
      data <- data_pol
      data <- switch(input$whichstate,
                     "Iowa" = data[data$STATEFP == "19", ],
                     "Oregon" = data[data$STATEFP == "41", ],
                     "Virginia" = data[data$STATEFP == "51", ])
      
      pal <- colorQuantile("Oranges", domain = data$index, probs = seq(0, 1, length = 6), right = TRUE)
      
      labels <- lapply(
        paste("<strong>County: </strong>",
              data$name,
              "<br />",
              "<strong>% Political capital index:</strong>",
              round(data$index, 2), 
              "<br />",
              "<strong>Quintile:</strong>",
              data$indexQuint),
        htmltools::HTML
      )
      
      leaflet(data) %>%
        addTiles() %>%
        addPolygons(fillColor = ~pal(data$index), 
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
        addLegend("bottomleft", pal = pal, values = ~data$index,
                  title = "Index<br>(Quintile Group)", opacity = 1,
                  na.label = "Not Available",
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }
  })
  
  
  
  #############################################END MAIN NEW PLOT
  
  
  
  
  
  
  
  #
  # Subplots: No internet ------------------------------------------------
  #
  
  # Education 
  #output$subplot_noint_edu <- renderLeaflet({
  output$subplot_partic_pol <- renderLeaflet({
    if (input$whichtopic == "Political capital") {
      #data <- data_edu
      data <- data_pol
      data <- switch(input$whichstate,
                     "Iowa" = data[data$STATEFP == "19", ],
                     "Oregon" = data[data$STATEFP == "41", ],
                     "Virginia" = data[data$STATEFP == "51", ])
      
      pal <- colorQuantile("Blues", domain = data$votepart, probs = seq(0, 1, length = 6), right = TRUE)
      
      labels <- lapply(
        paste("<strong>County: </strong>",
              data$name,
              "<br />",
              "<strong>% Households without internet access:</strong>",
              round(data$votepart, 2), 
              "<br />",
              "<strong>Quintile:</strong>",
              data$votepartQuint),
        htmltools::HTML
      )
      
      leaflet(data) %>%
        addTiles() %>%
        addPolygons(fillColor = ~pal(data$votepart), 
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
        addLegend("bottomleft", pal = pal, values = ~data$votepart,
                  title = "Percent<br>(Quintile Group)", opacity = 1,
                  na.label = "Not Available",
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }
  })
  
  # Work
  # output$subplot_noint_work <- renderLeaflet({
  #   if (input$whichtopic == "Remote work") {
  #     data <- data_work
  #     data <- switch(input$whichstate,
  #                    "Iowa" = data[data$STATEFP == "19", ],
  #                    "Oregon" = data[data$STATEFP == "41", ],
  #                    "Virginia" = data[data$STATEFP == "51", ])
  #     
  #     pal <- colorQuantile("Blues", domain = data$nointernet, probs = seq(0, 1, length = 6), right = TRUE)
  #     
  #     labels <- lapply(
  #       paste("<strong>County: </strong>",
  #             data$name,
  #             "<br />",
  #             "<strong>% Households without broadband internet:</strong>",
  #             round(data$nointernet, 2), 
  #             "<br />",
  #             "<strong>Quintile:</strong>",
  #             data$votepartQuint),
  #       htmltools::HTML
  #     )
  #     
  #     leaflet(data) %>%
  #       addTiles() %>%
  #       addPolygons(fillColor = ~pal(data$nointernet), 
  #                   fillOpacity = 0.8,
  #                   stroke = TRUE,
  #                   weight = 0.9,
  #                   color = "gray",
  #                   smoothFactor = 0.7,
  #                   label = labels,
  #                   labelOptions = labelOptions(direction = "bottom",
  #                                               style = list(
  #                                                 "font-size" = "12px",
  #                                                 "border-color" = "rgba(0,0,0,0.5)",
  #                                                 direction = "auto"
  #                                               ))) %>%
  #       addLegend("bottomleft", pal = pal, values = ~data$nointernet,
  #                 title = "Percent<br>(Quintile Group)", opacity = 1,
  #                 na.label = "Not Available",
  #                 labFormat = function(type, cuts, p) {
  #                   n = length(cuts)
  #                   paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
  #                 })
  #   }
  # })
  
  # Telehealth
  # output$subplot_noint_med <- renderLeaflet({
  #   if (input$whichtopic == "Telemental health") {
  #     data <- data_med
  #     data <- switch(input$whichstate,
  #                    "Iowa" = data[data$STATEFP == "19", ],
  #                    "Oregon" = data[data$STATEFP == "41", ],
  #                    "Virginia" = data[data$STATEFP == "51", ])
  #     
  #     pal <- colorQuantile("Blues", domain = data$nointernet, probs = seq(0, 1, length = 6), right = TRUE)
  #     
  #     labels <- lapply(
  #       paste("<strong>County: </strong>",
  #             data$name,
  #             "<br />",
  #             "<strong>% Households without internet access:</strong>",
  #             round(data$nointernet, 2), 
  #             "<br />",
  #             "<strong>Quintile:</strong>",
  #             data$nointernetQuint),
  #       htmltools::HTML
  #     )
  #     
  #     leaflet(data) %>%
  #       addTiles() %>%
  #       addPolygons(fillColor = ~pal(data$nointernet), 
  #                   fillOpacity = 0.8,
  #                   stroke = TRUE,
  #                   weight = 0.9,
  #                   color = "gray",
  #                   smoothFactor = 0.7,
  #                   label = labels,
  #                   labelOptions = labelOptions(direction = "bottom",
  #                                               style = list(
  #                                                 "font-size" = "12px",
  #                                                 "border-color" = "rgba(0,0,0,0.5)",
  #                                                 direction = "auto"
  #                                               ))) %>%
  #       addLegend("bottomleft", pal = pal, values = ~data$nointernet,
  #                 title = "Percent<br>(Quintile Group)", opacity = 1,
  #                 na.label = "Not Available",
  #                 labFormat = function(type, cuts, p) {
  #                   n = length(cuts)
  #                   paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
  #                 })
  #   }
  # })
  
  
  #
  # Subplots: No computer -----------------------------------------------
  #
  
  # Education
  output$subplot_nocomp_edu <- renderLeaflet({
    if (input$whichtopic == "Political capital") {
      #data <- data_edu
      data <- data_pol
      data <- switch(input$whichstate,
                     "Iowa" = data[data$STATEFP == "19", ],
                     "Oregon" = data[data$STATEFP == "41", ],
                     "Virginia" = data[data$STATEFP == "51", ])
      
      pal <- colorQuantile("Blues", domain = data$assn2014, probs = seq(0, 1, length = 6), right = TRUE)
      
      labels <- lapply(
        paste("<strong>County: </strong>",
              data$name,
              "<br />",
              "<strong>% Households without a computer:</strong>",
              round(data$assn2014, 2), 
              "<br />",
              "<strong>Quintile:</strong>",
              data$assn2014Quint),
        htmltools::HTML
      )
      
      leaflet(data) %>%
        addTiles() %>%
        addPolygons(fillColor = ~pal(data$assn2014), 
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
        addLegend("bottomleft", pal = pal, values = ~data$assn2014,
                  title = "Percent<br>(Quintile Group)", opacity = 1,
                  na.label = "Not Available",
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }
  })
  
  # Work
  # output$subplot_nocomp_work <- renderLeaflet({
  #   if (input$whichtopic == "Political capital") {
  #     data <- data_work
  #     data <- switch(input$whichstate,
  #                    "Iowa" = data[data$STATEFP == "19", ],
  #                    "Oregon" = data[data$STATEFP == "41", ],
  #                    "Virginia" = data[data$STATEFP == "51", ])
  #     
  #     pal <- colorQuantile("Blues", domain = data$nocomputer, probs = seq(0, 1, length = 6), right = TRUE)
  #     
  #     labels <- lapply(
  #       paste("<strong>County: </strong>",
  #             data$name,
  #             "<br />",
  #             "<strong>% Households without a computer:</strong>",
  #             round(data$nocomputer, 2), 
  #             "<br />",
  #             "<strong>Quintile:</strong>",
  #             data$nocomputerQuint),
  #       htmltools::HTML
  #     )
  #     
  #     leaflet(data) %>%
  #       addTiles() %>%
  #       addPolygons(fillColor = ~pal(data$nocomputer), 
  #                   fillOpacity = 0.8,
  #                   stroke = TRUE,
  #                   weight = 0.9,
  #                   color = "gray",
  #                   smoothFactor = 0.7,
  #                   label = labels,
  #                   labelOptions = labelOptions(direction = "bottom",
  #                                               style = list(
  #                                                 "font-size" = "12px",
  #                                                 "border-color" = "rgba(0,0,0,0.5)",
  #                                                 direction = "auto"
  #                                               ))) %>%
  #       addLegend("bottomleft", pal = pal, values = ~data$nocomputer,
  #                 title = "Percent<br>(Quintile Group)", opacity = 1,
  #                 na.label = "Not Available",
  #                 labFormat = function(type, cuts, p) {
  #                   n = length(cuts)
  #                   paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
  #                 })
  #   }
  # })
  
  # Telehealth
  # output$subplot_nocomp_med <- renderLeaflet({
  #   if (input$whichtopic == "Telemental health") {
  #     data <- data_med
  #     data <- switch(input$whichstate,
  #                    "Iowa" = data[data$STATEFP == "19", ],
  #                    "Oregon" = data[data$STATEFP == "41", ],
  #                    "Virginia" = data[data$STATEFP == "51", ])
  #     
  #     pal <- colorQuantile("Blues", domain = data$nocomputer, probs = seq(0, 1, length = 6), right = TRUE)
  #     
  #     labels <- lapply(
  #       paste("<strong>County: </strong>",
  #             data$name,
  #             "<br />",
  #             "<strong>% Households without a computer:</strong>",
  #             round(data$nocomputer, 2),
  #             "<br />",
  #             "<strong>Quintile:</strong>",
  #             data$nocomputerQuint),
  #       htmltools::HTML
  #     )
  #     
  #     leaflet(data) %>%
  #       addTiles() %>%
  #       addPolygons(fillColor = ~pal(data$nocomputer), 
  #                   fillOpacity = 0.8,
  #                   stroke = TRUE,
  #                   weight = 0.9,
  #                   color = "gray",
  #                   smoothFactor = 0.7,
  #                   label = labels,
  #                   labelOptions = labelOptions(direction = "bottom",
  #                                               style = list(
  #                                                 "font-size" = "12px",
  #                                                 "border-color" = "rgba(0,0,0,0.5)",
  #                                                 direction = "auto"
  #                                               ))) %>%
  #       addLegend("bottomleft", pal = pal, values = ~data$nocomputer,
  #                 title = "Percent<br>(Quintile Group)", opacity = 1,
  #                 na.label = "Not Available",
  #                 labFormat = function(type, cuts, p) {
  #                   n = length(cuts)
  #                   paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
  #                 })
  #   }
  # })
  
  #
  # Subplots: various -----------------------------------------------
  #
  
  # In K12 / contribution (num1000)
  output$subplot_ink12 <- renderLeaflet({
    if (input$whichtopic == "Political capital") {
      #data <- data_edu
      data <- data_pol
      data <- switch(input$whichstate,
                     "Iowa" = data[data$STATEFP == "19", ],
                     "Oregon" = data[data$STATEFP == "41", ],
                     "Virginia" = data[data$STATEFP == "51", ])
      
      pal <- colorQuantile("Blues", domain = data$num1000, probs = seq(0, 1, length = 6), right = TRUE)
      
      labels <- lapply(
        paste("<strong>County: </strong>",
              data$name,
              "<br />",
              "<strong>% Population enrolled in K-12:</strong>",
              round(data$num1000, 2), 
              "<br />",
              "<strong>Quintile:</strong>",
              data$num1000Quint),
        htmltools::HTML
      )
      
      leaflet(data) %>%
        addTiles() %>%
        addPolygons(fillColor = ~pal(data$num1000), 
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
        addLegend("bottomleft", pal = pal, values = ~data$num1000,
                  title = "Percent<br>(Quintile Group)", opacity = 1,
                  na.label = "Not Available",
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }
  })
  
  
############## 3D plot for the index
  #library(plot3D)
  
  
  output$plotly_index3d <- renderPlotly({
    
    index3d <- plot_ly(data_pol, x = ~votepart, y = ~assn2014, z = ~num1000,  color = ~state, text= ~name, width="auto", height = 800)
    index3d <- index3d %>% add_markers()
    index3d <- index3d  %>% layout(scene = list(xaxis = list(title = 'Representation (voter turnout)'),
                                                yaxis = list(title = 'Participation (organizations)'),
                                                zaxis = list(title = 'Contribution (people per 1000)')))
    
    index3d
  })
#####################################
  

  ##############START ADD ##############
  # Plot summary 3 states by subdomain
 
  
  # masterdata <- read_csv("~/EM/gates/rivanna_data/working/dashboard/em_master_data_final_I.csv")

  #mdata <- read_csv("~/EM/gates/src/dashboard/polcap4/em_master_data_final_II.csv")
  mdata <- read_csv("em_master_data_final_II.csv")
  
  # #library(reshape)
  # mdata <- melt(masterdata, id.vars = c("domain", "sub_domain", "questions") , measure.vars = c("Virginia", "Iowa", "Oregon" ))
  # mdata<- mdata %>% rename( state=variable)
  av_mdata <- mdata %>% group_by(state, domain, sub_domain) %>% summarise(mean= mean(num))

  output$plotly_domains <- renderPlotly({
    qn <- ggplot(av_mdata, aes(x=1, y=mean)) +
      geom_point(aes(text= sub_domain, colour = factor(state)), position = position_jitter(width = 1),
                 size = 2, show.legend = TRUE)+
      xlab("") + ylab("Composite index") +
      geom_boxplot(aes(y=mean),  alpha = 0.2, width = .3, colour = "BLACK")+
      theme(legend.position="bottom", axis.text.y = element_blank(), axis.ticks.y = element_blank())+
      coord_flip()

    ggplotly(qn) %>%
      layout(legend = list(orientation = "h", x = 0.25, y = -0.4))
  })
  
  ##############FINISH ADD ##############
  
  
  
  # Occupation
  # output$subplot_occup <- renderLeaflet({
  #   if (input$whichtopic == "Political capital") {
  #     data <- data_work
  #     data <- switch(input$whichstate,
  #                    "Iowa" = data[data$STATEFP == "19", ],
  #                    "Oregon" = data[data$STATEFP == "41", ],
  #                    "Virginia" = data[data$STATEFP == "51", ])
  #     
  #     pal <- colorQuantile("Blues", domain = data$occup, probs = seq(0, 1, length = 6), right = TRUE)
  #     
  #     labels <- lapply(
  #       paste("<strong>County: </strong>",
  #             data$name,
  #             "<br />",
  #             "<strong>% Non-remote workers in non-remote-friendly occupations:</strong>",
  #             round(data$occup, 2), 
  #             "<br />",
  #             "<strong>Quintile:</strong>",
  #             data$occupQuint),
  #       htmltools::HTML
  #     )
  #     
  #     leaflet(data) %>%
  #       addTiles() %>%
  #       addPolygons(fillColor = ~pal(data$occup), 
  #                   fillOpacity = 0.8,
  #                   stroke = TRUE,
  #                   weight = 0.9,
  #                   color = "gray",
  #                   smoothFactor = 0.7,
  #                   label = labels,
  #                   labelOptions = labelOptions(direction = "bottom",
  #                                               style = list(
  #                                                 "font-size" = "12px",
  #                                                 "border-color" = "rgba(0,0,0,0.5)",
  #                                                 direction = "auto"
  #                                               ))) %>%
  #       addLegend("bottomleft", pal = pal, values = ~data$occup,
  #                 title = "Percent<br>(Quintile Group)", opacity = 1,
  #                 na.label = "Not Available",
  #                 labFormat = function(type, cuts, p) {
  #                   n = length(cuts)
  #                   paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
  #                 })
  #   }
  # })
  
  # Industry
  # output$subplot_industr <- renderLeaflet({
  #   if (input$whichtopic == "Political capital") {
  #     data <- data_work
  #     data <- switch(input$whichstate,
  #                    "Iowa" = data[data$STATEFP == "19", ],
  #                    "Oregon" = data[data$STATEFP == "41", ],
  #                    "Virginia" = data[data$STATEFP == "51", ])
  #     
  #     pal <- colorQuantile("Blues", domain = data$industr, probs = seq(0, 1, length = 6), right = TRUE)
  #     
  #     labels <- lapply(
  #       paste("<strong>County: </strong>",
  #             data$name,
  #             "<br />",
  #             "<strong>% Non-remote workers in non-remote-friendly industries:</strong>",
  #             round(data$industr, 2), 
  #             "<br />",
  #             "<strong>Quintile:</strong>",
  #             data$industrQuint),
  #       htmltools::HTML
  #     )
  #     
  #     leaflet(data) %>%
  #       addTiles() %>%
  #       addPolygons(fillColor = ~pal(data$industr), 
  #                   fillOpacity = 0.8,
  #                   stroke = TRUE,
  #                   weight = 0.9,
  #                   color = "gray",
  #                   smoothFactor = 0.7,
  #                   label = labels,
  #                   labelOptions = labelOptions(direction = "bottom",
  #                                               style = list(
  #                                                 "font-size" = "12px",
  #                                                 "border-color" = "rgba(0,0,0,0.5)",
  #                                                 direction = "auto"
  #                                               ))) %>%
  #       addLegend("bottomleft", pal = pal, values = ~data$industr,
  #                 title = "Percent<br>(Quintile Group)", opacity = 1,
  #                 na.label = "Not Available",
  #                 labFormat = function(type, cuts, p) {
  #                   n = length(cuts)
  #                   paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
  #                 })
  #   }
  # })
  
  # Poor mental health days
  # output$subplot_poorment <- renderLeaflet({
  #   if (input$whichtopic == "Telemental health") {
  #     data <- data_med
  #     data <- switch(input$whichstate,
  #                    "Iowa" = data[data$STATEFP == "19", ],
  #                    "Oregon" = data[data$STATEFP == "41", ],
  #                    "Virginia" = data[data$STATEFP == "51", ])
  #     
  #     pal <- colorQuantile("Blues", domain = data$avgnum_poormenth, probs = seq(0, 1, length = 6), right = TRUE)
  #     
  #     labels <- lapply(
  #       paste("<strong>County: </strong>",
  #             data$name,
  #             "<br />",
  #             "<strong>Average number of poor mental health days in past month:</strong>",
  #             round(data$avgnum_poormenth, 2), 
  #             "<br />",
  #             "<strong>Quintile:</strong>",
  #             data$menthdaysQuint),
  #       htmltools::HTML
  #     )
  #     
  #     leaflet(data) %>%
  #       addTiles() %>%
  #       addPolygons(fillColor = ~pal(data$avgnum_poormenth), 
  #                   fillOpacity = 0.8,
  #                   stroke = TRUE,
  #                   weight = 0.9,
  #                   color = "gray",
  #                   smoothFactor = 0.7,
  #                   label = labels,
  #                   labelOptions = labelOptions(direction = "bottom",
  #                                               style = list(
  #                                                 "font-size" = "12px",
  #                                                 "border-color" = "rgba(0,0,0,0.5)",
  #                                                 direction = "auto"
  #                                               ))) %>%
  #       addLegend("bottomleft", pal = pal, values = ~data$avgnum_poormenth,
  #                 title = "Number<br>(Quintile Group)", opacity = 1,
  #                 na.label = "Not Available",
  #                 labFormat = function(type, cuts, p) {
  #                   n = length(cuts)
  #                   paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
  #                 })
  #   }
  # })
  
  # Percent uninsured
  # output$subplot_unins <- renderLeaflet({
  #   if (input$whichtopic == "Telemental health") {
  #     data <- data_med
  #     data <- switch(input$whichstate,
  #                    "Iowa" = data[data$STATEFP == "19", ],
  #                    "Oregon" = data[data$STATEFP == "41", ],
  #                    "Virginia" = data[data$STATEFP == "51", ])
  #     
  #     pal <- colorQuantile("Blues", domain = data$pct_unins, probs = seq(0, 1, length = 6), right = TRUE)
  #     
  #     labels <- lapply(
  #       paste("<strong>County: </strong>",
  #             data$name,
  #             "<br />",
  #             "<strong>% Population without health insurance:</strong>",
  #             round(data$pct_unins, 2),
  #             "<br />",
  #             "<strong>Quintile:</strong>",
  #             data$uninsQuint),
  #       htmltools::HTML
  #     )
  #     
  #     leaflet(data) %>%
  #       addTiles() %>%
  #       addPolygons(fillColor = ~pal(data$pct_unins), 
  #                   fillOpacity = 0.8,
  #                   stroke = TRUE,
  #                   weight = 0.9,
  #                   color = "gray",
  #                   smoothFactor = 0.7,
  #                   label = labels,
  #                   labelOptions = labelOptions(direction = "bottom",
  #                                               style = list(
  #                                                 "font-size" = "12px",
  #                                                 "border-color" = "rgba(0,0,0,0.5)",
  #                                                 direction = "auto"
  #                                               ))) %>%
  #       addLegend("bottomleft", pal = pal, values = ~data$pct_unins,
  #                 title = "Percent<br>(Quintile Group)", opacity = 1,
  #                 na.label = "Not Available",
  #                 labFormat = function(type, cuts, p) {
  #                   n = length(cuts)
  #                   paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
  #                 })
  #   }
  # })
  
  # Mental health providers
  # output$subplot_healthprov <- renderLeaflet({
  #   if (input$whichtopic == "Telemental health") {
  #     data <- data_med
  #     data <- switch(input$whichstate,
  #                    "Iowa" = data[data$STATEFP == "19", ],
  #                    "Oregon" = data[data$STATEFP == "41", ],
  #                    "Virginia" = data[data$STATEFP == "51", ])
  #     
  #     pal <- colorQuantile("Blues", domain = data$menthprov_per100k, probs = seq(0, 1, length = 6), right = TRUE, reverse = TRUE)
  #     
  #     labels <- lapply(
  #       paste("<strong>County: </strong>",
  #             data$name,
  #             "<br />",
  #             "<strong>Number of mental health providers per 100,000 population:</strong>",
  #             round(data$menthprov_per100k, 2), 
  #             "<br />",
  #             "<strong>Quintile:</strong>",
  #             data$menthprovQuint),
  #       htmltools::HTML
  #     )
  #     
  #     leaflet(data) %>%
  #       addTiles() %>%
  #       addPolygons(fillColor = ~pal(data$menthprov_per100k), 
  #                   fillOpacity = 0.8,
  #                   stroke = TRUE,
  #                   weight = 0.9,
  #                   color = "gray",
  #                   smoothFactor = 0.7,
  #                   label = labels,
  #                   labelOptions = labelOptions(direction = "bottom",
  #                                               style = list(
  #                                                 "font-size" = "12px",
  #                                                 "border-color" = "rgba(0,0,0,0.5)",
  #                                                 direction = "auto"
  #                                               ))) %>%
  #       addLegend("bottomleft", pal = pal, values = ~data$menthprov_per100k,
  #                 title = "Number<br>(Quintile Group)", opacity = 1,
  #                 na.label = "Not Available",
  #                 labFormat = function(type, cuts, p) {
  #                   n = length(cuts)
  #                   paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
  #                 })
  #   }
  # })
  
}


#
# App ---------------------------------------------
#

shinyApp(ui = ui, server = server)