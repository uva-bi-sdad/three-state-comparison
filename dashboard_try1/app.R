#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
# 

library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(shiny)
library(leaflet)
library(sf)
#library(dplyr)
library(plotly)
library(ggplot2)
library(base)
library(stats)
library(graphics)
library(readr)
library(rsconnect)
#library(USAboundaries)

#devtools::install_github("ropensci/USAboundaries")


# |-------------- Data reading & processing ------------|

data_seg <- read_rds("data_seg.rds")
names(data_seg)[1]<- "geoid"

# |-------------- Setting up the UI ------------|

header <- dashboardHeader()

sidebar <- dashboardSidebar()

#   selectInput("selected", "Select State:", 
#               choices = c("Iowa", "Virginia", "Oregon"), 
#               selected = "Iowa",
#               selectize=TRUE)
#   
# )

body <- dashboardBody(width=12,
                      
                      
                      fluidRow(width=8, align = "Center",
                               h1(strong("Segregation Index"), align = "Center")
                               )
                      ,
                      
                      #1 fluidRow
                      fluidRow( width=12,

                                column(width = 4,
                                       h2(strong("Oregon"), align = "Center"),
                                       p(),
                                       leafletOutput("map_or", width = "100%")
                                ),

                                column(width = 4,
                                       h2(strong("Iowa"), align = "Center"),
                                       p(),
                                       leafletOutput("map", width = "100%")
                                ),


                                column(width = 4,
                                       h2(strong("Virginia"), align = "Center"),
                                       p(),
                                       leafletOutput("map_va", width = "100%")
                                )
                      ),

                      br(),
                      br(),
                      
                      
                      ##2 fluidRow
                      fluidRow(width=8, align = "Center",
                               h3(strong("Comparison of States Segregation Index"), align = "Center"),
                               plotlyOutput("index", width = "60%")
                      ),
                      
                      
                      br(),
                      br(),
                      br(),
                      br(),
                      ##3 fluidRow new
                      
                      # fluidRow( width=12,
                      # 
                      #           column(width = 4,
                      #                  h3(strong("White Non Hispanic "), align = "Center", style = "color:gray"),
                      #                  h4("proportion", align = "Center", style = "color:gray"),
                      #                  p()
                      #                  ,
                      #                  plotOutput("white", width = "100%")
                      #           )
                      # 
                      #           ,
                      # 
                      #           column(width = 4,
                      #                  h3(strong("Poverty"), align = "Center", style = "color:gray"),
                      #                  h4("proportion", align = "Center", style = "color:gray"),
                      #                  p()
                      #                  ,
                      #                  plotOutput("poverty", width = "100%")
                      #           ),
                      # 
                      # 
                      #           column(width = 4,
                      #                  h3(strong("Commuters to work"), align = "Center", style = "color:gray"),
                      #                  h4("proportion more than 15 minutes", align = "Center", style = "color:gray"),
                      #                  p()
                      #                  ,
                      #                  plotOutput("commuter", width = "100%")
                      #           )
                      # )

                 ##4 distributions over time
                 #################################################
                 #White
                 
                 
                 fluidRow( width=12, h3("White Non Hispanic ", align= "Center") ),
                 
                 fluidRow( width=12,
                                     column(width = 4,
                                            h3(strong(" Oregon"), align = "Center", style = "color:gray"),
                                            h4("proportion", align = "Center", style = "color:gray"),
                                            p()
                                            ,
                                            plotOutput("white_dis_or", width = "100%")
                                     ),
                           
                           column(width = 4,
                                  h3(strong("Iowa "), align = "Center", style = "color:gray"),
                                  h4("proportion", align = "Center", style = "color:gray"),
                                  p()
                                  ,
                                  plotOutput("white_dis_ia", width = "100%")
                           ),
                           
                           column(width = 4,
                                  h3(strong("Virginia "), align = "Center", style = "color:gray"),
                                  h4("proportion", align = "Center", style = "color:gray"),
                                  p()
                                  ,
                                  plotOutput("white_dis_va", width = "100%")
                           )
                     
                 ),
                 
                 #################################################
                 #Poor
                 
                 
                 fluidRow( width=12, h3("Poverty ", align= "Center") ),
                 
                 fluidRow( width=12,
                           column(width = 4,
                                  h3(strong(" Oregon"), align = "Center", style = "color:gray"),
                                  h4("proportion", align = "Center", style = "color:gray"),
                                  p()
                                  ,
                                  plotOutput("poor_dis_or", width = "100%")
                           ),
                           
                           column(width = 4,
                                  h3(strong("Iowa "), align = "Center", style = "color:gray"),
                                  h4("proportion", align = "Center", style = "color:gray"),
                                  p()
                                  ,
                                  plotOutput("poor_dis_ia", width = "100%")
                           ),
                           
                           column(width = 4,
                                  h3(strong("Virginia "), align = "Center", style = "color:gray"),
                                  h4("proportion", align = "Center", style = "color:gray"),
                                  p()
                                  ,
                                  plotOutput("poor_dis_va", width = "100%")
                           )
                           
                 )
                      
                      
                      
)

ui <- dashboardPage(header, sidebar, body)


#states3 <- data_seg %>%  dplyr::filter(COUNTY!=0) %>% dplyr::filter(STNAME=="Oregon" |STNAME=="Iowa"|STNAME=="Virginia")
max_seg <- max(data_seg$seg)
min_seg <- min(data_seg$seg)

##############################################################################################################################
##############################################################################################################################


server <- function(input, output) {
    
    output$map <- renderLeaflet({
        selected <- "Iowa"
        
        #states <- USAboundaries::us_boundaries(type="county")
        states <- read_rds("states.rds")
        states <- states %>% filter(state_name == "Iowa") %>%
            mutate(
                COUNTY = as.numeric(countyfp),
                STATE = as.numeric(statefp)
            )
        plotdat <- states %>% left_join(data_seg %>% 
                                            select(geoid, seg, white_id, poor_id, commute_id), 
                                        by=c("geoid"))
        
        
        # range of numbers in the color palette
        scale_range <- c(min_seg, max_seg ) #max(plotdat$POPESTIMATE2019))
        # missing values are bright green, so we can see them and fix them :)
        pal <- colorNumeric("Reds", scale_range, na.color = "#aaff56", reverse=FALSE)
        
        
        leaflet() %>%
            addProviderTiles("CartoDB.Positron") %>%  
            addPolygons(data = plotdat,
                        color = "#000000", # outline of polygons
                        fillColor = ~pal(plotdat$seg), # color mapping
                        fillOpacity = 0.9,
                        weight = 0.2,
                        smoothFactor = 0.2,
                        # text to be shown on click is in html
                        popup = ~ paste0(name,", ", str_extract(state_name, "^([^,]*)"), "<br>", round(seg,3) )) %>%
            setView(lng = -93.581543, lat = 42.032974,  zoom = 6.4) 
        # 
        # %>% 
        #   addLegend(pal = pal,
        #             values = scale_range,
        #             position = "bottomright",
        #             title = "Population<br>Estimate 2019"
        #            
        #           
        #   )
    })
    
    ###map VA
    output$map_va <- renderLeaflet({
        selected <- "Virginia"
        
        #states <- USAboundaries::us_boundaries(type="county")
        states <- read_rds("states.rds")
        states <- states %>% filter(state_name == selected) %>%
            mutate(
                COUNTY = as.numeric(countyfp),
                STATE = as.numeric(statefp)
            )
        plotdat <- states %>% left_join(data_seg %>% 
                                            select(geoid, seg, white_id, poor_id, commute_id), 
                                        by=c("geoid"))
        
        
        # range of numbers in the color palette
        scale_range <- c(min_seg, max_seg ) #max(plotdat$POPESTIMATE2019))
        # missing values are bright green, so we can see them and fix them :)
        pal <- colorNumeric("Reds", scale_range, na.color = "#aaff56", reverse=FALSE)
        
        
        leaflet() %>%
            addProviderTiles("CartoDB.Positron") %>%  
            addPolygons(data = plotdat,
                        color = "#000000", # outline of polygons
                        fillColor = ~pal(plotdat$seg), # color mapping
                        fillOpacity = 0.9,
                        weight = 0.2,
                        smoothFactor = 0.2,
                        # text to be shown on click is in html
                        popup = ~ paste0(name,", ", str_extract(state_name, "^([^,]*)"), "<br>",  round(seg,3)  )) %>%
            setView(lng = -78.6569, lat = 37.4316,  zoom = 6.4) %>% 
            addLegend(pal = pal,
                      values = scale_range,
                      position = "bottomright",
                      title = "Segregation<br>Index 2019",
                      guide_legend(direction="horizontal")
                      
                      
            ) 
        # %>% 
        #   guide_legend(direction="horizontal")
    })  
    
    
    ###map OR
    output$map_or <- renderLeaflet({
        selected <- "Oregon"
        
        #states <- USAboundaries::us_boundaries(type="county")
        states <- read_rds("states.rds")
        states <- states %>% filter(state_name == selected) %>%
            mutate(
                COUNTY = as.numeric(countyfp),
                STATE = as.numeric(statefp)
            )
        plotdat <- states %>% left_join(data_seg %>% 
                                            select(geoid, seg, white_id, poor_id, commute_id), 
                                        by=c("geoid"))
        
        
        # range of numbers in the color palette
        scale_range <- c(min_seg, max_seg ) #max(plotdat$POPESTIMATE2019))
        # missing values are bright green, so we can see them and fix them :)
        pal <- colorNumeric("Reds", scale_range, na.color = "#aaff56", reverse=FALSE)
        
        
        leaflet() %>%
            addProviderTiles("CartoDB.Positron") %>%  
            addPolygons(data = plotdat,
                        color = "#000000", # outline of polygons
                        fillColor = ~pal(plotdat$seg), # color mapping
                        fillOpacity = 0.9,
                        weight = 0.2,
                        smoothFactor = 0.2,
                        # text to be shown on click is in html
                        popup = ~ paste0(name,", ", str_extract(state_name, "^([^,]*)"), "<br>", round(seg,3) )) %>%
            setView(lng = -120.5542, lat = 43.8041,  zoom = 6.4) 
        # %>% 
        #   addLegend(pal = pal,
        #             values = scale_range,
        #             position = "bottomright",
        #             title = "Population<br>Estimate 2019"
        #   )
    }) 
    
    
    
    #################################################### 
    ### scatter plot with SegIndex for 3 states
    
    output$index <- renderPlotly({
        
        q <- ggplot(data_seg, aes(x=1, y=seg, label= name )) +
            geom_point(aes( colour = factor(State)), position = position_jitter(width = 1), 
                       size = 1, show.legend = TRUE)+
            xlab("") + ylab("") +
            geom_boxplot(aes(y=seg),  alpha = 0.2, width = .3, colour = "BLACK")+
            theme(legend.position="bottom", axis.text.y = element_blank(), axis.ticks.y = element_blank())+
            coord_flip() 
        
        ggplotly(q) %>%
            layout(legend = list(orientation = "h", x = 0.42, y = -0.2) )
        
        
    })
    
    #################################################### 
    ### Distributions - quedan así hasta segundo aviso/ stays until further notice
    
    # #setwd("~/Documents/Gates_EM/gates/sources/")
    # source("R_rainclouds.R")
    # #source("summarySE.R")
    # #library(dplyr)
    # 
    # output$white <- renderPlot({
    # 
    #     white_dis <- ggplot(data_seg , aes(x= State, y= white_noh_pc*100 , fill = State))+
    #         geom_flat_violin(position = position_nudge(x = .2, y = 0),adjust = 2)+
    #         geom_point(position = position_jitter(width = .15), size = .3)+
    #         geom_boxplot(outlier.colour="black", outlier.shape=1, outlier.size=1, notch=FALSE, width = .1, colour = "BLACK", fill="transparent") +
    #         ylab('')+xlab('')+
    #         coord_flip(ylim = c( min(data_seg$white_noh_pc)*100, 100))+
    #         guides(fill = FALSE)+
    #         geom_hline(yintercept = mean(data_seg$white_noh_pc)*100, color = "red", size=0.2)+
    #         #ggtitle('High School Graduation rate by county, 2018 ') +
    #         theme(
    #             panel.grid.major = element_line(size = 0.25, linetype = 'solid', colour = "gray"),
    #             axis.line = element_line(colour = "transparent")
    #         )
    #     white_dis
    # }
    # )
    # 
    # output$poverty <- renderPlot({
    # 
    #     poverty_dis <- ggplot(data_seg , aes(x= State, y= pov , fill = State))+
    #         geom_flat_violin(position = position_nudge(x = .2, y = 0),adjust = 2)+
    #         geom_point(position = position_jitter(width = .15), size = .3)+
    #         geom_boxplot(outlier.colour="black", outlier.shape=1, outlier.size=1, notch=FALSE, width = .1, colour = "BLACK", fill="transparent") +
    #         ylab('')+xlab('')+
    #         coord_flip(ylim = c(min(data_seg$pov ), max(data_seg$pov )))+
    #         guides(fill = FALSE)+
    #         geom_hline(yintercept = mean(data_seg$pov), color = "red", size=0.2)+
    #         #ggtitle('High School Graduation rate by county, 2018 ') +
    #         theme(
    #             panel.grid.major = element_line(size = 0.25, linetype = 'solid', colour = "gray"),
    #             axis.line = element_line(colour = "transparent")
    #         )
    #     poverty_dis
    # }
    # )
    # 
    # 
    # 
    # output$commuter <- renderPlot({
    # 
    #     commuter_dis <- ggplot(data_seg , aes(x= State, y= commute , fill = State))+
    #         geom_flat_violin(position = position_nudge(x = .2, y = 0),adjust = 2)+
    #         geom_point(position = position_jitter(width = .15), size = .3)+
    #         geom_boxplot(outlier.colour="black", outlier.shape=1, outlier.size=1, notch=FALSE, width = .1, colour = "BLACK", fill="transparent") +
    #         ylab('')+xlab('')+
    #         coord_flip(ylim = c(min(data_seg$commute ), max(data_seg$commute )))+
    #         guides(fill = FALSE)+
    #         geom_hline(yintercept = mean(data_seg$commute), color = "red", size=0.2)+
    #         #ggtitle('High School Graduation rate by county, 2018 ') +
    #         theme(
    #             panel.grid.major = element_line(size = 0.25, linetype = 'solid', colour = "gray"),
    #             axis.line = element_line(colour = "transparent")
    #         )
    #     commuter_dis
    # }
    # )
    
    
##############  DISTRIBUTIONS 2018-2010 ############## 
##WHITE
    pop3states <- read_csv("pop3states.csv")

    output$white_dis_va <-  renderPlot(
        {
            va<- pop3states %>% filter(state=="Virginia") 
            ggplot(va, aes(pc, fill = as.character(year)  )) + 
                geom_density(aes(pc, fill = as.character(year), weight=pop ),alpha = 0.2)+
                ggtitle(' ') +
                labs(fill='Year')
        }
    )
    
    output$white_dis_or <-  renderPlot(
        {
            or<- pop3states %>% filter(state=="Oregon") 
            ggplot(or, aes(pc, fill = as.character(year)  )) + 
                geom_density(aes(pc, fill = as.character(year), weight=pop ),alpha = 0.2)+
                ggtitle(' ') + 
                theme(legend.position = "none")  
        }
    )
    
    
    output$white_dis_ia <-  renderPlot(
        {
            ia<- pop3states %>% filter(state=="Iowa") 
            ggplot(ia, aes(pc, fill = as.character(year)  )) + 
                geom_density(aes(pc, fill = as.character(year), weight=pop ),alpha = 0.2)+
                ggtitle(' ') + 
                theme(legend.position = "none")  
        }
    )    
  
    
    ##POVERTY
    poor3states <- read_rds("poverty3states.rds")
    poor3states <- separate(poor3states, col="Geographic Area Name", sep = "," , into = c("county","state"), remove = FALSE)
    poor3states <- merge(x=poor3states, y=pop3states %>% filter(year==2019) %>% select(`Geographic Area Name`, pop), by = "Geographic Area Name" , all.x = TRUE )
  
    output$poor_dis_va <-  renderPlot(
        {
            va<- poor3states %>% filter(state==" Virginia") 
            ggplot(va, aes(poor, fill = as.character(year)  )) + 
                geom_density(aes(poor, fill = as.character(year), weight=pop ),alpha = 0.2)+
                ggtitle(' ') +
                labs(fill='Year') 
        }
    )
    
    output$poor_dis_ia <-  renderPlot(
        {
            ia<- poor3states %>% filter(state==" Iowa") 
            ggplot(ia, aes(poor, fill = as.character(year)  )) + 
                geom_density(aes(poor, fill = as.character(year), weight=pop ),alpha = 0.2)+
                ggtitle(' ') +
                labs(fill='Year') + 
                theme(legend.position = "none")
        }
    )
    
    
    output$poor_dis_or <-  renderPlot(
        {
            or<- poor3states %>% filter(state==" Oregon") 
            ggplot(or, aes(poor, fill = as.character(year)  )) + 
                geom_density(aes(poor, fill = as.character(year), weight=pop ),alpha = 0.2)+
                ggtitle(' ') +
                labs(fill='Year') + 
                theme(legend.position = "none")
        }
    )

    
}

shinyApp(ui, server)