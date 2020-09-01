    library(tidycensus)
    library(ggplot2)
    library(sf)
    library(scales)
    library(viridis)

This file compares palette ideas for displaying rurality measures, using
the [Index of Relative Rurality
(IRR)](https://purr.purdue.edu/publications/2960/1).

First, we read in the data that Brandon compiled:

    ThreeState <- read.csv("~/three-state-comparison/data/ThreeState_data.csv")

Next we use the tidycensus package to acquire county-level shapefiles
and merge it to our three state data.

    state_geo <- get_acs(geography="county", 
      state=c(19, 41, 51), #IA = 19, OR = 41, VA = 51
      variables="C02003_001", # we have to define a variable for the function to work, will be removed since we don't need this data
      year=2018, 
      survey="acs5", 
      cache_table=TRUE, 
      output="wide", 
      geometry=TRUE, 
      keep_geo_vars=TRUE
    )

    state_geo <- state_geo[, c("STATEFP", "GEOID")]

    ThreeState <- merge(ThreeState, state_geo, by = "GEOID")
    st_geometry(ThreeState) <- ThreeState$geometry

    ThreeState$group[which(ThreeState$IRR<0.2)]= "< 0.2"
    ThreeState$group[which(ThreeState$IRR>=0.2 & ThreeState$IRR<0.4)]= "[0.2, 0.4)"
    ThreeState$group[which(ThreeState$IRR>=0.4 & ThreeState$IRR<0.6)]="[0.4, 0.6)"
    ThreeState$group[which(ThreeState$IRR>=0.6 & ThreeState$IRR<0.8)]= "[0.6, 0.8)"
    ThreeState$group[which(ThreeState$IRR>=0.8)]= "0.8 <="


    ThreeState$group <- factor(ThreeState$group, levels = c("< 0.2", "[0.2, 0.4)", "[0.4, 0.6)", "[0.6, 0.8)", "0.8 <="))

Viridis Plasma 10 Category
--------------------------

    #Viridis Color Palette option=PLASMA
    #https://www.thinkingondata.com/something-about-viridis-library/


    PLASMA<-viridis_pal(option="plasma")(12)  
    PLASMA <- PLASMA[2:11]


    #ThreeState$group2[which(ThreeState$IRR<0.1)]= "< 0.1" there are not data values in this category for IA, OR, VA
    ThreeState$group2[which(ThreeState$IRR>=0.1 & ThreeState$IRR<0.2)]= "[0.1, 0.2)"
    ThreeState$group2[which(ThreeState$IRR>=0.2 & ThreeState$IRR<0.3)]= "[0.2, 0.3)"
    ThreeState$group2[which(ThreeState$IRR>=0.3 & ThreeState$IRR<0.4)]= "[0.3, 0.4)"
    ThreeState$group2[which(ThreeState$IRR>=0.4 & ThreeState$IRR<0.5)]="[0.4, 0.5)"
    ThreeState$group2[which(ThreeState$IRR>=0.5 & ThreeState$IRR<0.6)]= "[0.5, 0.6)"
    ThreeState$group2[which(ThreeState$IRR>=0.6 & ThreeState$IRR<0.7)]= "[0.6, 0.7)"
    ThreeState$group2[which(ThreeState$IRR>=0.7 & ThreeState$IRR<0.8)]= "[0.7, 0.8)"
    ThreeState$group2[which(ThreeState$IRR>=0.8 & ThreeState$IRR<0.9)]= "[0.8, 0.9)"
    ThreeState$group2[which(ThreeState$IRR>=0.9)]= "0.9 <="


    ThreeState$group2 <- factor(ThreeState$group2, levels = c("< 0.1", "[0.1, 0.2)", "[0.2, 0.3)", "[0.3, 0.4)","[0.4, 0.5)", "[0.5, 0.6)", "[0.6, 0.7)","[0.7, 0.8)", "[0.8, 0.9)",  "0.9 <="))



    P4<-rep(NA, length=length(ThreeState$IRR))
      #P4[which(ThreeState$IRR<0.1)]=PLASMA[1]
      P4[which(ThreeState$IRR>=0.1 & ThreeState$IRR < 0.2)]=PLASMA[2]
      P4[which(ThreeState$IRR>=0.2 & ThreeState$IRR < 0.3)]=PLASMA[3]
      P4[which(ThreeState$IRR>=0.3 & ThreeState$IRR < 0.4)]=PLASMA[4]
      P4[which(ThreeState$IRR>=0.4 & ThreeState$IRR < 0.5)]=PLASMA[5]
      P4[which(ThreeState$IRR>=0.5 & ThreeState$IRR < 0.6)]=PLASMA[6]
      P4[which(ThreeState$IRR>=0.6 & ThreeState$IRR < 0.7)]=PLASMA[7]
      P4[which(ThreeState$IRR>=0.7 & ThreeState$IRR < 0.8)]=PLASMA[8]
      P4[which(ThreeState$IRR>=0.8 & ThreeState$IRR < 0.9)]=PLASMA[9]
      P4[which(ThreeState$IRR>=0.9)]=PLASMA[10]

      
    ggplot() + 
      geom_sf(data = ThreeState[ThreeState$STATEFP == 19, ], aes(fill = group2), color = "white") + 
      scale_fill_manual(values = c(PLASMA[4], PLASMA[5], PLASMA[6])) +
      theme(panel.background = element_rect(fill  = "transparent"),
            plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
            legend.title = element_text(size = 11, face = "bold"),
            legend.text = element_text(size = 11),
            legend.position = "right", 
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank())
    ggplot() + 
      geom_sf(data = ThreeState[ThreeState$STATEFP == 41, ], aes(fill = group2), color = "white") + 
      scale_fill_manual(values = c(PLASMA[3], PLASMA[4], PLASMA[5], PLASMA[6], PLASMA[7])) +
      theme(panel.background = element_rect(fill  = "transparent"),
            plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
            legend.title = element_text(size = 11, face = "bold"),
            legend.text = element_text(size = 11),
            legend.position = "right", 
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank())
    ggplot() + 
      geom_sf(data = ThreeState[ThreeState$STATEFP == 51, ], aes(fill = group2), color = "white") + 
      scale_fill_manual(values = c(PLASMA[2], PLASMA[3], PLASMA[4], PLASMA[5], PLASMA[6], PLASMA[7])) +
      theme(panel.background = element_rect(fill  = "transparent"),
            plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
            legend.title = element_text(size = 11, face = "bold"),
            legend.text = element_text(size = 11),
            legend.position = "right", 
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank())

<img src="irr_color_palette_examples_files/figure-markdown_strict/unnamed-chunk-4-1.png" width="50%" /><img src="irr_color_palette_examples_files/figure-markdown_strict/unnamed-chunk-4-2.png" width="50%" /><img src="irr_color_palette_examples_files/figure-markdown_strict/unnamed-chunk-4-3.png" width="50%" />

Viridis 10 Category
-------------------

    #Viridis Color Palette option-VIRIDIS
    #https://www.thinkingondata.com/something-about-viridis-library/
    #https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html  

    VIRIDIS<-viridis_pal()(12)  
    VIRIDIS <- VIRIDIS[2:11]

    P4<-rep(NA, length=length(ThreeState$IRR))
      #P4[which(ThreeState$IRR<0.1)]=VIRIDIS[1]
      P4[which(ThreeState$IRR>=0.1 & ThreeState$IRR < 0.2)]=VIRIDIS[2]
      P4[which(ThreeState$IRR>=0.2 & ThreeState$IRR < 0.3)]=VIRIDIS[3]
      P4[which(ThreeState$IRR>=0.3 & ThreeState$IRR < 0.4)]=VIRIDIS[4]
      P4[which(ThreeState$IRR>=0.4 & ThreeState$IRR < 0.5)]=VIRIDIS[5]
      P4[which(ThreeState$IRR>=0.5 & ThreeState$IRR < 0.6)]=VIRIDIS[6]
      P4[which(ThreeState$IRR>=0.6 & ThreeState$IRR < 0.7)]=VIRIDIS[7]
      P4[which(ThreeState$IRR>=0.7 & ThreeState$IRR < 0.8)]=VIRIDIS[8]
      P4[which(ThreeState$IRR>=0.8 & ThreeState$IRR < 0.9)]=VIRIDIS[9]
      P4[which(ThreeState$IRR>=0.9)]=VIRIDIS[10]

    ggplot() + 
      geom_sf(data = ThreeState[ThreeState$STATEFP == 19, ], aes(fill = group2), color = "white") + 
      scale_fill_manual(values = c(VIRIDIS[4], VIRIDIS[5], VIRIDIS[6])) +
      theme(panel.background = element_rect(fill  = "transparent"),
            plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
            legend.title = element_text(size = 11, face = "bold"),
            legend.text = element_text(size = 11),
            legend.position = "right", 
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank())
    ggplot() + 
      geom_sf(data = ThreeState[ThreeState$STATEFP == 41, ], aes(fill = group2), color = "white") + 
      scale_fill_manual(values = c(VIRIDIS[3], VIRIDIS[4], VIRIDIS[5], VIRIDIS[6], VIRIDIS[7])) +
      theme(panel.background = element_rect(fill  = "transparent"),
            plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
            legend.title = element_text(size = 11, face = "bold"),
            legend.text = element_text(size = 11),
            legend.position = "right", 
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank())
    ggplot() + 
      geom_sf(data = ThreeState[ThreeState$STATEFP == 51, ], aes(fill = group2), color = "white") + 
      scale_fill_manual(values = c(VIRIDIS[2], VIRIDIS[3], VIRIDIS[4], VIRIDIS[5], VIRIDIS[6], VIRIDIS[7])) +
      theme(panel.background = element_rect(fill  = "transparent"),
            plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
            legend.title = element_text(size = 11, face = "bold"),
            legend.text = element_text(size = 11),
            legend.position = "right", 
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank())

<img src="irr_color_palette_examples_files/figure-markdown_strict/unnamed-chunk-5-1.png" width="50%" /><img src="irr_color_palette_examples_files/figure-markdown_strict/unnamed-chunk-5-2.png" width="50%" /><img src="irr_color_palette_examples_files/figure-markdown_strict/unnamed-chunk-5-3.png" width="50%" />

### Color Blind Palette

    cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

    #Assign colors to each the IRR

    P1<-rep(NA, length=length(ThreeState$IRR))
      P1[which(ThreeState$IRR<0.2)]=cbPalette[6]
      P1[which(ThreeState$IRR>=0.2 & ThreeState$IRR<0.4)]=cbPalette[3]
      P1[which(ThreeState$IRR>=0.4 & ThreeState$IRR<0.6)]=cbPalette[1]
      P1[which(ThreeState$IRR>=0.6 & ThreeState$IRR<0.8)]=cbPalette[2]
      P1[which(ThreeState$IRR>=0.8)]=cbPalette[7]


    ggplot() + 
      geom_sf(data = ThreeState[ThreeState$STATEFP == 19, ], aes(fill = group)) + 
      scale_fill_manual(values = c( cbPalette[3], cbPalette[1])) +
      theme(panel.background = element_rect(fill  = "transparent"),
            plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
            legend.title = element_text(size = 11, face = "bold"),
            legend.text = element_text(size = 11),
            legend.position = "right", 
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank())
    ggplot() + 
      geom_sf(data = ThreeState[ThreeState$STATEFP == 41, ], aes(fill = group)) + 
      scale_fill_manual(values = c( cbPalette[3], cbPalette[1], cbPalette[2])) +
      theme(panel.background = element_rect(fill  = "transparent"),
            plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
            legend.title = element_text(size = 11, face = "bold"),
            legend.text = element_text(size = 11),
            legend.position = "right", 
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank())

    ggplot() + 
      geom_sf(data = ThreeState[ThreeState$STATEFP == 51, ], aes(fill = group)) + 
      scale_fill_manual(values = c(cbPalette[6], cbPalette[3], cbPalette[1], cbPalette[2])) +
      theme(panel.background = element_rect(fill  = "transparent"),
            plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
            legend.title = element_text(size = 11, face = "bold"),
            legend.text = element_text(size = 11),
            legend.position = "right", 
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank())

<img src="irr_color_palette_examples_files/figure-markdown_strict/unnamed-chunk-6-1.png" width="50%" /><img src="irr_color_palette_examples_files/figure-markdown_strict/unnamed-chunk-6-2.png" width="50%" /><img src="irr_color_palette_examples_files/figure-markdown_strict/unnamed-chunk-6-3.png" width="50%" />

### Evo Palette

    #EvoPalette
    #http://gradientdescending.com/evolve-new-colour-palettes-in-r-with-evopalette/ 
    #palette_box()$paralabrax_clathratus

    P2<-rep(NA, length=length(ThreeState$IRR))
      P2[which(ThreeState$IRR<0.2)]="#D6CEA2"
      P2[which(ThreeState$IRR>=0.2 & ThreeState$IRR<0.4)]="#B9B27C"
      P2[which(ThreeState$IRR>=0.4 & ThreeState$IRR<0.6)]="#97965F"
      P2[which(ThreeState$IRR>=0.6 & ThreeState$IRR<0.8)]="#717341"
      P2[which(ThreeState$IRR>=0.8)]="#43431D"

    ggplot() + 
      geom_sf(data = ThreeState[ThreeState$STATEFP == 19, ], aes(fill = group)) + 
      scale_fill_manual(values = c("#B9B27C", "#97965F")) +
      theme(panel.background = element_rect(fill  = "transparent"),
            plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
            legend.title = element_text(size = 11, face = "bold"),
            legend.text = element_text(size = 11),
            legend.position = "right", 
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank())

    ggplot() + 
      geom_sf(data = ThreeState[ThreeState$STATEFP == 41, ], aes(fill = group)) + 
      scale_fill_manual(values = c("#B9B27C", "#97965F", "#717341")) +
      theme(panel.background = element_rect(fill  = "transparent"),
            plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
            legend.title = element_text(size = 11, face = "bold"),
            legend.text = element_text(size = 11),
            legend.position = "right", 
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank())
    ggplot() + 
      geom_sf(data = ThreeState[ThreeState$STATEFP == 51, ], aes(fill = group)) + 
      scale_fill_manual(values = c("#D6CEA2", "#B9B27C", "#97965F", "#717341")) +
      theme(panel.background = element_rect(fill  = "transparent"),
            plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
            legend.title = element_text(size = 11, face = "bold"),
            legend.text = element_text(size = 11),
            legend.position = "right", 
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank())

<img src="irr_color_palette_examples_files/figure-markdown_strict/unnamed-chunk-7-1.png" width="50%" /><img src="irr_color_palette_examples_files/figure-markdown_strict/unnamed-chunk-7-2.png" width="50%" /><img src="irr_color_palette_examples_files/figure-markdown_strict/unnamed-chunk-7-3.png" width="50%" />

### Evo Palette 2

    #EvoPalette
    #http://gradientdescending.com/evolve-new-colour-palettes-in-r-with-evopalette/ 
    #palette_box()$marc_chagall

    P3<-rep(NA, length=length(ThreeState$IRR))
      P3[which(ThreeState$IRR<0.2)]="#D88744"
      P3[which(ThreeState$IRR>=0.2 & ThreeState$IRR<0.4)]="#D6B352"
      P3[which(ThreeState$IRR>=0.4 & ThreeState$IRR<0.6)]="#A07D78"
      P3[which(ThreeState$IRR>=0.6 & ThreeState$IRR<0.8)]="#60A8BC"
      P3[which(ThreeState$IRR>=0.8)]="#3F6F76"
      



    ggplot() + 
      geom_sf(data = ThreeState[ThreeState$STATEFP == 19, ], aes(fill = group)) + 
      scale_fill_manual(values = c("#D6B352", "#A07D78")) +
      theme(panel.background = element_rect(fill  = "transparent"),
            plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
            legend.title = element_text(size = 11, face = "bold"),
            legend.text = element_text(size = 11),
            legend.position = "right", 
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank())

    ggplot() + 
      geom_sf(data = ThreeState[ThreeState$STATEFP == 41, ], aes(fill = group)) + 
      scale_fill_manual(values = c("#D6B352", "#A07D78", "#60A8BC")) +
      theme(panel.background = element_rect(fill  = "transparent"),
            plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
            legend.title = element_text(size = 11, face = "bold"),
            legend.text = element_text(size = 11),
            legend.position = "right", 
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank())


    ggplot() + 
      geom_sf(data = ThreeState[ThreeState$STATEFP == 51, ], aes(fill = group)) + 
      scale_fill_manual(values = c("#D88744", "#D6B352", "#A07D78", "#60A8BC")) +
      theme(panel.background = element_rect(fill  = "transparent"),
            plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
            legend.title = element_text(size = 11, face = "bold"),
            legend.text = element_text(size = 11),
            legend.position = "right", 
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank())

<img src="irr_color_palette_examples_files/figure-markdown_strict/unnamed-chunk-8-1.png" width="50%" /><img src="irr_color_palette_examples_files/figure-markdown_strict/unnamed-chunk-8-2.png" width="50%" /><img src="irr_color_palette_examples_files/figure-markdown_strict/unnamed-chunk-8-3.png" width="50%" />

### Viridis Plasma

    #Viridis Color Palette option=PLASMA
    #https://www.thinkingondata.com/something-about-viridis-library/
    library(scales)
    library(viridis)
    PLASMA<-viridis_pal(option="plasma")(7)  

    P4<-rep(NA, length=length(ThreeState$IRR))
      P4[which(ThreeState$IRR<0.2)]=PLASMA[2]
      P4[which(ThreeState$IRR>=0.2 & ThreeState$IRR<0.4)]=PLASMA[3]
      P4[which(ThreeState$IRR>=0.4 & ThreeState$IRR<0.6)]=PLASMA[4]
      P4[which(ThreeState$IRR>=0.6 & ThreeState$IRR<0.8)]=PLASMA[5]
      P4[which(ThreeState$IRR>=0.8)]=PLASMA[6]
      
    ggplot() + 
      geom_sf(data = ThreeState[ThreeState$STATEFP == 19, ], aes(fill = group), color = "white") + 
      scale_fill_manual(values = c(PLASMA[3], PLASMA[4])) +
      theme(panel.background = element_rect(fill  = "transparent"),
            plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
            legend.title = element_text(size = 11, face = "bold"),
            legend.text = element_text(size = 11),
            legend.position = "right", 
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank())
    ggplot() + 
      geom_sf(data = ThreeState[ThreeState$STATEFP == 41, ], aes(fill = group), color = "white") + 
      scale_fill_manual(values = c(PLASMA[3], PLASMA[4], PLASMA[5])) +
      theme(panel.background = element_rect(fill  = "transparent"),
            plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
            legend.title = element_text(size = 11, face = "bold"),
            legend.text = element_text(size = 11),
            legend.position = "right", 
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank())
    ggplot() + 
      geom_sf(data = ThreeState[ThreeState$STATEFP == 51, ], aes(fill = group), color = "white") + 
      scale_fill_manual(values = c(PLASMA[2], PLASMA[3], PLASMA[4], PLASMA[5])) +
      theme(panel.background = element_rect(fill  = "transparent"),
            plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
            legend.title = element_text(size = 11, face = "bold"),
            legend.text = element_text(size = 11),
            legend.position = "right", 
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank())

<img src="irr_color_palette_examples_files/figure-markdown_strict/unnamed-chunk-9-1.png" width="50%" /><img src="irr_color_palette_examples_files/figure-markdown_strict/unnamed-chunk-9-2.png" width="50%" /><img src="irr_color_palette_examples_files/figure-markdown_strict/unnamed-chunk-9-3.png" width="50%" />

### Viridis

    #Viridis Color Palette option-VIRIDIS
    #https://www.thinkingondata.com/something-about-viridis-library/
    #https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html  

    VIRIDIS<-viridis_pal()(7)  

    P5<-rep(NA, length=length(ThreeState$IRR))
      P5[which(ThreeState$IRR<0.2)]=VIRIDIS[2]
      P5[which(ThreeState$IRR>=0.2 & ThreeState$IRR<0.4)]=VIRIDIS[3]
      P5[which(ThreeState$IRR>=0.4 & ThreeState$IRR<0.6)]=VIRIDIS[4]
      P5[which(ThreeState$IRR>=0.6 & ThreeState$IRR<0.8)]=VIRIDIS[5]
      P5[which(ThreeState$IRR>=0.8)]=VIRIDIS[6] 

    ggplot() + 
      geom_sf(data = ThreeState[ThreeState$STATEFP == 19, ], aes(fill = group), color = "white") + 
      scale_fill_manual(values = c(VIRIDIS[3], VIRIDIS[4])) +
      theme(panel.background = element_rect(fill  = "transparent"),
            plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
            legend.title = element_text(size = 11, face = "bold"),
            legend.text = element_text(size = 11),
            legend.position = "right", 
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank())
    ggplot() + 
      geom_sf(data = ThreeState[ThreeState$STATEFP == 41, ], aes(fill = group), color = "white") + 
      scale_fill_manual(values = c(VIRIDIS[3], VIRIDIS[4], VIRIDIS[5])) +
      theme(panel.background = element_rect(fill  = "transparent"),
            plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
            legend.title = element_text(size = 11, face = "bold"),
            legend.text = element_text(size = 11),
            legend.position = "right", 
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank())
    ggplot() + 
      geom_sf(data = ThreeState[ThreeState$STATEFP == 51, ], aes(fill = group), color = "white") + 
      scale_fill_manual(values = c(VIRIDIS[2], VIRIDIS[3], VIRIDIS[4], VIRIDIS[5])) +
      theme(panel.background = element_rect(fill  = "transparent"),
            plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
            legend.title = element_text(size = 11, face = "bold"),
            legend.text = element_text(size = 11),
            legend.position = "right", 
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank())

<img src="irr_color_palette_examples_files/figure-markdown_strict/unnamed-chunk-10-1.png" width="50%" /><img src="irr_color_palette_examples_files/figure-markdown_strict/unnamed-chunk-10-2.png" width="50%" /><img src="irr_color_palette_examples_files/figure-markdown_strict/unnamed-chunk-10-3.png" width="50%" />

### Color Picker

    CP<-c("#2B2509", "#493F16", "#695B23", "#8B7931", "#AE983F", "#D3B94E")
    P6<-rep(NA, length=length(ThreeState$IRR))
      P6[which(ThreeState$IRR<0.2)]=CP[1]
      P6[which(ThreeState$IRR>=0.2 & ThreeState$IRR<0.4)]=CP[2]
      P6[which(ThreeState$IRR>=0.4 & ThreeState$IRR<0.6)]=CP[3]
      P6[which(ThreeState$IRR>=0.6 & ThreeState$IRR<0.8)]=CP[4]
      P6[which(ThreeState$IRR>=0.8)]=CP[5]   

    ggplot() + 
      geom_sf(data = ThreeState[ThreeState$STATEFP == 19, ], aes(fill = group), color = "white") + 
      scale_fill_manual(values = c(CP[2], CP[3])) +
      theme(panel.background = element_rect(fill  = "transparent"),
            plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
            legend.title = element_text(size = 11, face = "bold"),
            legend.text = element_text(size = 11),
            legend.position = "right", 
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank())
    ggplot() + 
      geom_sf(data = ThreeState[ThreeState$STATEFP == 41, ], aes(fill = group), color = "white") + 
      scale_fill_manual(values = c(CP[2], CP[3], CP[4])) +
      theme(panel.background = element_rect(fill  = "transparent"),
            plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
            legend.title = element_text(size = 11, face = "bold"),
            legend.text = element_text(size = 11),
            legend.position = "right", 
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank())

    ggplot() + 
      geom_sf(data = ThreeState[ThreeState$STATEFP == 51, ], aes(fill = group), color = "white") + 
      scale_fill_manual(values = c(CP[1], CP[2], CP[3], CP[4])) +
      theme(panel.background = element_rect(fill  = "transparent"),
            plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
            legend.title = element_text(size = 11, face = "bold"),
            legend.text = element_text(size = 11),
            legend.position = "right", 
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank()) 

<img src="irr_color_palette_examples_files/figure-markdown_strict/unnamed-chunk-11-1.png" width="50%" /><img src="irr_color_palette_examples_files/figure-markdown_strict/unnamed-chunk-11-2.png" width="50%" /><img src="irr_color_palette_examples_files/figure-markdown_strict/unnamed-chunk-11-3.png" width="50%" />

Coolors

    #Coolors for iOS 
    #https://coolors.co/232d4b-2c4f6b-0e879c-60999a-d1e0bf-d9e12b-e6ce3a-e6a01d-e57200-fdfdfd
    CO1<-c("#e5e8b6", "#b4c4ae", "#a2abab", "#7d869c", "#586994")
    P7<-rep(NA, length=length(ThreeState$IRR))
      P7[which(ThreeState$IRR<0.2)]=CO1[1]
      P7[which(ThreeState$IRR>=0.2 & ThreeState$IRR<0.4)]=CO1[2]
      P7[which(ThreeState$IRR>=0.4 & ThreeState$IRR<0.6)]=CO1[3]
      P7[which(ThreeState$IRR>=0.6 & ThreeState$IRR<0.8)]=CO1[4]
      P7[which(ThreeState$IRR>=0.8)]=CO1[5]    
      
    ggplot() + 
      geom_sf(data = ThreeState[ThreeState$STATEFP == 19, ], aes(fill = group), color = "white") + 
      scale_fill_manual(values = c(CO1[2], CO1[3])) +
      theme(panel.background = element_rect(fill  = "transparent"),
            plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
            legend.title = element_text(size = 11, face = "bold"),
            legend.text = element_text(size = 11),
            legend.position = "right", 
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank())
    ggplot() + 
      geom_sf(data = ThreeState[ThreeState$STATEFP == 41, ], aes(fill = group), color = "white") + 
      scale_fill_manual(values = c(CO1[2], CO1[3], CO1[4])) +
      theme(panel.background = element_rect(fill  = "transparent"),
            plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
            legend.title = element_text(size = 11, face = "bold"),
            legend.text = element_text(size = 11),
            legend.position = "right", 
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank())

    ggplot() + 
      geom_sf(data = ThreeState[ThreeState$STATEFP == 51, ], aes(fill = group), color = "white") + 
      scale_fill_manual(values = c(CO1[1], CO1[2], CO1[3], CO1[4])) +
      theme(panel.background = element_rect(fill  = "transparent"),
            plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
            legend.title = element_text(size = 11, face = "bold"),
            legend.text = element_text(size = 11),
            legend.position = "right", 
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank())   

<img src="irr_color_palette_examples_files/figure-markdown_strict/unnamed-chunk-12-1.png" width="50%" /><img src="irr_color_palette_examples_files/figure-markdown_strict/unnamed-chunk-12-2.png" width="50%" /><img src="irr_color_palette_examples_files/figure-markdown_strict/unnamed-chunk-12-3.png" width="50%" />

Coolors

    #Coolors for iOS 
    #https://coolors.co/232d4b-2c4f6b-0e879c-60999a-d1e0bf-d9e12b-e6ce3a-e6a01d-e57200-fdfdfd
    CO2<-c("#e7e393", "#f4c95d", "#dd7230", "#854d27", "#2e1f27")
    P8<-rep(NA, length=length(ThreeState$IRR))
      P8[which(ThreeState$IRR<0.2)]=CO2[1]
      P8[which(ThreeState$IRR>=0.2 & ThreeState$IRR<0.4)]=CO2[2]
      P8[which(ThreeState$IRR>=0.4 & ThreeState$IRR<0.6)]=CO2[3]
      P8[which(ThreeState$IRR>=0.6 & ThreeState$IRR<0.8)]=CO2[4]
      P8[which(ThreeState$IRR>=0.8)]=CO2[5]  
      
    ggplot() + 
      geom_sf(data = ThreeState[ThreeState$STATEFP == 19, ], aes(fill = group), color = "white") + 
      scale_fill_manual(values = c(CO2[2], CO2[3])) +
      theme(panel.background = element_rect(fill  = "transparent"),
            plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
            legend.title = element_text(size = 11, face = "bold"),
            legend.text = element_text(size = 11),
            legend.position = "right", 
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank())
    ggplot() + 
      geom_sf(data = ThreeState[ThreeState$STATEFP == 41, ], aes(fill = group), color = "white") + 
      scale_fill_manual(values = c(CO2[2], CO2[3], CO2[4])) +
      theme(panel.background = element_rect(fill  = "transparent"),
            plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
            legend.title = element_text(size = 11, face = "bold"),
            legend.text = element_text(size = 11),
            legend.position = "right", 
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank())

    ggplot() + 
      geom_sf(data = ThreeState[ThreeState$STATEFP == 51, ], aes(fill = group), color = "white") + 
      scale_fill_manual(values = c(CO2[1], CO2[2], CO2[3], CO2[4])) +
      theme(panel.background = element_rect(fill  = "transparent"),
            plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
            legend.title = element_text(size = 11, face = "bold"),
            legend.text = element_text(size = 11),
            legend.position = "right", 
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank()) 

<img src="irr_color_palette_examples_files/figure-markdown_strict/unnamed-chunk-13-1.png" width="50%" /><img src="irr_color_palette_examples_files/figure-markdown_strict/unnamed-chunk-13-2.png" width="50%" /><img src="irr_color_palette_examples_files/figure-markdown_strict/unnamed-chunk-13-3.png" width="50%" />