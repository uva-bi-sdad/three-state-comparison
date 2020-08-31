#Color Palette Ideas for the EM Dashboard
#Assign colors to the IRR [0.0-0.2), [0.2-0.4), [0.4-0.6), [0.6-0.8), [0.8-1.0]

#Color blind palette with grey
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
wheel <- function(col, radius = 1, ...)
  pie(rep(1, length(col)), col = col, radius = radius, ...)
wheel(cbPalette)
#Assign colors to each the IRR
P1<-rep(NA, length=length(ThreeState$IRR))
  P1[which(ThreeState$IRR<0.2)]=cbPalette[6]
  P1[which(ThreeState$IRR>=0.2 & ThreeState$IRR<0.4)]=cbPalette[3]
  P1[which(ThreeState$IRR>=0.4 & ThreeState$IRR<0.6)]=cbPalette[1]
  P1[which(ThreeState$IRR>=0.6 & ThreeState$IRR<0.8)]=cbPalette[2]
  P1[which(ThreeState$IRR>=0.8)]=cbPalette[7]

#EvoPalette
#http://gradientdescending.com/evolve-new-colour-palettes-in-r-with-evopalette/ 
devtools::install_github("doehm/evoPalette")  
launch_evo_palette()
#palette_box()$paralabrax_clathratus
#[1] "#E6EEE0" "#D6CEA2" "#B9B27C" "#97965F" "#717341" "#43431D"
P2<-rep(NA, length=length(ThreeState$IRR))
  P2[which(ThreeState$IRR<0.2)]="#D6CEA2"
  P2[which(ThreeState$IRR>=0.2 & ThreeState$IRR<0.4)]="#B9B27C"
  P2[which(ThreeState$IRR>=0.4 & ThreeState$IRR<0.6)]="#97965F"
  P2[which(ThreeState$IRR>=0.6 & ThreeState$IRR<0.8)]="#717341"
  P2[which(ThreeState$IRR>=0.8)]="#43431D"
  
#EvoPalette
#http://gradientdescending.com/evolve-new-colour-palettes-in-r-with-evopalette/ 
devtools::install_github("doehm/evoPalette")  
launch_evo_palette()
#palette_box()$marc_chagall
#[1] "#A07D78" "#D88744" "#D6B352" "#60A8BC" "#3F6F76" "#62496F"
P3<-rep(NA, length=length(ThreeState$IRR))
  P3[which(ThreeState$IRR<0.2)]="#D88744"
  P3[which(ThreeState$IRR>=0.2 & ThreeState$IRR<0.4)]="#D6B352"
  P3[which(ThreeState$IRR>=0.4 & ThreeState$IRR<0.6)]="#A07D78"
  P3[which(ThreeState$IRR>=0.6 & ThreeState$IRR<0.8)]="#60A8BC"
  P3[which(ThreeState$IRR>=0.8)]="#3F6F76"  
  
#Viridis Color Palette option=PLASMA
#https://www.thinkingondata.com/something-about-viridis-library/
library(scales)
library(viridis)
show_col(viridis_pal(option="plasma")(7)); PLASMA<-viridis_pal(option="plasma")(7)  
P4<-rep(NA, length=length(ThreeState$IRR))
  P4[which(ThreeState$IRR<0.2)]=PLASMA[2]
  P4[which(ThreeState$IRR>=0.2 & ThreeState$IRR<0.4)]=PLASMA[3]
  P4[which(ThreeState$IRR>=0.4 & ThreeState$IRR<0.6)]=PLASMA[4]
  P4[which(ThreeState$IRR>=0.6 & ThreeState$IRR<0.8)]=PLASMA[5]
  P4[which(ThreeState$IRR>=0.8)]=PLASMA[6]
  
#Viridis Color Palette option-VIRIDIS
#https://www.thinkingondata.com/something-about-viridis-library/
#https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html  
library(scales)
library(viridis)
show_col(viridis_pal()(7)); VIRIDIS<-viridis_pal()(7)  
P5<-rep(NA, length=length(ThreeState$IRR))
  P5[which(ThreeState$IRR<0.2)]=VIRIDIS[2]
  P5[which(ThreeState$IRR>=0.2 & ThreeState$IRR<0.4)]=VIRIDIS[3]
  P5[which(ThreeState$IRR>=0.4 & ThreeState$IRR<0.6)]=VIRIDIS[4]
  P5[which(ThreeState$IRR>=0.6 & ThreeState$IRR<0.8)]=VIRIDIS[5]
  P5[which(ThreeState$IRR>=0.8)]=VIRIDIS[6] 
  
#Colorpicker for data  
#http://tristen.ca/hcl-picker/#/hlc/6/1/2B2509/D3B94E
CP<-c("#2B2509", "#493F16", "#695B23", "#8B7931", "#AE983F", "#D3B94E")
P6<-rep(NA, length=length(ThreeState$IRR))
  P6[which(ThreeState$IRR<0.2)]=CP[1]
  P6[which(ThreeState$IRR>=0.2 & ThreeState$IRR<0.4)]=CP[2]
  P6[which(ThreeState$IRR>=0.4 & ThreeState$IRR<0.6)]=CP[3]
  P6[which(ThreeState$IRR>=0.6 & ThreeState$IRR<0.8)]=CP[4]
  P6[which(ThreeState$IRR>=0.8)]=CP[5]   
  
#Coolors for iOS 
#https://coolors.co/232d4b-2c4f6b-0e879c-60999a-d1e0bf-d9e12b-e6ce3a-e6a01d-e57200-fdfdfd
CO1<-c("#e5e8b6", "#b4c4ae", "#a2abab", "#7d869c", "#586994")
P7<-rep(NA, length=length(ThreeState$IRR))
  P7[which(ThreeState$IRR<0.2)]=CO1[1]
  P7[which(ThreeState$IRR>=0.2 & ThreeState$IRR<0.4)]=CO1[2]
  P7[which(ThreeState$IRR>=0.4 & ThreeState$IRR<0.6)]=CO1[3]
  P7[which(ThreeState$IRR>=0.6 & ThreeState$IRR<0.8)]=CO1[4]
  P7[which(ThreeState$IRR>=0.8)]=CO1[5]     

#Coolors for iOS 
#https://coolors.co/232d4b-2c4f6b-0e879c-60999a-d1e0bf-d9e12b-e6ce3a-e6a01d-e57200-fdfdfd
CO2<-c("#e7e393", "#f4c95d", "#dd7230", "#854d27", "#2e1f27")
P8<-rep(NA, length=length(ThreeState$IRR))
  P8[which(ThreeState$IRR<0.2)]=CO[1]
  P8[which(ThreeState$IRR>=0.2 & ThreeState$IRR<0.4)]=CO2[2]
  P8[which(ThreeState$IRR>=0.4 & ThreeState$IRR<0.6)]=CO2[3]
  P8[which(ThreeState$IRR>=0.6 & ThreeState$IRR<0.8)]=CO2[4]
  P8[which(ThreeState$IRR>=0.8)]=CO2[5]         