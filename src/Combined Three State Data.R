#construct the data set for the three state summary
library(readxl)
library(stringr)

#Rurality Index
IRR<-read_excel("~/git/CLD3/DATA/IRR_2010.xlsx")
  IRR$State<-word(IRR$County, -1); 
  IRR<-IRR[(IRR$State=="Iowa" | IRR$State=="Oregon" | IRR$State=="Virginia"),]
  #Get rid of West Virginia 
  IRR<-IRR[(IRR$FIPS < 54000),] #(269 rows)
  IRR$Type<-ifelse(grepl("city", IRR$County)==TRUE, "City", "County")
  names(IRR)<-c("GEOID","Location","IRR","State","County"); dim(IRR)
  #Bedford is no longer an indpendent city in 2013 it returned to a town within Bedford 
  IRR<-IRR[-which(IRR[,1]=="51515"), ]; dim(IRR)
  
 #############################################################################
 #Social Capital Rate: Voting Fraction
 #Data downloaded from MIT Election Data + Science Lab
 #https://electionlab.mit.edu/
  library(readxl)
  SC <- read_excel("~git/CLD3/DATA/election-context-2018.xlsx", 
                   col_types = c("text", "text", "text", 
                                 "numeric", "numeric", "blank", "blank", 
                                 "blank", "blank", "blank", "blank", 
                                 "blank", "blank", "blank", "blank", 
                                 "blank", "blank", "blank", "blank", 
                                 "blank", "blank", "blank", "numeric", 
                                 "blank", "blank", "blank", "blank", 
                                 "blank", "blank", "blank", "blank", 
                                 "blank", "blank", "blank", "blank", 
                                 "blank", "blank", "blank", "blank"))
 dim(SC)
 SC<-SC[-which(SC[,3]=="51515"),c(1:5,23) ]; dim(SC)
 #Calculate fraction of voters in 2016 election
 Num<-(SC$trump16+SC$clinton16)
 Den<-SC$cvap
 Rate<-Num/Den
 temp<-data.frame(GEOID=SC$fips, GOTV=Rate)
 IRR<-merge(temp, IRR, by="GEOID"); dim(IRR)
 rm(temp)
 
 #############################################################################
 #High School Graduation Rate
 #ACS 5-YR 2018 
 #High school graduation rate diploma+GED 18-24 year olds
 #Calculated as: ((B15001_003-B15001_004-B15001_005)+(B15001_044-B15001_045-B15001_046))/
 #(B15001_003+B15001_044) 
 HSvars<-
   c(
     #Female 18-24 less than 9th grade
     "B15001_045",
     #Male 18-24 less than 9th grade
     "B15001_004",
     #Female 18-24 9th to 12th grade, no diploma
     "B15001_046",
     #Male 18-24 9th to 12th grade, no diploma
     "B15001_005",
     #Total males 18-24
     "B15001_003",
     #Total females 18-24
     "B15001_044"
   )
 HS<-tidycensus::get_acs(geography="county", state=c(19, 41, 51), variables=HSvars, year=2018, survey="acs5",
                         cache_table=TRUE, output="wide", geometry=FALSE, keep_geo_vars=FALSE)
 Num<-(HS$B15001_003E-HS$B15001_004E-HS$B15001_005E)+(HS$B15001_044E-HS$B15001_045E-HS$B15001_046E)
 Den<-(HS$B15001_003E+HS$B15001_044E)
 Rate<-Num/Den
 temp<-data.frame(GEOID=as.numeric(as.character(HS$GEOID)), HS=Rate)
 IRR<-merge(temp, IRR, by="GEOID"); dim(IRR)
 rm(temp)
 
 #############################################################################
 #Fraction of Single Parent Families
 #ACS 5-YR 2018 
 #Calculated as: (SP$B11003_016E+SP$B11003_010E)/(SP$B11003_003E+SP$B11003_016E+SP$B11003_010E)
 SPvars<-
   c(
     #Female householder, no husband present, with own children of the householder under 18 years 
     "B11003_016",
     #Male householder, no wife present, with own children of the householder under 18 years 
     "B11003_010",
     #Total Married-couple family with own children of the householder under 18 years 
     "B11003_003"
   )
 SP<-tidycensus::get_acs(geography="county", state=c(19, 41, 51), variables=SPvars, year=2018, survey="acs5",
                         cache_table=TRUE, output="wide", geometry=FALSE, keep_geo_vars=FALSE)
 dim(SP)
 Num<-(SP$B11003_016E+SP$B11003_010E)
 Den<-(SP$B11003_003E+SP$B11003_016E+SP$B11003_010E)
 Rate<-Num/Den
 temp<-data.frame(GEOID=as.numeric(as.character(SP$GEOID)), SP=Rate)
 IRR<-merge(temp, IRR, by="GEOID"); dim(IRR)
 rm(temp)
 
 #############################################################################
 #Fraction of Commutes < 15 Minutes
 #ACS 5-YR 2018 Table B08303
 #Percentage with commute times <15 minutes
 #Calculated as: ((B08303_002E+B08303_003E+B08303_0034)/B08303_001E)
 CT<-read_excel("~/git/CLD3/DATA/ACSDT5Y2018.B08303_commute_times_2020-08-16T113321.xlsx")

 dim(CT)
 temp<-CT[,c(1,3)]
 temp$GEOID<-substring(CT$id, 10, 14); temp<-temp[,c(2,3)]; names(temp)<-c("CT","GEOID")
 temp$GEOID<-as.numeric(as.character(SP$GEOID))
 IRR<-merge(temp, IRR, by="GEOID"); dim(IRR)
 rm(temp)
 
 #############################################################################
 #Fraction of Non-whites
 #ACS 5-YR 2018 
 #Calculated as: (NW$C02003_001E-NW$C02003_003E)/(NW$C02003_001E)
 NHWvars<-
   c(
     #Total population of one race White
     "C02003_003",
     #Total population
     "C02003_001"
   )
 NHW<-tidycensus::get_acs(geography="county", state=c(19, 41, 51), variables=NHWvars, year=2018, survey="acs5",
                          cache_table=TRUE, output="wide", geometry=FALSE, keep_geo_vars=FALSE)
 View(NHW)
 Num<-(NHW$C02003_003E)
 Den<-(NHW$C02003_001E)
 Rate<-Num/Den
 temp<-data.frame(GEOID=NHW$GEOID, NHW=Rate)
 temp$GEOID<-as.numeric(as.character(NHW$GEOID))
 IRR<-merge(temp, IRR, by="GEOID"); dim(IRR)
 rm(temp)
 
 View(IRR)
 