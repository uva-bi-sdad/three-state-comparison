library(dplyr)

# County to County Outflow:
download.file("https://www.irs.gov/pub/irs-soi/countyoutflow1718.csv", destfile = "data/IRS/countyoutflow1718.csv")

# County to County Inflow:
download.file("https://www.irs.gov/pub/irs-soi/countyinflow1718.csv", destfile = "data/IRS/countyinflow1718.csv")


# Outflows
outflow <- read.csv("data/IRS/countyoutflow1718.csv")

# Outflow from OR, IA, VA to other locations

three_state_outflow <- outflow %>%
  filter((y1_statefips == 19|y1_statefips == 41|y1_statefips == 51) & (grepl("^[A-z].* (County|city) Total Migration-US and Foreign$", y2_countyname)|
                                 grepl("^[A-z].* (County|city) Non-migrants$", y2_countyname) ))

#write.csv(three_state_outflow, "data/IRS/three_state_outflow.csv", row.names = F)

inflow <- read.csv("data/IRS/countyinflow1718.csv")

three_state_inflow <- inflow %>%
  filter((y2_statefips == 19|y2_statefips == 41|y2_statefips == 51) & (grepl("^[A-z].* (County|city) Total Migration-US and Foreign$", y1_countyname)|
                                   grepl("^[A-z].* (County|city) Non-migrants$", y1_countyname) ))

#write.csv(three_state_inflow, "data/IRS/three_state_inflow.csv", row.names = F)
