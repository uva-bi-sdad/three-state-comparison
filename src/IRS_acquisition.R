library(dplyr)

# County to County Outflow:
download.file("https://www.irs.gov/pub/irs-soi/countyoutflow1718.csv", destfile = "data/IRS/countyoutflow1718.csv")

# County to County Inflow:
download.file("https://www.irs.gov/pub/irs-soi/countyinflow1718.csv", destfile = "data/IRS/countyinflow1718.csv")


# Outflows
outflow <- read.csv("data/IRS/countyoutflow1718.csv")

# Outflow from OR, IA, VA to other locations
# Outflow state origin is variable y1_statefips

three_state_outflow <- outflow %>%
  filter((y1_statefips == 19|y1_statefips == 41|y1_statefips == 51) & 
           (grepl("^[A-z].* (County|city) Total Migration-US and Foreign$", y2_countyname)|
                                 grepl("^[A-z].* (County|city) Non-migrants$", y2_countyname) ))

#write.csv(three_state_outflow, "data/IRS/three_state_outflow.csv", row.names = F)

inflow <- read.csv("data/IRS/countyinflow1718.csv")

# inflow to OR, IA, VA from other locations
# inflow state destination is variable y2_statefips

three_state_inflow <- inflow %>%
  filter((y2_statefips == 19|y2_statefips == 41|y2_statefips == 51) & 
           (grepl("^[A-z].* (County|city) Total Migration-US and Foreign$", y1_countyname)|
                                   grepl("^[A-z].* (County|city) Non-migrants$", y1_countyname) ))

#write.csv(three_state_inflow, "data/IRS/three_state_inflow.csv", row.names = F)


# combined data table with better column names
three_state_outflow<- read.csv("data/IRS/three_state_outflow.csv")
three_state_inflow<- read.csv("data/IRS/three_state_inflow.csv")

three_state_outflow <- three_state_outflow %>%
  select(y1_statefips, y1_countyfips, y2_countyname, n1, n2, agi) %>%
  rename(state_fips = y1_statefips, county_fips = y1_countyfips, name = y2_countyname, n1_out = n1, n2_out = n2, agi_out = agi)

three_state_inflow <- three_state_inflow %>%
  select(y2_statefips, y2_countyfips, y1_countyname, n1, n2, agi) %>%
  rename(state_fips = y2_statefips, county_fips = y2_countyfips, name = y1_countyname, n1_in = n1, n2_in = n2, agi_in = agi)



three_state_combined <- merge(three_state_outflow, three_state_inflow, by = c("state_fips", "county_fips", "name"))

write.csv(three_state_combined, "data/IRS/three_state_combined.csv", row.names = F)





