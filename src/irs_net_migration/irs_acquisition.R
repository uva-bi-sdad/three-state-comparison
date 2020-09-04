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


# net migration data frame

three_state_combined <- read.csv("data/IRS/three_state_combined.csv")


# this checks that the net in and net out for total non-migrants is equal
three_state_combined %>% filter(grepl("^[A-z].* (County|city) Non-migrants$", name)) %>%
  mutate(check = ifelse(n1_out == n1_in, T, F)) %>%
  summarize(sum(check == F))

# using non migrants as a proxy for "total population", we have net migration as
# following this definition data.un.org/Glossary.aspx?q=Net+migration+rate+(per+1%2C000+population)
# (in migrants - out migrants/ nonmigrants) *1000

net_migration <- three_state_combined %>%
  select(state_fips, county_fips, name, n1_out, n1_in) %>%
  mutate(county_fips = ifelse(nchar(county_fips) ==1, paste("00", county_fips, sep = ""), 
                              ifelse(nchar(county_fips) == 2, paste("0", county_fips, sep = ""), county_fips)), 
         state_fips = ifelse(nchar(state_fips) == 1, paste("0", state_fips, sep = ""), state_fips),
         fips = paste(state_fips, county_fips, sep = "")) %>%
  select(fips, name, n1_out, n1_in) %>% 
  filter(grepl("^[A-z].* (County|city) Total Migration-US and Foreign$", name)) %>% 
  mutate(three_state_combined %>% filter(grepl("^[A-z].* (County|city) Non-migrants$", name)) %>% select(n1_in) %>% rename(nonmigrants = n1_in),
         net_per_1000 = ((n1_in - n1_out)/nonmigrants)*1000) 

write.csv(net_migration, "data/IRS/net_migration.csv", row.names = F)

















