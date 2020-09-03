library(tidycensus)
library(acs) 
library(dplyr)
library(sf) 
library(ggplot2)
library(ggthemes)
library(viridis)
library(scales)
library(naniar)
library(readr)

# variable list 
acs_variable_list <- load_variables(2017, "acs5", cache = TRUE)
# https://data.census.gov/cedsci/ 
census_api_key(Sys.getenv("CENSUS_API_KEY"))

tables <- c(
  "B01001", # age  and sex
  "B02001"  # sex/gender and race/ethnicity 
) 

# Get tables for counties --------------------------------------------------------------------------------------

# Iowa (FIPS 19)
dataia <- get_acs(geography = "county", state = 19, table = tables[1], year = 2018, survey = "acs5", 
                  cache_table = TRUE, output = "wide", geometry = TRUE, keep_geo_vars = TRUE)
for(i in 2:length(tables)){
  tmp <- get_acs(geography = "county", state = 19, table = tables[i],year = 2018, survey = "acs5", 
                 cache_table = TRUE, output = "wide", geometry = FALSE)
  dataia <- left_join(dataia, tmp)
}
remove(tmp)

# Oregon (FIPS 41)
dataor <- get_acs(geography = "county", state = 41, table = tables[1], year = 2018, survey = "acs5", 
                  cache_table = TRUE, output = "wide", geometry = TRUE, keep_geo_vars = TRUE)
for(i in 2:length(tables)){
  tmp <- get_acs(geography = "county", state = 41, table = tables[i],year = 2018, survey = "acs5", 
                 cache_table = TRUE, output = "wide", geometry = FALSE)
  dataor <- left_join(dataor, tmp)
}
remove(tmp)

# Virginia (FIPS 51)
datava <- get_acs(geography = "county", state = 51, table = tables[1], year = 2018, survey = "acs5", 
                  cache_table = TRUE, output = "wide", geometry = TRUE, keep_geo_vars = TRUE)
for(i in 2:length(tables)){
  tmp <- get_acs(geography = "county", state = 51, table = tables[i],year = 2018, survey = "acs5", 
                 cache_table = TRUE, output = "wide", geometry = FALSE)
  datava <- left_join(datava, tmp)
}
remove(tmp)

# Join
demo <- rbind(dataia, dataor, datava)

demo <- demo %>% transmute(
  STATEFP = STATEFP, 
  COUNTYFP = COUNTYFP, 
  COUNTYNS = COUNTYNS, 
  AFFGEOID = AFFGEOID, 
  GEOID = GEOID, 
  LSAD = LSAD, 
  NAME.x = NAME.x, 
  NAME.y = NAME.y,
  NAME = NAME,
  ALAND = ALAND, 
  AWATER = AWATER, 
  pct_male = B01001_002E / B01001_001E * 100,
  pct_female = B01001_026E / B01001_001E * 100,
  pct_under_5 = (B01001_003E + B01001_027E) / B01001_001E * 100,
  pct_5_to_9 = (B01001_004E + B01001_028E) / B01001_001E * 100,
  pct_10_to_14 = (B01001_005E + B01001_029E) / B01001_001E * 100,
  pct_15_to_17 = (B01001_006E + B01001_030E) / B01001_001E * 100,
  pct_18_to_19 = (B01001_007E + B01001_031E) / B01001_001E * 100,
  pct_20 = (B01001_008E + B01001_032E) / B01001_001E * 100,
  pct_21 = (B01001_009E + B01001_033E) / B01001_001E * 100,
  pct_22_to_24 = (B01001_010E + B01001_034E) / B01001_001E * 100,
  pct_25_to_29 = (B01001_011E + B01001_035E) / B01001_001E * 100,
  pct_30_to_34 = (B01001_012E + B01001_036E) / B01001_001E * 100,
  pct_35_to_39 = (B01001_013E + B01001_037E) / B01001_001E * 100,
  pct_40_to_44 = (B01001_014E + B01001_038E) / B01001_001E * 100,
  pct_45_to_49 = (B01001_015E + B01001_039E) / B01001_001E * 100,
  pct_50_to_54 = (B01001_016E + B01001_040E) / B01001_001E * 100,
  pct_55_to_59 = (B01001_017E + B01001_041E) / B01001_001E * 100,
  pct_60_to_61 = (B01001_018E + B01001_042E) / B01001_001E * 100,
  pct_62_to_64 = (B01001_019E + B01001_043E) / B01001_001E * 100,
  pct_65_to_66 = (B01001_020E + B01001_044E) / B01001_001E * 100,
  pct_67_to_69 = (B01001_021E + B01001_045E) / B01001_001E * 100,
  pct_70_to_74 = (B01001_022E + B01001_046E) / B01001_001E * 100,
  pct_75_to_79 = (B01001_023E + B01001_047E) / B01001_001E * 100,
  pct_80_to_84 = (B01001_024E + B01001_048E) / B01001_001E * 100,
  pct_over_85 = (B01001_025E + B01001_049E) / B01001_001E * 100,
  pct_white = B02001_002E / B02001_001E * 100,
  pct_black = B02001_003E / B02001_001E * 100,
  pct_native_am = B02001_004E / B02001_001E * 100,
  pct_asian = B02001_005E / B02001_001E * 100,
  pct_pacific_isl = B02001_006E / B02001_001E * 100,
  pct_other = B02001_007E / B02001_001E * 100,
  pct_multi = B02001_008E / B02001_001E * 100,
  geometry = geometry
)














