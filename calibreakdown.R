library(leaflet)
library(shiny)
#library(bslib)
#library(readr)
library(stringr)
library(sf)
#library(jsonlite)
library(dplyr)
library(data.table)
library(leaflet.extras)
library(geojsonio)
library(ggplot2)
library(tidycensus)
#library(scales)
#library(shinydashboard)
library(htmltools)
#library(mapview)
library(tigris)
#library(htmlwidgets)
#library(terra)
#library(mapdeck)

#file with all centers of the states
centers <- state_centers <- read.csv("region_centers.csv",header = TRUE)

#table with all socio demographic characteristics, 2016-2020
socio_ct <- read.table("ICPSR_38528-Socio_Demo/DS0004-16-20_CT/38528-0004-Data.tsv", sep = "\t", header = TRUE,colClasses = c("TRACT_FIPS20" = "character"))

#aggregate city boundaries for top ten most populated us cities
ten_major_cities <- read_sf("aggregate_city_boundaries/aggregate_city_boundaries.shp")
#all census tracts, ignoring whether they have eworker data
census_tracts <- read_sf("cb_2020_us_tract_500k/cb_2020_us_tract_500k.shp")

#census tract essential workers data
ew_census <- read.table("38974-0001-Data.tsv", sep = "\t", header = TRUE, colClasses = c("TRACT_FIPS20" = "character"))
ew_census$TRACT_FIPS20 <- as.character(ew_census$TRACT_FIPS20) #make the tract character type

#breakdowns of all eworker data BY OCCUPATION TYPE (14 per ACLU)
essential_breakdown <- read.csv("essential_occ_breakdown.csv",
                                colClasses = c("tract_fips20" = "character"))

census_api_key("435e35ce52f2a17b534b647f04e05ef832527479", install = TRUE,overwrite = TRUE)

cali_tracts <- get_acs(geography = "tract",
                       variables = "B19013_001",
                       state = "CA",
                       year = 2021,
                       geometry = TRUE) %>%
  select(GEOID, geometry)

cali_cities <- places(year = 2021, cb = TRUE) %>%
  filter(STATEFP == "06")

#all data merged from sociodemo and eworker onto the geodata for all census tracts
cali_data_merged <- ew_census %>%
  left_join(socio_ct, by = "TRACT_FIPS20") %>%
  left_join(essential_breakdown, by = c("TRACT_FIPS20" = "tract_fips20")) %>%
  filter(str_starts(TRACT_FIPS20, "06"))

california <- cali_data_merged %>%
  left_join(cali_tracts, by = c("TRACT_FIPS20" = "GEOID")) %>%
  st_as_sf()

california_cities <- california %>%
  st_join(cali_cities) %>%
  distinct(TRACT_FIPS20, .keep_all = TRUE) %>%
  select(-AFFGEOID, -GEOID, -LSAD, -ALAND, -AWATER)

df <- california_cities %>%
  select(STATEFP,
         TRACT_FIPS20,
         NAME,
         NAMELSAD,
         STATE_NAME,
         geometry,
         WRKR,
         EWRKR_TOT,
         EWRKR_PROP,
         TOTPOP20,
         POPDEN16_20,
         PHISPANIC16_20,
         PNHWHITE16_20,
         PNHBLACK16_20,
         PFBORN16_20,
         PLIMENG16_20,
         PED1_16_20,
         PED2_16_20,
         PED3_16_20,
         PFAMINCLT40K16_20,
         PFAMINCGE40LT75K16_20,
         PFAMINCGE75LT125K16_20,
         PFAMINCGE125K16_20,
         PNVMAR16_20,
         P18YR_16_20,
         P18_29_16_20,
         P30_39_16_20,
         P40_49_16_20,
         P50_69_16_20,
         PGE70_16_20,
         PUNEMP16_20,
         PPOV16_20,
         PPUBAS16_20,
         PFHFAM16_20,
         PSNGPNT16_20,
         POWNOC16_20,
         AFFLUENCE16_20,
         DISADVANTAGE16_20,
         HISPAN_FORBORN_LIMENG16_20,
         MEDFAMINC16_20,
         MEDFAMINC_NHWHITE16_20,
         MEDFAMINC_BLACK16_20,
         MEDFAMINC_HISPANIC16_20,
         RATIO_MEDFAMINC_NHWTOB16_20,
         RATIO_MEDFAMINC_NHWTOHISP16_20,
         c24010e16,
         c24010e20,
         c24010e21,
         c24010e24,
         c24010e25,
         c24010e26,
         c24010e28,
         c24010e29,
         c24010e31,
         c24010e32,
         c24010e33,
         c24010e35,
         c24010e36,
         c24010e37)

df <- df %>%
  rename(
    health_pract = c24010e16,
    construction = c24010e32,
    farmfishforest = c24010e31,
    maintenance = c24010e33,
    moving = c24010e37,
    production = c24010e35,
    transportation = c24010e36,
    office_admin = c24010e29,
    sales = c24010e28,
    building_grounds = c24010e25,
    food_prep_serve = c24010e24,
    health_support = c24010e20,
    personal_care = c24010e26,
    protective_service = c24010e21,
    CTNAME = NAMELSAD,
    MAJOR_CITY = NAME
  )

#add regions so that people can see their us census regions at a time in addition to individual states
df <- df %>%
  mutate(census_region = case_when(
    (STATE_NAME %in% c("Maine", "Vermont", "New Hampshire", "Massachusetts", "Connecticut", "New York", "Pennsylvania", "Rhode Island", "New Jersey", "Delaware", "Maryland")) ~ "Northeast",
    (STATE_NAME %in% c("Michigan", "Ohio", "Indiana", "Illinois", "Wisconsin", "Minnesota", "Iowa", "Missouri", "Kansas", "Nebraska", "South Dakota", "North Dakota")) ~ "Midwest",
    (STATE_NAME %in% c("Montana", "Wyoming", "Colorado", "Utah", "Nevada", "California", "Idaho", "Washington", "Oregon", "Alaska", "Hawaii")) ~ "West",
    (STATE_NAME %in% c("Arizona", "New Mexico", "Texas", "Oklahoma")) ~ "Southwest",
    (STATE_NAME %in% c("Arkansas", "Louisiana", "Mississippi", "Alabama", "Georgia", "South Carolina", "North Carolina", "Florida", "Virginia", "West Virginia", "Kentucky", "Tennessee", "District of Columbia")) ~ "Southeast",
    (STATE_NAME %in% c("Commonwealth of the Northern Mariana Islands", "Guam", "United States Virgin Islands", "Puerto Rico", "American Samoa")) ~ "U.S. Territories"
  ))

# Proportions for Los Angeles
df <- df %>%
  mutate(health_pract_p = health_pract / EWRKR_TOT,
         construction_p = construction / EWRKR_TOT,
         farmfishforest_p = farmfishforest / EWRKR_TOT,
         maintenance_p = maintenance / EWRKR_TOT,
         moving_p = moving / EWRKR_TOT,
         production_p = production / EWRKR_TOT,
         transportation_p = transportation / EWRKR_TOT,
         office_admin_p = office_admin / EWRKR_TOT,
         sales_p = sales / EWRKR_TOT,
         building_grounds_p = building_grounds / EWRKR_TOT,
         food_prep_serve_p = food_prep_serve / EWRKR_TOT,
         health_support_p = health_support / EWRKR_TOT,
         personal_care_p = personal_care / EWRKR_TOT,
         protective_service_p = protective_service / EWRKR_TOT)


occupation_cols <- c("health_pract_p",
                     "construction_p",
                     "farmfishforest_p",
                     "maintenance_p",
                     "moving_p",
                     "production_p",
                     "transportation_p",
                     "office_admin_p",
                     "sales_p",
                     "building_grounds_p",
                     "food_prep_serve_p",
                     "health_support_p",
                     "personal_care_p",
                     "protective_service_p")

col_grouping <- list(
  All = names(df),
  Age = c("P18YR_16_20", "P18_29_16_20", "P30_39_16_20", "P40_49_16_20", "P50_69_16_20", "PGE70_16_20"),
  Essential_Occupations = grep("_p$", names(df), value = TRUE),
  Education = c("PED1_16_20", "PED2_16_20", "PED3_16_20"),
  Population = c("TOTPOP20", "POPDEN16_20"),
  Workers = c("WRKR", "EWRKR_TOT", "EWRKR_PROP"),
  Income = c("PFAMINCLT40K16_20", "PFAMINCGE40LT75K16_20", "PFAMINCGE75LT125K16_20", "PFAMINCGE125K16_20"),
  Median_Family_Income = c("MEDFAMINC16_20", "MEDFAMINC_NHWHITE16_20", "MEDFAMINC_BLACK16_20", "MEDFAMINC_HISPANIC16_20")
)


#Calculating california's median case count per neighborhood (coefficient of variation (CV)).
df_removena <- df %>%
  filter(!is.na(EWRKR_PROP))

# Calculate median of the proportions
median_propew <- median(df$EWRKR_PROP,na.rm = TRUE)

# Calculate standard deviation of the proportions
sd_proportion <- sd(df$EWRKR_PROP,na.rm = TRUE)

# Calculate coefficient of variation using median as the central measure
cv_proportion <- sd_proportion / median_propew

print(cv_proportion)

# California's coefficient of variation is 0.2550823.
