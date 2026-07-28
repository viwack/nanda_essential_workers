# Making big dataset and filtering by state at the end

library(leaflet)
library(shiny)
#library(bslib)
library(readr)
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
# ten_major_cities <- read_sf("aggregate_city_boundaries/aggregate_city_boundaries.shp")
#all census tracts, ignoring whether they have eworker data
census_tracts <- read_sf("cb_2020_us_tract_500k/cb_2020_us_tract_500k.shp")

#census tract essential workers data
ew_census <- read.table("38974-0001-Data.tsv", sep = "\t", header = TRUE, colClasses = c("TRACT_FIPS20" = "character"))
ew_census$TRACT_FIPS20 <- as.character(ew_census$TRACT_FIPS20) #make the tract character type

#breakdowns of all eworker data BY OCCUPATION TYPE (14 per ACLU)
acs_dta <- read.csv("ew_components - Copy.csv", 
                    colClasses = c("tract_fips20" = "character"))

cmap_aclu_cols <- c("c24010e16",
                    "c24010e52",
                    "c24010e32",
                    "c24010e68",
                    "c24010e31",
                    "c24010e67",
                    "c24010e33",
                    "c24010e69",
                    "c24010e37",
                    "c24010e73",
                    "c24010e35",
                    "c24010e71",
                    "c24010e36",
                    "c24010e72",
                    "c24010e29",
                    "c24010e65",
                    "c24010e28",
                    "c24010e64",
                    "c24010e25",
                    "c24010e61",
                    "c24010e24",
                    "c24010e60",
                    "c24010e20",
                    "c24010e56",
                    "c24010e26",
                    "c24010e62",
                    "c24010e21",
                    "c24010e57")

acs_dta <- acs_dta %>%
  select(tract_fips20, totpop,c24010e1, all_of(cmap_aclu_cols))

occupation_num_cols <- c("health_pract",
                         "construction",
                         "farmfishforest",
                         "maintenance",
                         "moving",
                         "production",
                         "transportation",
                         "office_admin",
                         "sales",
                         "building_grounds",
                         "food_prep_serve",
                         "health_support",
                         "personal_care",
                         "protective_service")

acs_dta <- acs_dta %>%
  mutate(
    health_pract = c24010e16 + c24010e52,
    construction = c24010e32 + c24010e68,
    farmfishforest = c24010e31 + c24010e67,
    maintenance = c24010e33 + c24010e69,
    moving = c24010e37 + c24010e73,
    production = c24010e35 + c24010e71,
    transportation = c24010e36 + c24010e72,
    office_admin = c24010e29 + c24010e65,
    sales = c24010e28 + c24010e64,
    building_grounds = c24010e25 + c24010e61,
    food_prep_serve = c24010e24 + c24010e60,
    health_support = c24010e20 + c24010e56,
    personal_care = c24010e26 + c24010e62,
    protective_service = c24010e21 + c24010e57,
  ) %>%
  select(tract_fips20, totpop,c24010e1, all_of(occupation_num_cols)) %>%
  mutate(tract_fips20 = as.character(tract_fips20))

ew_census <- ew_census %>%
  left_join(acs_dta, by = c("TRACT_FIPS20" = "tract_fips20"))

#all data merged from sociodemo and eworker onto the geodata for all census tracts
all_data <- ew_census %>%
  left_join(socio_ct, by = "TRACT_FIPS20")

df <- all_data %>%
  select(TRACT_FIPS20,
         WRKR,
         EWRKR_TOT,
         EWRKR_PROP,
         all_of(occupation_num_cols),
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
         RATIO_MEDFAMINC_NHWTOHISP16_20)

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

state_fips <- c("01" = "AL", "02" = "AK", "04" = "AZ", "05" = "AR", "06" = "CA",
                "08" = "CO", "09" = "CT", "10" = "DE", "11" = "DC", "12" = "FL",
                "13" = "GA", "15" = "HI", "16" = "ID", "17" = "IL", "18" = "IN",
                "19" = "IA", "20" = "KS", "21" = "KY", "22" = "LA", "23" = "ME",
                "24" = "MD", "25" = "MA", "26" = "MI", "27" = "MN", "28" = "MS",
                "29" = "MO", "30" = "MT", "31" = "NE", "32" = "NV", "33" = "NH",
                "34" = "NJ", "35" = "NM", "36" = "NY", "37" = "NC", "38" = "ND",
                "39" = "OH", "40" = "OK", "41" = "OR", "42" = "PA", "44" = "RI",
                "45" = "SC", "46" = "SD", "47" = "TN", "48" = "TX", "49" = "UT",
                "50" = "VT", "51" = "VA", "53" = "WA", "54" = "WV", "55" = "WI",
                "56" = "WY", "60" = "AS", "66" = "GU", "69" = "MP", "72" = "PR", "78" = "VI")

state_region <- c(
  "AL" = "South",       "AK" = "West",        "AZ" = "West",       "AR" = "South",
  "CA" = "West",        "CO" = "West",        "CT" = "Northeast",  "DE" = "South",
  "DC" = "South",       "FL" = "South",       "GA" = "South",      "HI" = "West",
  "ID" = "West",        "IL" = "Midwest",     "IN" = "Midwest",    "IA" = "Midwest",
  "KS" = "Midwest",     "KY" = "South",       "LA" = "South",      "ME" = "Northeast",
  "MD" = "South",       "MA" = "Northeast",   "MI" = "Midwest",    "MN" = "Midwest",
  "MS" = "South",       "MO" = "Midwest",     "MT" = "West",       "NE" = "Midwest",
  "NV" = "West",        "NH" = "Northeast",   "NJ" = "Northeast",  "NM" = "West",
  "NY" = "Northeast",   "NC" = "South",       "ND" = "Midwest",    "OH" = "Midwest",
  "OK" = "South",       "OR" = "West",        "PA" = "Northeast",  "RI" = "Northeast",
  "SC" = "South",       "SD" = "Midwest",     "TN" = "South",      "TX" = "South",
  "UT" = "West",        "VT" = "Northeast",   "VA" = "South",      "WA" = "West",
  "WV" = "South",       "WI" = "Midwest",     "WY" = "West",       "AS" = "Territory",
  "GU" = "Territory",   "MP" = "Territory",   "PR" = "Territory",  "VI" = "Territory"
)

# Add a column with the state abbreviation
df <- df %>%
  mutate(State = state_fips[as.character(substr(TRACT_FIPS20, 1, 2))])

df_medians <- df %>%
  group_by(State) %>%
  summarise(median = median(EWRKR_PROP, na.rm = TRUE))

# Calculate standard deviation of the proportions
df_sds <- df %>%
  group_by(State) %>%
  summarise(sd = sd(EWRKR_PROP, na.rm = TRUE))

# Calculate coefficient of variation using median as the central measure
df_cov <- df_medians %>%
  left_join(df_sds, by = "State") %>%
  mutate(cov = sd / median)


#Per capita and by deciles

# Step 1: Compute deciles within each state
deciles <- df %>%
  group_by(State) %>%
  mutate(EWRKR_decile = ntile(EWRKR_PROP, 10)) %>%
  ungroup()

# Step 2: Compute summary statistics for each state-decile group
decile_stats <- deciles %>%
  group_by(State, EWRKR_decile) %>%
  summarise(
    mean_proportion = mean(EWRKR_PROP, na.rm = TRUE),
    median_proportion = median(EWRKR_PROP, na.rm = TRUE),
    sd_proportion = sd(EWRKR_PROP, na.rm = TRUE),
    cv_proportion = sd_proportion / mean_proportion, # Coefficient of Variation
    n = n(),  # Sample size per decile per state
    se = sd_proportion / sqrt(n), # Standard Error
    ci_95 = 1.96 * se  # 95% Confidence Interval
  ) %>%
  ungroup()

# Step 3: Merge statistics with geospatial data for visualization
deciles_geo <- deciles %>%
  select(TRACT_FIPS20, State, EWRKR_PROP, EWRKR_TOT, EWRKR_decile) %>%
  left_join(census_tracts, by = c("TRACT_FIPS20" = "GEOID")) %>%
  st_as_sf()

# Step 4: Visualize the decile distribution with ggplot2
ggplot(data = deciles_geo) +
  geom_sf(aes(fill = factor(EWRKR_decile)), color = NA) +
  scale_fill_viridis_d(name = "Decile") + 
  theme_minimal() +
  labs(title = "Essential Workers per Capita by Census Tract Deciles (Per State)",
       subtitle = "Decile distribution within each state",
       caption = "Source: Your Data Source")

# Step 5: Print the decile-level summary statistics for reference
decile_stats %>%
  filter(!is.na(EWRKR_decile)) %>%
  arrange(desc(cv_proportion))




# Calculate deciles for essential workers per capita, IGNORING STATE-BY-STATE
deciles_ignore_state <- df %>%
  mutate(EWRKR_decile = ntile(EWRKR_PROP, 10))

deciles_geo_ntl <- deciles_ignore_state %>%
  left_join(census_tracts, by = c("TRACT_FIPS20" = "GEOID")) %>%
  st_as_sf()

decile_stats_ntnl <- deciles_ignore_state %>%
  group_by(EWRKR_decile) %>%
  summarise(
    mean_proportion = mean(EWRKR_PROP, na.rm = TRUE),
    median_proportion = median(EWRKR_PROP, na.rm = TRUE),
    sd_proportion = sd(EWRKR_PROP, na.rm = TRUE),
    cv_proportion = sd_proportion / mean_proportion, # Coefficient of Variation
    n = n(),  # Sample size per decile per state
    se = sd_proportion / sqrt(n), # Standard Error
    ci_95 = 1.96 * se  # 95% Confidence Interval
  ) %>%
  ungroup()

decile_stats_ntnl %>%
  filter(!is.na(EWRKR_decile)) %>%
  arrange(desc(cv_proportion))

dec_1 <- deciles_geo_ntl %>%
  filter(EWRKR_decile == 1)

# 
#   ggplot() + geom_sf(aes(fill = factor(EWRKR_decile)), color = NA) +
#     theme_minimal() + theme(legend.position = "none")
#     labs(title = "Essential Workers per Capita by Census Tract Deciles (NATIONAL)",
#          subtitle = "Decile distribution across census tracts",
#          caption = "Source: NaNDA Essential Workers Dataset")
#   

# Why are these counties in decile 1 so significantly more heterogenous in terms of essential
# worker proportions across the United States? This was specifically when ignoring the state.
# Note: the size of this decile was n = 8438

leaflet() %>%
  addTiles() %>%
  setView(-95.0382679, 42.3489054, zoom = 3) %>%
  addPolygons(
    data = dec_1,
    weight = 1,
    opacity = 1,
    color = "green",
    fillOpacity = 0.4,
    label = ~paste0("Region: ", NAMELSADCO),
    labelOptions = labelOptions(
      style = list("color" = "black", "font-size" = "12px"),
      textOnly = TRUE
    ))

# # Example assuming 'df' has a geometry column for spatial data, such as from a simple features object
# ggplot(data = deciles_geo) +
#   geom_sf(aes(fill = factor(EWRKR_decile)), color = NA) +
#   scale_fill_viridis_d(name = "Decile") + # Or another color scale that you prefer
#   theme_minimal() +
#   labs(title = "Essential Workers per Capita by Census Tract Deciles",
#        subtitle = "Decile distribution across census tracts",
#        caption = "Source: Your Data Source")
