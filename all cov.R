# States Summaries ----
library(tidycensus)
library(sf)
library(dplyr)
library(tigris)
library(RColorBrewer)
library(leaflet)
library(scales)
library(viridis)
library(ggplot2)
library(stringr)

## Results: Each State and their Median/Mean/CV/SD ----
state_list <- c(state.abb, "DC", "AS", "GU", "MP", "PR", "VI") # AS: American Samoa, GU: Guam, MP: Northern Mariana Islands, PR: Puerto Rico, VI: Virgin Islands

results <- data.frame(State = character(), 
                      Median = numeric(), 
                      SD = numeric(), 
                      Mean = numeric(), 
                      CV = numeric(),
                      CV_Lower = numeric(),
                      CV_Upper = numeric(),
                      stringsAsFactors = FALSE)
# rbinds a row for each state with their geoinfo and their ewrkr info
for (state_code in state_list) {
  tracts <- get_acs(geography = "tract",
                    variables = "B19013_001",
                    state = state_code,
                    year = 2021,
                    geometry = TRUE) %>%
    dplyr::select(GEOID, geometry)
  
  state_data <- tracts %>%
    left_join(df, by = c("GEOID" = "TRACT_FIPS20")) %>%
    st_as_sf()
  
  state_data_no_na <- state_data %>%
    filter(!is.na(EWRKR_PROP))
  
  if (nrow(state_data_no_na) > 1) {
    median_propew <- median(state_data_no_na$EWRKR_PROP, na.rm = TRUE)
    mean_propew <- mean(state_data_no_na$EWRKR_PROP, na.rm = TRUE)
    sd_proportion <- sd(state_data_no_na$EWRKR_PROP, na.rm = TRUE)
    n <- nrow(state_data_no_na)
    
    cv_proportion <- sd_proportion / mean_propew
    
    # Approximate standard error of CV
    cv_se <- (1 / sqrt(2 * n)) * cv_proportion
    
    # 95% Confidence Interval using normal approximation
    z <- 1.96
    cv_lower <- cv_proportion - z * cv_se
    cv_upper <- cv_proportion + z * cv_se
    
    results <- rbind(results, data.frame(State = state_code, 
                                         Median = median_propew, 
                                         SD = sd_proportion,
                                         Mean = mean_propew,
                                         CV = cv_proportion,
                                         CV_Lower = cv_lower,
                                         CV_Upper = cv_upper))
  } else {
    results <- rbind(results, data.frame(State = state_code, 
                                         Median = NA, 
                                         SD = NA,
                                         Mean = NA,
                                         CV = NA,
                                         CV_Lower = NA,
                                         CV_Upper = NA))
  }
}


#view results
ggplot(results, aes(x = reorder(State, CV), y = CV)) +
  geom_point(color = "steelblue", size = 3) +
  geom_errorbar(aes(ymin = CV_Lower, ymax = CV_Upper), width = 0.2, color = "gray40") +
  coord_flip() +
  labs(title = "Coefficient of Variation (CV) of EWRKR_PROP by State",
       x = "State",
       y = "Coefficient of Variation (CV)") +
  theme_minimal()

# Print result table
results %>%
  arrange(desc(CV))

## Mississippi ----
ms <- df %>%
  filter(State == "MS")

miss_metrics <- tracts(state = "MS", year = 2020, cb = TRUE, class = "sf")

ms <- ms %>%
  left_join(miss_metrics, by = c("TRACT_FIPS20" = "GEOID")) %>%
  st_as_sf()

ms_places <- places(state = "MS", year = 2020, cb = TRUE, class = "sf")

ms_geo <- st_join(ms, ms_places) %>%
  distinct(TRACT_FIPS20,.keep_all = TRUE)


jackson <- ms_geo %>%
  filter(NAMELSAD.y == "Jackson city")

pal <- colorNumeric(
  palette = "YlOrRd",  # You can change the palette (e.g., "Blues", "Greens", etc.)
  domain = ms_geo$EWRKR_PROP
)

occ_sums_jackson <- data.frame(
  Occupation = occupation_num_cols,
  Total = colSums(jackson_nosf[occupation_num_cols], na.rm = TRUE)
)

occ_sums_jackson <- occ_sums_jackson %>%
  mutate(sum = sum(Total)) %>%
  mutate(proportion = Total/sum) %>%
  arrange(desc(proportion))

ggplot(occupation_sums_df, 
       aes(
         x = "", 
         y = proportion, 
         fill = reorder(Occupation, -proportion)  # Sort in descending order
       )) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(
    aes(label = paste0(round(100 * proportion, 3), "%")), 
    position = position_stack(vjust = 0.5), 
    size = 4, 
    colour = "white"
  ) +
  scale_fill_viridis_d(option = "D") +
  theme_void() +
  labs(
    title = "Breakdown of Essential Occupation Types - US", 
    fill = "Occupation"
  )





leaflet() %>%
  addTiles() %>%
  setView(-95.0382679, 42.3489054, zoom = 3) %>%
  addPolygons(
    data = ms_geo,
    fillColor = ~pal(EWRKR_PROP),
    weight = 1,
    opacity = 1,
    color = "white",      # Border color
    fillOpacity = 0.7,
    label = ~paste0("Tract ID: ", TRACT_FIPS20),
    highlight = highlightOptions(
      weight = 2,
      color = "#666",
      fillOpacity = 0.9,
      bringToFront = TRUE
    )
  ) %>%
  addLegend(
    pal = pal,
    values = ms_geo$EWRKR_PROP,
    title = "EWRKR_PROP",
    position = "bottomright"
  )

### Tract_28075000401 ----
tract_28075000401 <- df %>%
  filter(TRACT_FIPS20 == "28075000401")

# Occupation labels for the proportion of each occupation type
occupation_labels <- c(
  health_pract_p = "Health Practitioners",
  construction_p = "Construction",
  farmfishforest_p = "Farm/Fish/Forest",
  maintenance_p = "Maintenance",
  moving_p = "Moving",
  production_p = "Production",
  transportation_p = "Transportation",
  office_admin_p = "Office/Administrative",
  sales_p = "Sales",
  building_grounds_p = "Building and Grounds",
  food_prep_serve_p = "Food Services",
  health_support_p = "Health Support",
  personal_care_p = "Personal Services",
  protective_service_p = "Protective Services"
)

long_28075000401 <- tract_28075000401 %>%
  pivot_longer(
    cols = all_of(names(occupation_labels)),
    names_to = "occupation",
    values_to = "proportion"
  ) %>%
  filter(proportion > 0) %>%
  mutate(
    label_name = occupation_labels[occupation],
    label = paste0(percent(proportion, accuracy = 0.1))
  ) %>%
  arrange(desc(proportion)) %>%
  mutate(
    cumulative = cumsum(proportion),
    midpoint = cumulative - proportion / 2,
    label_y = midpoint
  )

#Pie Chart for MS Tract
ggplot(long_28075000401, aes(x = "", y = proportion, fill = label_name)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(round(100 * proportion, 1), "%")), 
            position = position_stack(vjust = 0.5), size = 5, colour = "white") +
  scale_fill_viridis_d(option = "D") +
  theme_void() +
  labs(title = "Occupation Proportions for CT 28075000401 - Jackson, MS", fill = "Occupation")

#Reg Bar Chart
ggplot(long_28075000401, aes(x = label_name, y = proportion, fill = label_name)) +
  geom_bar(stat = "identity", color = "white") +
  geom_text(aes(label = label), vjust = -0.5, color = "black", size = 3.2, lineheight = 0.9) +
  scale_fill_viridis_d(option = "D") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "CT28075000401 Occupation Types", x = "Occupation", y = "Proportion of Essential Workers", fill = "Occupation")

## DC ----
dc <- df %>%
  filter(State == "DC") %>%
  left_join(census_tracts, by = c("TRACT_FIPS20" = "GEOID")) %>%
  st_as_sf()


pal_dc <- colorNumeric(
  palette = "YlOrRd",  # You can change the palette (e.g., "Blues", "Greens", etc.)
  domain = dc$EWRKR_PROP
)


leaflet() %>%
  addTiles() %>%
  setView(-95.0382679, 42.3489054, zoom = 3) %>%
  addPolygons(
    data = dc,
    fillColor = ~pal_dc(EWRKR_PROP),
    weight = 1,
    opacity = 1,
    color = "white",      # Border color
    fillOpacity = 0.7,
    label = ~paste0("Tract ID: ", TRACT_FIPS20),
    highlight = highlightOptions(
      weight = 2,
      color = "#666",
      fillOpacity = 0.9,
      bringToFront = TRUE
    )
  ) %>%
  addLegend(
    pal = pal_dc,
    values = dc$EWRKR_PROP,
    title = "EWRKR_PROP",
    position = "bottomright"
  )

### Tract_11001004704 ----
tract_11001004704 <- dc%>%
  filter(TRACT_FIPS20 == "11001004704")

long_11001004704 <- tract_11001004704 %>%
  pivot_longer(
    cols = all_of(names(occupation_labels)),
    names_to = "occupation",
    values_to = "proportion"
  ) %>%
  filter(proportion > 0) %>%
  mutate(
    label_name = occupation_labels[occupation],
    label = paste0(percent(proportion, accuracy = 0.1))
  ) %>%
  arrange(desc(proportion)) %>%
  mutate(
    cumulative = cumsum(proportion),
    midpoint = cumulative - proportion / 2,
    label_y = midpoint
  )

ggplot(long_11001004704, aes(x = "", y = proportion, fill = label_name)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(round(100 * proportion, 1), "%")), 
            position = position_stack(vjust = 0.5), size = 5, colour = "white") +
  scale_fill_viridis_d(option = "D") +
  theme_void() +
  labs(title = "Occupation Proportions for CT 11001004704 - Washington, DC", fill = "Occupation")

#Reg Bar Chart
ggplot(long_11001004704, aes(x = label_name, y = proportion, fill = label_name)) +
  geom_bar(stat = "identity", color = "white") +
  geom_text(aes(label = label), vjust = -0.5, color = "black", size = 3.2, lineheight = 0.9) +
  scale_fill_viridis_d(option = "D") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "CT11001004704 Occupation Types", x = "Occupation", y = "Proportion of Essential Workers", fill = "Occupation")




## US ----
us <- df %>% left_join(census_tracts, by = c("TRACT_FIPS20" = "GEOID")) %>% st_as_sf()
us <- us %>% mutate(region = recode(State, !!!state_region))

us %>%
  group_by(region) %>%
  summarise(Mean = mean(EWRKR_PROP), SD = sd(EWRKR_PROP), CV = sd(EWRKR_PROP)/mean(EWRKR_PROP))

pal_us <- colorBin(
  palette = "YlOrRd",
  domain = us$EWRKR_PROP,
  bins = c(0, 0.4, 0.6, 0.7, 0.75, 0.8, 0.9, 1),
  na.color = "grey"
)

pal_us <- colorBin(
  palette = "YlOrRd", 
  domain = us$EWRKR_PROP,
  bins = c(0, 0.4, 0.6, 0.7, 0.75, 0.8, 0.9, 1),
  na.color = "grey"
)

leaflet() %>%
  addTiles() %>%
  setView(-95.0382679, 42.3489054, zoom = 3) %>%
  addPolygons(
    data = us,
    fillColor = ~pal_us(EWRKR_PROP),
    weight = 1,
    opacity = 0.8,
    color = ~pal_us(EWRKR_PROP),      # Border color
    fillOpacity = 0.7
  ) %>%
  addLegend(
    pal = pal_us,
    values = us$EWRKR_PROP,
    title = "Prop. Essential Workers",
    position = "bottomright"
  )

### US by Region ----
south <- us %>% filter(region == "South")
west <- us %>% filter(region == "West")
northeast <- us %>% filter(region == "Northeast")
midwest <- us %>% filter(region == "Midwest")
territory <- us %>% filter(region == "Territory")

leaflet() %>%
  addTiles() %>%
  setView(-95.0382679, 42.3489054, zoom = 5) %>%
  addPolygons(
    data = west,
    fillColor = ~pal_us(EWRKR_PROP),
    weight = 1,
    opacity = 0.8,
    color = ~pal_us(EWRKR_PROP),      # Border color
    fillOpacity = 0.7
  ) %>%
  addLegend(
    pal = pal_us,
    values = west$EWRKR_PROP,
    title = "EWRKR_PROP",
    position = "bottomright"
  )

# us_states <- states(year = 2021)
# > results %>%
#   + 
#   > us_means <- results %>%
#   + select(State, Mean) %>%
#   + left_join(us_states, by = c("State" = "STUSPS"))

pal_us_aggregate <- colorNumeric(
  palette = "YlOrRd",
  domain = us_means$Mean
)

leaflet() %>%
  addTiles() %>%
  setView(-95.0382679, 42.3489054, zoom = 4) %>%
  addPolygons(
    data = us_means,
    fillColor = ~pal_us_aggregate(Mean),
    weight = 0.5,
    opacity = 0.5,
    color = "white",
    fillOpacity = 0.7
  ) %>%
  addLegend(
    pal = pal_us_aggregate,
    values = us_means$Mean,
    title = "Average Proportion",
    position = "bottomright"
  )

## Density Plot US ALL ----
ggplot(df, aes(x = EWRKR_PROP)) +
  geom_density(fill = "lightblue", alpha = 0.6) +
  labs(
    title = "Density of Essential Worker Proportions by Census Tract",
    x = "Proportion of Essential Workers") +
  theme_minimal()



## 100% EWRKR Tracts ----
hundred <- us %>%
  filter(EWRKR_PROP == 1)

leaflet() %>%
  addTiles() %>%
  setView(-95.0382679, 42.3489054, zoom = 3) %>%
  addPolygons(
    data = hundred,
    fillColor = ~pal_us(EWRKR_PROP),
    weight = 0.1,
    opacity = 0.5,
    color = "lightgrey",      # Border color
    fillOpacity = 0.7
  ) %>%
  addLegend(
    pal = pal_us,
    values = hundred$EWRKR_PROP,
    title = "EWRKR_PROP",
    position = "bottomright"
  )


missing_ewrkrprop <- df %>%
  group_by(State) %>%
  summarize(missing_nas = sum(is.na(EWRKR_PROP))) %>%
  arrange(desc(missing_nas))

# sanity check to make sure the tracts with NAs are actually population-less
df %>%
  filter(is.na(EWRKR_PROP)) %>%
  filter(TOTPOP20 != 0) %>%
  dplyr::select(TRACT_FIPS20, State, EWRKR_TOT, WRKR, TOTPOP20) %>%
  group_by(State) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))


us_places <- us %>%
  st_join(places, left = TRUE) %>%
  distinct(TRACT_FIPS20, .keep_all = TRUE)


## Cities ----
los_angeles <- us_places %>%
  filter(grepl("los angeles", NAMELSAD.y, ignore.case = TRUE))

nyc <- us_places %>%
  filter(grepl("new york city", NAMELSAD.y, ignore.case = TRUE))

atlanta <- us_places %>%
  filter(grepl("atlanta", NAMELSAD.y, ignore.case = TRUE))

chicago <- us_places %>%
  filter(grepl("chicago", NAMELSAD.y, ignore.case = TRUE))

detroit <- us_places %>%
  filter(grepl("detroit", NAMELSAD.y, ignore.case = TRUE))

dc <- us %>%
  filter(State == "DC")

leaflet() %>%
  addTiles() %>%
  setView(-95.0382679, 42.3489054, zoom = 5) %>%
  addPolygons(
    data = detroit,
    fillColor = ~pal_us(EWRKR_PROP),
    weight = 1,
    opacity = 0.8,
    color = ~pal_us(EWRKR_PROP),      # Border color
    fillOpacity = 0.7
  ) %>%
  addLegend(
    pal = pal_us,
    values = detroit$EWRKR_PROP,
    title = "EWRKR_PROP",
    position = "bottomright"
  )


occupation_sums_df <- data.frame(
  Occupation = occupation_num_cols,
  Total = colSums(df[occupation_num_cols], na.rm = TRUE)
)
  
occupation_sums_df <- occupation_sums_df %>%
  mutate(sum = sum(Total)) %>%
  mutate(proportion = Total/sum)

ggplot(occupation_sums_df, 
       aes(
         x = "", 
         y = proportion, 
         fill = reorder(Occupation, -proportion)  # Sort in descending order
       )) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(
    aes(label = paste0(round(100 * proportion, 3), "%")), 
    position = position_stack(vjust = 0.5), 
    size = 4, 
    colour = "white"
  ) +
  scale_fill_viridis_d(option = "D") +
  theme_void() +
  labs(
    title = "Breakdown of Essential Occupation Types - US", 
    fill = "Occupation"
  )
  


ggplot(occupation_sums_df, 
       aes(
         x = "", 
         y = proportion, 
         fill = Occupation
       )) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  # Add labels outside wedges with occupation names
  geom_text(
    aes(
      label = paste0(Occupation_clean, " (", round(100 * proportion, 3), "%)"),
      x = 1.7  # Push labels outward
    ),
    position = position_stack(vjust = 0.5),
    size = 3.5,  # Adjust size as needed
    color = "black",
    check_overlap = TRUE  # Avoid overlapping text
  ) +
  scale_fill_viridis_d(option = "C") +
  theme_void() +
  labs(title = "Breakdown of Essential Occupation Types - US") +
  theme(legend.position = "none")  # Remove legend


# First, create a named vector for clean labels
clean_names <- c(
  health_pract = "Healthcare practitioners and technical",
  construction = "Construction and extraction",
  farmfishforest = "Farming, fishing, and forestry",
  maintenance = "Installation, maintenance, and repair",
  moving = "Material moving",
  production = "Production",
  transportation = "Transportation",
  office_admin = "Office and administrative support",
  sales = "Sales and related",
  building_grounds = "Building and grounds cleaning",
  food_prep_serve = "Food preparation and serving",
  health_support = "Healthcare support",
  personal_care = "Personal care and service",
  protective_service = "Protective service"
)

# Apply clean names to the dataframe
occupation_sums_df <- occupation_sums_df %>%
  mutate(Occupation_clean = clean_names[Occupation])

occupation_sums_df <- occupation_sums_df %>%
  mutate(
    Occupation_clean = clean_names[Occupation],
    ymax = cumsum(proportion),
    ymin = lag(ymax, default = 0),
    ypos = (ymin + ymax)/2  # Middle of each wedge
  )

# Plot
ggplot(occupation_sums_df, 
       aes(x = 1, y = proportion, fill = Occupation_clean)) +
  geom_bar(stat = "identity", width = 1, color = "white", linewidth = 0.3) +
  coord_polar(theta = "y") +
  # Labels (outside wedges)
  geom_text(
    aes(
      x = 1.7,
      y = ypos,
      label = paste0(str_wrap(Occupation_clean, width = 20), "\n", round(100 * proportion, 1), "%")
    ),
    size = 3,
    color = "black",
    lineheight = 0.8
  ) +
  # Leader lines (now works with explicit y positions)
  geom_segment(
    aes(
      x = 1.1,
      xend = 1.6,
      y = ypos,
      yend = ypos
    ),
    color = "gray60",
    linewidth = 0.2
  ) +
  scale_fill_viridis_d(option = "D") +
  theme_void() +
  labs(title = "Breakdown of Essential Occupation Types - US") +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
  ) +
  xlim(0.5, 2)




mean_ewrkr <- mean(ma$EWRKR_PROP, na.rm = TRUE)
se_ewrkr <- sd(ma$EWRKR_PROP, na.rm = TRUE) / sqrt(sum(!is.na(ma$EWRKR_PROP)))

# 95% confidence interval
lower <- mean_ewrkr - 1.96 * se_ewrkr
upper <- mean_ewrkr + 1.96 * se_ewrkr

c(lower, upper)
