### Clean Code

### Map and Line Graph Figure 1 (Obj 1.)

library(tidyverse)
library(ggplot2)
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(sf)
library(tigris)
library(raster)

iNaturalist_introduced <- read.csv("iNaturalist_introduced.csv")
eddmaps_introduced_clean <- read.csv("/Users/mario/Desktop/iNat-vs.-Eddmaps/Data/Data_MZ/eddmaps_introduced.csv")
# iNat cleanup
iNat <- readRDS("/Users/mario/Desktop/iNat-vs.-Eddmaps/Data/iNat_herp_data.RDS")
filtered_data_iNat <- iNat %>%
  filter(species %in% iNaturalist_introduced$scientific_name) %>%
  filter(!(species %in% "Anaxyrus fowleri")) %>%
  filter(!(species %in% "Desmognathus conanti")) %>%
  filter(!(species %in% "Pseudotriton ruber"))%>%
  filter(!(species %in% "Eurycea cirrigera"))%>%
  filter(!(species %in% "Eurycea guttolineata"))%>%
  filter(!(species %in% "Eretmochelys imbricata")) %>%
  filter(!(species %in% "Desmognathus conanti")) %>%
  filter(!(species %in% "Incilius nebulifer")) %>%
  filter(!(species %in% "Lampropeltis rhombomaculata")) %>%
  filter(!(species %in% "Lepidochelys kempii")) %>%
  filter(!(species %in% "Lithobates virgatipes"))

filtered_data_iNat <- filtered_data_iNat %>%
  dplyr::select(species, decimalLatitude, decimalLongitude, day, month, year) %>%
  filter(!is.na(decimalLatitude) & !is.na(decimalLongitude)) ### There are no instances of Lat or Long being NAs

# Rename columns for latitude and longitude
filtered_data_iNat <- filtered_data_iNat %>%
  rename(Latitude = decimalLatitude, Longitude = decimalLongitude)

filtered_data_iNat <- filtered_data_iNat %>%
  mutate(Year = as.numeric(year)) %>%  # Convert Year to numeric
  filter(Year >= 2010 & Year <= 2023) %>% # Filter for years 2010-2023
  mutate(source = "iNaturalist") %>%
  rename(Month = month, Day = day) %>%
  mutate(Month = as.numeric(Month), Day = as.numeric(Day), Year = as.numeric(Year))

### Plot a map to make sure all points are within Florida Boundary
filtered_data_iNat_florida <- filtered_data_iNat %>%
  filter(Latitude >= 24.396308 & Latitude <= 31.000888, 
         Longitude >= -87.634938 & Longitude <= -80.031362) %>%
  dplyr::select(-year)

#EDDMapS
eddmaps <- read.csv("/Users/mario/Desktop/iNat-vs.-Eddmaps/Data/EDDmapS_observations.csv") ### Need to clean up the Eddmaps data.

### Run to pull all introduced species from the raw eddmaps data to the cleaned species list we have made.
filtered_data_eddmaps <- eddmaps %>%
  filter(SciName %in% eddmaps_introduced_clean$scientific_name)

### Select the columns relavent to MCPs
filtered_data_eddmaps <- filtered_data_eddmaps %>%
  dplyr::select(SciName, ObsDate, Latitude, Longitude) %>%
  filter(!is.na(Latitude) & !is.na(Longitude))

filtered_data_eddmaps <- filtered_data_eddmaps %>%
  mutate(SciName = case_when(
    SciName == "Chelonoidis carbonaria" ~ "Chelonoidis carbonarius",
    SciName == "Leiocephalus carinatus armouri" ~ "Leiocephalus carinatus",
    SciName == "Sphaerodactylus argus argus" ~ "Sphaerodactylus argus",
    SciName == "Anolis equestris persparsus" ~ "Anolis equestris",
    SciName == "Python molurus ssp. bivittatus" ~ "Python bivittatus",
    SciName == "Boa constrictor constrictor"~ "Boa constrictor",
    SciName == "Anolis cristatellus cristatellus" ~ "Anolis cristatellus",
    SciName == "Bufo marinus" ~ "Rhinella marina",
    SciName == "Geochelone sulcata" ~ "Centrochelys sulcata",
    SciName == "Geochelone carbonaria" ~ "Chelonoidis carbonarius",
    SciName == "Chelonoidis denticulatus" ~ "Chelonoidis denticulata",
    SciName == "Ramphotyphlops braminus"~ "Indotyphlops braminus",
    SciName == "Lygodactylus luteopicturatus" ~ "Lygodactylus picturatus",
    SciName == "Chelonoidis denticulata" ~ "Chelonoidis denticulatus",
    SciName == "Leiocephalus schreibersii schreibersii" ~ "Leiocephalus schreibersii",
    SciName == "Leiolepis belliana belliana" ~ "Leiolepis belliana",
    SciName == "Litoria caerulea" ~ "Ranoidea caerulea",
    SciName == "Epicrates cenchria cenchria" ~ "Epicrates cenchria",
    SciName == "Epicrates cenchria maurus" ~ "Epicrates cenchria",
    TRUE ~ SciName  # Keep all other names unchanged
  ))

### Seperate the date stamp on Eddmaps
filtered_data_eddmaps <- filtered_data_eddmaps %>%
  separate(ObsDate, into = c("Month", "Day", "Year"), sep = "/")

filtered_data_eddmaps <- filtered_data_eddmaps %>%
  mutate(Year = as.numeric(Year)) %>%  # Convert Year to numeric
  filter(Year >= 2010 & Year <= 2023) %>% # Filter for years 2010-2023
  mutate(source = "EDDMapS") %>%
  mutate(Month = as.numeric(Month), Day = as.numeric(Day), Year = as.numeric(Year)) %>%
  rename(species = SciName)

# Define Florida's geographic boundaries for map
filtered_data_eddmaps_florida <- filtered_data_eddmaps %>%
  filter(Latitude >= 24.396308 & Latitude <= 31.000888, 
         Longitude >= -87.634938 & Longitude <= -80.031362)


# Join them
combined_data <- bind_rows(filtered_data_eddmaps_florida, filtered_data_iNat_florida) 

combined_obs_coordinates <- combined_data %>%
  dplyr::select(Longitude, Latitude, source) %>%  # Adjust column names based on your dataset
  na.omit() %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

fl_coordinates <- combined_obs_coordinates %>%
  filter(
    st_coordinates(.)[,2] >= 24.396308 & st_coordinates(.)[,2] <= 31.000968 &  # Latitude bounds
      st_coordinates(.)[,1] >= -87.6349 & st_coordinates(.)[,1] <= -80.031362     # Longitude bounds
  )

location_obs_counts <- fl_coordinates %>%
  group_by(geometry) %>%
  summarize(total_obs = n(), .groups = "drop")


# Step 1: Get Florida counties shapefile
fl_counties <- counties(state = "FL", cb = TRUE, class = "sf") %>% 
  st_transform(crs = 4326)  # Ensure CRS is consistent

# Step 2: Ensure observation points have the same CRS
fl_coordinates <- fl_coordinates %>% 
  st_transform(crs = 4326)  

# Step 3: Spatial Join - Assign each observation to a county
observations_per_county <- st_join(fl_coordinates, fl_counties, join = st_within) %>%
  st_drop_geometry() %>%  # Remove spatial information to avoid issues with join
  group_by(NAME, source) %>%  # Group by county name
  summarize(total_obs = n(), .groups = "drop")  # Count observations per county

# Step 4: Merge with Florida county shapefile
fl_counties <- left_join(fl_counties, observations_per_county, by = "NAME")

eddmaps_total <- sum(fl_counties$total_obs[fl_counties$source == "EDDMapS"], na.rm = TRUE)
inat_total <- sum(fl_counties$total_obs[fl_counties$source == "iNaturalist"], na.rm = TRUE)

# Format the labels with source name and observation count
source_labels <- c(
  EDDMapS = paste0("EDDMapS (n = ", format(eddmaps_total, big.mark = ","), ")"),
  iNaturalist = paste0("iNaturalist (n = ", format(inat_total, big.mark = ","), ")")
)

# Create the map with custom facet labels and improved spacing
Fig_1_Map <- ggplot(fl_counties) +
  geom_sf(aes(fill = total_obs), color = "black") +
  scale_fill_viridis_c(
    option = "cividis",
    trans = "log",
    breaks = c(0, 10, 100, 1000, 10000),  # Adjust based on your range
    labels = scales::comma,  # Ensures whole numbers in legend
    na.value = "gray80"
  ) +
  facet_wrap(~source, labeller = labeller(source = source_labels)) +
  theme_classic() +  
  theme(
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11),
    strip.text = element_text(size = 9.5, face = "bold"),  # Make the facet titles bold and larger
    panel.spacing = unit(1.5, "cm"),  # Increase the space between panels
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")  # Add margins around the entire plot
  ) +
  labs(fill = "Total Observations")

# Display the map
Fig_1_Map

#get a summary of iNat data
iNat_obs_summary <- filtered_data_iNat %>%
  group_by(species) %>%
  summarize(inat_number_of_obs=n()) 

# get a summary of EDDMaps data
EDDMaps_obs_summary <- filtered_data_eddmaps %>%
  group_by(species) %>%
  summarize(eddmaps_number_of_obs=n())

iNatandEddMap_combined <- iNat_obs_summary %>%
  left_join(EDDMaps_obs_summary)

iNatandEddMap_matched <- iNatandEddMap_combined %>%
  dplyr::filter(complete.cases(.))

# First, create a new column to identify the target species
iNatandEddMap_matched <- iNatandEddMap_matched %>%
  mutate(highlight_species = case_when(
    species == "Iguana iguana" ~ "Iguana iguana",
    species == "Lepidodactylus lugubris" ~ "Lepidodactylus lugubris",
    species == "Leiocephalus carinatus" ~ "Leiocephalus carinatus",
    TRUE ~ "Other species"
  ))

# Then modify your plot
Fig_1_Line <- ggplot(iNatandEddMap_matched, aes(x=inat_number_of_obs, y=eddmaps_number_of_obs)) +
  # Add background points first
  geom_point(data = subset(iNatandEddMap_matched)) +
  # Add highlighted species with different colors and slightly larger size
  # Add labels for the highlighted species
  scale_x_log10() +
  scale_y_log10() +
  geom_smooth(method = "lm") +
  labs(x = "iNaturalist observations",
       y = "EDDMapS observations",
       color = "Species") +
  theme_classic() +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14))

Fig_1_Line

# Analysis! If overdispersion is present, use negative binomial.
model2_improved <- MASS::glm.nb(eddmaps_number_of_obs ~ inat_number_of_obs,
                                data = iNatandEddMap_matched,
                                control = glm.control(maxit = 100))
summary(model2_improved)







# Create a list of all unique species from both datasets
all_species <- unique(c(filtered_data_iNat$species, filtered_data_eddmaps$species))

# Create a new dataframe to store presence information
presence_df <- data.frame(Species = all_species,
                          iNaturalist_Present = NA,
                          EDDMapS_Present = NA)

# Check presence in iNaturalist dataset
presence_df$iNaturalist_Present <- ifelse(presence_df$Species %in% filtered_data_iNat_florida$species, "Yes", "No")

# Check presence in EDDMapS dataset
presence_df$EDDMapS_Present <- ifelse(presence_df$Species %in% filtered_data_eddmaps_florida$species, "Yes", "No")

# Sort alphabetically by species name
presence_df <- presence_df[order(presence_df$Species), ]

# Print the first few rows to verify
head(presence_df)

# Show the total number of species
cat("Total number of unique species:", nrow(presence_df), "\n")

# Count species present in each dataset
cat("Species present in iNaturalist:", sum(presence_df$iNaturalist_Present == "Yes"), "\n")
cat("Species present in EDDMapS:", sum(presence_df$EDDMapS_Present == "Yes"), "\n")

# Count species present in both datasets
cat("Species present in both datasets:", 
    sum(presence_df$iNaturalist_Present == "Yes" & presence_df$EDDMapS_Present == "Yes"), "\n")

# Count species present in only one dataset
cat("Species present only in iNaturalist:", 
    sum(presence_df$iNaturalist_Present == "Yes" & presence_df$EDDMapS_Present == "No"), "\n")
cat("Species present only in EDDMapS:", 
    sum(presence_df$iNaturalist_Present == "No" & presence_df$EDDMapS_Present == "Yes"), "\n")

# Save the dataframe to a CSV file
write.csv(presence_df, "species_presence_comparison.csv", row.names = FALSE)



iNatandEddMap_matched <- iNatandEddMap_matched %>%
  mutate(highlight_species = case_when(
    species == "Iguana iguana" ~ "Iguana iguana",
    species == "Salvator merianae" ~ "Salvator merianae",
    species == "Leiocephalus carinatus" ~ "Leiocephalus carinatus",
    TRUE ~ "Other species"
  ))


# Then create the plot with highlighted species
Fig_1_Line_species <- ggplot(iNatandEddMap_matched, aes(x=inat_number_of_obs, y=eddmaps_number_of_obs)) +
  # Add background points first (grey for "Other species")
  geom_point(data = subset(iNatandEddMap_matched, highlight_species == "Other species"), 
             color = "grey80", alpha = 0.5) +
  # Add highlighted species with different colors and slightly larger size
  geom_point(data = subset(iNatandEddMap_matched, highlight_species != "Other species"), 
             aes(color = highlight_species), size = 3) +
  # Add text labels for the highlighted species
  scale_x_log10() +
  scale_y_log10() +
  geom_smooth(method = "lm", color = "black") +
  scale_color_manual(values = c(
    "Iguana iguana" = "red",
    "Salvator merianae" = "blue",
    "Leiocephalus carinatus" = "green4"
  )) +
  labs(x = "iNaturalist observations",
       y = "EDDMapS observations",
       color = "Species") +
  theme_classic() +
  theme(legend.position = "bottom")

# Print the plot
Fig_1_Line_species



# Pie chart code

presence <- read.csv("species_presence_comparison.csv")
presence <- presence %>%
  filter(!(Species %in% "Basiliscus spp.")) %>%
  filter(!(Species %in% "Iguana spp.")) %>%
  filter(!(Species %in% "Leiocephalus spp.")) %>%
  filter(!(Species %in% "Python spp.")) %>%
  filter(!(Species %in% "Trioceros spp.")) %>%
  rename(iNaturalist = iNaturalist_Present) %>%
  rename(EDDMapS = EDDMapS_Present)


# Create the comparison categories
presence_summary <- presence %>%
  mutate(
    Category = case_when(
      iNaturalist == "Yes" & EDDMapS == "Yes" ~ "Both platforms",
      iNaturalist == "Yes" & EDDMapS == "No" ~ "iNaturalist only",
      iNaturalist == "No" & EDDMapS == "Yes" ~ "EDDMapS only",
      TRUE ~ "Neither" # This would be species not observed on either platform
    )
  ) %>%
  count(Category) %>%
  filter(Category != "Neither") # Remove if you don't want to show species not on either platform

# Create the pie chart
ggplot(presence_summary, aes(x = "", y = n, fill = Category)) +
  geom_col(width = 1) +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = c(
    "Both platforms" = "#7FB069",
    "iNaturalist only" = "#A7FD25", 
    "EDDMapS only" = "#FD7B25"
  )) +
  theme_void() +
  labs(
    title = NULL,
    fill = "Platform"
  ) +
  theme(
    text = element_text(family = "Times New Roman"),
    plot.title = element_text(hjust = 0.5, size = 14),
    legend.text = element_text(size = 12)
  ) +
  geom_text(aes(label = paste0(n, "\n(", round(n/sum(n)*100, 1), "%)")), 
            position = position_stack(vjust = 0.5),
            size = 6)


















# ==============================================================================



### Density Histogram for Urbanization (Obj 2.)

#Convert the data to an sf object with spatial coordinates.
iNat_spatial <- iNat %>%
  st_as_sf(coords=c("decimalLongitude", "decimalLatitude"), crs=4326)

#Load the population density raster file.
pop_density <- raster("fl_pop_density.tif")

#Check the coordinate reference system (CRS) of both datasets.
st_crs(iNat_spatial)
crs(pop_density)
crs(pop_density) <- "+proj=longlat +datum=WGS84 +no_defs"

#Extract raster values (population density) for each observation point.
iNat_spatial$PopDensity <- raster::extract(pop_density, st_coordinates(iNat_spatial))


#define urbanization levels MAKE IT A CONTINUOUS GRADIENT TO AVOID CATEGORIZATION DEFENSE
iNat_spatial_defined <- iNat_spatial %>%
  mutate(Urbanization = PopDensity) %>%  # Assign population density as urbanization
  st_drop_geometry() %>%  # Remove spatial attributes
  as.data.frame() %>%  # Ensure it's a regular data frame
  dplyr::select(PopDensity, Urbanization, species, eventDate, coordinateUncertaintyInMeters)


EDDmaps <- read_csv("EDDMapS_observations.csv")

# Remove rows with missing coordinates
EDDmaps_clean <- EDDmaps %>%
  filter(!is.na(Longitude) & !is.na(Latitude))

# Convert to sf object
EDDmaps_spatial <- EDDmaps_clean %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

# Extract population density for EDDMapS observations
EDDmaps_spatial$PopDensity <- raster::extract(pop_density, st_coordinates(EDDmaps_spatial))

# Convert to data frame and retain necessary columns
EDDmaps_spatial_defined <- EDDmaps_spatial %>%
  mutate(Urbanization = PopDensity) %>%  # Assign population density as urbanization
  st_drop_geometry() %>%
  as.data.frame() %>%
  dplyr::select(PopDensity, Urbanization, SciName, ObsDate)

# Add a new column to identify the source dataset
EDDmaps_spatial_defined <- EDDmaps_spatial_defined %>%
  mutate(Source = "EDDMapS")

iNat_spatial_defined <- iNat_spatial_defined %>%
  mutate(Source = "iNaturalist")

# Combine both datasets into one
PopData_combined <- bind_rows(EDDmaps_spatial_defined, iNat_spatial_defined)


library(scales)

# Create a density histogram (Fixing up - MZ)
ggplot(PopData_combined, aes(x = PopDensity, fill = Source, y = ..density..)) +
  geom_density(alpha = 0.4, position = "dodge", bins = 30) +
  scale_fill_manual(values = c("EDDMapS" = "#FD7B25", "iNaturalist" = "#A7FD25")) +
  theme_minimal() +
  scale_x_log10(labels = label_number()) +  # Format numbers without scientific notation
  labs(
    title = "Urbanization Density of Observations",
    x = "Urbanization Level (PopDensity)",
    y = "Density",
    fill = "Dataset"
  ) +
  theme(text = element_text(family = "Times New Roman")) +
  theme_classic()

PopData_combined$LogPopDensity <- log(PopData_combined$PopDensity + 1)
model_gaussian <- glm(LogPopDensity ~ Source, 
                      data = PopData_combined,
                      family = gaussian(link = "identity"))
summary(model_gaussian)





# Calculate comprehensive statistics for both original and log-transformed data
pop_stats_detailed <- PopData_combined %>%
  filter(!is.na(PopDensity)) %>%  # Remove missing values
  group_by(Source) %>%
  summarise(
    n = n(),
    # Original scale statistics
    mean_pop_density = mean(PopDensity, na.rm = TRUE),
    sd_pop_density = sd(PopDensity, na.rm = TRUE),
    se_pop_density = sd(PopDensity, na.rm = TRUE) / sqrt(n()),
    # Log scale statistics  
    mean_log_pop_density = mean(LogPopDensity, na.rm = TRUE),
    sd_log_pop_density = sd(LogPopDensity, na.rm = TRUE),
    se_log_pop_density = sd(LogPopDensity, na.rm = TRUE) / sqrt(n()),
    .groups = 'drop'
  )

print(pop_stats_detailed)

# Display formatted results
cat("Population Density Statistics:\n")
cat("=============================\n")
for(i in 1:nrow(pop_stats_detailed)) {
  cat(sprintf("%s:\n", pop_stats_detailed$Source[i]))
  cat(sprintf("  Original scale: Mean = %.2f ± %.2f (SD = %.2f, n = %d)\n", 
              pop_stats_detailed$mean_pop_density[i],
              pop_stats_detailed$se_pop_density[i],
              pop_stats_detailed$sd_pop_density[i],
              pop_stats_detailed$n[i]))
  cat(sprintf("  Log scale: Mean = %.3f ± %.5f (SD = %.3f)\n\n", 
              pop_stats_detailed$mean_log_pop_density[i],
              pop_stats_detailed$se_log_pop_density[i],
              pop_stats_detailed$sd_log_pop_density[i]))
}












# =============================================================================

### MCP analysis (Obj 3.)
library(dplyr)
library(tidyr)
library(sp)
library(adehabitatHR)
library(sf)     # For spatial operations
library(units)  # For unit conversion

# Process EDDMapS data
eddmaps_new2 <- read.csv("/Users/mario/Desktop/iNat-vs.-Eddmaps/Data/EDDmapS_observations.csv")

filtered_data_eddmaps2 <- eddmaps_new2 %>%
  filter(SciName %in% eddmaps_introduced_clean$scientific_name)

filtered_data_eddmaps2 <- filtered_data_eddmaps2 %>%
  dplyr::select(SciName, ObsDate, Latitude, Longitude) %>%
  filter(!is.na(Latitude) & !is.na(Longitude))

filtered_data_eddmaps2 <- filtered_data_eddmaps2 %>%
  mutate(SciName = case_when(
    SciName == "Chelonoidis carbonaria" ~ "Chelonoidis carbonarius",
    SciName == "Leiocephalus carinatus armouri" ~ "Leiocephalus carinatus",
    SciName == "Sphaerodactylus argus argus" ~ "Sphaerodactylus argus",
    SciName == "Anolis equestris persparsus" ~ "Anolis equestris",
    SciName == "Python molurus ssp. bivittatus" ~ "Python bivittatus",
    SciName == "Boa constrictor constrictor"~ "Boa constrictor",
    SciName == "Anolis cristatellus cristatellus" ~ "Anolis cristatellus",
    SciName == "Bufo marinus" ~ "Rhinella marina",
    SciName == "Geochelone sulcata" ~ "Centrochelys sulcata",
    SciName == "Geochelone carbonaria" ~ "Chelonoidis carbonarius",
    SciName == "Chelonoidis denticulatus" ~ "Chelonoidis denticulata",
    SciName == "Ramphotyphlops braminus"~ "Indotyphlops braminus",
    SciName == "Lygodactylus luteopicturatus" ~ "Lygodactylus picturatus",
    SciName == "Chelonoidis denticulata" ~ "Chelonoidis denticulatus",
    SciName == "Leiocephalus schreibersii schreibersii" ~ "Leiocephalus schreibersii",
    SciName == "Leiolepis belliana belliana" ~ "Leiolepis belliana",
    SciName == "Litoria caerulea" ~ "Ranoidea caerulea",
    SciName == "Epicrates cenchria cenchria" ~ "Epicrates cenchria",
    SciName == "Epicrates cenchria maurus" ~ "Epicrates cenchria",
    TRUE ~ SciName  # Keep all other names unchanged
  ))


filtered_data_eddmaps2 <- filtered_data_eddmaps2 %>%
  rename(species = SciName)

filtered_data_eddmaps2 <- filtered_data_eddmaps2 %>%
  separate(ObsDate, into = c("Month", "Day", "Year"), sep = "/")

filtered_data_eddmaps2 <- filtered_data_eddmaps2 %>%
  mutate(source = "EDDMapS")

# Define Florida's geographic boundaries for map
filtered_data_eddmaps_florida2 <- filtered_data_eddmaps2 %>%
  filter(Latitude >= 24.396308 & Latitude <= 31.000888,
         Longitude >= -87.634938 & Longitude <= -80.031362)

# Process iNaturalist data
iNat2 <- readRDS("/Users/mario/Desktop/iNat-vs.-Eddmaps/Data/iNat_herp_data.RDS")

filtered_data_iNat2 <- iNat2 %>%
  filter(species %in% iNaturalist_introduced$scientific_name) %>%
  filter(!(species %in% "Anaxyrus fowleri")) %>%
  filter(!(species %in% "Desmognathus conanti")) %>%
  filter(!(species %in% "Pseudotriton ruber"))%>%
  filter(!(species %in% "Eurycea cirrigera"))%>%
  filter(!(species %in% "Eurycea guttolineata"))%>%
  filter(!(species %in% "Eretmochelys imbricata")) %>%
  filter(!(species %in% "Desmognathus conanti")) %>%
  filter(!(species %in% "Incilius nebulifer")) %>%
  filter(!(species %in% "Lampropeltis rhombomaculata")) %>%
  filter(!(species %in% "Lepidochelys kempii")) %>%
  filter(!(species %in% "Lithobates virgatipes"))



filtered_data_iNat2 <- filtered_data_iNat2 %>%
  dplyr::select(species, decimalLatitude, decimalLongitude, day, month, year) %>%
  filter(!is.na(decimalLatitude) & !is.na(decimalLongitude))

filtered_data_iNat2 <- filtered_data_iNat2 %>%
  rename(Latitude = decimalLatitude, Longitude = decimalLongitude, Month = month, Day = day, Year = year)

filtered_data_iNat2 <- filtered_data_iNat2 %>%
  mutate(Month = as.character(Month),
         Day = as.character(Day),
         Year = as.character(Year))

filtered_data_iNat2 <- filtered_data_iNat2 %>%
  mutate(source = "iNaturalist")

filtered_data_iNat_florida2 <- filtered_data_iNat2 %>%
  filter(Latitude >= 24.396308 & Latitude <= 31.000888,
         Longitude >= -87.634938 & Longitude <= -80.031362)

# Filter for years between 2014 and 2024
filtered_data_eddmaps_florida2 <- filtered_data_eddmaps_florida2 %>%
  mutate(Year = as.numeric(Year)) %>%
  filter(Year >= 2014 & Year <= 2024) %>%
  mutate(Year = as.character(Year))

filtered_data_iNat_florida2 <- filtered_data_iNat_florida2 %>%
  mutate(Year = as.numeric(Year)) %>%
  filter(Year >= 2014 & Year <= 2024) %>%
  mutate(Year = as.character(Year))

# Function to calculate 95% MCP area in square kilometers
calculate_mcp_area <- function(data, percent = 95) {
  # Skip if there are fewer than 5 points (minimum required for reliable MCP)
  if(nrow(data) < 5) {
    return(NA)
  }
  
  # Create a spatial points data frame
  coordinates <- data[, c("Longitude", "Latitude")]
  sp_points <- SpatialPoints(coordinates,
                             proj4string = CRS("+proj=longlat +datum=WGS84"))
  
  # Transform to an equal-area projection for accurate area calculation
  # Using Albers Equal Area projection for North America
  sp_points_projected <- spTransform(sp_points,
                                     CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-84 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"))
  
  # Calculate MCP with percent (typically 95%)
  tryCatch({
    mcp_result <- mcp(SpatialPointsDataFrame(sp_points_projected, data = data.frame(ID = rep(1, nrow(data)))),
                      percent = percent, unin = "m", unout = "km2")
    
    # Extract area in square kilometers
    mcp_area <- as.numeric(st_area(st_as_sf(mcp_result)) / 1000000)  # Convert m² to km²
    return(mcp_area)
  }, error = function(e) {
    return(NA)  # Return NA if MCP calculation fails
  })
}

# Count observations per species per application
species_counts <- full_join(
  filtered_data_eddmaps_florida2 %>% 
    group_by(species) %>% 
    summarize(EDDMapS_count = n(), .groups = "drop"),
  
  filtered_data_iNat_florida2 %>% 
    group_by(species) %>% 
    summarize(iNat_count = n(), .groups = "drop"),
  
  by = "species"
) %>%
  mutate(
    EDDMapS_count = replace_na(EDDMapS_count, 0),
    iNat_count = replace_na(iNat_count, 0)
  )

# Filter for species that have at least 5 observations in either application
valid_species <- species_counts %>%
  filter(EDDMapS_count >= 5 | iNat_count >= 5)

# Create empty dataframe to store results
mcp_results_by_species <- data.frame()

# Calculate MCP for each valid species per application
for(i in 1:nrow(valid_species)) {
  species_i <- valid_species$species[i]
  
  # Filter data for this species
  inat_data <- filtered_data_iNat_florida2 %>%
    filter(species == species_i)
  
  eddmaps_data <- filtered_data_eddmaps_florida2 %>%
    filter(species == species_i)
  
  combined_data <- bind_rows(inat_data, eddmaps_data)
  
  # Calculate MCP areas (only if there are enough points)
  inat_mcp_area <- if(nrow(inat_data) >= 5) calculate_mcp_area(inat_data, 95) else NA
  eddmaps_mcp_area <- if(nrow(eddmaps_data) >= 5) calculate_mcp_area(eddmaps_data, 95) else NA
  combined_mcp_area <- if(nrow(combined_data) >= 5) calculate_mcp_area(combined_data, 95) else NA
  
  # Create row for results
  result_row <- data.frame(
    Species = species_i,
    iNat_mcp_area_km2 = inat_mcp_area,
    EDDMapS_mcp_area_km2 = eddmaps_mcp_area,
    Combined_mcp_area_km2 = combined_mcp_area,
    iNat_observations = nrow(inat_data),
    EDDMapS_observations = nrow(eddmaps_data),
    Combined_observations = nrow(combined_data)
  )
  
  # Add to results dataframe
  mcp_results_by_species <- bind_rows(mcp_results_by_species, result_row)
}

# Calculate differences and ratios between platforms
mcp_results_by_species <- mcp_results_by_species %>%
  mutate(
    Area_difference_km2 = abs(iNat_mcp_area_km2 - EDDMapS_mcp_area_km2),
    Area_ratio = pmax(iNat_mcp_area_km2, EDDMapS_mcp_area_km2, na.rm = TRUE) / 
      pmin(iNat_mcp_area_km2, EDDMapS_mcp_area_km2, na.rm = TRUE),
    Larger_platform = case_when(
      is.na(iNat_mcp_area_km2) & !is.na(EDDMapS_mcp_area_km2) ~ "EDDMapS",
      !is.na(iNat_mcp_area_km2) & is.na(EDDMapS_mcp_area_km2) ~ "iNaturalist",
      iNat_mcp_area_km2 > EDDMapS_mcp_area_km2 ~ "iNaturalist",
      EDDMapS_mcp_area_km2 > iNat_mcp_area_km2 ~ "EDDMapS",
      TRUE ~ "Equal"
    )
  )

# Display results
print(mcp_results_by_species)

# Summary statistics
summary_stats <- mcp_results_by_species %>%
  summarize(
    Total_Species = n(),
    Species_with_both_MCPs = sum(!is.na(iNat_mcp_area_km2) & !is.na(EDDMapS_mcp_area_km2)),
    Species_with_only_iNat_MCP = sum(!is.na(iNat_mcp_area_km2) & is.na(EDDMapS_mcp_area_km2)),
    Species_with_only_EDDMapS_MCP = sum(is.na(iNat_mcp_area_km2) & !is.na(EDDMapS_mcp_area_km2)),
    Avg_iNat_Area_km2 = mean(iNat_mcp_area_km2, na.rm = TRUE),
    Avg_EDDMapS_Area_km2 = mean(EDDMapS_mcp_area_km2, na.rm = TRUE),
    Avg_Combined_Area_km2 = mean(Combined_mcp_area_km2, na.rm = TRUE),
    Median_Area_Ratio = median(Area_ratio, na.rm = TRUE)
  )

print(summary_stats)

# Write results to CSV
# write.csv(mcp_results_by_species, "species_mcp_comparison.csv", row.names = FALSE)

# Create a standardized score to compare platforms (-1 to 1 scale)
# Prepare data for visualization
plot_data <- mcp_results_by_species %>%
  filter(!is.na(iNat_mcp_area_km2) & !is.na(EDDMapS_mcp_area_km2)) %>%  # Only include species with data from both sources
  mutate(
    # Calculate comparison metric from -1 (favoring iNat) to 1 (favoring EDDMapS)
    comparison_score = (EDDMapS_mcp_area_km2 - iNat_mcp_area_km2) / 
      (EDDMapS_mcp_area_km2 + iNat_mcp_area_km2),
    # Log ratio for alternative visualization
    log_ratio = log2(EDDMapS_mcp_area_km2 / iNat_mcp_area_km2)
  ) %>%
  arrange(comparison_score)

# Add row numbers for ordering on the y-axis
plot_data$species_order <- 1:nrow(plot_data)

# Load ggplot2 library
library(ggplot2)

# Create the comparison plot
mcp_comparison_plot <- ggplot(plot_data, aes(x = comparison_score, y = reorder(Species, comparison_score))) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", size = 0.5) +
  geom_point(aes(size = Combined_observations, 
                 color = abs(comparison_score)), 
             alpha = 0.8) +
  scale_color_gradient(low = "blue", high = "red", name = "Disparity") +
  scale_size_continuous(name = "Total Observations") +
  scale_x_continuous(limits = c(-1, 1), 
                     breaks = seq(-1, 1, 0.5),
                     labels = c("iNaturalist\nlarger", "-0.5", "No Difference", "0.5", "EDDMapS\nlarger")) +
  labs(title = NULL,
       subtitle = NULL,
       x = "Relative MCP Size Comparison Score",
       y = "Species") +
  # Format y-axis labels to have italicized species names
  scale_y_discrete(labels = function(x) {
    lapply(x, function(y) bquote(italic(.(y))))
  }) +
  theme(
    axis.text.y = element_text(size = 8),
    panel.grid.major.y = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  ) + theme_classic()

mcp_comparison_plot



paired_data <- mcp_results_by_species %>%
  filter(!is.na(EDDMapS_mcp_area_km2) & !is.na(iNat_mcp_area_km2))

paired_data <- paired_data %>%
  mutate(
    log_EDDMapS_area = log10(EDDMapS_mcp_area_km2),
    log_iNat_area = log10(iNat_mcp_area_km2),
    log_ratio = log10(EDDMapS_mcp_area_km2 / iNat_mcp_area_km2),
    diff_log_area = log_EDDMapS_area - log_iNat_area
  )

long_data <- paired_data %>%
  pivot_longer(
    cols = c(EDDMapS_mcp_area_km2, iNat_mcp_area_km2),
    names_to = "platform",
    values_to = "mcp_area"
  ) %>%
  mutate(
    platform = factor(platform, levels = c("iNat_mcp_area_km2", "EDDMapS_mcp_area_km2")),
    log_mcp_area = log10(mcp_area)
  )

# Linear model with platform as predictor
mcp_platform_model <- glm(log_mcp_area ~ platform, data = long_data)
summary(mcp_platform_model)

# For visualization
library(ggplot2)
ggplot(long_data, aes(x = platform, y = mcp_area)) +
  geom_boxplot() +
  scale_y_log10() +
  labs(title = "Comparison of MCP Areas Between Platforms",
       x = "Platform",
       y = "MCP Area (km²) - Log Scale") +
  theme_minimal()

library(lme4)

# Mixed effects model with species as random effect
mcp_mixed_model <- lmer(log_mcp_area ~ platform + (1|Species), data = long_data)
summary(mcp_mixed_model)
anova(mcp_mixed_model)

# Get confidence intervals
confint(mcp_mixed_model)

# Create model testing if log ratio differs from 0
ratio_model <- lm(log_ratio ~ 1, data = paired_data)
summary(ratio_model)

# Create variables for observation counts in long format
long_data <- long_data %>%
  mutate(
    obs_count = case_when(
      platform == "EDDMapS_mcp_area_km2" ~ paired_data$EDDMapS_observations[match(Species, paired_data$Species)],
      platform == "iNat_mcp_area_km2" ~ paired_data$iNat_observations[match(Species, paired_data$Species)]
    ),
    log_obs_count = log10(obs_count)
  )

# Model MCP area as function of platform while controlling for observation count
mcp_obs_model <- glm(log_mcp_area ~ platform + log_obs_count, data = long_data)
summary(mcp_obs_model)

# Mixed effects version
mcp_obs_mixed_model <- lmer(log_mcp_area ~ platform + log_obs_count + (1|Species), data = long_data)
summary(mcp_obs_mixed_model)

# Test for interaction between platform and observation count
mcp_interaction_model <- glm(log_mcp_area ~ platform * log_obs_count, data = long_data)
summary(mcp_interaction_model)
anova(mcp_obs_model, mcp_interaction_model)  # Test if interaction improves model

# Visualize relationships
ggplot(long_data, aes(x = log_obs_count, y = log_mcp_area, color = platform)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Relationship Between Observation Count and MCP Area",
       x = "Log10(Observation Count)",
       y = "Log10(MCP Area in km²)") +
  theme_minimal()



# Supplemental figure for MCP

plot_data_combined <- long_data %>%
  filter(!is.na(mcp_area) & !is.na(obs_count)) %>%
  mutate(
    Platform = case_when(
      platform == "iNat_mcp_area_km2" ~ "iNaturalist",
      platform == "EDDMapS_mcp_area_km2" ~ "EDDMapS"
    )
  )

# Create the main plot with overall trend
mcp_obs_plot <- ggplot(plot_data_combined, aes(x = obs_count, y = mcp_area)) +
  geom_point(aes(color = Platform), alpha = 0.7, size = 2.5) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.2, size = 1.2, color = "black") +
  scale_x_log10(
  ) +
  scale_y_log10(
    breaks = c(0.1, 1, 10, 100, 1000, 10000),
    labels = c("0.1", "1", "10", "100", "1000", "10000")
  ) +
  scale_color_manual(values = c("EDDMapS" = "#FD7B25", "iNaturalist" = "#A7FD25")) +
  labs(
    title = NULL,
    x = "Number of Observations (log scale)",
    y = "MCP Area (km²; log scale)",
    color = "Platform")+
  theme_classic() +
  theme(
    text = element_text(family = "Times New Roman"),
    plot.title = element_text(size = 14, hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10),
    plot.caption = element_text(size = 9, hjust = 0, margin = margin(t = 15)),
    legend.position = c(0.15, 0.85),
    legend.background = element_rect(fill = "white", color = "black", size = 0.3),
    legend.margin = margin(6, 6, 6, 6)
  ) +
  annotation_logticks(sides = "bl")

# Display the plot
print(mcp_obs_plot)
