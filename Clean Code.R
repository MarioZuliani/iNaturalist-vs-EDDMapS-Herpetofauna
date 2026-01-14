### Clean Code
### Map and Line Graph Figure 1 (Obj 1.)

library(tidyverse)
library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)
library(sf)
library(tigris)
library(raster)
library(purrr)
library(patchwork)
library(scales)

iNaturalist_introduced <- read.csv("Data/iNaturalist_introduced.csv")
eddmaps_introduced_clean <- read.csv("Data/eddmaps_introduced.csv")


# =========================
# iNat cleanup
# =========================
iNat <- readRDS("Data/iNat_herp_data.RDS")

filtered_data_iNat <- iNat %>%
  filter(species %in% iNaturalist_introduced$scientific_name) %>%
  filter(!(species %in% "Anaxyrus fowleri")) %>%
  filter(!(species %in% "Desmognathus conanti")) %>%
  filter(!(species %in% "Pseudotriton ruber")) %>%
  filter(!(species %in% "Eurycea cirrigera")) %>%
  filter(!(species %in% "Eurycea guttolineata")) %>%
  filter(!(species %in% "Eretmochelys imbricata")) %>%
  filter(!(species %in% "Incilius nebulifer")) %>%
  filter(!(species %in% "Lampropeltis rhombomaculata")) %>%
  filter(!(species %in% "Lepidochelys kempii")) %>%
  filter(!(species %in% "Lithobates virgatipes"))

filtered_data_iNat <- filtered_data_iNat %>%
  dplyr::select(species, decimalLatitude, decimalLongitude, day, month, year, coordinateUncertaintyInMeters) %>%
  filter(!is.na(decimalLatitude) & !is.na(decimalLongitude)) %>%
  # NOTE (Obj 1): no coordinate uncertainty filter applied
  rename(Latitude = decimalLatitude, Longitude = decimalLongitude) %>%
  mutate(Year = as.numeric(year)) %>%
  filter(Year >= 2014 & Year <= 2024) %>%
  mutate(source = "iNaturalist") %>%
  rename(Month = month, Day = day) %>%
  mutate(Month = as.numeric(Month), Day = as.numeric(Day), Year = as.numeric(Year))

### Plot a map to make sure all points are within Florida Boundary
filtered_data_iNat_florida <- filtered_data_iNat %>%
  filter(
    Latitude  >= 24.396308 & Latitude  <= 31.000888,
    Longitude >= -87.634938 & Longitude <= -80.031362
  ) %>%
  dplyr::select(-year)

# =========================
# EDDMapS cleanup + cross-post audit
# =========================
eddmaps <- read.csv("Data/EDDMapS_observations.csv")  ### Need to clean up the Eddmaps data.

### Run to pull all introduced species from the raw eddmaps data to the cleaned species list we have made.
eddmaps_introduced_pre <- eddmaps %>%
  filter(SciName %in% eddmaps_introduced_clean$scientific_name)

# --- Detect cross-posts robustly (captures "iNaturalist Database", etc.) ---
eddmaps_crossposts_pre <- eddmaps_introduced_pre %>%
  filter(!is.na(reporter)) %>%
  filter(str_detect(str_squish(str_to_lower(reporter)), "inaturalist"))

# --- Remove cross-posts for platform-independence (case/whitespace safe) ---
filtered_data_eddmaps <- eddmaps_introduced_pre %>%
  filter(is.na(reporter) | !str_detect(str_squish(str_to_lower(reporter)), "inaturalist"))

# (Optional) quick sanity prints
cat("EDDMapS introduced-only rows (pre): ", nrow(eddmaps_introduced_pre), "\n")
cat("EDDMapS iNat-crossposts detected (pre): ", nrow(eddmaps_crossposts_pre), "\n")
cat("EDDMapS rows after removing crossposts: ", nrow(filtered_data_eddmaps), "\n\n")

### Select the columns relavent to MCPs
filtered_data_eddmaps <- filtered_data_eddmaps %>%
  dplyr::select(SciName, ObsDate, Latitude, Longitude, CoordAcc) %>%
  mutate(coordinateUncertaintyInMeters = as.numeric(as.character(CoordAcc))) %>%
  filter(!is.na(Latitude) & !is.na(Longitude)) %>%
  dplyr::select(-CoordAcc)


filtered_data_eddmaps <- filtered_data_eddmaps %>%
  mutate(SciName = case_when(
    SciName == "Chelonoidis carbonaria" ~ "Chelonoidis carbonarius",
    SciName == "Leiocephalus carinatus armouri" ~ "Leiocephalus carinatus",
    SciName == "Sphaerodactylus argus argus" ~ "Sphaerodactylus argus",
    SciName == "Anolis equestris persparsus" ~ "Anolis equestris",
    SciName == "Python molurus ssp. bivittatus" ~ "Python bivittatus",
    SciName == "Boa constrictor constrictor" ~ "Boa constrictor",
    SciName == "Anolis cristatellus cristatellus" ~ "Anolis cristatellus",
    SciName == "Bufo marinus" ~ "Rhinella marina",
    SciName == "Geochelone sulcata" ~ "Centrochelys sulcata",
    SciName == "Geochelone carbonaria" ~ "Chelonoidis carbonarius",
    SciName == "Chelonoidis denticulatus" ~ "Chelonoidis denticulata",
    SciName == "Ramphotyphlops braminus" ~ "Indotyphlops braminus",
    SciName == "Lygodactylus luteopicturatus" ~ "Lygodactylus picturatus",
    SciName == "Chelonoidis denticulata" ~ "Chelonoidis denticulatus",
    SciName == "Leiocephalus schreibersii schreibersii" ~ "Leiocephalus schreibersii",
    SciName == "Leiolepis belliana belliana" ~ "Leiolepis belliana",
    SciName == "Litoria caerulea" ~ "Ranoidea caerulea",
    SciName == "Epicrates cenchria cenchria" ~ "Epicrates cenchria",
    SciName == "Epicrates cenchria maurus" ~ "Epicrates cenchria",
    SciName == "Anolis coelestinus" ~ "Anolis cristatellus",
    SciName == "Cosymbotus platyurus" ~ "Hemidactylus platyurus",
    SciName == "Norops garmani" ~ "Anolis garmani",
    SciName == "Corallus hortulanus" ~ "Corallus hortulana",
    SciName == "Sphaerodactylus elegans elegans" ~ "Sphaerodactylus elegans",
    TRUE ~ SciName
  ))

### Separate the date stamp on EDDMapS
filtered_data_eddmaps <- filtered_data_eddmaps %>%
  separate(ObsDate, into = c("Month", "Day", "Year"), sep = "/") %>%
  mutate(Year = as.numeric(Year)) %>%
  filter(Year >= 2014 & Year <= 2024) %>%
  mutate(source = "EDDMapS") %>%
  mutate(Month = as.numeric(Month), Day = as.numeric(Day), Year = as.numeric(Year)) %>%
  rename(species = SciName)


# Define Florida's geographic boundaries for map
filtered_data_eddmaps_florida <- filtered_data_eddmaps %>%
  filter(
    Latitude  >= 24.396308 & Latitude  <= 31.000888,
    Longitude >= -87.634938 & Longitude <= -80.031362
  )

# =========================
# Cross-post audit summary (simple + consistent with YOUR pipeline)
# =========================
edd_crosspost_removed_summary <- tibble(
  dataset = "EDDMapS",
  crossposts_detected_introduced_pre = nrow(eddmaps_crossposts_pre),
  removed_at_introduced_step = nrow(eddmaps_crossposts_pre),
  eddmaps_final_n = nrow(filtered_data_eddmaps_florida),
  pct_of_final_removed_as_crossposts = round(
    100 * nrow(eddmaps_crossposts_pre) / max(1, nrow(filtered_data_eddmaps_florida)),
    4
  )
)
print(edd_crosspost_removed_summary)

# =========================
# Join them
# =========================
combined_data <- bind_rows(filtered_data_eddmaps_florida, filtered_data_iNat_florida)

combined_obs_coordinates <- combined_data %>%
  dplyr::select(Longitude, Latitude, source) %>%
  na.omit() %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

fl_coordinates <- combined_obs_coordinates %>%
  filter(
    st_coordinates(.)[,2] >= 24.396308 & st_coordinates(.)[,2] <= 31.000968 &
      st_coordinates(.)[,1] >= -87.6349 & st_coordinates(.)[,1] <= -80.031362
  )

location_obs_counts <- fl_coordinates %>%
  group_by(geometry) %>%
  summarize(total_obs = n(), .groups = "drop")

# Step 1: Get Florida counties shapefile
fl_counties <- counties(state = "FL", cb = TRUE, class = "sf") %>%
  st_transform(crs = 4326)

# Step 2: Ensure observation points have the same CRS
fl_coordinates <- fl_coordinates %>%
  st_transform(crs = 4326)

# Step 3: Spatial Join - Assign each observation to a county
observations_per_county <- st_join(fl_coordinates, fl_counties, join = st_within) %>%
  st_drop_geometry() %>%
  group_by(NAME, source) %>%
  summarize(total_obs = n(), .groups = "drop")

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
    breaks = c(0, 10, 100, 1000, 10000),
    labels = scales::comma,
    na.value = "gray80"
  ) +
  facet_wrap(~source, labeller = labeller(source = source_labels)) +
  theme_classic() +
  theme(
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11),
    strip.text = element_text(size = 9.5, face = "bold"),
    panel.spacing = unit(1.5, "cm"),
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
    legend.position = "none"   # <- remove legend
  ) +
  labs(fill = "Total Observations")

Fig_1_Map



# ============================================================
# UPDATED Scatter / model / presence / pie chart (USE NEW DATA)
# ============================================================

# --- Per-species counts built from the NEW Florida-filtered tables ---
iNat_obs_summary <- filtered_data_iNat_florida %>%
  count(species, name = "inat_number_of_obs")

EDDMaps_obs_summary <- filtered_data_eddmaps_florida %>%
  count(species, name = "eddmaps_number_of_obs")

iNatandEddMap_matched <- full_join(iNat_obs_summary, EDDMaps_obs_summary, by = "species") %>%
  mutate(
    inat_number_of_obs    = replace_na(inat_number_of_obs, 0L),
    eddmaps_number_of_obs = replace_na(eddmaps_number_of_obs, 0L)
  )

# (Recommended) model/correlation only for species present on BOTH platforms
iNatandEddMap_both <- iNatandEddMap_matched %>%
  filter(inat_number_of_obs > 0, eddmaps_number_of_obs > 0)

# Optional highlight column (kept, but not required)
iNatandEddMap_both <- iNatandEddMap_both %>%
  mutate(highlight_species = case_when(
    species == "Iguana iguana" ~ "Iguana iguana",
    species == "Lepidodactylus lugubris" ~ "Lepidodactylus lugubris",
    species == "Leiocephalus carinatus" ~ "Leiocephalus carinatus",
    TRUE ~ "Other species"
  ))

# --- Scatter plot (now reflects NEW cleaned counts) ---
Fig_1_Line <- ggplot(iNatandEddMap_both, aes(x = inat_number_of_obs, y = eddmaps_number_of_obs)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_x_log10(limits = c(1, NA)) +
  scale_y_log10(limits = c(1, NA)) +
  labs(
    x = "iNaturalist observations",
    y = "EDDMapS observations"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11)
  )

Fig_1_Line

# --- comparison of iNaturalist and eddmaps counts (NEW) ---
model_nb_log <- MASS::glm.nb(eddmaps_number_of_obs ~ log(inat_number_of_obs + 1),
                             data = iNatandEddMap_both)
summary(model_nb_log)

# --- spearman's correlation (NEW) ---
cor.test(iNatandEddMap_both$inat_number_of_obs,
         iNatandEddMap_both$eddmaps_number_of_obs,
         method = "spearman")


# =========================
# Presence table (NEW: derived from NEW cleaned FL data; no CSV)
# =========================
all_species <- sort(unique(c(filtered_data_iNat_florida$species,
                             filtered_data_eddmaps_florida$species)))

presence_df <- tibble(Species = all_species) %>%
  mutate(
    iNaturalist = if_else(Species %in% filtered_data_iNat_florida$species, "Yes", "No"),
    EDDMapS     = if_else(Species %in% filtered_data_eddmaps_florida$species, "Yes", "No")
  )

# clean the data
presence_df <- presence_df %>%
  filter(!(Species %in% c("Basiliscus spp.", "Iguana spp.", "Leiocephalus spp.", "Python spp.", "Trioceros spp."))) %>%
  mutate(Species = str_extract(Species, "^\\S+\\s+\\S+")) %>% # remove subspecies level detail
  group_by(Species) %>%
  summarise(
    iNaturalist = if_else(any(iNaturalist == "Yes", na.rm = TRUE), "Yes", "No"),
    EDDMapS     = if_else(any(EDDMapS == "Yes",     na.rm = TRUE), "Yes", "No"),
    .groups = "drop"
  )

head(presence_df)

cat("Total number of unique species:", nrow(presence_df), "\n")
cat("Species present in iNaturalist:", sum(presence_df$iNaturalist == "Yes"), "\n")
cat("Species present in EDDMapS:", sum(presence_df$EDDMapS == "Yes"), "\n")
cat("Species present in both datasets:",
    sum(presence_df$iNaturalist == "Yes" & presence_df$EDDMapS == "Yes"), "\n")
cat("Species present only in iNaturalist:",
    sum(presence_df$iNaturalist == "Yes" & presence_df$EDDMapS == "No"), "\n")
cat("Species present only in EDDMapS:",
    sum(presence_df$iNaturalist == "No" & presence_df$EDDMapS == "Yes"), "\n")

write_csv(presence_df, "Data/species_presence_up.csv")

# =========================
# Pie chart (NEW: uses presence_df; no CSV)
# =========================

presence_summary <- presence_df %>%
  mutate(
    Category = case_when(
      iNaturalist == "Yes" & EDDMapS == "Yes" ~ "Both platforms",
      iNaturalist == "Yes" & EDDMapS == "No" ~ "iNaturalist only",
      iNaturalist == "No"  & EDDMapS == "Yes" ~ "EDDMapS only",
      TRUE ~ "Neither"
    )
  ) %>%
  count(Category) %>%
  filter(Category != "Neither")

ggplot(presence_summary, aes(x = "", y = n, fill = Category)) +
  geom_col(width = 1) +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = c(
    "Both platforms" = "#7FB069",
    "iNaturalist only" = "#A7FD25",
    "EDDMapS only" = "#FD7B25"
  )) +
  theme_void() +
  labs(title = NULL, fill = "Platform") +
  theme(
    text = element_text(family = "Times New Roman"),
    plot.title = element_text(hjust = 0.5, size = 14),
    legend.position = "none"
  ) +
  geom_text(aes(label = paste0(n, "\n(", round(n/sum(n)*100, 1), "%)")),
            position = position_stack(vjust = 0.5),
            size = 6)


























## Check for species traits ------------------------------------------------

# read in species trait data for reptiles
trait_data_rep <- read_csv("Data/ReptTraits dataset v1-2_data.csv")

# let's see how many species we can gather trait data on from our dataset
trait_inat_eddmaps <- left_join(iNatandEddMap_matched, trait_data_rep, by=c("species"="Species"))

# let's get trait data for amphibians
trait_data_amp <- read_csv("Data/AmphiBIO_v1.csv")

# let's see how many species we can gather trait data on from our dataset
trait_inat_eddmaps <- left_join(trait_inat_eddmaps, trait_data_amp %>% dplyr::select(-Order, -Family, -Genus), by=c("species"="Species"))

trait_inat_eddmaps <- trait_inat_eddmaps %>%
  filter(inat_number_of_obs > 0, 
         eddmaps_number_of_obs > 0)

# let's repeat the analysis above while incorporating some species traits
# the traits we would be most interested in are: habitat type, active time, maximum body mass
# let's filter out the other data types
trait_inat_eddmaps_prep <- trait_inat_eddmaps %>%
  dplyr::select(species, inat_number_of_obs, eddmaps_number_of_obs, 
         `Habitat type`, `Active time`, `Maximum body mass (g)`,
         Diu, Noc, Crepu, Body_mass_g, Fos, Ter, Aqu, Arb) 

trait_inat_eddmaps_clean <- trait_inat_eddmaps_prep %>%
  mutate(
    `Active time` = case_when(
      rowSums(across(c(Diu, Noc, Crepu), ~ . == 1), na.rm = TRUE) > 1 ~ "Cathemeral",
      Diu == 1 ~ "Diurnal",
      Noc == 1 ~ "Nocturnal",
      Crepu == 1 ~ "Crepuscular",
      TRUE ~ `Active time`  # keeps NA if none apply
    )
  ) %>%
  mutate(
    `Maximum body mass (g)` = coalesce(Body_mass_g, `Maximum body mass (g)`)
  )

# what percentage of data do we now have trait data for?
nrow(trait_inat_eddmaps %>% filter(complete.cases(Genus)))/nrow(trait_inat_eddmaps)*100
# 82.0%
nrow(trait_inat_eddmaps_clean %>% filter(!is.na(`Habitat type`) | !is.na(`Active time`) | !is.na(`Maximum body mass (g)`) |
                                           !is.na(Fos) | !is.na(Ter) | !is.na(Aqu) | !is.na(Arb)))/
  nrow(trait_inat_eddmaps_clean)

# how many amphibian species do we have habitat data on?
nrow(trait_inat_eddmaps_clean %>% filter(!is.na(Fos) | !is.na(Ter) | !is.na(Aqu) | !is.na(Arb)))
# only 7, so let's focus the habitat analysis only on reptiles

# now, we will use our earlier spearman's rank correlation to assess these different categories

# let's start with active time
# how many observations do we have for each "active time" 
trait_inat_eddmaps_clean %>%
  group_by(`Active time`) %>%
  summarise(n=n())
# The only categories we will use are cathemeral, diurnal, and nocturnal

# cathemeral
cathemeral <- trait_inat_eddmaps_clean %>%
  filter(`Active time`=="Cathemeral")
cor.test(cathemeral$inat_number_of_obs,
         cathemeral$eddmaps_number_of_obs,
         method = "spearman")

# diurnal
diurnal <- trait_inat_eddmaps_clean %>%
  filter(`Active time`=="Diurnal")
cor.test(diurnal$inat_number_of_obs,
         diurnal$eddmaps_number_of_obs,
         method = "spearman")

# nocturnal
nocturnal <- trait_inat_eddmaps_clean %>%
  filter(`Active time`=="Nocturnal")
cor.test(nocturnal$inat_number_of_obs,
         nocturnal$eddmaps_number_of_obs,
         method = "spearman")


# get log ratio of iNaturalist observations
trait_inat_eddmaps_clean <- trait_inat_eddmaps_clean %>%
  filter(inat_number_of_obs > 0,
         eddmaps_number_of_obs > 0) %>%
  mutate(
    inat_prop = inat_number_of_obs / sum(inat_number_of_obs, na.rm = TRUE),
    eddmaps_prop = eddmaps_number_of_obs / sum(eddmaps_number_of_obs, na.rm = TRUE),
    prop_ratio = log10(inat_prop / eddmaps_prop)
  )

hist(trait_inat_eddmaps_clean$prop_ratio)

# examine raw data
(activity <- ggplot(trait_inat_eddmaps_clean %>% filter(complete.cases(`Active time`), `Active time`!="Crepuscular"), aes(x=`Active time`, y=prop_ratio)) +
    geom_boxplot() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    labs(y="Log-proportional ratio of observations\n(iNaturalist / EDDMapS)",
         title="A.") + 
    theme_classic(base_size = 14))

# run three seperate GLM for each activity pattern to see if there is a significant difference between the platforms
glm_diurnal <- glm(prop_ratio ~ 1, data = subset(trait_inat_eddmaps_clean, `Active time` == "Diurnal"))
summary(glm_diurnal)

glm_nocturnal <- glm(prop_ratio ~ 1, data = subset(trait_inat_eddmaps_clean, `Active time` == "Nocturnal"))
summary(glm_nocturnal)

glm_cathemeral <- glm(prop_ratio ~ 1, data = subset(trait_inat_eddmaps_clean, `Active time` == "Cathemeral"))
summary(glm_cathemeral)

# Get estimates
coefs <- tibble(
  Activity = c("Diurnal", "Nocturnal", "Cathemeral"),
  Estimate = c(coef(glm_diurnal), coef(glm_nocturnal), coef(glm_cathemeral)),
  SE = c(summary(glm_diurnal)$coefficients[,"Std. Error"],
         summary(glm_nocturnal)$coefficients[,"Std. Error"],
         summary(glm_cathemeral)$coefficients[,"Std. Error"])
)

# Compute 95% confidence intervals
coefs <- coefs %>%
  mutate(
    lower = Estimate - 1.96*SE,
    upper = Estimate + 1.96*SE
  )

# Coefficient plot with Nocturnal in blue
(coef_activity <- ggplot(coefs, aes(x = Activity, y = Estimate)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_identity() +  # use the actual colors in the column
  labs(
    x = "Activity period",
    y = "Log-proportional ratio of observations\n(iNaturalist / EDDMapS)"
  ) +
  theme_classic(base_size = 14))


# log-transform body size since itis highly skewed
trait_inat_eddmaps_clean$log_body_size <- log10(trait_inat_eddmaps_clean$`Maximum body mass (g)`)

body_mass_glm <- glm(prop_ratio ~ log_body_size, data=trait_inat_eddmaps_clean, family = gaussian)
summary(body_mass_glm)
plot(body_mass_glm)

(body_size <- ggplot(trait_inat_eddmaps_clean, aes(x = `Maximum body mass (g)`, y = prop_ratio)) +
  geom_point() +
  scale_x_log10(labels = trans_format("log10", math_format(10^.x))) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(y = "Log-proportional ratio of observations\n(iNaturalist / EDDMapS)",
       title="B.") + 
  theme_classic(base_size = 14))

# now let's do this for habitat type
# this column is going to have to be restructured
# Get all unique habitat types
habitats <- trait_inat_eddmaps_clean %>%
  filter(!is.na(`Habitat type`)) %>%
  pull(`Habitat type`) %>%
  str_split("/") %>% 
  unlist() %>% 
  str_trim() %>%
  unique()

# Create TRUE/FALSE columns
trait_habitat <- trait_inat_eddmaps_clean %>%
  bind_cols(
    lapply(habitats, function(hab) {
      ifelse(
        is.na(trait_inat_eddmaps_clean$`Habitat type`),
        NA,
        str_detect(trait_inat_eddmaps_clean$`Habitat type`, fixed(hab))
      )
    }) %>% setNames(habitats)
  )

# Check result
head(trait_habitat)

# check for correlations
# Select only habitat columns
habitat_cols <- trait_habitat %>% dplyr::select(Savanna:Desert)

# Compute correlation matrix
cor_matrix <- cor(habitat_cols, use = "pairwise.complete.obs")
cor_matrix
# there does not appear to be concerning correlations

# let's determine which habitats have more than a handful of species occupying them
# Count how many species occur in each habitat
habitat_counts <- trait_habitat %>%
  dplyr::select(all_of(habitats)) %>%  # your TRUE/FALSE habitat columns
  summarise(across(everything(), ~sum(. == TRUE, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "habitat", values_to = "n_species")

# Habitats with at least 5 species
habitats_filtered <- habitat_counts %>%
  filter(n_species >= 5) %>%
  pull(habitat)
habitats_filtered

# I am also going to remove deser

habitat_glm <- glm(prop_ratio ~ Savanna + Forest + Shrubland + Grassland + Wetlands + Rocky, 
                   data=trait_habitat, family = gaussian)
summary(habitat_glm)

# examine raw data
# Gather habitats into long format
trait_habitat_long <- trait_habitat %>%
  dplyr::select(species, prop_ratio, all_of(habitats)) %>%
  pivot_longer(
    cols = habitats_filtered[-7],
    names_to = "Habitat",
    values_to = "Present"
  ) %>%
  filter(Present == TRUE)  # only include habitats the species occupies

# Plot
(habitat <- ggplot(trait_habitat_long, aes(x = Habitat, y = prop_ratio)) +
  geom_boxplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # reference line
  labs(
    y = "Log-proportional ratio of observations\n(iNaturalist / EDDMapS)",
    x = "Habitat type",
    title="C."
  ) +
  theme_classic(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)))

# make a coefficient plot
# Get coefficients and SEs
coefs <- tibble(
  Habitat = c("Intercept", "Savanna", "Forest", "Shrubland", "Grassland", "Wetlands", "Rocky"),
  Estimate = coef(habitat_glm),
  SE = summary(habitat_glm)$coefficients[, "Std. Error"]
)

# Compute 95% confidence intervals
coefs <- coefs %>%
  mutate(
    lower = Estimate - 1.96*SE,
    upper = Estimate + 1.96*SE
  )

(coef_habitat <- ggplot(coefs %>% filter(Habitat != "Intercept"), aes(x = Habitat, y = Estimate)) +
    geom_point(size = 4, color = "black") +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, color = "black") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(
      x = "Habitat type",
      y = "Log-proportional ratio of observations\n(iNaturalist / EDDMapS)"
    ) +
    theme_classic(base_size = 14))

# use patchwork to make a nice, combined figure
(coef_activity + body_size) / coef_habitat

ggsave("Figures/species_traists_platform.jpeg", height=8, width=8, units="in")

# ==============================================================================









### Density Histogram for Population Density / Urbanization Proxy (Obj 2.)

# -----------------------------
# 1) Apply 1000 m uncertainty filter (Obj 2 only)
# -----------------------------
inat_obj2 <- filtered_data_iNat_florida %>%
  filter(is.na(coordinateUncertaintyInMeters) | coordinateUncertaintyInMeters <= 1000) %>%
  mutate(Source = "iNaturalist")

edd_obj2 <- filtered_data_eddmaps_florida %>%
  filter(is.na(coordinateUncertaintyInMeters) | coordinateUncertaintyInMeters <= 1000) %>%
  mutate(Source = "EDDMapS")

# -----------------------------
# 2) Extract population density for each record (raster)
# -----------------------------
pop_density <- raster("Data/fl_pop_density.tif")
crs(pop_density) <- "+proj=longlat +datum=WGS84 +no_defs"

extract_popdensity <- function(df, r) {
  pts <- st_as_sf(df, coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE)
  df$PopDensity <- raster::extract(r, st_coordinates(pts))
  df
}

inat_obj2 <- extract_popdensity(inat_obj2, pop_density)
edd_obj2  <- extract_popdensity(edd_obj2,  pop_density)

# combine + remove missing/non-finite + remove zeros (log + log10 plots require > 0)
PopData_obj2 <- bind_rows(inat_obj2, edd_obj2) %>%
  filter(!is.na(PopDensity), is.finite(PopDensity), PopDensity > 0)

# -----------------------------
# 3) Define response coding for binomial GLM (explicit + reproducible)
#    Interpretation: odds(record is iNaturalist) vs EDDMapS
# -----------------------------
PopData_obj2 <- PopData_obj2 %>%
  mutate(
    Source = factor(Source, levels = c("EDDMapS", "iNaturalist")),  # EDDMapS reference
    is_iNat = if_else(Source == "iNaturalist", 1L, 0L),
    LogPopDensity = log(PopDensity)  # transformation for interpretability + functional form
  )

# -----------------------------
# 4) Visual checks (reviewer-requested)
#    A) Distribution (raw shown on log10 x-axis; log shown directly)
# -----------------------------
p_hist_raw <- ggplot(PopData_obj2, aes(x = PopDensity, fill = Source)) +
  geom_histogram(position = "identity", alpha = 0.35, bins = 50) +
  scale_x_log10(labels = label_number()) +
  scale_fill_manual(values = c("EDDMapS" = "#FD7B25", "iNaturalist" = "#A7FD25")) +
  labs(x = "Population density (persons/km²; log10 x-axis)",
       y = "Count",
       fill = "Platform") +
  theme_classic()

p_hist_log <- ggplot(PopData_obj2, aes(x = LogPopDensity, fill = Source)) +
  geom_histogram(position = "identity", alpha = 0.35, bins = 50) +
  scale_fill_manual(values = c("EDDMapS" = "#FD7B25", "iNaturalist" = "#A7FD25")) +
  labs(x = "log(PopDensity)",
       y = "Count",
       fill = "Platform") +
  theme_classic()

print(p_hist_raw)
print(p_hist_log)

#    B) Empirical relationship: Pr(iNat) vs density (raw + log)
p_prob_raw <- ggplot(PopData_obj2, aes(x = PopDensity, y = is_iNat)) +
  geom_point(alpha = 0.05) +
  geom_smooth(method = "gam", formula = y ~ s(x), se = TRUE, color = "black") +
  scale_x_log10(labels = label_number()) +
  labs(x = "Population density (persons/km²; log10 x-axis)",
       y = "Pr(record is iNaturalist)") +
  theme_classic()

p_prob_log <- ggplot(PopData_obj2, aes(x = LogPopDensity, y = is_iNat)) +
  geom_point(alpha = 0.05) +
  geom_smooth(method = "gam", formula = y ~ s(x), se = TRUE, color = "black") +
  labs(x = "log(PopDensity)",
       y = "Pr(record is iNaturalist)") +
  theme_classic()

print(p_prob_raw)
print(p_prob_log)

# -----------------------------
# 5) Primary model (binomial GLM)
# -----------------------------
m_logit <- glm(is_iNat ~ LogPopDensity, data = PopData_obj2, family = binomial)
summary(m_logit)

# Odds ratio (per 1-unit increase in log density)
OR <- exp(coef(m_logit)["LogPopDensity"])
CI <- exp(confint(m_logit)["LogPopDensity", ])
cat("\nOdds ratio for iNaturalist (vs EDDMapS) per 1-unit increase in log(PopDensity):\n")
cat(sprintf("  OR = %.3f (95%% CI: %.3f–%.3f)\n", OR, CI[1], CI[2]))

# -----------------------------
# 6) Model form comparison (simple + transparent)
# -----------------------------
m_logit_raw <- glm(is_iNat ~ PopDensity, data = PopData_obj2, family = binomial)
cat("\nAIC comparison:\n")
cat(sprintf("  AIC (raw PopDensity): %.2f\n", AIC(m_logit_raw)))
cat(sprintf("  AIC (log PopDensity): %.2f\n", AIC(m_logit)))

# -----------------------------
# 7) Manuscript figure: density curves (same colors as before)
# -----------------------------
p_density <- ggplot(PopData_obj2, aes(x = PopDensity, fill = Source)) +
  geom_density(alpha = 0.35) +
  scale_x_log10(labels = label_number()) +
  scale_fill_manual(values = c("EDDMapS" = "#FD7B25", "iNaturalist" = "#A7FD25")) +
  labs(x = "Population density (persons/km²; log10 x-axis)",
       y = "Density",
       fill = "Platform") +
  theme_classic()

print(p_density)

# So in manuscript we would explain that we did the log transformation for interpritability, not for an "assumption" requirement.








# =============================================================================

### MCP analysis (Obj 3.)
library(dplyr)
library(tidyr)
library(sp)
library(adehabitatHR)
library(sf)     # For spatial operations
library(units)  # For unit conversion

# Process EDDMapS data
filtered_data_eddmaps_florida2 <- filtered_data_eddmaps_florida %>% filter(Year >= 2014 & Year <= 2024)


# Process iNaturalist data
filtered_data_iNat_florida2 <- filtered_data_iNat_florida %>% filter(Year >= 2014 & Year <= 2024)


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
    legend.position = c(0.85, 0.15),
    legend.background = element_rect(fill = "white", color = "black", size = 0.3),
    legend.margin = margin(6, 6, 6, 6)
  ) +
  annotation_logticks(sides = "bl")

# Display the plot
print(mcp_obs_plot)















# ==============================================
# Fig 1 Scatter Plot WITH "Both platforms" + exclusives
# (uses Florida-filtered dataframes)
# ==============================================

# ------- Build per-species counts (Florida-filtered) -------
iNat_counts <- filtered_data_iNat_florida %>%
  count(species, name = "inat_n")

EDD_counts <- filtered_data_eddmaps_florida %>%
  count(species, name = "edd_n")

counts <- full_join(iNat_counts, EDD_counts, by = "species")

# ------- Assign platform category + build plotting coords -------
epsilon <- 0.5  # axis floor for "absent" side on log scale

plot_df <- counts %>%
  mutate(
    platform = case_when(
      !is.na(inat_n) & !is.na(edd_n) ~ "Both platforms",
      !is.na(inat_n) &  is.na(edd_n) ~ "iNaturalist only",
      is.na(inat_n) & !is.na(edd_n) ~ "EDDMapS only",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(platform)) %>%
  mutate(
    x_plot = if_else(is.na(inat_n), epsilon, as.double(inat_n)),
    y_plot = if_else(is.na(edd_n), epsilon, as.double(edd_n))
  )

# ------- Jitter settings (multiplicative in log space) -------
set.seed(42)
jitter_dex <- 0.05   # ±0.05 log10 units ≈ ±12%
floor_jit  <- 0.10   # lift off axis floor (log10 units)

plot_df <- plot_df %>%
  mutate(
    jx = 10^(runif(n(), -jitter_dex, jitter_dex)),
    jy = 10^(runif(n(), -jitter_dex, jitter_dex)),
    xj = if_else(x_plot == epsilon, epsilon * 10^(runif(n(), 0, floor_jit)), x_plot * jx),
    yj = if_else(y_plot == epsilon, epsilon * 10^(runif(n(), 0, floor_jit)), y_plot * jy)
  )

# ------- Plot (regression line ONLY on "Both platforms") -------
Fig_1_Line_with_platforms <- ggplot() +
  geom_point(
    data = plot_df,
    aes(x = xj, y = yj, color = platform),
    size = 2.6, alpha = 0.9
  ) +
  geom_smooth(
    data = plot_df %>% filter(platform == "Both platforms"),
    aes(x = x_plot, y = y_plot),
    method = "lm", se = TRUE, color = "black"
  ) +
  scale_x_log10(limits = c(epsilon, NA)) +
  scale_y_log10(limits = c(epsilon, NA)) +
  scale_color_manual(
    values = c(
      "Both platforms" = "#7FB069",
      "iNaturalist only" = "#A7FD25",
      "EDDMapS only"     = "#FD7B25"
    ),
    name = NULL
  ) +
  labs(
    x = "iNaturalist observations",
    y = "EDDMapS observations"
  ) +
  theme_classic() +
  theme(legend.position = "none")   # <- drop legend

Fig_1_Line_with_platforms










# ---- Choose species to highlight ----
species_to_highlight <- c("Graptemys pseudogeographica", "Xenopus laevis")

# Build a small table of those species from your per-species counts
hl_raw <- counts %>%
  filter(species %in% species_to_highlight)

# If none found, bail early (optional)
if (nrow(hl_raw) == 0) message("None of the highlight species were found in 'counts'.")

# Compute plotted positions with same floor & multiplicative jitter you used above
set.seed(101)  # separate seed for reproducible highlight positions
hl_points <- hl_raw %>%
  mutate(
    x_plot = if_else(is.na(inat_n), epsilon, as.double(inat_n)),
    y_plot = if_else(is.na(edd_n),  epsilon, as.double(edd_n)),
    jx     = 10^(runif(n(), -jitter_dex, jitter_dex)),
    jy     = 10^(runif(n(), -jitter_dex, jitter_dex)),
    xh = if_else(
      is.na(inat_n),
      epsilon * 10^(runif(n(), 0, floor_jit)),  # nudge off x-floor
      as.double(inat_n) * jx                    # multiplicative jitter
    ),
    yh = if_else(
      is.na(edd_n),
      epsilon * 10^(runif(n(), 0, floor_jit)),  # nudge off y-floor
      as.double(edd_n) * jy                     # multiplicative jitter
    )
  )

# ---- Overlay highlight points + labels on your existing figure ----
Fig_1_Line_with_exclusive_highlights <- Fig_1_Line_with_exclusive +
  geom_point(
    data = hl_points,
    aes(x = xh, y = yh, fill = species),
    shape = 21, color = "black", stroke = 1.2, size = 4.2,
    inherit.aes = FALSE
  ) +
  ggrepel::geom_text_repel(
    data = hl_points,
    aes(x = xh, y = yh, label = species),
    size = 3.6, fontface = "bold",
    box.padding = 0.3, point.padding = 0.25, segment.size = 0.4,
    max.overlaps = Inf, inherit.aes = FALSE
  ) +
  scale_fill_manual(
    values = c(
      "Graptemys pseudogeographica" = "gold",
      "Xenopus laevis"              = "deepskyblue"
    ),
    guide = "none"
  )

Fig_1_Line_with_exclusive_highlights



# ========================
# Splitting by Rep and Amphib
# ========================

# ---- Taxonomic groups from iNat introduced list ----
taxon_groups_iNat <- iNaturalist_introduced %>%
  transmute(
    Species = scientific_name,
    group   = case_when(
      taxon == "Amphibia" ~ "Amphibian",
      taxon == "Reptilia" ~ "Reptile",
      TRUE                ~ NA_character_
    )
  ) %>%
  distinct()

# ---- Taxonomic groups from EDDMapS introduced list ----
taxon_groups_edd <- eddmaps_introduced_clean %>%
  transmute(
    Species = scientific_name,
    group   = case_when(
      taxon == "Amphibia" ~ "Amphibian",
      taxon == "Reptilia" ~ "Reptile",
      TRUE                ~ NA_character_
    )
  ) %>%
  distinct()

# ---- Combine into a master lookup ----
all_groups <- bind_rows(taxon_groups_iNat, taxon_groups_edd) %>%
  group_by(Species) %>%
  summarise(
    group = {
      g <- unique(na.omit(group))
      if (length(g) == 0) NA_character_ else g[1]
    },
    .groups = "drop"
  )

presence <- read.csv("Data/species_presence_comparison.csv")
presence <- presence %>%
  filter(!(Species %in% "Basiliscus spp.")) %>%
  filter(!(Species %in% "Iguana spp.")) %>%
  filter(!(Species %in% "Leiocephalus spp.")) %>%
  filter(!(Species %in% "Python spp.")) %>%
  filter(!(Species %in% "Trioceros spp.")) %>%
  rename(iNaturalist = iNaturalist_Present) %>%
  rename(EDDMapS = EDDMapS_Present)

# ---- Attach reptile/amphibian group to presence table ----
species_groups <- presence %>%
  left_join(all_groups, by = "Species")
# Columns: Species, iNaturalist, EDDMapS, group

# ---- iNaturalist: number of reptile vs amphibian species ----
inat_species_counts <- species_groups %>%
  filter(iNaturalist == "Yes", !is.na(group)) %>%
  count(group, name = "n_species") %>%
  mutate(prop_species = n_species / sum(n_species))

inat_species_counts

# ---- EDDMapS: number of reptile vs amphibian species ----
eddmaps_species_counts <- species_groups %>%
  filter(EDDMapS == "Yes", !is.na(group)) %>%
  count(group, name = "n_species") %>%
  mutate(prop_species = n_species / sum(n_species))

eddmaps_species_counts

both_species_counts <- species_groups %>%
  filter((iNaturalist == "Yes" | EDDMapS == "Yes"),
         !is.na(group)) %>%
  count(group, name = "n_species") %>%
  mutate(prop_species = n_species / sum(n_species))

both_species_counts












# =======================================================
# Check to see how many obs removed > 1000 m uncertainty
# =======================================================

# --- EDDMapS---
edd_pre_unc <- eddmaps %>%
  filter(SciName %in% eddmaps_introduced_clean$scientific_name) %>%
  dplyr::select(SciName, ObsDate, Latitude, Longitude, CoordAcc) %>%
  filter(!is.na(Latitude) & !is.na(Longitude)) %>%
  mutate(
    coordinateUncertaintyInMeters = as.numeric(as.character(CoordAcc))
  )


filtered_data_eddmaps <- edd_pre_unc %>%
  filter(is.na(coordinateUncertaintyInMeters) | coordinateUncertaintyInMeters <= 1000) %>%
  dplyr::select(-CoordAcc)


edd_removed_summary <- tibble(
  dataset = "EDDMapS",
  n_before = nrow(edd_pre_unc),
  n_after  = nrow(filtered_data_eddmaps),
  removed  = n_before - n_after,
  pct_removed = round(100 * removed / n_before, 2),
  
  # Diagnostics from PRE data:
  n_uncertainty_NA = sum(is.na(edd_pre_unc$coordinateUncertaintyInMeters)),
  n_uncertainty_gt1000 = sum(edd_pre_unc$coordinateUncertaintyInMeters > 1000, na.rm = TRUE),
  
  # With NA-kept logic:
  removed_uncertainty_NA = 0,
  removed_uncertainty_gt1000 = sum(edd_pre_unc$coordinateUncertaintyInMeters > 1000, na.rm = TRUE)
)

edd_removed_summary


# --- iNat ---
iNat_pre_unc <- iNat %>%
  filter(species %in% iNaturalist_introduced$scientific_name) %>%
  filter(!(species %in% c(
    "Anaxyrus fowleri", "Desmognathus conanti", "Pseudotriton ruber",
    "Eurycea cirrigera", "Eurycea guttolineata", "Eretmochelys imbricata",
    "Incilius nebulifer", "Lampropeltis rhombomaculata", "Lepidochelys kempii",
    "Lithobates virgatipes"
  ))) %>%
  dplyr::select(
    species, decimalLatitude, decimalLongitude, day, month, year,
    coordinateUncertaintyInMeters
  ) %>%
  filter(!is.na(decimalLatitude) & !is.na(decimalLongitude))


filtered_data_iNat <- iNat_pre_unc %>%
  filter(is.na(coordinateUncertaintyInMeters) | coordinateUncertaintyInMeters <= 1000)


inat_removed_summary <- tibble(
  dataset = "iNaturalist",
  n_before = nrow(iNat_pre_unc),
  n_after  = nrow(filtered_data_iNat),
  removed  = n_before - n_after,
  pct_removed = round(100 * removed / n_before, 2),
  
  # Diagnostics about what existed in the PRE data:
  n_uncertainty_NA = sum(is.na(iNat_pre_unc$coordinateUncertaintyInMeters)),
  n_uncertainty_gt1000 = sum(iNat_pre_unc$coordinateUncertaintyInMeters > 1000, na.rm = TRUE),
  
  # With NA-kept logic, these are not removed due to uncertainty:
  removed_uncertainty_NA = 0,
  removed_uncertainty_gt1000 = sum(iNat_pre_unc$coordinateUncertaintyInMeters > 1000, na.rm = TRUE)
)

inat_removed_summary
