# =============================================================================
# KDE analysis (Obj 3.) — mirrors your MCP pipeline but uses 95% Kernel UD
# =============================================================================
library(dplyr)
library(tidyr)
library(sp)
library(adehabitatHR)
library(sf)
library(units)
library(ggplot2)
library(lme4)

# -----------------------------
# 0) Load + prep (unchanged filters/cleaning)
# -----------------------------
eddmaps_new2 <- read.csv("/Users/mario/Desktop/iNat-vs.-Eddmaps/Data/EDDmapS_observations.csv")

filtered_data_eddmaps2 <- eddmaps_new2 %>%
  filter(SciName %in% eddmaps_introduced_clean$scientific_name) %>%
  dplyr::select(SciName, ObsDate, Latitude, Longitude) %>%
  filter(!is.na(Latitude) & !is.na(Longitude)) %>%
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
    SciName == "Chelonoidis denticulatus" ~ "Chelonoidis denticulatus",
    SciName == "Ramphotyphlops braminus"~ "Indotyphlops braminus",
    SciName == "Lygodactylus luteopicturatus" ~ "Lygodactylus picturatus",
    SciName == "Chelonoidis denticulata" ~ "Chelonoidis denticulatus",
    SciName == "Leiocephalus schreibersii schreibersii" ~ "Leiocephalus schreibersii",
    SciName == "Leiolepis belliana belliana" ~ "Leiolepis belliana",
    SciName == "Litoria caerulea" ~ "Ranoidea caerulea",
    SciName == "Epicrates cenchria cenchria" ~ "Epicrates cenchria",
    SciName == "Epicrates cenchria maurus" ~ "Epicrates cenchria",
    SciName == "Trachemys scripta elegans" ~ "Trachemys scripta elegans",
    TRUE ~ SciName
  )) %>%
  rename(species = SciName) %>%
  separate(ObsDate, into = c("Month", "Day", "Year"), sep = "/") %>%
  mutate(source = "EDDMapS")

filtered_data_eddmaps_florida2 <- filtered_data_eddmaps2 %>%
  filter(Latitude >= 24.396308 & Latitude <= 31.000888,
         Longitude >= -87.634938 & Longitude <= -80.031362)

iNat2 <- readRDS("Data/iNat_herp_data.RDS")

filtered_data_iNat2 <- iNat2 %>%
  filter(species %in% iNaturalist_introduced$scientific_name) %>%
  filter(!(species %in% c(
    "Anaxyrus fowleri","Desmognathus conanti","Pseudotriton ruber",
    "Eurycea cirrigera","Eurycea guttolineata","Eretmochelys imbricata",
    "Incilius nebulifer","Lampropeltis rhombomaculata","Lepidochelys kempii",
    "Lithobates virgatipes"))) %>%
  dplyr::select(species, decimalLatitude, decimalLongitude, day, month, year) %>%
  filter(!is.na(decimalLatitude) & !is.na(decimalLongitude)) %>%
  rename(Latitude = decimalLatitude, Longitude = decimalLongitude,
         Month = month, Day = day, Year = year) %>%
  mutate(Month = as.character(Month),
         Day = as.character(Day),
         Year = as.character(Year),
         source = "iNaturalist")

filtered_data_iNat_florida2 <- filtered_data_iNat2 %>%
  filter(Latitude >= 24.396308 & Latitude <= 31.000888,
         Longitude >= -87.634938 & Longitude <= -80.031362)

# Years filter
filtered_data_eddmaps_florida2 <- filtered_data_eddmaps_florida2 %>%
  mutate(Year = as.numeric(Year)) %>%
  filter(Year >= 2014 & Year <= 2024) %>%
  mutate(Year = as.character(Year))

filtered_data_iNat_florida2 <- filtered_data_iNat_florida2 %>%
  mutate(Year = as.numeric(Year)) %>%
  filter(Year >= 2014 & Year <= 2024) %>%
  mutate(Year = as.character(Year))

# -----------------------------
# 1) KDE area helper (95% UD in km^2)
# -----------------------------
calculate_kde_area <- function(data, h = "href", percent = 95, grid = 300) {
  # Require ≥5 unique points to be comparable to your MCP threshold
  coords <- data[, c("Longitude", "Latitude")]
  coords <- coords[complete.cases(coords), , drop = FALSE]
  coords_unique <- unique(coords)
  if (nrow(coords_unique) < 5) return(NA_real_)
  
  # Points in WGS84 → Equal-area (Albers) for North America
  sp_pts <- sp::SpatialPoints(coords_unique, proj4string = sp::CRS("+proj=longlat +datum=WGS84"))
  ea_crs <- sp::CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-84 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")
  sp_pts_ea <- sp::spTransform(sp_pts, ea_crs)
  
  # Data frame SAME number of rows as points
  df <- data.frame(id = factor(rep("all", length(sp_pts_ea))))
  rownames(df) <- seq_len(nrow(df))
  spdf <- sp::SpatialPointsDataFrame(sp_pts_ea, data = df)
  
  # Kernel UD → vertices → area
  ud <- tryCatch(adehabitatHR::kernelUD(spdf["id"], h = h, grid = grid, same4all = TRUE),
                 error = function(e) NULL)
  if (is.null(ud)) return(NA_real_)
  
  ver <- tryCatch(adehabitatHR::getverticeshr(ud, percent),
                  error = function(e) NULL)
  if (is.null(ver)) return(NA_real_)
  
  as.numeric(sf::st_area(sf::st_as_sf(ver)) / 1e6)  # km^2
}

# -----------------------------
# 2) Species counts + filter
# -----------------------------
species_counts <- full_join(
  filtered_data_eddmaps_florida2 %>% group_by(species) %>% summarize(EDDMapS_count = n(), .groups = "drop"),
  filtered_data_iNat_florida2    %>% group_by(species) %>% summarize(iNat_count   = n(), .groups = "drop"),
  by = "species"
) %>%
  mutate(
    EDDMapS_count = tidyr::replace_na(EDDMapS_count, 0),
    iNat_count    = tidyr::replace_na(iNat_count, 0)
  )

valid_species <- species_counts %>%
  filter(EDDMapS_count >= 5 | iNat_count >= 5)

# -----------------------------
# 3) Loop species → KDE areas
# -----------------------------
kde_results_by_species <- data.frame()

for (i in seq_len(nrow(valid_species))) {
  species_i <- valid_species$species[i]
  
  inat_data <- filtered_data_iNat_florida2    %>% filter(species == species_i)
  eddmaps_data <- filtered_data_eddmaps_florida2 %>% filter(species == species_i)
  combined_data <- bind_rows(inat_data, eddmaps_data)
  
  inat_kde_area     <- if (nrow(inat_data)     >= 5) calculate_kde_area(inat_data)     else NA_real_
  eddmaps_kde_area  <- if (nrow(eddmaps_data)  >= 5) calculate_kde_area(eddmaps_data)  else NA_real_
  combined_kde_area <- if (nrow(combined_data) >= 5) calculate_kde_area(combined_data) else NA_real_
  
  kde_results_by_species <- bind_rows(kde_results_by_species, data.frame(
    Species = species_i,
    iNat_kde_area_km2 = inat_kde_area,
    EDDMapS_kde_area_km2 = eddmaps_kde_area,
    Combined_kde_area_km2 = combined_kde_area,
    iNat_observations = nrow(inat_data),
    EDDMapS_observations = nrow(eddmaps_data),
    Combined_observations = nrow(combined_data)
  ))
}

# -----------------------------
# 4) Comparisons/summary
# -----------------------------
kde_results_by_species <- kde_results_by_species %>%
  mutate(
    Area_difference_km2 = abs(iNat_kde_area_km2 - EDDMapS_kde_area_km2),
    Area_ratio = pmax(iNat_kde_area_km2, EDDMapS_kde_area_km2, na.rm = TRUE) /
      pmin(iNat_kde_area_km2, EDDMapS_kde_area_km2, na.rm = TRUE),
    Larger_platform = case_when(
      is.na(iNat_kde_area_km2) & !is.na(EDDMapS_kde_area_km2) ~ "EDDMapS",
      !is.na(iNat_kde_area_km2) & is.na(EDDMapS_kde_area_km2) ~ "iNaturalist",
      iNat_kde_area_km2 > EDDMapS_kde_area_km2 ~ "iNaturalist",
      EDDMapS_kde_area_km2 > iNat_kde_area_km2 ~ "EDDMapS",
      TRUE ~ "Equal"
    )
  )

print(kde_results_by_species)

summary_stats <- kde_results_by_species %>%
  summarize(
    Total_Species = n(),
    Species_with_both_KDEs = sum(!is.na(iNat_kde_area_km2) & !is.na(EDDMapS_kde_area_km2)),
    Species_with_only_iNat_KDE = sum(!is.na(iNat_kde_area_km2) & is.na(EDDMapS_kde_area_km2)),
    Species_with_only_EDDMapS_KDE = sum(is.na(iNat_kde_area_km2) & !is.na(EDDMapS_kde_area_km2)),
    Avg_iNat_Area_km2 = mean(iNat_kde_area_km2, na.rm = TRUE),
    Avg_EDDMapS_Area_km2 = mean(EDDMapS_kde_area_km2, na.rm = TRUE),
    Avg_Combined_Area_km2 = mean(Combined_kde_area_km2, na.rm = TRUE),
    Median_Area_Ratio = median(Area_ratio, na.rm = TRUE)
  )
print(summary_stats)

# -----------------------------
# 5) Figure 1 — platform KDE comparison (dot plot like yours)
# -----------------------------
plot_data <- kde_results_by_species %>%
  filter(!is.na(iNat_kde_area_km2) & !is.na(EDDMapS_kde_area_km2)) %>%
  mutate(
    comparison_score = (EDDMapS_kde_area_km2 - iNat_kde_area_km2) /
      (EDDMapS_kde_area_km2 + iNat_kde_area_km2),
    log_ratio = log2(EDDMapS_kde_area_km2 / iNat_kde_area_km2)
  ) %>%
  arrange(comparison_score)

plot_data$species_order <- 1:nrow(plot_data)

kde_comparison_plot <- ggplot(plot_data,
                              aes(x = comparison_score, y = reorder(Species, comparison_score))) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", size = 0.5) +
  geom_point(aes(size = Combined_observations, color = abs(comparison_score)), alpha = 0.8) +
  scale_color_gradient(low = "blue", high = "red", name = "Disparity") +
  scale_size_continuous(name = "Total Observations") +
  scale_x_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 0.5),
                     labels = c("iNaturalist\nlarger", "-0.5", "No Difference", "0.5", "EDDMapS\nlarger")) +
  labs(x = "Relative KDE (95%) Size Comparison Score", y = "Species") +
  scale_y_discrete(labels = function(x) lapply(x, function(y) bquote(italic(.(y))))) +
  theme_classic() +
  theme(
    axis.text.y = element_text(size = 8),
    panel.grid.major.y = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )
print(kde_comparison_plot)

# -----------------------------
# 6) Figure 2 — KDE area vs. observation count (mirrors your MCP figure)
# -----------------------------
paired_data <- kde_results_by_species %>%
  filter(!is.na(EDDMapS_kde_area_km2) & !is.na(iNat_kde_area_km2)) %>%
  mutate(
    log_EDDMapS_area = log10(EDDMapS_kde_area_km2),
    log_iNat_area    = log10(iNat_kde_area_km2),
    log_ratio        = log10(EDDMapS_kde_area_km2 / iNat_kde_area_km2),
    diff_log_area    = log_EDDMapS_area - log_iNat_area
  )

long_data <- paired_data %>%
  pivot_longer(
    cols = c(EDDMapS_kde_area_km2, iNat_kde_area_km2),
    names_to = "platform",
    values_to = "kde_area"
  ) %>%
  mutate(
    platform = factor(platform, levels = c("iNat_kde_area_km2", "EDDMapS_kde_area_km2")),
    log_kde_area = log10(kde_area)
  )

# attach per-platform obs counts
long_data <- long_data %>%
  mutate(
    obs_count = case_when(
      platform == "EDDMapS_kde_area_km2" ~ paired_data$EDDMapS_observations[match(Species, paired_data$Species)],
      platform == "iNat_kde_area_km2"    ~ paired_data$iNat_observations[match(Species, paired_data$Species)]
    ),
    log_obs_count = log10(obs_count)
  )

# Models (named parallel to MCP section)
kde_platform_model     <- glm(log_kde_area ~ platform, data = long_data); summary(kde_platform_model)
kde_mixed_model        <- lmer(log_kde_area ~ platform + (1|Species), data = long_data); summary(kde_mixed_model)
kde_obs_model          <- glm(log_kde_area ~ platform + log_obs_count, data = long_data); summary(kde_obs_model)
kde_obs_mixed_model    <- lmer(log_kde_area ~ platform + log_obs_count + (1|Species), data = long_data); summary(kde_obs_mixed_model)
kde_interaction_model  <- glm(log_kde_area ~ platform * log_obs_count, data = long_data); summary(kde_interaction_model)
anova(kde_obs_model, kde_interaction_model)

# Plot (supplemental-style)
plot_data_combined <- long_data %>%
  filter(!is.na(kde_area) & !is.na(obs_count)) %>%
  mutate(Platform = case_when(
    platform == "iNat_kde_area_km2" ~ "iNaturalist",
    platform == "EDDMapS_kde_area_km2" ~ "EDDMapS"
  ))

kde_obs_plot <- ggplot(plot_data_combined, aes(x = obs_count, y = kde_area)) +
  geom_point(aes(color = Platform), alpha = 0.7, size = 2.5) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.2, size = 1.2, color = "black") +
  scale_x_log10() +
  scale_y_log10(
    breaks = c(0.1, 1, 10, 100, 1000, 10000),
    labels = c("0.1", "1", "10", "100", "1000", "10000")
  ) +
  scale_color_manual(values = c("EDDMapS" = "#FD7B25", "iNaturalist" = "#A7FD25")) +
  labs(
    title = NULL,
    x = "Number of Observations (log scale)",
    y = "KDE Area (km²; 95%, log scale)",
    color = "Platform") +
  theme_classic() +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.title = element_text(size = 12),
    axis.text  = element_text(size = 10),
    legend.title = element_text(size = 11),
    legend.text  = element_text(size = 10),
    legend.position = c(0.15, 0.85),
    legend.background = element_rect(fill = "white", color = "black", size = 0.3)
  ) +
  annotation_logticks(sides = "bl")
print(kde_obs_plot)
