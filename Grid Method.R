# =========================================================
# Grid-based range analysis over Florida (sf-based workflow)
# =========================================================
library(dplyr)
library(tidyr)
library(sf)
library(units)
library(ggplot2)
library(fitdistrplus)
library(tigris)     # pulls Florida boundary (can be swapped for your own polygon)
library(DHARMa)
options(tigris_use_cache = TRUE)


# -----------------------------
# 0) Inputs (apply 1000 m filter HERE for this script)
# -----------------------------
# Use the Florida-filtered tables as the base (the ones you already have)
# and apply the same coordinate uncertainty filter used elsewhere.

inat_grid <- filtered_data_iNat_florida2 %>%
  filter(is.na(coordinateUncertaintyInMeters) | coordinateUncertaintyInMeters <= 1000)

edd_grid <- filtered_data_eddmaps_florida2 %>%
  filter(is.na(coordinateUncertaintyInMeters) | coordinateUncertaintyInMeters <= 1000)

stopifnot(all(c("species","Latitude","Longitude") %in% names(inat_grid)))
stopifnot(all(c("species","Latitude","Longitude") %in% names(edd_grid)))


# -----------------------------
# 1) Florida polygon (equal-area)
# -----------------------------
# Option A (auto): use tigris to get Florida
fl_wgs84 <- states(cb = TRUE, year = 2023) |>
  filter(STUSPS == "FL") |>
  st_as_sf()

# Option B (manual): if you already have a Florida shapefile/polygon in WGS84:
# fl_wgs84 <- st_read("path/to/florida_boundary.shp")

# Equal-area (Albers) CRS for North America (meters)
ea_crs <- st_crs("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-84 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")
fl_ea  <- st_transform(fl_wgs84, ea_crs)

# -----------------------------
# 2) Build grid (fishnet) over Florida and mask
# -----------------------------
# Choose either a cell WIDTH (km) or a target CELL AREA (km^2). Here we use width:
cell_width_km <- 10  # <- change this to 5, 10, 50, etc.
cell_width_m  <- cell_width_km * 1000

# Create grid covering Florida bbox in EA CRS, then clip to Florida polygon
grid_all   <- st_make_grid(fl_ea, cellsize = cell_width_m, square = TRUE)
grid_ea    <- st_intersection(st_as_sf(grid_all), fl_ea)  # mask to FL
grid_ea$cell_id <- seq_len(nrow(grid_ea))
grid_ea$cell_area_km2 <- as.numeric(set_units(st_area(grid_ea), km^2))

# -----------------------------
# 3) Convert points → sf and project to equal-area
# -----------------------------
to_points_sf <- function(df) {
  st_as_sf(df,
           coords = c("Longitude", "Latitude"),
           crs = 4326, remove = FALSE) |>
    st_transform(ea_crs)
}

inat_pts_ea    <- to_points_sf(inat_grid) |> mutate(platform = "iNaturalist")
eddmaps_pts_ea <- to_points_sf(edd_grid)  |> mutate(platform = "EDDMapS")


# (Optional) drop exact duplicate coordinates within species × platform to reduce bias
dedupe_points <- function(sf_pts) {
  sf_pts |>
    mutate(x = st_coordinates(geometry)[,1],
           y = st_coordinates(geometry)[,2]) |>
    distinct(platform, species, x, y, .keep_all = TRUE) |>
    dplyr::select(-x, -y)
}
inat_pts_ea    <- dedupe_points(inat_pts_ea)
eddmaps_pts_ea <- dedupe_points(eddmaps_pts_ea)

# -----------------------------
# 4) Join points to grid cells
# -----------------------------
# Return occupied cells per species × platform with area (km^2)
cells_by_species <- function(pts, grid) {
  # spatial join: each point inherits a cell_id
  pts_in <- st_join(pts, grid |> dplyr::select(cell_id, cell_area_km2), left = FALSE)  # keep only points that hit FL grid
  if (nrow(pts_in) == 0) return(tibble())
  
  pts_in |>
    st_drop_geometry() |>
    group_by(platform, species, cell_id) |>
    summarise(n_obs = n(), cell_area_km2 = first(cell_area_km2), .groups = "drop_last") |>
    summarise(
      occupied_cells = n(),
      area_km2 = sum(cell_area_km2, na.rm = TRUE),
      .groups = "drop"
    )
}

inat_cells    <- cells_by_species(inat_pts_ea, grid_ea)
eddmaps_cells <- cells_by_species(eddmaps_pts_ea, grid_ea)

# Observation tallies per species × platform (for plotting covariate)
obs_counts <- bind_rows(
  inat_pts_ea    |> st_drop_geometry() |> count(platform, species, name = "obs_count"),
  eddmaps_pts_ea |> st_drop_geometry() |> count(platform, species, name = "obs_count")
)

# Combine platform summaries into wide table
grid_area_by_species <- full_join(
  inat_cells |> rename(iNat_area_km2 = area_km2, iNat_cells = occupied_cells) |> mutate(platform = "iNaturalist"),
  eddmaps_cells |> rename(EDD_area_km2 = area_km2, EDD_cells = occupied_cells) |> mutate(platform = "EDDMapS"),
  by = join_by(species)
) |>
  # add obs counts
  left_join(
    obs_counts |> tidyr::pivot_wider(names_from = platform, values_from = obs_count,
                                     values_fill = 0, names_prefix = "n_"),
    by = "species"
  ) |>
  mutate(
    # Ensure NAs are zeros for areas where absent
    iNat_area_km2 = replace_na(iNat_area_km2, 0),
    EDD_area_km2  = replace_na(EDD_area_km2, 0),
    iNat_cells    = replace_na(iNat_cells, 0L),
    EDD_cells     = replace_na(EDD_cells, 0L),
    n_iNaturalist = replace_na(n_iNaturalist, 0L),
    n_EDDMapS     = replace_na(n_EDDMapS, 0L),
    combined_obs  = n_iNaturalist + n_EDDMapS
  )

# Optionally require a minimum number of occupied cells per platform (stability)
min_cells <- 1  # set to 2–3 if you want stricter ranges
grid_area_by_species <- grid_area_by_species |>
  mutate(
    iNat_area_km2 = ifelse(iNat_cells >= min_cells, iNat_area_km2, NA_real_),
    EDD_area_km2  = ifelse(EDD_cells  >= min_cells, EDD_area_km2,  NA_real_)
  )

# -----------------------------
# 5) Comparison metrics (same logic as MCP plots)
# -----------------------------
plot_data <- grid_area_by_species |>
  filter(!is.na(iNat_area_km2) & !is.na(EDD_area_km2)) |>
  mutate(
    comparison_score = (EDD_area_km2 - iNat_area_km2) / (EDD_area_km2 + iNat_area_km2),
    log_ratio        = log2((EDD_area_km2 + 1e-9) / (iNat_area_km2 + 1e-9)) # tiny offset for safety
  ) |>
  arrange(comparison_score)

# -----------------------------
# 6) Figure 1 — relative platform difference (dot plot)
# -----------------------------
p1 <- ggplot(plot_data,
             aes(x = comparison_score, y = reorder(species, comparison_score))) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", size = 0.5) +
  geom_point(aes(size = combined_obs, color = abs(comparison_score)), alpha = 0.85) +
  scale_color_gradient(low = "blue", high = "red", name = "Disparity") +
  scale_size_continuous(name = "Total Observations") +
  scale_x_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 0.5),
                     labels = c("iNaturalist\nlarger", "-0.5", "No Difference", "0.5", "EDDMapS\nlarger")) +
  scale_y_discrete(labels = function(x) lapply(x, function(y) bquote(italic(.(y))))) +
  labs(x = "Relative Grid-Range Size Comparison Score",
       y = "Species") +
  theme_classic() +
  theme(axis.text.y = element_text(size = 8),
        panel.grid.major.y = element_line(color = "gray90"),
        panel.grid.minor = element_blank(),
        legend.position = "right")
print(p1)

# -----------------------------
# 7) Figure 2 — grid-range area vs. observation count
# -----------------------------
long_for_plot <- grid_area_by_species |>
  dplyr::select(species, iNat_area_km2, EDD_area_km2, n_iNaturalist, n_EDDMapS) |>
  pivot_longer(c(iNat_area_km2, EDD_area_km2), names_to = "platform", values_to = "grid_area") |>
  mutate(
    Platform = ifelse(platform == "iNat_area_km2", "iNaturalist", "EDDMapS"),
    obs_count = ifelse(Platform == "iNaturalist", n_iNaturalist, n_EDDMapS)
  ) |>
  filter(!is.na(grid_area), obs_count > 0)

p2 <- ggplot(long_for_plot, aes(x = obs_count, y = grid_area)) +
  geom_point(aes(color = Platform), alpha = 0.7, size = 2.6) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.2, size = 1.1, color = "black") +
  scale_x_log10() +
  scale_y_log10(breaks = c(1,10,100,1000,10000,100000),
                labels = c("1","10","100","1,000","10,000","100,000")) +
  scale_color_manual(values = c("EDDMapS" = "#FD7B25", "iNaturalist" = "#A7FD25")) +
  labs(x = "Number of Observations (log scale)",
       y = paste0("Grid-Range Area (km²; cell width ", cell_width_km, " km; log scale)"),
       color = "Platform") +
  theme_classic() +
  annotation_logticks(sides = "bl")
print(p2)

# -----------------------------
# 8) (Optional) Export grid to shapefile/GeoPackage
# -----------------------------
# st_write(grid_ea, "outputs/florida_grid_25km.gpkg", layer = "grid25km", delete_dsn = TRUE)





library(lme4)

# -----------------------------
# 1) Prepare long-format data
# -----------------------------
grid_long <- grid_area_by_species |>
  dplyr::select(Species = species,
         iNat_area_km2, EDD_area_km2,
         n_iNaturalist, n_EDDMapS) |>
  tidyr::pivot_longer(
    cols = c(iNat_area_km2, EDD_area_km2),
    names_to = "platform",
    values_to = "grid_area"
  ) |>
  mutate(
    Platform = ifelse(platform == "iNat_area_km2", "iNaturalist", "EDDMapS"),
    obs_count = ifelse(Platform == "iNaturalist", n_iNaturalist, n_EDDMapS),
    log_grid_area = log10(grid_area),
    log_obs_count = log10(obs_count)
  ) |>
  filter(!is.na(log_grid_area), !is.na(log_obs_count), obs_count > 0)

# -----------------------------
# 2) Base GLM: area ~ platform + log(obs)
# -----------------------------

# plot histogram
ggplot(grid_long, aes(x = grid_area)) +
  geom_histogram(position = "identity", alpha = 0.35, bins = 50) +
  labs(x = "Grid-Range Area (km²)",
       y = "Count") +
  theme_classic()

ggsave("Figures/grid_hist.jpeg", height=4, width=4, units="in")

ggplot(grid_long, aes(x = obs_count)) +
  geom_histogram(position = "identity", alpha = 0.35, bins = 50) +
  labs(x = "Number of Observations",
       y = "Count") +
  theme_classic()

ggsave("Figures/grid_hist_obs.jpeg", height=4, width=4, units="in")

# check the response distribution
hist(grid_long$log_grid_area)
hist(grid_long$grid_area)

fit_norm <- fitdist(grid_long$log_grid_area, "norm")
fit_lnorm <- fitdist(grid_long$log_grid_area, "lnorm")
fit_gamma <- fitdist(grid_long$log_grid_area, "gamma")

gofstat(list(fit_norm, fit_lnorm, fit_gamma))

grid_obs_model <- glm(log_grid_area ~ Platform + log_obs_count, data = grid_long)
summary(grid_obs_model)

png("Figures/DHARMa_grid_log_area_log_obs.png", width = 2000, height = 1500, res = 300)
grid1 <- simulateResiduals(grid_obs_model)
plot(grid1)
dev.off()

grid_obs_model2 <- glm(grid_area ~ Platform + log_obs_count, data = grid_long)
summary(grid_obs_model2)

png("Figures/DHARMa_grid_area_log_obs.png", width = 2000, height = 1500, res = 300)
grid2 <- simulateResiduals(grid_obs_model2)
plot(grid2)
dev.off()

grid_obs_model3 <- glm(grid_area ~ Platform + log_obs_count, family=Gamma(link="log"), data = grid_long)
summary(grid_obs_model3)

png("Figures/DHARMa_grid_area_log_obs_gamma.png", width = 2000, height = 1500, res = 300)
grid3 <- simulateResiduals(grid_obs_model3)
plot(grid3)
dev.off()

grid_obs_model4 <- glm(log_grid_area ~ Platform + obs_count, data = grid_long)
summary(grid_obs_model4)

png("Figures/DHARMa_log_grid_area_obs.png", width = 2000, height = 1500, res = 300)
grid4 <- simulateResiduals(grid_obs_model4)
plot(grid4)

grid_obs_model5 <- glm(grid_area ~ Platform + obs_count, family=Gamma(link="log"), data = grid_long)
summary(grid_obs_model5)

png("Figures/DHARMa_grid_area_obs_gamma.png", width = 2000, height = 1500, res = 300)
grid5 <- simulateResiduals(grid_obs_model5)
plot(grid5)
dev.off()

res_lmer <- simulateResiduals(lmer1)
plot(res_lmer)

# model testing
plot(mcp_obs_model$fitted.values, resid(mcp_obs_model))
abline(h = 0, lty = 2)

qqnorm(resid(mcp_obs_model))
qqline(resid(mcp_obs_model))

# FINAL model
grid_obs_model <- glm(grid_area ~ Platform + log_obs_count, family=Gamma(link="log"), data = grid_long)
summary(grid_obs_model)

# -----------------------------
# 3) Mixed-effects model (species random effect)
# -----------------------------
grid_obs_mixed_model <- lmer(log_grid_area ~ Platform + log_obs_count + (1|Species), data = grid_long)
summary(grid_obs_mixed_model)

# use GLMER so we can specify the family as Gamma with a log link based on model testing above
library(glmmTMB)

grid_obs_mixed_model <- glmmTMB(
  grid_area ~ Platform + log_obs_count + (1 | Species),
  family = Gamma(link = "log"),
  data = grid_long
)
summary(grid_obs_mixed_model)

png("Figures/DHARMa_grid_area_mm.png", width = 2000, height = 1500, res = 300)
grid_mm <- simulateResiduals(grid_obs_mixed_model)
plot(grid_mm)
dev.off()


# model testing
plot(mcp_obs_mixed_model)

qqnorm(residuals(mcp_obs_mixed_model))
qqline(residuals(mcp_obs_mixed_model))

qqnorm(ranef(mcp_obs_mixed_model)$Species[,1])
qqline(ranef(mcp_obs_mixed_model)$Species[,1])

# -----------------------------
# 4) Interaction test
# -----------------------------
grid_interaction_model <- glm(log_grid_area ~ Platform * log_obs_count, data = grid_long)
summary(grid_interaction_model)

# model testing
plot(mcp_obs_model$fitted.values, resid(mcp_obs_model))
abline(h = 0, lty = 2)

qqnorm(resid(mcp_obs_model))
qqline(resid(mcp_obs_model))

# Likelihood ratio test: does interaction improve fit?
anova(grid_obs_model, grid_interaction_model)

