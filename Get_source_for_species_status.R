# Find source for each non-native species
# Brittany Mason

library(tidyverse)
library(reptiledbr)

# read in data from Krysko et al. 2016
data_krysko <- read_csv("Data/native_nonnative_status/Krysko_et_al_2016.csv")

data_krysko <- data_krysko %>%
  mutate(Species = str_extract(`Scientific Name`, "^\\S+\\s+\\S+"))

head(data_krysko)

# read in table S1
species_list <- read_csv("Data/species_presence_comparison.csv")

species_together <- left_join(species_list, data_krysko, by="Species")

# get species not in this list
species_abs <- species_together %>%
  filter(is.na(`Scientific Name`))

write_csv(species_abs, "Data/native_nonnative_status/species_not_matched_krysko.csv")

# use the reptile database to get the others
reptile_data <- get_reptiledb_data(species_abs$Species)

reptile_df <- reptile_data %>%
  unnest(data)

reptile_data_fixed <- reptile_data %>%
  mutate(
    data = map(data, ~ {
      if (is.null(.x) || length(.x) == 0 || ncol(.x) == 0) {
        tibble(attribute = character(), value = character())
      } else {
        .x
      }
    })
  )

reptile_long <- reptile_data_fixed %>%
  unnest(data)

reptile_wide <- reptile_long %>%
  pivot_wider(
    names_from  = attribute,
    values_from = value
  ) %>%
  as.data.frame()

reptile_db <- data.frame(species=reptile_wide$input_name, distribution=reptile_wide$Distribution)

write_csv(reptile_db, "Data/native_nonnative_status/Reptile_Database_Distribution.csv")


# Get all native species --------------------------------------------------


all_inat <- readRDS("Data/all_inat_rg_obs.RDS")

# filter to 2014 - 2024
all_inat <- all_inat %>%
  mutate(date=ymd(observed_on)) %>%
  filter(date < ymd("2024-09-01"),  date >= ymd("2014-01-01"))

all_EDDMapS <- read_csv("Data/EDDMapS_observations.csv")

all_EDDMapS <- all_EDDMapS %>%
  mutate(date=mdy(ObsDate)) %>%
  filter(date < ymd("2024-09-01"),  date >= ymd("2014-01-01"))

# list of iNaturalist species
inat_species <- unique(all_inat$taxon_species_name)

# what species are present here, but not in our non-native species list
inat_native <- setdiff(inat_species, species_list$Species)

# how about EDDMapS?
eddmaps_species <- unique(all_EDDMapS$SciName)

# clean up the names so it is only genus species
eddmaps_clean <- eddmaps_species[
  # remove spp. / spp
  !str_detect(eddmaps_species, "\\b(spp|ssp)\\.?\\b") &
    # keep exactly two words
    str_count(eddmaps_species, "\\S+") == 2
]

eddmaps_native <- setdiff(eddmaps_clean, species_list$Species)

species_native <- na.omit(unique(c(inat_native, eddmaps_native)))

native_df <- as.data.frame(species_native)

# now read in Florida museum list of native/non-native species
fm_data <- read_csv("Data/native_nonnative_status/floridamuseum_speciesstatus.csv")

fm_data$sci_name <- paste(fm_data$Genus, fm_data$Species, sep=" ")


fm_data$sci_name <- word(fm_data$sci_name, 1, 2)

fm_data_gr <- fm_data %>%
  group_by(sci_name) %>%
  summarise(native_status = first(`FL Native`))

native_fm <- left_join(native_df, fm_data_gr, by=c("species_native"="sci_name"))

native_fm <- native_fm %>%
  mutate(source=ifelse(complete.cases(native_status), "Florida Natural History Museum", NA))

# for the rest of the reptiles, we can use the Reptile Database
# we will get the native range for each species
reptile_data_n <- get_reptiledb_data(native_fm[is.na(native_fm$native_status),]$species_native)

reptile_df_n <- reptile_data_n %>%
  unnest(data)

reptile_data_fixed_n <- reptile_data_n %>%
  mutate(
    data = map(data, ~ {
      if (is.null(.x) || length(.x) == 0 || ncol(.x) == 0) {
        tibble(attribute = character(), value = character())
      } else {
        .x
      }
    })
  )

reptile_long_n <- reptile_data_fixed_n %>%
  unnest(data)

reptile_wide_n <- reptile_long_n %>%
  pivot_wider(
    names_from  = attribute,
    values_from = value
  ) %>%
  as.data.frame()

reptile_db_n <- data.frame(species=reptile_wide_n$input_name, distribution=reptile_wide_n$Distribution)

# now select which distributions say "Florida"
reptile_fl_db <- reptile_db_n %>%
  filter(str_detect(distribution, "Florida"))

# manually examine the others
reptile_unk_fl_db <- reptile_db_n %>%
  filter(!str_detect(distribution, "Florida"))

write_csv(reptile_unk_fl_db, "Data/potential_missing_species.csv")

# now add these to the data frame from earlier
native_fm <- native_fm %>%
  mutate(
    native_status = if_else(species_native %in% reptile_fl_db$species, "Yes", native_status),
    source = if_else(species_native %in% reptile_fl_db$species, "The Reptile Database", source)
  )
native_fm
