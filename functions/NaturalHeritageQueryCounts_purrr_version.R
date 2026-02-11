load(paste0("./combined_location_data/Points_sf_Berberis_thunbergii.RData"))


distinct_combined_points_sf %>%
  st_drop_geometry() %>%
  dplyr::filter(state == "Maryland") %>%
  write_csv("Berberis_thunbergii_records.csv", na ="")

merged_points_sf %>%
  st_drop_geometry() %>%
  dplyr::filter(state == "Maryland") %>%
  write_csv("Berberis_thunbergii_occurrences.csv", na ="")


Heritage_Check <- function(file_path) {

  # Extract species name from file name
  species_name <- str_remove(basename(file_path), "_records\\.csv$")

  # Read and prepare rank points

  #If the data are in a shapefile use this
  # rank_utm <- sf::st_read(here::here("nameofshapefile.shp"),
  #                                  quiet = TRUE) %>%  #   st_make_valid() %>%
  #   sf::st_transform(26918)

  # Read and prepare query points
  query_utm <- read_csv(file_path) %>%
    select(-geometry) %>%
    st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
    st_transform(crs = 26918)

# Count query records within distances
  rank_with_records <- rank_utm %>%
    mutate(
      records_within_100m  = lengths(st_is_within_distance(rank_utm,
                                                           query_utm, dist = 100)),
      records_within_250m  = lengths(st_is_within_distance(rank_utm,
                                                           query_utm, dist = 250)),
      records_within_500m  = lengths(st_is_within_distance(rank_utm,
                                                           query_utm, dist = 500)),
      records_within_1000m = lengths(st_is_within_distance(rank_utm,
                                                           query_utm, dist = 1000))
    ) %>%
    st_drop_geometry()

  threshold <- 300

occurrences_utm <- query_utm %>%
    group_by(cluster) %>%
    dplyr::summarise(
      geometry = st_union(geometry),  # Merge geometries within each cluster
      num_points = n(),  # Count the number of points in each cluster
      year = min(year, na.rm = TRUE),
      across(-c(year, geometry), ~ first(na.omit(.))),
      .groups = "drop"  # Drop the grouping after summarizing
    ) %>%
    dplyr::mutate(geometry = st_centroid(geometry)) %>%
    dplyr::filter(state =="Maryland")

# Count query occurrences within distances
rank_with_occurrences <- rank_utm %>%
  mutate(
    occurrences_within_100m  = lengths(st_is_within_distance(rank_utm,
                                                         occurrences_utm, dist = 100)),
    occurrences_within_250m  = lengths(st_is_within_distance(rank_utm,
                                                         occurrences_utm, dist = 250)),
    occurrences_within_500m  = lengths(st_is_within_distance(rank_utm,
                                                         occurrences_utm, dist = 500)),
    occurrences_within_1000m = lengths(st_is_within_distance(rank_utm,
                                                         occurrences_utm, dist = 1000))
  ) %>%
  st_drop_geometry()

rank_with_counts <- full_join(rank_with_records, rank_with_occurrences, by = c("species", "Group","Srank", "State", "Federal"))

# Write output CSV with species name
write_csv(rank_with_counts, paste0(species_name, "_Full_Heritage_Counts.csv"), na = "")

}


# ---- Apply to all files in a directory ----
occurrence_files <- list.files(
  pattern = "_records\\.csv$",   # or "_occurrences\\.csv$" if needed
  full.names = TRUE
)

purrr::walk(occurrence_files, Heritage_Check)