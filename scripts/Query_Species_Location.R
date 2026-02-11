## This function runs the four queries for species record from four databases (GBIF, iNat, FIA, and EddMaps) for chosen species or groups of species and then filters for occurrences in Maryland and Adjacent States.

## It is designed to be run from within the GIS R project.

## Each query is a separate function and each has its own file in the functions folder of the GIS R project.  The output will be saved to an RData file in the combined_location_data folder within the project.  The RData output file must be in the folder for the quarto document to be created.

## The script can take 5-10 minutes to run if there are a lot of records in GBIF or iNaturalist.

## Usage: You can search for single species or groups of congeners by scientific name
## species_location_query("Berberis thunbergii")
## species_location_query(c("Wisteria sinensis", "Wisteria formosa", "Wisteria floribunda"))

## Author: Maile Neel, 2025.

species_location_query <- function(species_list) {

## Load Packages
if (!require("pacman", , quietly = TRUE)) install.packages("pacman")
  pacman::p_load(dbscan,
                 here,
                 httr,
                 jsonlite,
                 lubridate,
                 sf,
                 raster,
                 terra,
                 tidycensus,
                 tidyverse,
                 tidyterra,
                 forcats,
                 update = FALSE)

## Load Functions
  if(!exists("GBIF_Query", mode="function"))
    source(here::here("functions", "Query_GBIF_Function.R"))

  if(!exists("iNaturalist_Query", mode="function"))
    source(here::here("functions", "Query_iNaturalist_Function.R"))

  if(!exists("FIA_Query", mode="function"))
    source(here::here("functions", "Query_FIA_Function.R"))

  if(!exists("EDDMapS_Query", mode="function"))
    source(here::here("functions", "Query_EDDMapS_API.R"))

  source(here::here("functions", "Query_Intro_Info.R"))

  GBIF_Query_retry <- function(species_name, retries = 3) {
    for (i in seq_len(retries)) {
      tryCatch({
        return(GBIF_Query(species_name))
      }, error = function(e) {
        if (i == retries) stop(e)
        Sys.sleep(1.5 ^ i)  # Exponential backoff
      })
    }
  }

# This function does all the querying for one species.  Below it is mapped over all names in the species list given as the argument to the species_location_query() function.

single_species_query <- function(species_name) {

# Create file safe version of the species for file naming
file_safe_name <- str_replace_all(species_name, " ", "_")

## Run query for information for the introduction (synonyms, global and US distribution).

# Complete workflow:
# ==================

# Set your Trefle token
trefle_token <- "usr-HStZpCPORUKz7eKzpv4hv2YDcw71uCBn5zhKBhqv9X4"

# Combine Trefle + GBIF data
query_distribution_and_native_range(species_name, trefle_token)

## Run the actual queries for each species - these queries will take a long time.  The GBIF, iNaturalist and EDDMapS queries are going out to the web and pulling all records.  The FIA data have been downloaded and the local database is being queried so that is quicker.
message("Getting GBIF location data - this can take a several minutes if there are many records...")
GBIF_results <- suppressMessages(suppressWarnings({
                    sink(stderr(), type = "message")
                    result <- GBIF_Query(species_name)
                    sink(type = "message")
                     result
                      }))
  iNat_results <- iNaturalist_Query(species_name)
  FIA_results <- FIA_Query(species_name)
  message("\nThe wait is almost over! I just have to get the EddMaps records...")
  EDDMap_results <-EDDMapS_Query(species_name)

## combine the results of the query and filter out identical Latitude/Longitudes as duplicate records. Record the original source of all records, including those in multiple databases.

results_list <- list(GBIF_results,
                     iNat_results,
                     FIA_results,
                     EDDMap_results) %>%
    purrr::compact() ## removes null and empty results

# Combine all sources and round coordinates
if (length(results_list) == 0) {
  stop("No results found from any data source for ", species_name)
}

## Combine the results
all_points <- dplyr::bind_rows(results_list) %>%
  dplyr::mutate(
    Latitude  = round(Latitude, 6),
    Longitude = round(Longitude, 6)
  )

# To get information on how many records are coming from the different database and how much overlap there is, summarize source databases per coordinate while keeping geometry
  source_summary <- all_points %>%
    dplyr::group_by(species, year, state, Longitude, Latitude) %>%
    dplyr::summarise(
      databases   = paste(sort(unique(source)), collapse = ", "),
      n_databases = dplyr::n_distinct(source),
      n_records   = dplyr::n(),
      .groups     = "drop",
      geometry    = first(geometry)  # keep one geometry per coordinate
    )

# Remove lingering geometry duplicates (if any)
prefiltered_distinct_combined_points_sf <- source_summary %>%
    dplyr::distinct(st_coordinates(.), .keep_all = TRUE) %>%
    sf::st_transform(26918) # NAD83 UTM Zone 18

# Extract NLCD raster values - keep this in UTM to make the raster project correctly
  nlcd_types <- rast("./spatial_data/usa_land_cover_2020v2_30m/NLCD_for_IPAC.tif") %>%
    { if (!compareCRS(., "EPSG:26918")) terra::project(., "EPSG:26918") else . }

  extracted <- terra::extract(nlcd_types, terra::vect(prefiltered_distinct_combined_points_sf))

# Ensure one row per point in case extract duplicates
  extracted <- extracted %>%
    dplyr::group_by(ID) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::select(-ID)

# Combine NLCD info and filter out water
  distinct_combined_points_sf <- dplyr::bind_cols(prefiltered_distinct_combined_points_sf, extracted) %>%
    dplyr::filter(is.na(label) | label != "Water")

# Summarize number of points per combination of source databases
  numbers <- distinct_combined_points_sf %>%
    sf::st_drop_geometry() %>%
    dplyr::group_by(databases, n_databases) %>%
    dplyr::summarise(Num_records = n(), .groups = "drop") %>%
    dplyr::arrange(n_databases, desc(Num_records)) %>%
    dplyr::select(-n_databases)

write_csv(numbers, file = paste0("./combined_location_data/database_source_total_numbers_", file_safe_name, ".csv"))

## Set distance threshold.  Points that are closer to each other than this distance will be merged into the same record.  All point locations for merged records will be retained in multipoint geometry and all point locations will be used in other proximity calculations.

threshold <- 300

# Extract coordinates for clustering nearby records into occurrences
coords <- st_coordinates(distinct_combined_points_sf)

#Run DBSCAN to cluster points that are within the threshold distance
db <- dbscan::dbscan(coords, eps = threshold, minPts = 1)

# Assign cluster IDs
distinct_combined_points_sf$cluster <- factor(db$cluster)

# Merge geometries by cluster
  merged_points_sf <- distinct_combined_points_sf %>%
    group_by(cluster) %>%
    dplyr::summarise(
      geometry = st_union(geometry),  # Merge geometries within each cluster
      num_points = n(),  # Count the number of points in each cluster
      year = min(year, na.rm = TRUE),
      across(-c(year, geometry), ~ first(na.omit(.))),
      .groups = "drop"  # Drop the grouping after summarizing
    ) %>%
    dplyr::mutate(geometry = st_centroid(geometry)) %>%
    dplyr::filter(state !="New Jersey")

  return(list(
    distinct_combined_points_sf = distinct_combined_points_sf,
    merged_points_sf = merged_points_sf,
    species_name = species_name
  ))
}

# Run single_species_query function for each species
all_results <- map(species_list, single_species_query)

# Combine all results
distinct_combined_points_sf <- all_results %>%
  map("distinct_combined_points_sf") %>%
  map(~ st_transform(., 4326)) %>%  # Ensure consistent CRS
  bind_rows()

merged_points_sf <- all_results %>%
  map("merged_points_sf") %>%
  map(~ st_transform(., 4326)) %>%  # Ensure consistent CRS
  bind_rows()

# Determine file name prefix (genus if >1 species,
# full species with underscore between the names)
name_prefix <- if (length(species_list) > 1) {
  str_split(species_list[[1]], " ", simplify = TRUE)[1]
} else {
  str_replace_all(species_list[[1]], " ", "_")
}

# Save all records to csv
distinct_combined_points_sf %>%
  st_drop_geometry() %>%
  as.data.frame() %>%
   select(species, year, state, Longitude, Latitude, databases, n_databases, n_records) %>%  # Explicitly select non-list columns
  write_csv(file = paste0("./combined_location_data/all_records_", name_prefix, ".csv"))

# Save all records to csv
merged_points_sf %>%
  st_drop_geometry() %>%
  as.data.frame() %>%
  select(species, year, state, Longitude, Latitude, databases, n_databases, n_records) %>%  # Explicitly select non-list columns
  write_csv(file = paste0("./combined_location_data/merged_points_", name_prefix, ".csv"))

# Save combined data
save(merged_points_sf,
  distinct_combined_points_sf,
  file = paste0("./combined_location_data/Points_sf_", name_prefix, ".RData"))


search_date <- Sys.Date() %>%
  format(., "%Y-%m-%d")

writeLines(search_date, here::here("GBIF_location_data",
  paste0("gbif_search_date_", name_prefix, ".txt")))

message("Search complete. You now have the RData files for points and introductory information\n\n
 you need for rendering the assessment. They are in the folder ./combined_location_data folder.")

return(list(
  merged = merged_points_sf,
  distinct = distinct_combined_points_sf
))
}

#################################################
## Usage Examples
#################################################
# results <- species_location_query("Aralia elata")
# results <- species_location_query("Berberis thunbergii")
# results <- species_location_query("Cytisus scoparius")
# results <- species_location_query("Ligustrum obtusifolium")
# results <- species_location_query("Euonymus alatus")
# results <-species_location_query("Wisteria floribunda", "Wisteria x formosa", "Wisteria sinensis")
# results <- species_location_query("Pyrus calleryana")
# results <- species_location_query("Tetradium daniellii")
# results <- species_location_query(c("Hedera helix", "Hedera hibernica"))
# results <- species_location_query("Nandina domestica")
# results_aurea <- species_location_query("Phyllostachys aurea")
# results_aureosulcata <- species_location_query("Phyllostachys aureosulcata")
