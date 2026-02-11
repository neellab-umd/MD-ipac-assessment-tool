
## Usage: This function queries GBIf for US records from the geographic area bounding Maryland and surrounding states for a chosen species and then filters for occurrences in Maryland and Adjacent States. The rgbif function

  ## Author: Maile Neel, 2025.


GBIF_Query <- function(species_name) {
  ## Load packages

  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(rgbif,
                 here,
                 magrittr,
                 sf,
                 ggspatial,
                 scales,
                 tidyverse)


#Turn off spherical geometry
  sf::sf_use_s2(FALSE)
  options(sf_use_s2 = FALSE)  # Also set as an option

## Load shapefile of state and province boundaries and filter to focus area

dmv <- sf::st_read(here::here("spatial_data",  "State_Boundaries", "boundaries_p_2021_v3.shp"),
                     quiet = TRUE) %>%
    filter(COUNTRY == "USA" &
           STATEABB %in% c("US-DE", "US-DC",
                             "US-MD", "US-PA",
                             "US-VA", "US-WV")) %>%
    st_make_valid() %>%
    sf::st_transform(4326) ## CRS for GBIF

## Get the bounding box for the focus area to limit the GBIF Query
  bbox_wkt <- dmv %>%
    suppressWarnings(suppressMessages(st_union())) %>%       # Combine geometries
    st_bbox() %>%        # Get bounding box
    st_as_sfc() %>%      # Convert bbox to simple feature geometry
    st_as_text()

  ## Create both versions of the species name (only if it contains " x ")
  species_names <- c(species_name)

  # If name contains " x ", also search without it
  if (str_detect(species_name, " x ")) {
    species_names <- c(species_names, str_remove(species_name, "x "))
  }

  ## Get GBIF species keys for all name variants
  species_key <- species_names %>%
    map(~ name_backbone(name = .x)$usageKey) %>%
    compact() %>%
    unique()

## Do a search for one occurrence and then get the total number of occurrences from the metadata.
  result <- occ_search(
    taxonKey = species_key,
    geometry = bbox_wkt,  # *** ADD THIS ***
    limit = 1
  )  # Get metadata only

  total_records <- result$meta$count  # Extract total count

## Print reassuring message

  message("I know it doesn't look like anything is happening, but I am busy searching through ",
          total_records, " ", species_name, " records from GBIF.")

  flush.console()

## Do the real search
  occurrences <- occ_search(
    taxonKey = species_key,
    geometry = bbox_wkt,
    limit = total_records,
    fields = c("name",
               "decimalLatitude",
               "decimalLongitude",
               "country",
               "stateProvince",
               "eventDate",
               "year",
               "basisOfRecord",
               "issues",
               "identifiedBy",
               "datasetKey")
  )

## pull the occurrence data out of the GBIF list.
  occurrences_df <- occurrences$data  %>%  # Extract the occurrence data
    dplyr::filter(!is.na(decimalLatitude) & !is.na(decimalLongitude) & !is.na(year)) %>%
    dplyr::mutate(
      species = species_name,
      # Add identifiedBy if it doesn't exist
      identifiedBy = if ("identifiedBy" %in% names(.)) identifiedBy else NA_character_
    ) %>%
    dplyr::select(species, year, stateProvince,
                  decimalLongitude, decimalLatitude, basisOfRecord, identifiedBy) %>%
    dplyr::mutate(source = "GBIF") %>%
    dplyr::filter(basisOfRecord != "LIVING_SPECIMEN") %>% # get rid of cultivated plants
    ## This eliminates records that do not have an identifier and also are not herbarium records.
    ## Most herbarium records do not say who identified them.
    dplyr::filter(
      basisOfRecord == "PRESERVED_SPECIMEN" |
        !is.na(identifiedBy)
    )
## Now convert the points to a spatial object and intersect them with our state boundaries. Eliminate records that are not within the states of interest and identify the state in which the points fall.  This corrects erroneous states in GBIF.

## Convert data.frame to an sf object
sf::sf_use_s2(FALSE)

old_warn <- getOption("warn")
options(warn = -1)


occurrences_df_sf =
  sf::st_as_sf(occurrences_df,
               coords = c("decimalLongitude",
                          "decimalLatitude"),
               crs = "EPSG:4326",
               remove = FALSE) %>%
    filter(rowSums(suppressWarnings(suppressMessages(
      st_intersects(., dmv, sparse = FALSE)))) > 0) %>%
    dplyr::rename(Longitude = decimalLongitude,
                  Latitude = decimalLatitude,
                  state = stateProvince) %>%
    {suppressWarnings(st_join(., dmv %>% dplyr::select(NAME_En),
                              left = FALSE))} %>%# Join with boundaries, keeping only matching points
    dplyr::mutate(state = NAME_En) %>%  # Replace incorrect state with Name_En
    dplyr::select(-NAME_En)%>%  # Remove the temporary NAME_En column
    dplyr::filter(!is.na(state)) #Remove any points that are outside the focus area

# Create file safe version of the species for file naming
file_safe_name <- str_replace_all(species_name, " ", "_")

# Create dynamic file name
 file_name <- paste0("gbif_occurrences_", file_safe_name, "_DMV.csv")

# Write occurrence data to csv for future use.
  write_csv(occurrences_df_sf, here::here("GBIF_location_data", file_name), na = "")


return(occurrences_df_sf)
}


