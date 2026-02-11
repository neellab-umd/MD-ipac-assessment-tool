## Function for pulling invasive plant occurrences from EDD_MapS data.

## EDD_MapS files for Maryland and surrounding states were manually downloaded from the bugwood site on September 20, 2025 and saved to this project. I have not figured out how to download automatically from the site. The manual query used was for all plant records from 1901 - present for Maryland, Virginia, West Virginia, Pennsylvania, DC, and Delaware.

## Upon download, the zip file is extracted and most columns in mappings.csv beyond the latitude longitude were manually deleted to prevent errors upon reading in the data. Make sure you retain the Verified and IDCred columns.

## This function uses the saved file.

## The query should be updated periodically to get the latest occurrences.

## Usage EDDMapS_Query(species_name)

## Maile Neel 2025.

EDDMapS_Query <- function(species_name) {

## Load Packages
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(here,
                 magrittr,
                 tidyverse,
                 remotes,
                 sf)

## This function filters the mappings.csv for records of the chosen species that have latitude longitude coordinates.  Many records do not have location information.  The state information for each record is buried in a location variable.

## Print reassuring message
cat("I am now looking through EDDMaps for ", species_name, " records.\n")

flush.console()

## Read and filter data to get records wth coordinates for the species in question
eddmaps_data <- read_csv(paste0("./EDDMaps_location_data/mappings.csv"),
    show_col_types = FALSE) %>%
    dplyr::filter(!is.na(Latitude), SciName == species_name)


## Early return if no results
  if (nrow(eddmaps_data) == 0) {
    message("No EDDMapS results found for ", species_name, ".")

    return(NULL)
  }

## Continue processing only if data for the species are found
  eddmaps_invasive_data <- eddmaps_data %>%
    dplyr::mutate(state = str_extract(Location,
      "(?<=, )[^,]+(?=, United States)")) %>%
    dplyr::mutate(state = str_replace_all(state,
      "D\\. Columbia", "District of Columbia"))  %>%
    dplyr::rename(species = SciName) %>%
    dplyr::mutate(ObsDate = mdy(ObsDate),
      year = year(ObsDate)) %>%
    dplyr::select(species, year, state, Longitude, Latitude) %>%
    dplyr::mutate(source = "EDDMapS") %>%
    st_as_sf(coords = c("Longitude", "Latitude"),
      crs = 4326,
      remove = FALSE)

  return(eddmaps_invasive_data)
}
