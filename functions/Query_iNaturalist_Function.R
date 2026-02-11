# Function for getting iNaturalist records

## Maile Neel 2025

## Usage: The function requires the argument species_name be defined. Example usage is

## iNaturalist_Query("Berberis thunbergii")


iNaturalist_Query <- function(species_name) {
  ## Load packages

  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(rinat,
                 here,
                 magrittr,
                 httr,
                 jsonlite,
                 sf,
                 tidyverse)


## Define helper function to handle cases with hybrid names.

  make_species_pattern <- function(species_name) {
    parts <- str_split(species_name, " ")[[1]]
    genus <- parts[1]
    epithet <- parts[length(parts)]  # Last word
    # Pattern: genus + any characters + epithet
    paste0("^", genus, "\\s+.*?\\s*", epithet, "$")
  }

## Create pattern for matching
  species_pattern <- make_species_pattern(species_name)

## Load shapefile with state boundaries
  boundaries <- (sf::st_read(here::here("spatial_data",  "State_Boundaries", "boundaries_p_2021_v3.shp"), quiet = TRUE)) %>%
    dplyr::filter(COUNTRY == "USA" & !(STATEABB %in% c("US-AK", "US-HI", "US-PR", "US-VI"))) %>%
    sf::st_make_valid() %>%
    sf::st_transform(4326)

sf::sf_use_s2(FALSE)

## Retrieve taxon ID for the chosen species from iNaturalist in the form
 # species_taxon_id <- get_taxon_id(species_name)

  ## Define the abbreviations used in the boundary map for the states you are interested in
  states_of_interest <- c("US-DE", "US-MD", "US-PA", "US-VA", "US-WV", "US-DC")


  ## Define a list of bounding boxes for states and DC.  The iNat search is done by state and each one is constrained to the bounding box.  Searching by state name was unreliable because not all observations have a state assigned and some are placed in the wrong state.  The bounding boxes of one state will extend into adjacent states but we filter those duplicates out later.

  state_bboxes <- tibble(
    state = c("DE", "MD", "PA", "VA", "WV", "DC"),
    ymin = c(38.45, 37.89, 39.72, 36.54, 37.12, 38.79),  # Southwest latitudes (ymin)
    xmin = c(-75.8, -79.5, -80.52, -83.68, -82.64, -77.1), # Southwest longitudes (xmin)
    ymax = c(39.85, 39.72, 42.27, 39.46, 40.64, 38.99),  # Northeast latitudes (ymax)
    xmax = c(-74.9, -75.0, -74.7, -75.2, -77.7, -76.9)  # Northeast longitudes (xmax)
  )

## Print reassuring message

  message("Thanks for waiting patiently while I get your ",
          species_name,
          " records from iNaturalist.")

  flush.console()

# Query for observations of the species within each bounding box. This approach limits the results for each query to avoid hitting the 10,000 record limit in iNat.  The results for each state are combined later.
  inat_results <- state_bboxes %>%
    dplyr::mutate(data = pmap(
      list(ymin, xmin, ymax, xmax),
      function(ymin, xmin, ymax, xmax) {
        suppressMessages(suppressWarnings(
          tryCatch({
            get_inat_obs(
              query = species_name,
              maxresults = 10000,
              bounds = c(ymin, xmin, ymax, xmax)
            ) %>%
              as_tibble()
          }, error = function(e) {
            # Return NULL silently
            return(NULL)
          })
        ))
      }
    ))

## Safety catch when there are no records of the species in iNat
  if (all(map_lgl(inat_results$data, ~ is.null(.) || (is.data.frame(.) && nrow(.) == 0)))) {
    message("No iNaturalist results found for ", species_name, ". Returning empty sf.")
    return(NULL)
  }

# Process records
  inat_results <- inat_results %>%
    unnest(data) %>%
    ## Filter to match species pattern (handles × vs x variations)
    dplyr::filter(str_detect(scientific_name, regex(species_pattern, ignore_case = TRUE))) %>%
    ## Standardize the species name to the input format
    dplyr::mutate(scientific_name = species_name) %>%  # Use the input name for consistency
    ## Wrangle query results
    dplyr::select(scientific_name, longitude, latitude, quality_grade, observed_on) %>%
    dplyr::mutate(
      across(c(latitude, longitude), as.numeric),
      source = "iNaturalist"
    ) %>%
    dplyr::filter(
      !is.na(latitude) & !is.na(longitude),  # Ensure valid coordinates
      quality_grade == "research"             # Ensure good ID
    ) %>%
    st_as_sf(coords = c("longitude", "latitude"),
             crs = 4326,
             remove = FALSE) %>%
    filter(suppressWarnings(suppressMessages(st_intersects(
      .,
      boundaries %>% filter(STATEABB %in% states_of_interest),
      sparse = FALSE
    ))) %>% rowSums() > 0) %>%
    st_join(boundaries %>% filter(STATEABB %in% states_of_interest)) %>%
    dplyr::distinct(latitude, longitude, .keep_all = TRUE) %>%
    dplyr::rename(
      species = scientific_name,
      state = NAME_En,
      Latitude = latitude,
      Longitude = longitude
    ) %>%
    dplyr::mutate(
      observed_on = as.Date(observed_on),
      year = year(observed_on),
      .after = species
    ) %>%
    dplyr::select(-c(NAME_Es, NAME_Fr, COUNTRY, STATEABB, CAPITAL,
                     AreaKm2, quality_grade, observed_on)) %>%
    dplyr::relocate(state, .after = year)

  return(inat_results)
}

