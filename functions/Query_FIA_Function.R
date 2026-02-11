## Function for pulling invasive plant occurrences from FIA data. FIA files for Maryland and surrounding states were manually downloaded from the DataMart site on February 23, 2025 and saved to this project. They were too large to download automatically and the future availability of federal data were uncertain. This function uses the saved files.

## Usage FIA_Query(species_name)

## Maile Neel 2025.


FIA_Query <- function(species_name) {

## rFIA is not on CRAN so you have do download and install if from github
  suppressPackageStartupMessages({
    if (!require("rFIA", quietly = TRUE)) {
      remotes::install_github("hunter-stanke/rFIA", upgrade = "never")
    }
  })

   ## Load Packages
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(here,
                 magrittr,
                 remotes,
                 sf,
                 tidyverse)

## Crosswalk Species Name with codes from USDA Master Invasive Species List.
  #read in species codes for invasive species
  species_code_crosswalk <- read_csv("./FIA_Data/v9-5_2024-11_Natl_MasterInvasiveSpeciesList.csv",show_col_types = FALSE)

  species_df <- mutate(tibble(Scientific_Name = species_name))

# Crosswalk parsed names to FIA species codes
  matched_species <- species_df %>%
    left_join(species_code_crosswalk, by = c("Scientific_Name")) %>%
    dplyr::select(-FIA_code)


# List of states you have downloaded

states <- c("MD", "DE","VA", "PA", "WV") # Modify as needed. There are no FIA plots in DC

## This function filters the invasive species csv and pulls plots for each state for the species being queried that have the chosen invasive species. It then merges the plot location information from the plot csv for the same plot number.

screen_fia_plots_for_invasives <- function(state) {

invasive_subplot_data <- read_csv(paste0("./FIA_Data/",
                                         state,
                                         "_INVASIVE_SUBPLOT_SPP.csv"),
                                  show_col_types = FALSE) %>%
      dplyr::select(CN, PLT_CN, STATECD, INVYR, VEG_SPCD, COVER_PCT)  %>%
      mutate(INVYR = as.numeric(INVYR))

    # Read plot data
    plot_data <- read_csv(paste0("./FIA_Data/",
                                 state,
                                 "_PLOT.csv"),
                          show_col_types = FALSE) %>%
      dplyr::select(CN, LON, LAT)

    # Filter for invasive species and Merge with plot data
    plots_with_invasive <- invasive_subplot_data %>%
      inner_join(matched_species, by = c("VEG_SPCD" = "PLANTS_code")) %>%
      left_join(plot_data, by = c("PLT_CN" = "CN")) %>%
      mutate(STATECD = case_when(
        STATECD == 24 ~ "Maryland",
        STATECD == 10 ~ "Delaware",
        STATECD == 51 ~ "Virginia",
        STATECD == 54 ~ "West Virginia",
        STATECD == 42 ~ "Pennsylvania",
        TRUE ~ as.character(STATECD)  # Keep other values unchanged
      ))

    return(plots_with_invasive) }

  # Use map to apply that function to files for all states of interest. Then, select and rename columns so they match the GBIF data. Add a column that indicates the data are from FIA.

  ## Print reassuring message

  message("I am now checking the FIA plots for ", species_name, ".")

  flush.console()

  fia_invasive_data <- map_dfr(states, screen_fia_plots_for_invasives) %>%
    dplyr::select(Scientific_Name, INVYR, STATECD, LON, LAT) %>%
    rename(species = Scientific_Name, year = INVYR,
           Longitude = LON, Latitude = LAT, state = STATECD) %>%
    mutate(source = "FIA",
           state = as.character(state))  # Convert state code to character

  if (nrow(fia_invasive_data) == 0) {
    message("No FIA records found for ", species_name, ".")

    return(NULL)

  } else {
    fia_invasive_data <- sf::st_as_sf(fia_invasive_data,
                                      coords = c("Longitude", "Latitude"),
                                      crs = "EPSG:4326",
                                      remove = FALSE)
  }

  return(fia_invasive_data)
}
