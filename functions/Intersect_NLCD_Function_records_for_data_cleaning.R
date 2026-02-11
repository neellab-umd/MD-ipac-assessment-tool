
NLCD_Intersect_Records <- function(species_name) {

  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(here,
                 magrittr,
                 sf,
                 raster,
                 terra,
                 tidyterra,
                 ggspatial,
                 ggthemes,
                 flextable,
                 ggtext,
                 tigris,
                 scales,
                 tidyverse)

  ## Read in acreages for NLCD Types


  ## Read in NLCD Types raster data for MD----
  nlcd_types  <- rast("./spatial_data/usa_land_cover_2020v2_30m/NLCD_for_IPAC.tif")%>%
    { if (!compareCRS(., "EPSG:26918")) terra::project(., "EPSG:26918") else . }

  ## load shapefile of state and state boundaries and filter for Maryland.
  maryland_boundary <- sf::st_read(here("spatial_data", "State_Boundaries",
                                        "maryland.state.boundary.shp"),
                                   quiet = TRUE) %>%
    st_make_valid() %>%
    {if (sf::st_crs(.) != sf::st_crs(nlcd_types))
      sf::st_transform(., crs = sf::st_crs(nlcd_types))
      else .}

  ## Load the two saved objects with specified species occurrence points: merged_points_sf and distinct_combined_points_sf. Only the merged points (i.e., the clusters) are used in this script.

  load(paste0("./combined_location_data/Points_sf_",
              species_name, ".RData"))

  distinct_combined_points_sf <- distinct_combined_points_sf %>%
    dplyr::filter(state == "Maryland")

  ##Create function to extract NLCD type for each point

  extract_points_with_nlcd <- function(points_sf, nlcd_types) {

    extracted <- terra::extract(nlcd_types, terra::vect(points_sf))

    points_with_data <- dplyr::bind_cols(points_sf, extracted) %>%
      dplyr::select(-state)
  }

  ## RUN THE EXTRACTION FUNCTION ----

records_with_nlcd <- extract_points_with_nlcd(
    points_sf = distinct_combined_points_sf,
    nlcd_types = nlcd_types
  )

}
