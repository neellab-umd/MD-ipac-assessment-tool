
Quad_Density_Intersect <- function(species_name) {

  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(here,
                 magrittr,
                 tidyverse,
                 ggthemes,
                 sf,
                 ggspatial,
                 scales,
                 ggtext)


## Load Aptos Font
  extrafont::loadfonts(device = "win", quiet = TRUE)

  ## load shapefiles ----

## shape files for Maryland boundary and USGS quad boundaries
  maryland_boundary <- sf::st_read(here::here("spatial_data",  "State_Boundaries", "boundaries_p_2021_v3.shp"),
                           quiet = TRUE) %>%
    filter(COUNTRY == "USA" & STATEABB %in% c("US-MD")) %>%
    st_make_valid() %>%
    sf::st_transform(4326)


  MD_Quads <- sf::st_read(here::here("spatial_data",
                                     "Maryland_USGS_Topo_Grids_-_Quad_Grid",
                                     "Maryland_USGS_Topo_Grids_-_Quad_Grid.shp"),
                              quiet = TRUE) %>%
    sf::st_transform(4326)


#Create a file safe name from species name
  file_safe_name <- str_replace_all(species_name, " ", "_")


## Load two objects with species occurrences: merged_points_sf and distinct_combined_points_sf

load(paste0("./combined_location_data/Points_sf_",
            file_safe_name, ".RData"))

# Convert point data to sf object and ensure merged_points_sf it has the correct CRS

  ## Filter occurrences to get only Maryland points and create a spatial sf object.
  maryland_points_sf <- merged_points_sf %>%
    filter(state == "Maryland") %>%
    st_drop_geometry() %>%  # Remove the existing UTM geometry
    st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)  # Create new geometry from lat/lon

  # Spatially join occurrences with geologic quads
  maryland_with_quads <- st_intersection(maryland_points_sf, maryland_boundary) %>%
    st_intersection(.,  MD_Quads)

  quad_counts <- maryland_with_quads %>%
    group_by(USGSQuad) %>%
    summarise(count = n(), # count the number of rows - these are the clusters
              earliest_year = min(year))

  df_quad_counts<-tibble(
    all_quads = nrow(MD_Quads),
    occupied_quads = nrow(quad_counts),
    percent_occupied  = round(nrow(quad_counts)/nrow(MD_Quads) *100,1))



  # Rename the 'USGSQuad' column in one of the data frames to avoid conflict
  maryland_with_quads <- maryland_with_quads %>%
    dplyr::rename(USGSQuad_Maryland = USGSQuad)

# Perform the spatial join ----
  maryland_with_quads <- st_join(maryland_with_quads,
                                    quad_counts,
                                     join = st_intersects)

MD_Quads <-  st_join( MD_Quads,
            quad_counts,
            join = st_intersects)

## Generate breaks for ggplot ----
   generate_integer_breaks <- function(x, max_breaks = 6) {
     # Start with pretty integer-based breaks
     breaks <- pretty(x, n = max_breaks)

     # Keep only integer values
     breaks <- breaks[breaks %% 1 == 0]

     # Trim to within the data range
     x_min <- floor(min(x, na.rm = TRUE))
     x_max <- ceiling(max(x, na.rm = TRUE))
     breaks <- breaks[breaks >= x_min & breaks <= x_max]

     # Ensure min and max are included (if integers)
     if (x_min %% 1 == 0 && !(x_min %in% breaks)) {
       breaks <- c(x_min, breaks)
     }
     if (x_max %% 1 == 0 && !(x_max %in% breaks)) {
       breaks <- c(breaks, x_max)
     }

     # Sort and return unique values
     sort(unique(breaks))
   }


#Get bounding box of the quad sf object to use for placing annotations.
 bbox <-st_bbox(MD_Quads)

## Plot the number of clusters by quad ----
quad_densities <- ggplot() +
    geom_sf(data =  MD_Quads, aes(fill = count), color = "darkgrey") +
    geom_sf(data = maryland_boundary, alpha = 0, linewidth = .5)+
   # geom_sf(data = maryland_with_quads,  aes(color = quad), size = .8) +
   coord_sf(expand = FALSE) +
  scale_fill_continuous(
    type = "viridis",
    option = "inferno",
    direction = -1,
    name = "Number of \nOccurrences",
    na.value = "transparent",
    breaks = generate_integer_breaks(MD_Quads$count),
    labels = generate_integer_breaks(MD_Quads$count)
  ) +

    ggspatial::annotation_scale(
      location = "bl",
      width_hint = 0.3,
      height = unit(0.11, "cm"),
      pad_x = unit(0.25, "in"),
      pad_y = unit(0.06, "in"),
      text_cex = 0.5,
      text_pad = unit(0.02, "cm")
    ) +
    ggspatial::annotation_north_arrow(
      location = "bl",
      which_north = "true",
      pad_x = unit(0.26, "in"),
      pad_y = unit(0.13, "in"),
      height = unit(.5, "cm"),
      width = unit(.6, "cm"),
      style = north_arrow_fancy_orienteering
    ) +
    ggthemes::theme_map() +
    theme(
      legend.title = element_text(family = "Aptos", size = 8, face = "bold", hjust = 0.4),
      legend.position = "inside",
      legend.position.inside = c(0.22, 0.25),
      legend.text = element_text(family = "Aptos", size = 7),
      legend.spacing.x = unit(.07, "cm"),
      legend.spacing.y = unit(.55, "cm"),
      legend.key.width = unit(.55, "cm"),
      legend.key.height = unit(.6, "cm"),
      panel.border = element_rect(fill = NA, colour = "white", linewidth = 1),
      axis.text = element_blank(),
      plot.margin = margin(-1, 0.5, -1, 0.5)
    ) +
  annotate(
      "text",
      x = bbox["xmin"] + 0.02 * (bbox["xmax"] - bbox["xmin"]),  # Nudging left
      y = bbox["ymin"] + 0.15 * (bbox["ymax"] - bbox["ymin"]),  # Nudging down
      label = paste0("Occurrences were found in ", df_quad_counts$percent_occupied, "% of quadrangles."),
      hjust = 0, vjust = 1,  # Right-align the text
      size = 3
    )

 return(list(quad_densities_plot = quad_densities,
   df_quad_counts = df_quad_counts))

}
