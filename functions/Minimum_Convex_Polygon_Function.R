
mcp_calculation <- function(species_name) {

if (!require("pacman")) install.packages("pacman")
  pacman::p_load(extrafont,
                 here,
                 magrittr,
                 tidyverse,
                 ggthemes,
                 lwgeom,
                 sf,
                 ggplot2,
                 ggrepel,
                 ggspatial,
                 scales,
                 gt,
                 ggtext)

## Load Aptos Font
  loadfonts(device = "win", quiet = TRUE)


##  Read in Maryland Boundary
maryland_boundary<- sf::st_read(here::here("spatial_data",  "State_Boundaries", "boundaries_p_2021_v3.shp"),
                           quiet = TRUE) %>%
    filter(COUNTRY == "USA" & (STATEABB %in% c("US-MD"))) %>%
    st_make_valid() %>%
    sf::st_transform(4326)

#Create a file safe name from species name
file_safe_name <- str_replace_all(species_name, " ", "_")

## Load two objects with species occurrences: merged_points_sf and distinct_combined_points_sf

    load(paste0("./combined_location_data/Points_sf_",
                file_safe_name, ".RData"))

# Filter distinct_combined_points to Maryland and convert to an sf object with correct CRS.

    maryland_points_sf <- merged_points_sf %>%
      filter(state == "Maryland") %>%
      st_drop_geometry() %>%  # Remove the existing UTM geometry
      st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)  # Create new geometry from lat/lon

## Calculate the minimum convex polygon for the points.

md_convex_hull <- st_convex_hull(st_union(maryland_points_sf)) %>%
  st_intersection(., maryland_boundary)

hull_area_acres <- units::set_units(st_area(md_convex_hull), "acres")
state_area_acres <- units::set_units(st_area(maryland_boundary), "acres")

percent_of_state <- round(hull_area_acres/state_area_acres*100,1)

mcp_size <- tibble(
hull_acres = units::set_units(st_area(md_convex_hull), "acres"),
state_acres = units::set_units(st_area(maryland_boundary), "acres"),
hull_percent = round(hull_area_acres/state_area_acres*100,1)
)
bbox <-st_bbox(maryland_boundary)

## Plot the occurrences by Province
mcp_map <- ggplot() +
    geom_sf(data = md_convex_hull, fill="purple") +
    geom_sf(data = maryland_boundary, alpha = 0, color = "black", linewidth = .55) +
    coord_sf(expand = FALSE) +
    ggspatial::annotation_scale(
      location = "bl",
      width_hint = 0.3,
      height = unit(0.13, "cm"),
      pad_x = unit(0.25, "in"),
      pad_y = unit(0.06, "in"),
     text_cex = 0.5,
      text_family = "Aptos",
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
     # panel.border = element_rect(fill = NA, colour = "white", linewidth = 1),
      axis.text = element_blank(),
   #   plot.margin = margin(1, 1, 1, 1)
    ) +
    annotate(
      "text",
      x = bbox["xmin"] + 0.0 * (bbox["xmax"] - bbox["xmin"]),  # Nudging left
      y = bbox["ymin"] + 0.52 * (bbox["ymax"] - bbox["ymin"]),  # Nudging down
      label = paste0("The MCP occupies ", mcp_size$hull_percent, "% of the land area of Maryland."),
      hjust = 0, vjust = 1,  # Right-align the text
      family = "Aptos",
      size = 3
    )

#sf::st_write(md_convex_hull, here("spatial_data",paste0(file_safe_name, "_mcp.shp")), append = FALSE, quiet = TRUE)


 return(list(
   mcp_map = mcp_map,
   mcp_size = mcp_size))

}
