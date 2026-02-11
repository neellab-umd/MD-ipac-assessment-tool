
BioNet_Intersect <- function(species_name) {
  # These packages are used in this function but they are all loaded in the Quarto script.
  # if (!require("pacman")) install.packages("pacman")
  # pacman::p_load(here,
  #                magrittr,
  #                tidyverse,
  #                ggthemes,
  #                ggspatial, #north arrow and scale
  #                sf,
  #                ggtext, ## needed for element_markdown in ggplot
  #                scales,
  #                flextable,
  #                showtext,
  #                patchwork,
  #                paletteer,
  #                update = FALSE)

# Determine file name
  file_safe_name <- str_replace_all(species_name, " ", "_")

## Load Aptos Font
  loadfonts(device = "win", quiet = TRUE)

## load shapefile of state boundaries and filter for Maryland. This is used only for plotting purposes.
maryland_boundary <- sf::st_read(here::here("spatial_data", "State_Boundaries",
  "boundaries_p_2021_v3.shp"),
  quiet = TRUE) %>%
    filter(COUNTRY == "USA" & (STATEABB %in% c("US-MD"))) %>%
    st_make_valid() %>%
    sf::st_transform(4326)

  sf::sf_use_s2(FALSE)

## load the two saved objects with species occurrence points: merged_points_sf and distinct_combined_points_sf. Only the merged points (i.e., the clusters) are used in this script.

load(paste0("./combined_location_data/Points_sf_",
  file_safe_name, ".RData"))

## Filter occurrences to get only Maryland points and create a spatial sf object.
maryland_points_sf <- merged_points_sf %>%
  filter(state == "Maryland") %>%
  st_drop_geometry() %>%  # Remove the existing UTM geometry
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)  # Create new geometry from lat/lon

## Load shapefile of BioNet Tiers
MD_BioNet <- sf::st_read(here::here("spatial_data", "BioNet", "Maryland_Biodiversity_Conservation_Network_-_BioNet.shp"),
                           quiet = TRUE) %>%
    sf::st_transform(4326) %>%
    mutate(BioNetTier = replace_na(BioNetTier, "No Tier"))


## Spatially join occurrences with bionet tiers
maryland_points_with_bionet_tiers <-
    suppressMessages(st_join(maryland_points_sf, MD_BioNet)) %>%
    mutate(BioNetTier = factor(BioNetTier,
                               levels = c("Tier 1", "Tier 2", "Tier 3",
                                          "Tier 4", "Tier 5", "No Tier"))) %>%
    replace_na(list(BioNetTier = "No Tier"))

# Get total number of polygons in each tier
total_polygons <- MD_BioNet %>%
  st_drop_geometry() %>%  # Remove geometry for simple counting
  count(BioNetTier, name = "total_polygons")

## Summarize number of BioNet polygons that include any cluster point
bionet_tier_counts <- maryland_points_with_bionet_tiers %>%
    group_by(BioNetTier) %>%
    summarise(num_polygons = n_distinct(OBJECTID),
              num_occurrences = n()) %>%
  left_join(total_polygons, by = "BioNetTier") %>%
  mutate(percent_polygons = round((num_polygons / total_polygons) * 100,1),
         percent_occurrences = round((num_occurrences /nrow(maryland_points_sf)) * 100,1)) %>%
  sf::st_drop_geometry() %>%
  as.data.frame()

## Create Summary Table ----

totals <- bionet_tier_counts %>%
  filter(BioNetTier != "No Tier") %>%
  summarise(
    BioNetTier = "Total",
    num_polygons = sum(num_polygons, na.rm = TRUE),
    percent_polygons = round(sum(num_polygons)/sum(total_polygons)*100,1),
    num_occurrences = sum(num_occurrences, na.rm = TRUE),
    percent_occurrences = round(sum(num_occurrences)/nrow(maryland_points_sf)*100,1)
    ) %>%
  as.data.frame()

flextable::set_flextable_defaults(font.family = "Aptos",
  font.size = 8)

bionet_table <- bionet_tier_counts %>%
  filter(BioNetTier != "No Tier") %>%
  select(BioNetTier, num_polygons, percent_polygons,
    num_occurrences, percent_occurrences) %>%
  rbind(as.data.frame(totals)) %>%
  flextable::flextable() %>%

  # Set lower-level headers
  set_header_labels(
    BioNetTier = "",
    num_polygons = "Number",
    percent_polygons = "Percent",
    num_occurrences = "Number",
    percent_occurrences = "Percent"
  ) %>%

  # Add higher-level headers
  add_header_row(
    values = c("", "Polygons", "Occurrences"),
    colwidths = c(1, 2, 2)  # Span multiple columns
  ) %>%

  # Merge and center-align the higher-level headers
  merge_at(i = 1, j = 2:3, part = "header") %>%  # Merge "Polygons with Points"
  merge_at(i = 1, j = 4:5, part = "header") %>%  # Merge "Occurrences"

  # Right-align numbers in the body
  align(j = c("num_polygons", "percent_polygons", "num_occurrences", "percent_occurrences"),
        align = "right", part = "body") %>%
  bold(part = "body", i = ~ BioNetTier == "Total") %>%
  theme_booktabs()  %>% # Apply clean styling

  line_spacing(i = NULL, j = NULL, space = 1, part = "header") %>%
  line_spacing(i = NULL, j = NULL, space = 1, part = "body") %>%
  # Merge and center-align the higher-level headers
  align(align = "center", part = "header") %>%

  # Additional centering of the upper-level header columns
  align(j = 2:5, align = "center", part = "header")

showtext_auto(FALSE)

## Create tableGrob ----
table_grob <- gen_grob(bionet_table)

## Create ggplot ----

bionet_map <- ggplot() +
   # geom_sf(data = MD_BioNet, fill="white", color = "gray") +
    geom_sf(data = maryland_boundary, alpha = 0, linewidth = 0.55)+
    geom_sf(data = filter(maryland_points_with_bionet_tiers,
                          BioNetTier !="No Tier"),
            aes(color = BioNetTier),
            size = 1.6) +
  coord_sf(xlim = c(st_bbox(maryland_boundary)$xmin - 0.025,
                    st_bbox(maryland_boundary)$xmax + 0.014),
           ylim = c(st_bbox(maryland_boundary)$ymin - 0.009,
                    st_bbox(maryland_boundary)$ymax + 0.014),
           expand = FALSE) +
  scale_color_manual(values = c(
    "#E53900", # brighter vermillion
    "#A95ACD", # stronger purple
    "#0072B2", # distinct deep blue
    "#2FBF71", # richer bluish green
    "#FFD700"  # vivid golden yellow
  )) +
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
    #labs(color = str_wrap(paste0("Number of BioNet Polygons Containing ", species_name),
    #                     width = 35)) +
 #   labs(color = paste0("BioNet \nTiers"))+
    ggthemes::theme_map() +
    theme(
      text = element_text(family = "Aptos"),
      legend.title = element_blank(),
      legend.position = "inside",
      legend.position.inside = c(0.85, 0.53),
      legend.text = element_text(size = 9, family  = "Aptos"),
      legend.spacing.x = unit(.07, "cm"),
      legend.spacing.y = unit(.05, "cm"),
      legend.key.width = unit(.25, "cm"),
      legend.key.height = unit(.43, "cm"),
      panel.border = element_rect(fill = NA, colour = "white", linewidth = .7),
      axis.text = element_blank(),
      plot.margin = margin(-1, .5, -1, .5)
    ) +
  guides(color = guide_legend(override.aes = list(size = 1.5)))

## Combine ggplot and table

plot_xmin <- -79.4   # Minimum longitude (west)
plot_xmax <- -75.0   # Maximum longitude (east)
plot_ymin <- 37.9    # Minimum latitude (south)
plot_ymax <- 39.7    # Maximum latitude (north)

table_width_proportion <- 0.45  # Table width as a fraction of plot width
table_height_proportion <- 0.35 # Table height as a fraction of plot height

# Calculate dimensions
plot_width <- plot_xmax - plot_xmin
plot_height <- plot_ymax - plot_ymin

table_width <- plot_width * table_width_proportion
table_height <- plot_height * table_height_proportion

# Positioning (left side, middle y-axis)
table_xmin <- plot_xmin
table_xmax <- plot_xmin + table_width
table_ymin <- plot_ymin + (plot_height - table_height) / 3.5
table_ymax <- table_ymin + table_height

bionet_map_with_table <- bionet_map +
  annotation_custom(
    grob = table_grob,
    xmin = table_xmin, xmax = table_xmax,
    ymin = table_ymin, ymax = table_ymax
  )

return(list(
  bionet_tier_counts = bionet_tier_counts,
  bionet_map = bionet_map,
  bionet_map_with_table = bionet_map_with_table,
  maryland_points_with_bionet_tiers = maryland_points_with_bionet_tiers,
  total_BioNet_polygons = total_polygons))
}

