
Province_Intersect <- function(species_name) {

  # if (!require("pacman")) install.packages("pacman")
  # pacman::p_load(here,
  #                magrittr,
  #                tidyverse,
  #                ggthemes,
  #                sf,
  #                ggplot2,
  #                ggrepel,
  #                ggspatial,
  #                scales,
  #                gt,
  #                ggtext)

#Create a file safe name from species name
  file_safe_name <- str_replace_all(species_name, " ", "_")

# load species occurrence points. This will load to objects with species occurrences: merged_points_sf and distinct_combined_points_sf

load(paste0("./combined_location_data/Points_sf_",
            file_safe_name, ".RData"))

sf::sf_use_s2(FALSE)

maryland_boundary <- sf::st_read(here::here("spatial_data",  "State_Boundaries", "boundaries_p_2021_v3.shp"),
                           quiet = TRUE) %>%
    filter(COUNTRY == "USA" & STATEABB %in% c("US-MD")) %>%
    st_make_valid() %>%
    sf::st_transform(4326)

  MD_Provinces <- sf::st_read(here::here("spatial_data",
                                         "Maryland_Geology_-_Physiographic_Provinces",
                                         "Maryland_Geology_-_Physiographic_Provinces.shp"),
                              quiet = TRUE) %>%
    sf::st_transform(4326)

  # Summarize occurrences by state and drop geometry
  summary_table <- merged_points_sf %>%
    group_by(state) %>%
    summarise(
      clusters = n(),
      points = sum(num_points)
    ) %>%
    st_drop_geometry() %>%
    gt() %>%
    cols_label(
      state = "State",
      clusters = md("**Number<br>of Occurrences**"),  # Wrap header using markdown
      points = md("**Total<br>Points**")  # Wrap header using markdown
    ) %>%
    fmt_number(
      columns = c(clusters, points),
      decimals = 0  # Remove decimal places
    ) %>%
    tab_options(
      table.font.size = "medium",
      heading.align = "center",
      column_labels.font.weight = "bold"
    )

# Ensure merged_points_sf is an sf object with correct CRS

  maryland_points_sf <- merged_points_sf %>%
    filter(state == "Maryland") %>%
    st_drop_geometry() %>%  # Remove the existing UTM geometry
    st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)  # Create new geometry from lat/lon

  # Spatially join occurrences with geologic provinces
  maryland_with_provinces <- st_intersection(maryland_points_sf, maryland_boundary) %>%
    st_intersection(., MD_Provinces) %>%
    dplyr::mutate(PROVINCE =
                    factor(PROVINCE,
                           levels = c("Appalachian Plateaus Province",
                                      "Ridge and Valley Province",
                                      "Blue Ridge Province",
                                      "Piedmont Plateau Province",
                                      "Atlantic Coastal Plain Province")))

  province_counts <- maryland_with_provinces %>%
    dplyr::group_by(PROVINCE) %>%
    dplyr::summarise(count = n())

  # Rename the 'PROVINCE' column in one of the data frames to avoid conflict
  maryland_with_provinces <- maryland_with_provinces %>%
    dplyr::rename(PROVINCE_Maryland = PROVINCE)

  # Perform the spatial join
  maryland_with_provinces <- st_join(maryland_with_provinces,
                                     province_counts,
                                     join = st_intersects)

  legend_labels <- province_counts %>%
    dplyr::mutate(label = paste0(PROVINCE, " (n = ", count, ")")) %>%
    pull(label)

  color_labels <- setNames(legend_labels, province_counts$PROVINCE)

  ## Plot the occurrences by Province
province_occurrences <- ggplot() +
    geom_sf(data = MD_Provinces, fill = "white", ) +
    geom_sf(data = maryland_boundary, alpha = 0, linewidth = .5)+
    geom_sf(data = maryland_with_provinces,  aes(color = PROVINCE), size = .8) +
    coord_sf(xlim = c(st_bbox(maryland_boundary)$xmin - 0.025,
                      st_bbox(maryland_boundary)$xmax + 0.014),
             ylim = c(st_bbox(maryland_boundary)$ymin - 0.009,
                      st_bbox(maryland_boundary)$ymax + 0.014),
             expand = FALSE) +
    scale_color_brewer(palette = "Dark2", labels = color_labels)+
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
    labs(color = "Number of Occurrences by Province") +
    ggthemes::theme_map() +
    theme(
      text = element_text(family = "Aptos"),
      legend.title = element_text(size = 9.4, face = "bold", hjust = 0.5),
      legend.position = "inside",
      legend.position.inside = c(0.06, 0.3),
      legend.text = element_text(size = 8),
      legend.spacing.x = unit(.07, "cm"),
      legend.spacing.y = unit(.4, "cm"),
      legend.key.width = unit(.15, "cm"),
      legend.key.height = unit(.42, "cm"),
      panel.border = element_rect(fill = NA, colour = "white", linewidth = 1),
      axis.text = element_blank(),
      plot.margin = margin(-1,.5,-1,.5)
    ) +
    guides(color = guide_legend(override.aes = list(size = 1.5)))

 return(list(df = maryland_with_provinces,
   province_plot = province_occurrences))

}
