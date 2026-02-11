library(tidyverse)
library(tidyr)  # Added for replace_na
library(sf)
library(spData)

################################################################
## Function that makes ggplot maps of the combined distribution
## and native range data for countries and states
################################################################

## This function relies on data in this file that is loaded in the quarto script:
## load(file = paste0("./combined_location_data/intro_info_list_",
##                    file_safe_name, ".RData"))

## That file is created by the functions in the Query_Intro_Info.R script.

## The function is run in the main Quarto document.

map_combined_distributions <- function(intro_info_list) {

  world_map <- st_read(system.file("shapes/world.gpkg", package = "spData"), quiet = TRUE) %>%
    st_cast("POLYGON") %>%
    # Identify French Guiana based on bounding box in northern S. America
    mutate(
      is_fg = st_within(
        st_geometry(.),
        st_as_sfc(st_bbox(c(
          xmin = -55, xmax = -50,
          ymin = 2,  ymax = 7
        ), crs = 4326)),
        sparse = FALSE
      )[,1]
    ) %>%
    # Fix attributes
    mutate(
      name_long = if_else(is_fg, "French Guiana", name_long),
      iso_a2    = if_else(is_fg, "GF", iso_a2)
    ) %>%
    select(-is_fg)

  # Prepare country data - join directly on ISO codes
  countries_with_status <- intro_info_list$countries %>%
    select(location, status, record_count)

  # Create world map - join on ISO codes
  world_distribution <- world_map %>%
    left_join(countries_with_status, by = c("iso_a2" = "location")) %>%
    mutate(status = replace_na(status, "Not documented"))

  world_plot <- ggplot(world_distribution) +
    geom_sf(aes(fill = status), color = "white", size = 0.05) +
    scale_fill_manual(
      values = c(
        "Native" = "#2E7D32",
        "Non-native" = "#FFA726",
        "Not documented" = "gray95"
      )) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 9),
      legend.title = element_blank(),
      legend.key.size = unit(.6, unit = "lines"),
      legend.margin = margin(t = -1, r = 0, b = 0, l = 0, unit = "pt")
    )

  # Create US state data
  states_with_status <- intro_info_list$us_states %>%
    select(location, status, record_count_max)


  # Load US boundaries - CONTERMINOUS ONLY (for standalone map)
  states_48_dc <- c(setdiff(state.name, c("Alaska", "Hawaii")),
                    "District of Columbia")

  us_map_48 <- st_read(here("spatial_data", "State_Boundaries",
                            "boundaries_p_2021_v3.shp"),
                       quiet = TRUE,
                       options = "ENCODING=UTF-8") %>%
    filter(COUNTRY == "USA" & NAME_En %in% states_48_dc) %>%
    mutate(NAME_En = iconv(NAME_En, from = "UTF-8", to = "UTF-8", sub = "")) %>%
    st_make_valid() %>%
    st_transform(4326)

  us_distribution_48 <- us_map_48 %>%
    left_join(states_with_status, by = c("NAME_En" = "location")) %>%
    mutate(
           status = replace_na(status, "Not documented"),
           count_bin = case_when(
              record_count_max == 0 ~ "ND",
              record_count_max <= 20 ~ "1-20",
              record_count_max <= 50 ~ "21-50",
              record_count_max <= 100 ~ "51-100",
              TRUE ~ ">100"
                                ),
           count_bin = factor(count_bin,
                         levels = c("ND", "1-20", "21-50", "51-100", ">100"))
      )

  #### Plot presence absence only

  us_presence_plot <- ggplot(us_distribution_48) +
    geom_sf(aes(fill = status), color = "white", size = 0.1,
            show.legend = FALSE) +
    scale_fill_manual(
      values = c(
        "Native" = "#2E7D32",
        "Non-native" = "#FFA726",
        "Not documented" = "gray92"
      )
    ) +
    theme_minimal()

# Alternative US map that shows density classes for the US.

  us_density_plot <- ggplot(us_distribution_48) +
    geom_sf(aes(fill = count_bin), color = "white", size = 0.2) +
    scale_fill_manual(
      values = c(
        "ND" = "gray92",
        "1-20" = "#FFE0B2",    # Very light cream
        "21-50" = "#FFB74D",   # Medium orange
        "51-100" = "#F57C00",  # Darker orange
        ">100" = "#BF360C"     # Very dark red-orange
      ),
      name = "Record Count"
    ) +
    theme_minimal() +
    theme(
      #legend.position.inside = TRUE,
      legend.position = c(0.92,0.26),
      legend.title = element_text(size = 8),
      legend.text  = element_text(size = 7),
      legend.key.size = unit(.7, unit = "lines")
    )


### Combined World and Presence Absence State Map ####
  # Load US boundaries - ALL 50 + DC (for combined map)
  states_all <- c(state.name, "District of Columbia")

  us_map_all <- st_read(here("spatial_data", "State_Boundaries",
                             "boundaries_p_2021_v3.shp"),
                        quiet = TRUE,
                        options = "ENCODING=UTF-8") %>%
    filter(COUNTRY == "USA" & NAME_En %in% states_all) %>%
    mutate(NAME_En = iconv(NAME_En, from = "UTF-8", to = "UTF-8", sub = "")) %>%
    st_make_valid() %>%
    st_transform(4326)

  us_distribution_all <- us_map_all %>%
    left_join(states_with_status, by = c("NAME_En" = "location")) %>%
    mutate(status = replace_na(status, "Not documented"))


  # Create combined map with all 50 states
  world_part <- world_distribution %>%
    filter(iso_a2 != "US") %>%
    select(status) %>%
    st_set_geometry("geometry")

  us_part <- us_distribution_all %>%
    select(status) %>%
    st_set_geometry("geometry")

  combined_map <- rbind(world_part, us_part)

  combined_plot <- ggplot(combined_map) +
    geom_sf(aes(fill = status), color = "white", size = 0.1) +
    scale_fill_manual(
      values = c(
        "Native" = "#2E7D32",
        "Non-native" = "#FFA726",
        "Not documented" = "gray92"
      )
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.key.size = unit(.9, unit = "lines"),
      legend.margin = margin(t = -1, r = 0, b = -1, l = 0, unit = "pt"),
      legend.title = element_blank(),
      plot.margin = margin(t = -1, r = 0, b = -1, l = 0, unit = "pt")
    )

  return(list(
    world_plot = world_plot,
    us_presence_plot = us_presence_plot,
    us_density_plot = us_density_plot,
    combined_plot = combined_plot
  ))
}
# Usage:
# # Create maps
# maps <- map_combined_distributions(intro_info_list)

#
# library(patchwork)
# maps$world_plot/maps$us_presence_plot

#
# maps$world_plot / maps$us_presence_plot + plot_layout(heights = c(1.5, 1))
#
# maps$world_plot / maps$us_density_plot + plot_layout(heights = c(1.5, 1))
#
# maps$combined_plot
