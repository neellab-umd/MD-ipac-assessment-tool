if (!require("pacman")) install.packages("pacman")

pacman::p_load(here,
               tidyverse,
               sf,
               update = FALSE)

Heritage_Check <- function(species_name) {

#If the data are in a shapefile use this
# rank_utm <- sf::st_read(here::here("nameofshapefile.shp"),
#                                  quiet = TRUE) %>%  #   st_make_valid() %>%
#   sf::st_transform(26918)

rank_utm <- read_csv("state_rank_test_set.csv") %>%
    st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>% #WGS84 - check what Heritage uses
    st_transform(crs = 26918) # Convert to UTM NAD83 Zone 18 for distance calcs
if (!require("pacman")) install.packages("pacman")
pacman::p_load(here,
               magrittr,
               tidyverse,
               sf)

query_utm <- read_csv(paste0(species_name, "_occurrences.csv")) %>%
    select(-geometry) %>%
    st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>% #WGS84#In WGS84
    st_transform(crs = 26918) #UTM NAD83 Zone 18

  # Combine with rank points
rank_with_counts <- rank_utm %>%
    mutate(
      n_within_100m  = lengths(st_is_within_distance(rank_utm, query_utm, dist = 100)),
      n_within_250m  = lengths(st_is_within_distance(rank_utm, query_utm, dist = 250)),
      n_within_500m  = lengths(st_is_within_distance(rank_utm, query_utm, dist = 500)),
      n_within_1000m = lengths(st_is_within_distance(rank_utm, query_utm, dist = 1000))
    ) %>%
    st_drop_geometry() %>%  #Gets rid of exact location
    write_csv(paste0(species_name, "_Heritage_Counts.csv", na = ""))

}
