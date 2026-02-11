
NLCD_Intersect <- function(species_name) {

  # if (!require("pacman")) install.packages("pacman")
  # pacman::p_load(here,
  #                magrittr,
  #                sf,
  #                raster,
  #                terra,
  #                tidyterra,
  #                ggspatial,
  #                ggthemes,
  #                flextable,
  #                ggtext,
  #                tigris,
  #                scales,
  #                tidyverse)

  ## Read in acreages for NLCD Types

NLCD_acreages <- read_csv(here::here("core_assets", "NLCD_Patch_acreage.csv"),
                          show_col_types = FALSE)

## Read in NLCD Types raster data for MD----
  nlcd_types  <- rast("./spatial_data/usa_land_cover_2020v2_30m/NLCD_for_IPAC.tif") #%>%
  #  { if (!compareCRS(., "EPSG:26918")) terra::project(., "EPSG:26918") else . }

## Read in NLCD patch ID raster data for MD----
  patches <-  rast("./spatial_data/usa_land_cover_2020v2_30m/NLCD_Patches_MD.tif") #%>%
 #   { if (!compareCRS(., "EPSG:26918")) terra::project(., "EPSG:26918") else . }


## load shapefile of state and state boundaries and filter for Maryland.
maryland_boundary <- sf::st_read(here("spatial_data", "State_Boundaries",
                                        "maryland.state.boundary.shp"), quiet = TRUE) %>%
    st_make_valid() %>%
    {if (sf::st_crs(.) != sf::st_crs(nlcd_types))
      sf::st_transform(., crs = sf::st_crs(nlcd_types))
      else .}

#Create a file safe name from species name
file_safe_name <- str_replace_all(species_name, " ", "_")

## Load the two saved objects with specified species occurrence points: merged_points_sf and distinct_combined_points_sf. Only the merged points (i.e., the clusters) are used in this script.

load(paste0("./combined_location_data/Points_sf_",
            file_safe_name, ".RData"))

  merged_points_md_sf <- merged_points_sf %>%
    dplyr::filter(state == "Maryland") %>%
    {if (sf::st_crs(.) != sf::st_crs(nlcd_types))
      sf::st_transform(., crs = sf::st_crs(nlcd_types))
      else .}

  ##Create function to extract NLCD type and patch information for each point

extract_points_with_nlcd <- function(points_sf, nlcd_types, patches) {
    ras <- c(patches, nlcd_types)
    names(ras) <- c("patch_id", "types")

    # create a cache file of the extraction results for each species. This will speed things up when you do multiple runs with the same species. This extraction operation is very slow - 20 minutes or so.

    if (!dir.exists("./cache")) dir.create("./cache")

    cache_file <- paste0("./cache/NLCD_extract_", file_safe_name, ".rds")
    points_file <- paste0("./combined_location_data/Points_sf_", file_safe_name, ".RData")

    # Check whether cache exists and is newer than the points file
    use_cache <- file.exists(cache_file) &&
      file.exists(points_file) &&
      file.info(cache_file)$mtime > file.info(points_file)$mtime

    if (use_cache) {
      message("✅ Using cached extract for ", species_name)
      extracted <- readRDS(cache_file)
    } else {
      message("⚙️ Creating new extract for ", species_name)
      extracted <- terra::extract(ras, terra::vect(points_sf))
      saveRDS(extracted, cache_file)
    }

    points_with_data <- dplyr::bind_cols(points_sf, extracted) %>%
      dplyr::select(-state) %>%
      tidyr::replace_na(list(patch_id = 1)) %>%
      mutate(cluster = as.numeric(cluster))

    points_joined <- points_with_data %>%
      sf::st_drop_geometry() %>%
         dplyr::full_join(NLCD_acreages,
             by = c("patch_id",
                 "types")) %>%
         dplyr::relocate(types) %>%
      tidyr::replace_na(list(patch_id = 0, cluster = 0,num_points = 0))

    patch_summary <- points_joined %>%
      # exclude the placeholder/no-intersection row (patch_id == 0)
      dplyr::filter(patch_id != 0) %>%
      dplyr::group_by(types, patch_id) %>%
      dplyr::summarise(
        patch_acres = first(patch_acres),
        # number of distinct (real) clusters in that patch (ignore 0 / NA)
        patch_n_clusters = n_distinct(cluster[!is.na(cluster) & cluster != 0]),
        .groups = "drop"
      )

    # 2) Per-type occupied-summary (only one row per patch contributes)
    occupied_summary <- patch_summary %>%
      dplyr::group_by(types) %>%
      dplyr::summarise(
        n_plant_clusters = sum(patch_n_clusters),   # sum of clusters across patches
        n_patches_with_plant = sum(patch_n_clusters > 0), # count of occupied patches
        patch_acres = round(sum(patch_acres[patch_n_clusters > 0], na.rm = TRUE),0), # acreage of occupied patches
        .groups = "drop"
      )

    # 3) Totals come straight from your NLCD_acreages table (one row per type)
    totals <- NLCD_acreages %>%
      dplyr::distinct(types, .keep_all = TRUE) %>%
      dplyr::transmute(
        types,
        total_type_acres = acres,
        total_num_patches = num_patches
      )


    # 4) Combine totals + occupied info (keeps all types in totals)
    summary_df <- totals %>%
      dplyr::left_join(occupied_summary, by = "types") %>%
      tidyr::replace_na(list(
        n_plant_clusters = 0,
        n_patches_with_plant = 0,
        patch_acres = 0
      )) %>%
      dplyr::relocate(
        types,  total_num_patches, total_type_acres,
        n_plant_clusters, n_patches_with_plant, patch_acres
      ) %>%
      dplyr::filter(!types %in% c("No Type Intersected", "Water"))

       return(list(
      summary = summary_df,
      points = points_with_data
    ))
}


## RUN THE EXTRACTION FUNCTION ----

points_by_type <- extract_points_with_nlcd(
  points_sf = merged_points_md_sf,
  nlcd_types = nlcd_types,
  patches = patches
)


## CREATE TABLE ----
species_name <- str_replace_all(file_safe_name, "_", " ")

nlcd_table <- as.data.frame(points_by_type$summary) %>%
  arrange(desc(n_plant_clusters), types) %>%
    flextable::flextable() %>%

    # Set lower-level headers
    set_header_labels(
      types = "NLCD Type",
      total_num_patches = "Number of Patches",
      total_type_acres = "Acres",
      n_plant_clusters = "Number of Occurrences",
      n_patches_with_plant = "Number of Occupied Patches",
      patch_acres = "Acreage of Occupied Patches"
    ) %>%

    # Add higher-level headers
  add_header_row(
    values = c("", "Total", ""),  # leave placeholder for last col
    colwidths = c(1, 2, 3)
  ) %>%
  flextable::compose(
    i = 1, j = 4, part = "header",  # location of your merged header cell
    value = as_paragraph(
      "With ", as_i(species_name)   # "With" plain, species name italic
    )
  ) %>%

  flextable::width(j = c(1), width = 2) %>%
  flextable::width(j = c(3), width = 0.85) %>%
  flextable::width(j = c(4,5,6), width = 0.97) %>%

  theme_booktabs()  %>% # Apply clean styling
  flextable::bold(part = "header") %>%
  fontsize(size = 10, part = "all") %>%

    # Merge higher-level headers
    flextable::merge_at(i = 1, j = 2:3, part = "header") %>%  # Merge "acres and patches"
    flextable::merge_at(i = 1, j = 4:6, part = "header") %>%  # Merge Occurrences

   # align numeric columns and headers
    flextable::valign(part = "header", valign = "bottom") %>%
    # Right-align numbers in the body

   flextable::align(j = c("total_num_patches","total_type_acres",
      "n_plant_clusters", "patch_acres"),
          align = "right", part = "body") %>%

  # center-align the higher-level headers
    flextable::align(align = "center", part = "header") %>%

  # Additional centering of the upper-level header columns
  flextable::align(j = 2:6, align = "center", part = "header") %>%

  # ---- add lightweight vertical lines to the right of column 1 and 3 ----
flextable::vline(
  j = 1,
  part = "all",
  border = officer::fp_border(color = "gray60", width = 1)
) %>%
  flextable::vline(
    j = 3,
    part = "all",
    border = officer::fp_border(color = "gray60", width = 1)
  ) %>%

  line_spacing(i = NULL, j = NULL, space = .8, part = "header") %>%
  line_spacing(i = NULL, j = NULL, space = .8, part = "body") %>%

  padding(part = "header", padding.top = 1.6, padding.bottom = 1.6) %>%
  padding(part = "body", padding.top = 1.7, padding.bottom = 1.7) %>%
  set_table_properties(opts_word = list(split = FALSE,
                                        keep_with_next = TRUE))




## Set up colors for NLCD Map

  nlcd_colors <- c(
    "Temperate or Subpolar Needleleaf Forest" = "#006400",
    "Tropical or Subtropical Broadleaf Evergreen Forest" = "#228B22",
    "Tropical or Subtropical Broadleaf Deciduous Forest" = "#66CD00",
    "Temperate or Subpolar Broadleaf Deciduous Forest" = "#32CD32",
    "Mixed Forest" = "#ADFF2F",
    "Tropical or Subtropical Shrubland" = "#C1CDCD",
    "Temperate or Subpolar Shrubland" = "#C1CDC1",
    "Tropical or Subtropical Grassland" = "#BDB76B",
    "Temperate or Subpolar Grassland" = "#EEE8AA",
    "Wetland" = "#46C7C7",
    "Cropland" = "#FFD700",
    "Barren Land" = "#D3D3D3",
    "Urban and Built-up" = "#FF0000",
    "Water" = "#4682B4",
    "Snow and Ice" = "#FFFFFF"
  )

### CREATE MAP----

nlcd_map <- ggplot() +
    geom_spatraster(data = nlcd_types, aes(fill = label), alpha = 0.6, na.rm = TRUE) +
    geom_sf(data = dplyr::filter(points_by_type$points, !is.na(types)),
            aes(color = types),
            size = 2.5) +
    geom_sf(data = maryland_boundary, alpha = 0, linewidth = 0.55) +
    scale_fill_manual(
      values = nlcd_colors,
      na.translate = FALSE
    ) +
    scale_color_manual(
      values = nlcd_colors,
      na.translate = FALSE
    ) +
    theme_map() +
    theme(
      legend.title = element_blank(),
      legend.position = "inside",
      legend.position.inside = c(0.03, 0.05),
      legend.text = element_text(size = 9),
      legend.spacing.x = unit(.06, "cm"),
      legend.spacing.y = unit(.04, "cm"),
      legend.key.width = unit(.25, "cm"),
      legend.key.height = unit(.4, "cm"),
      panel.border = element_rect(fill = NA, colour = "black", linewidth = 1),
      axis.text = element_blank()
    ) +
    guides(
      color = guide_legend(override.aes = list(size = 2)),
      fill = guide_legend(override.aes = list(alpha = 1))
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
)

  ## Add percentage column for Evidence calculation for Q9b
  final_summary <- points_by_type$summary %>%
    dplyr::mutate(percent_patches_with_plant = n_patches_with_plant/total_num_patches*100)

  return(list(
    df = final_summary,
    table = nlcd_table,
    plot = nlcd_map
  ))

}

