## Usage: Forest_Intersect(species name)

## Maile Neel 2025

Forest_Intersect <- function(species_name) {
  cat("\n=== Starting Memory: ===\n")
  print(gc())
if (!require("pacman")) install.packages("pacman")
  pacman::p_load(here,
   magrittr,
   tidyverse,
   ggthemes,
   sf,
   terra,
   tidyterra,
   ggspatial,
   tigris,
   scales,
   viridis,
  flextable,
   ggtext)


### READ IN ALL CSVS AND TIFS ----
  cat("\n=== After packages: ===\n")
  print(gc())
## Read in type code and name crosswalk csv

type_names <- read_csv(here::here("spatial_data",
  "Forest_Type_Tiffs",
  "Forest_Type_Crosswalk.csv"), show_col_types = FALSE)

group_names <- type_names %>%
  dplyr::select(new_FIA_Group, FIA_Group) %>%
  dplyr::distinct()

## Read in acreages for FIA Forest Types and Forest Groups that were calculated in the script Calculate_Forest_Type_Areas.R

#FIA_type_acreages <- read_csv(here::here("core_assets", "FIA_Forest_Type_acreage.csv"),show_col_types = FALSE)

#FIA_group_acreages <- read_csv(here::here("core_assets", "FIA_Forest_Group_acreage.csv"),show_col_types = FALSE)

FIA_group_patch_acreages <- read_csv(here::here("core_assets", "FIA_Forest_Group_Patch_acreage.csv"),show_col_types = FALSE)

## Read in FIA Forest Group raster data for MD.
group_forests <- rast(here::here("spatial_data", "Forest_Type_Tiffs", "FIA_Forest_Groups_MD.tif"))

## Read in raster of clump ID information for FIA Forest Groups for MD.
group_patches <- rast(here::here("spatial_data", "Forest_Type_Tiffs", "FIA_Forest_Group_Patches_MD.tif"))

## load shapefile of state and state boundaries and filter for Maryland.
maryland_boundary <- sf::st_read(here("spatial_data", "State_Boundaries",
  "maryland.state.boundary.shp"), quiet = TRUE) %>%
  st_make_valid() %>%
  {if (sf::st_crs(.) != sf::st_crs(group_forests))
sf::st_transform(., crs = sf::st_crs(group_forests))
else .}

#Create a file safe name from species name
file_safe_name <- str_replace_all(species_name, " ", "_")

## Load the two saved objects with specified occurrence points for the chosen species: merged_points_sf and distinct_combined_points_sf. Only the merged points (i.e., the clusters) are used in this script.

load(paste0("./combined_location_data/Points_sf_",
            file_safe_name, ".RData"))

merged_points_md_sf <- merged_points_sf %>%
  filter(state == "Maryland") %>%
  {if (sf::st_crs(.) != sf::st_crs(group_forests))
    sf::st_transform(., crs = sf::st_crs(group_forests))
    else .}
cat("\n=== Data loaded: ===\n")
print(gc())
## CREATE EXTRACTION FUNCTION ----

## This function extract forest and clump information for each point, joins the extracted points with acreage and patch number data.

extract_and_summarize <- function(points_sf, forests, patches) {

### Extract forest Groups ----

# Combine rasters and assign names
    ras <- c(patches, forests)
    names(ras) <- c("patch_id", "FIA_Group")

    # Extract raster values at point locations
    extracted <- terra::extract(ras, terra::vect(points_sf))

    points_with_data <- dplyr::bind_cols(points_sf, extracted) %>%
      dplyr::select(-state) %>%
      tidyr::replace_na(list(patch_id = 1)) # if occurrence is in na value of the raster, replace the patch_id with 1

    # Join group names and acreage data
    points_joined <- points_with_data %>%
      dplyr::full_join(FIA_group_patch_acreages,
        by = c("patch_id", "FIA_Group")) %>%
      dplyr::relocate(FIA_Group)
    tidyr::replace_na(list(patch_id = 0, num_points = 0))

    summary_df <- points_joined %>%
      group_by(FIA_Group, patch_id, patch_acres) %>%
      summarise(
        # does this patch contain plants?
        n_plant_clusters = if (all(cluster == 0, na.rm = TRUE)) 0
        else n_distinct(cluster),
        .groups = "drop"
      ) %>%
      group_by(FIA_Group) %>%
      summarise(
        total_num_patches    = n_distinct(patch_id),
        total_group_acres    = sum(patch_acres),
        n_patches_with_plant = sum(n_plant_clusters > 0),
        occupied_acres       = sum(patch_acres[n_plant_clusters > 0]),
        n_plant_clusters     = sum(n_plant_clusters),
        .groups = "drop"
      ) %>%
      relocate(FIA_Group, total_num_patches, total_group_acres,
               n_plant_clusters, n_patches_with_plant, occupied_acres)

  return(list(
    summary = sf::st_drop_geometry(summary_df),
    points = points_with_data
  ))
}

## RUN the EXTRACTION FUNCTION for forest groups----
group_output <- extract_and_summarize(
  points_sf = merged_points_md_sf,
  forests = group_forests,
  patches = group_patches
)
cat("\n=== After Extraction: ===\n")
print(gc())
group_summary_df <- group_output$summary

group_points_with_data <- group_output$points

### Create  Table ----
make_forest_table <- function(summary_df, species_name) {

species_name <- str_replace_all(file_safe_name, "_", " ")

    id_col <- "FIA_Group"
    id_label <- "FIA Forest Group"


  # build table
  ft <- summary_df %>%
    arrange(desc(n_plant_clusters), !!sym(id_col)) %>%
    filter(!is.na(!!sym(id_col))) %>%
    flextable::flextable() %>%

    # set lower-level headers (using a named list instead of :=)
    set_header_labels(
        total_num_patches    = "Number of Patches",
        total_group_acres    = "Acres",
        n_patches_with_plant = "Number of Occupied Patches",
        n_plant_clusters     = "Number of Occurrences",
        occupied_acres       = "Acreage of Occupied Patches"
    ) %>%

    # add higher-level headers
    add_header_row(
      values = c("", "Total", ""),  # leave placeholder for last col
      colwidths = c(1, 2, 3)
    ) %>%
    flextable::compose(
      i = 1, j = 4, part = "header",  # location of merged header cell
      value = as_paragraph(
        "With ", as_i(species_name)   # "With" plain, species name italicized
      )
    ) %>%

    flextable::width(j = c(1), width = 2) %>%
    flextable::width(j = c(3), width = 0.85) %>%
    flextable::width(j = c(4:6), width = 0.97) %>%

    theme_booktabs() %>%
    flextable::bold(part = "header") %>%
    flextable::fontsize(size = 10, part = "all") %>%


    # Merge higher-level headers
    flextable::merge_at(i = 1, j = 2:3, part = "header") %>%
    flextable::merge_at(i = 1, j = 4:6, part = "header") %>%

    # align numeric columns and headers
    flextable::valign(part = "header", valign = "bottom") %>%

    flextable::align(
      j = c("total_num_patches", "total_group_acres",
            "n_plant_clusters", "n_patches_with_plant",
            "occupied_acres"),
      align = "right", part = "body"
    ) %>%

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

    line_spacing(i = NULL, j = NULL, space = .8,
                 part = "header") %>%
    line_spacing(i = NULL, j = NULL, space = .8,
                 part = "body") %>%

    padding(part = "header", padding.top = 1.6, padding.bottom = 1.6) %>%
    padding(part = "body", padding.top = 1.7, padding.bottom = 1.7) %>%
    set_table_properties(opts_word = list(split = FALSE,
                                          keep_with_next = TRUE))

  ft
}

group_table <- make_forest_table(group_output$summary, species_name)
cat("\n=== After Table: ===\n")
print(gc())

### FIA Group Map ----
group_forests_plot <- terra::aggregate(group_forests, fact = 2, fun = "modal")
cat("\n=== After aggregate: ===\n")
print(gc())

# Get factor levels from the points that were actually extracted
# This ensures the palette matches what you're plotting as points
present_group_labels <- unique(group_output$points$FIA_Group)
present_group_labels <- present_group_labels[!is.na(present_group_labels)]

# Create palette for these labels
n_present_groups <- length(present_group_labels)
present_group_palette <- viridis::viridis(n_present_groups,
                                          option = "D", begin = .1, end = 1)

named_group_palette <- setNames(present_group_palette, present_group_labels)

# NOW aggregate for display purposes only
group_forests_plot <- terra::aggregate(group_forests, fact = 2, fun = "modal")

cat("\n=== After aggregate: ===\n")
print(gc())

# Create palette only for present labels
n_present_groups <- length(present_group_labels)
present_group_palette <- viridis::viridis(n_present_groups,
  option = "D", begin = .1, end = 1)
named_group_palette <- setNames(present_group_palette, present_group_labels)



FIA_group_map <- ggplot() +
  geom_spatraster(data = group_forests_plot, aes(fill = label),
    alpha = 0.5, show.legend = FALSE) +
  geom_sf(
    data = filter(group_output$points, !is.na(FIA_Group)),
    aes(color = FIA_Group),
    size = 2.5
  ) +
  geom_sf(data = maryland_boundary, alpha = 0, linewidth = 0.6) +
  theme_map() +
  theme(
    legend.title = element_blank(),
    legend.position = "inside",
    legend.position.inside = c(0.15, 0.19),
    legend.text = element_text(size = 9),
    legend.spacing.x = unit(.06, "cm"),
    legend.spacing.y = unit(.04, "cm"),
    legend.key.width = unit(.25, "cm"),
    legend.key.height = unit(.4, "cm"),
    panel.border = element_rect(fill = NA, colour = "black", linewidth = 1),
    axis.text = element_blank()
  ) +
  scale_fill_manual(
    values = named_group_palette,
    na.translate = FALSE
  ) +
  scale_color_manual(
    values = named_group_palette,
    na.translate = FALSE
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
cat("\n=== After Graph: ===\n")
print(gc())
## Return summary objects----
return(list(
  group_points = group_points_with_data,
  group = group_summary_df,
 # type_table = type_table,
  group_table = group_table,
  group_map = FIA_group_map,
  FIA_groups  = FIA_group_patch_acreages))

}

