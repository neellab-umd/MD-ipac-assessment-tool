
NLCD_Intersect <- function(species_name) {

  NLCD_acreages <- read_csv(here::here("core_assets", "NLCD_Patch_acreage.csv"),
                            show_col_types = FALSE)

  nlcd_types  <- rast("./spatial_data/usa_land_cover_2020v2_30m/NLCD_for_IPAC.tif")

  patches <- rast("./spatial_data/usa_land_cover_2020v2_30m/NLCD_Patches_MD.tif")

  maryland_boundary <- sf::st_read(
    here("spatial_data", "State_Boundaries", "maryland.state.boundary.shp"),
    quiet = TRUE
  ) %>%
    st_make_valid() %>%
    {if (sf::st_crs(.) != sf::st_crs(nlcd_types))
      sf::st_transform(., crs = sf::st_crs(nlcd_types))
      else .}

  points_file <- paste0("./combined_location_data/Points_sf_", species_name, ".RData")
  load(points_file)

  merged_points_md_sf <- merged_points_sf %>%
    dplyr::filter(state == "Maryland")

  # --- Extraction + caching ---
  extract_points_with_nlcd <- function(points_sf, nlcd_types, patches) {

    if (!dir.exists("./cache")) dir.create("./cache")

    cache_extract_file <- paste0("./cache/NLCD_extract_", species_name, ".rds")
    cache_summary_file <- paste0("./cache/NLCD_summary_", species_name, ".rds")

    points_mtime <- file.info(points_file)$mtime
    extract_mtime <- if (file.exists(cache_extract_file)) file.info(cache_extract_file)$mtime else as.POSIXct(0)
    summary_mtime <- if (file.exists(cache_summary_file)) file.info(cache_summary_file)$mtime else as.POSIXct(0)

    # --- Extract ---
    extract_changed <- TRUE
    if (file.exists(cache_extract_file) && extract_mtime > points_mtime) {
      message("✅ Using cached extract for ", species_name)
      extracted <- readRDS(cache_extract_file)
      old_hash <- attr(extracted, "hash")
      new_hash <- digest::digest(extracted)
      if (!is.null(old_hash) && identical(old_hash, new_hash)) {
        extract_changed <- FALSE
      }
    }
    if (!exists("extracted") || extract_changed) {
      message("⚙️ Creating new extract for ", species_name)
      ras <- c(patches, nlcd_types)
      names(ras) <- c("patch_id", "types")
      extracted <- terra::extract(ras, terra::vect(points_sf))
      attr(extracted, "hash") <- digest::digest(extracted)
      saveRDS(extracted, cache_extract_file)
    }

    # --- Summary ---
    summary_changed <- TRUE
    if (file.exists(cache_summary_file) && !extract_changed && summary_mtime > points_mtime) {
      message("✅ Using cached summary for ", species_name)
      return(readRDS(cache_summary_file))
    }

    message("⚙️ Recomputing summary for ", species_name)

    # --- Join + summarize ---
    points_with_data <- dplyr::bind_cols(points_sf, extracted) %>%
      dplyr::select(-state) %>%
      tidyr::replace_na(list(patch_id = 1)) %>%
      mutate(cluster = as.numeric(cluster))

    points_joined <- points_with_data %>%
      dplyr::full_join(NLCD_acreages, by = c("patch_id", "types")) %>%
      dplyr::relocate(types) %>%
      tidyr::replace_na(list(patch_id = 0, cluster = 0, num_points = 0))

    patch_summary <- points_joined %>%
      dplyr::filter(patch_id != 0) %>%
      dplyr::group_by(types, patch_id) %>%
      dplyr::summarise(
        patch_acres = first(patch_acres),
        patch_n_clusters = n_distinct(cluster[!is.na(cluster) & cluster != 0]),
        .groups = "drop"
      )

    occupied_summary <- patch_summary %>%
      dplyr::group_by(types) %>%
      dplyr::summarise(
        n_plant_clusters = sum(patch_n_clusters),
        n_patches_with_plant = sum(patch_n_clusters > 0),
        patch_acres = round(sum(patch_acres[patch_n_clusters > 0], na.rm = TRUE), 0),
        .groups = "drop"
      )

    totals <- NLCD_acreages %>%
      dplyr::distinct(types, .keep_all = TRUE) %>%
      dplyr::transmute(types,
                       total_type_acres = acres,
                       total_num_patches = num_patches)

    summary_df <- totals %>%
      dplyr::left_join(occupied_summary, by = "types") %>%
      tidyr::replace_na(list(n_plant_clusters = 0,
                             n_patches_with_plant = 0,
                             patch_acres = 0)) %>%
      dplyr::relocate(types, total_num_patches, total_type_acres,
                      n_plant_clusters, n_patches_with_plant, patch_acres) %>%
      dplyr::filter(types != "No Type Intersected")

    result <- list(
      summary = as.data.frame(sf::st_drop_geometry(summary_df)),
      points = points_with_data
    )

    saveRDS(result, cache_summary_file)
    return(result)
  }
  ##------------------ End extraction function ------------------

  points_by_type <- extract_points_with_nlcd(
    points_sf = merged_points_md_sf,
    nlcd_types = nlcd_types,
    patches = patches
  )

  ## ------------------ Generate flextable ------------------
  nlcd_table <- as.data.frame(points_by_type$summary) %>%
    dplyr::arrange(desc(n_plant_clusters), types) %>%
    flextable::flextable() %>%
    set_header_labels(
      types = "NLCD Type",
      total_num_patches = "Number of Patches",
      total_type_acres = "Acres",
      n_plant_clusters = "Number of Occurrences",
      n_patches_with_plant = "Number of Occupied Patches",
      patch_acres = "Acreage of Occupied Patches"
    ) %>%
    add_header_row(values = c("", "Total", ""), colwidths = c(1, 2, 3)) %>%
    flextable::compose(i = 1, j = 4, part = "header",
                       value = as_paragraph("With ", as_i(species_name))) %>%
    flextable::width(j = c(1), width = 1.5) %>%
    flextable::width(j = c(3), width = 0.79) %>%
    flextable::width(j = c(4,5,6), width = 0.97) %>%
    theme_booktabs() %>%
    flextable::bold(part = "header") %>%
    fontsize(size = 10, part = "all") %>%
    flextable::merge_at(i = 1, j = 2:3, part = "header") %>%
    flextable::merge_at(i = 1, j = 4:6, part = "header") %>%
    flextable::valign(part = "header", valign = "bottom") %>%
    flextable::align(j = c("total_num_patches","total_type_acres",
                           "n_plant_clusters", "patch_acres"),
                     align = "right", part = "body") %>%
    flextable::align(align = "center", part = "header") %>%
    flextable::align(j = 2:6, align = "center", part = "header") %>%
    flextable::vline(j = 1, part = "all",
                     border = officer::fp_border(color = "gray60", width = 1)) %>%
    flextable::vline(j = 3, part = "all",
                     border = officer::fp_border(color = "gray60", width = 1)) %>%
    line_spacing(i = NULL, j = NULL, space = .8, part = "header") %>%
    line_spacing(i = NULL, j = NULL, space = .8, part = "body") %>%
    padding(part = "header", padding.top = 1.6, padding.bottom = 1.6) %>%
    padding(part = "body", padding.top = 1.7, padding.bottom = 1.7) %>%
    set_table_properties(opts_word = list(split = FALSE, keep_with_next = TRUE))



  ## Add in column of the percent of patches with the plant that is needed for the evidence for question 9B.  Added here to prevent the conflict that prevented it from being removed headed into flextable.

  final.summary_df <- points_by_type$summary_df %>%
    dplyr::mutate(percent_patches_with_plant =
                    n_patches_with_plant / total_num_patches * 100)

  ## Return everything for downstream use
  return(list(
    df = final.summary_df,
    table = nlcd_table,
    plot = nlcd_map
  ))
}

