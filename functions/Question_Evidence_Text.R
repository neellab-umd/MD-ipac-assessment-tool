library(flextable)
library(dplyr)
library(scales)
library(english)

## This function populates the evidence cells in the question tables for questions that are answered by the GIS analysis.  The answer to each question is built as a paragraph that combines text with different faces and chunks that contain results of R code. All the spatial results are generated in this function but they are pulled into the appropriate table for a given question number.

## This function is called from within the quarto document where the species name is already a given and all spatial analysis results have been generated.

Question_Evidence <- function() {

### ========== HELPER FUNCTIONS ==========

  ## Format percentages to appropriately handle tiny values so they don't round to 0
  format_pct <- function(n, total, digits = 1, min_display = 0.1) {
    if (total == 0 || n == 0) return("")
    pct <- n / total * 100
    if (round(pct, digits) == 0) {
      paste0("<", min_display, "%")
    } else {
      paste0(round(pct, digits), "%")
    }
  }

  ## Convert numbers to words for values < 10 (VECTORIZED)
  num_to_word <- function(n) {
    map_chr(n, function(x) {
      if (is.na(x) || x >= 10) {
        return(as.character(x))
      } else {
        return(as.character(english(x)))
      }
    })
  }

  ## Ensure singular/plural verb agreement
  verb_agreement <- function(n, singular = "was", plural = "were") {
    if (is.na(n) || n != 1) {
      return(plural)
    } else {
      return(singular)
    }
  }

  ## ========== END HELPER FUNCTIONS ==========

 ## Question 5c -----------------
 ### Calculations ----
  n_occurrences_total_5c <- nrow(BioNet_Output$maryland_points_with_bionet_tiers)

  ## get all the counts for by tiers
  all_tiers <- c("Tier 1", "Tier 2", "Tier 3", "Tier 4", "Tier 5", "No Tier", "Tier 1–3")

  tier_summary <- BioNet_Output$bionet_tier_counts %>%
    # Add Tier 1–3 combined row
    bind_rows(
      tibble(
        BioNetTier = "Tier 1–3",
        num_occurrences = sum(.$num_occurrences[.$BioNetTier %in% c("Tier 1","Tier 2","Tier 3")], na.rm = TRUE),
        percent_occurrences = sum(.$percent_occurrences[.$BioNetTier %in% c("Tier 1","Tier 2","Tier 3")], na.rm = TRUE),
        num_polygons = sum(.$num_polygons[.$BioNetTier %in% c("Tier 1","Tier 2","Tier 3")], na.rm = TRUE),
        total_polygons = sum(.$total_polygons[.$BioNetTier %in% c("Tier 1","Tier 2","Tier 3")], na.rm = TRUE),
        percent_polygons = sum(.$percent_polygons[.$BioNetTier %in% c("Tier 1","Tier 2","Tier 3")], na.rm = TRUE)
      )
    ) %>%

    # Fill in missing tiers with zeros
    complete(BioNetTier = all_tiers, fill = list(
      num_occurrences = 0,
      percent_occurrences = 0,
      num_polygons = 0,
      total_polygons = NA_real_,
      percent_polygons = 0
    )) %>%

    # Create text columns with "none" for zeros, words for <10, and <0.1% for tiny percentages
    mutate(
      text_counts = ifelse(num_occurrences == 0, "none",
                           paste0(num_to_word(num_occurrences),  # Use num_to_word here
                                  " (", ifelse(round(percent_occurrences,1)==0,
                                               "<0.1%",
                                               paste0(formatC(percent_occurrences,
                                                              format="f", digits=1),
                                                      "%")), ")")),
      text_polygons = ifelse(num_polygons == 0 | is.na(num_polygons), "none",
                             paste0(num_to_word(num_polygons),  # Use num_to_word here
                                    " (", ifelse(round(percent_polygons,1)==0,
                                                 "<0.1%",
                                                 paste0(formatC(percent_polygons,
                                                                format="f", digits=1),
                                                        "%")), ")"))
    )

  # Create named vectors
  tier_count_texts <- deframe(tier_summary %>%
                                select(BioNetTier, text_counts))
  tier_polygon_texts <- deframe(tier_summary %>%
                                  select(BioNetTier, text_polygons))

  ### Create 5c Evidence Paragraphs
  n_bionet_tiers <- n_distinct(BioNet_Output$maryland_points_with_bionet_tiers$BioNetTier) - 1

  # Also create numeric counts for ensuring verb agreement
  tier_counts_numeric <- deframe(tier_summary %>% select(BioNetTier, num_occurrences))
  tier_polygons_numeric <- deframe(tier_summary %>% select(BioNetTier, num_polygons))

### Create 5c Evidence Paragraphs ----
 n_bionet_tiers <- n_distinct(BioNet_Output$maryland_points_with_bionet_tiers$BioNetTier) - 1

  Q5c.evidence <- as_paragraph(
    as_i(as.character(species_abb)),
    " occurrences ",
    verb_agreement(n_bionet_tiers, "was", "were"),
    " documented in ",
    as_chunk(num_to_word(n_bionet_tiers)),
    " of the five BioNet tiers (Figure 1). Of all ",
    as_chunk(n_occurrences_total_5c),
    " ",
    as_i(as.character(species_abb)),
    " occurrences documented in Maryland, ",

    as_chunk(tier_count_texts["Tier 1–3"]),
    " ",
    verb_agreement(tier_counts_numeric["Tier 1–3"], "was", "were"),
    " in lands ranked between Tier 1 and Tier 3, including ",

    as_chunk(tier_count_texts["Tier 1"]),
    " in the most critical category (Tier 1). \n\n",

    "Of all BioNet polygons ranked from Tier 1–3, ",
    as_chunk(tier_polygon_texts["Tier 1–3"]),
    " ",
    verb_agreement(tier_polygons_numeric["Tier 1–3"], "was", "were"),
    " occupied by ",
    as_i(as.character(species_abb)),
    " occurrences. Of these, ",

    as_chunk(tier_polygon_texts["Tier 1"]),
    " ",
    verb_agreement(tier_polygons_numeric["Tier 1"], "was", "were"),
    " Tier 1 polygon",
    ifelse(tier_polygons_numeric["Tier 1"] == 1, "", "s"),
    " (Figure 1).\n\n",

    "Tier 4 polygons supported ",
    as_chunk(tier_count_texts["Tier 4"]),
    " of the species occurrences.\n\n",

    "Tier 5 polygons supported ",
    as_chunk(tier_count_texts["Tier 5"]),
    " of the species occurrences.\n\n",

    "Polygons not ranked by BioNet supported ",
    as_chunk(tier_count_texts["No Tier"]),
    " of the species occurrences."
  )
 ### Create Grade ----

    Q5c.grade <- BioNet_Output$bionet_tier_counts%>%
      summarise(
        Grade = case_when(
          any(BioNetTier %in% c("Tier 1", "Tier 2", "Tier 3") & num_occurrences >= 1) ~ "A",
          any(BioNetTier %in% c("Tier 4") & num_occurrences >= 1) ~ "B",
          any(BioNetTier %in% c("Tier 5") & num_occurrences >= 1) ~ "C",
          any(BioNetTier == "No Tier") ~ "D",
          TRUE ~ NA_character_
        )
      ) %>%
      pull(Grade)


## Question 6 -----------------

## Create evidence text ----
    Q6.evidence <- as_paragraph(
    " The MCP for ",
    as_i(as.character(species_abb)),
    " encompassed ",

    as_chunk(mcp$mcp_size %>% pull(hull_percent) %>%
               as.numeric() %>% round(1)
             ),
    "% of the land area of Maryland (Figure 2)."
  )

## Create grade ----
  Q6.grade <- mcp$mcp_size %>%
    mutate(
      hull_pct = as.numeric(hull_percent),
      Grade = case_when(
        hull_pct > 30 ~ "A",
        hull_pct >= 10 ~ "B",
        hull_pct >= 1  ~ "C",
        hull_pct < 1 ~ "D",
        TRUE ~ NA_character_
      )
    ) %>%
    pull(Grade)

## Question 7 -----------------

 ## Create evidence text ----
  Q7.evidence <- as_paragraph(
    as_i(species_name),
    " occurrences ",
    verb_agreement(quad_density$df_quad_counts$occupied_quads, "was", "were"),
    " documented in ",
    as_chunk(num_to_word(quad_density$df_quad_counts$occupied_quads)),
    " of the ",
    "(",
    as_chunk(format_pct(quad_density$df_quad_counts$occupied_quads,
                        quad_density$df_quad_counts$all_quads)),
    ") US Geological Survey quadrangles in Maryland (Figure 3)."
  )

## Create grade ----
  Q7.grade <- quad_density$df_quad_counts %>%
    mutate(
      Grade = case_when(
        percent_occupied > 30 ~ "A",
        percent_occupied >= 10 ~ "B",
        percent_occupied >= 1  ~ "C",
        percent_occupied < 1 ~ "D",
        TRUE ~ NA_character_
      )
    ) %>%
    pull(Grade)

## Question 8 -----------------

## Create evidence text ----
  n_provinces <- province$df %>% dplyr::distinct(PROVINCE) %>% nrow()

  Q8.evidence <- as_paragraph(
    "The species was documented in ",
    as_chunk(num_to_word(n_provinces)),
    " of the five physiographic provinces in Maryland (Figure 4)."
  )

## Create Grade ----
  Q8.grade <- province$df %>%
    st_drop_geometry() %>%
    summarize(
      num_province = dplyr::n_distinct(PROVINCE),
      Grade = case_when(
        num_province >=4  ~ "A",
        num_province == 3 ~ "B",
        num_province == 2  ~ "C",
        num_province ==1 ~ "D",
        TRUE ~ NA_character_
      )
    ) %>%
    pull(Grade)

## Question 9a -----------------

  ## Calculations ----
  n_fia_groups <- points_fia_extract$group %>%
    dplyr::filter(FIA_Group != "Non Forest") %>%
    dplyr::distinct(FIA_Group) %>%
    nrow()

  n_groups_with_species <- points_fia_extract$group %>%
    dplyr::filter(n_plant_clusters > 0 & FIA_Group != "Non Forest") %>%
    nrow()

  n_total_occurrences <- points_fia_extract$group %>%
    dplyr::filter(FIA_Group != "Non Forest") %>%
    dplyr::pull(n_plant_clusters) %>%
    sum() %>%
    as.integer()

## Create evidence text ----
  Q9a.evidence <- as_paragraph(
    "Maryland supports ",
    as_chunk(num_to_word(n_fia_groups)),
    " FIA forest type",
    ifelse(n_fia_groups == 1, "", "s"),
    " at the group level. Of these forest type groups, ",
    as_chunk(num_to_word(n_groups_with_species)),
    " supported ",
    as_chunk(num_to_word(n_total_occurrences)),
    " occurrence",
    ifelse(n_total_occurrences == 1, "", "s"),
    " of ",
    as_i(as.character(species_abb)),
    " (Table 2).",
    "\n\nThe remaining occurrences were in areas coded as non-forested by the FIA program. These could include small patches of forest that are not recognized at the scale of the national inventory."
  )

  Q9a.grade <- points_fia_extract$group %>%
    dplyr::filter(FIA_Group !="Non Forest" & n_plant_clusters>0) %>%
    summarize(
      num_FIA_group = n_distinct(FIA_Group),
      Grade = case_when(
        num_FIA_group >= 4  ~ "A",
        num_FIA_group == 3 ~ "B",
        num_FIA_group == 2  ~ "C",
        num_FIA_group == 1 ~ "D",
        TRUE ~ NA_character_
      )
    ) %>%
    pull(Grade)


## Question 9b -----------------

  ## Calculations ----
  n_nlcd_types <- points_nlcd_extract$df %>% nrow()
  n_occurrences_nlcd <- points_nlcd_extract$df %>%
    dplyr::summarize(n = sum(n_plant_clusters)) %>%
    pull(n)
  n_types_with_species <- points_nlcd_extract$df %>%
    dplyr::filter(n_plant_clusters > 0) %>%
    summarize(n = n_distinct(types)) %>%
    pull(n)
  min_patches <- points_nlcd_extract$df %>%
    dplyr::filter(n_patches_with_plant > 0) %>%
    dplyr::summarise(min_val = min(n_patches_with_plant, na.rm = TRUE)) %>%
    dplyr::pull(min_val)
  max_patches <- points_nlcd_extract$df %>%
    dplyr::summarise(max_val = max(n_patches_with_plant, na.rm = TRUE)) %>%
    dplyr::pull(max_val)

## Create evidence text ----
  Q9b.evidence <- as_paragraph(
    "Maryland supports ",
    as_chunk(num_to_word(n_nlcd_types)),
    " terrestrial NLCD cover type",
    ifelse(n_nlcd_types == 1, "", "s"),
    ". A total of ",
    as_chunk(n_occurrences_nlcd),
    " occurrence",
    ifelse(n_occurrences_nlcd == 1, "", "s"),
    " of ",
    as_i(as.character(species_abb)),
    " ",
    verb_agreement(n_occurrences_nlcd, "was", "were"),
    " found in ",
    as_chunk(num_to_word(n_types_with_species)),
    " of those types (Table 3).",
    "\n\nThe number of patches within each NLCD type that had evidence of invasion by ",
    as_i(as.character(species_abb)),
    " ranged between ",
    as_chunk(num_to_word(min_patches)),
    " and ",
    as_chunk(num_to_word(max_patches)),
    " (Table 3) and the patches with occurrences accounted for ",
    as_chunk(format_pct(sum(points_nlcd_extract$df$patch_acres),
                        sum(points_nlcd_extract$df$total_type_acres))),
    " of the total area within Maryland."
  )
## Create grade ----
  Q9b.grade <- points_nlcd_extract$df %>%
    dplyr::filter(n_plant_clusters>0) %>%
    summarize(
      num_NLCD_types =  dplyr::n_distinct(types),
      Grade = case_when(
        num_NLCD_types >= 4  ~ "A",
        num_NLCD_types == 3 ~ "B",
        num_NLCD_types == 2  ~ "C",
        num_NLCD_types == 1 ~ "D",
        TRUE ~ NA_character_
      )
    ) %>%
    pull(Grade)

## Question 9 Grade-----------------

  # Take the "minimum" since A < B < C in factor ordering
  Q9.grade <- data.frame(
    Grade = pmin(Q9a.grade, Q9b.grade)
  ) %>%
    pull(Grade)

  min_quest <- case_when(
    Q9a.grade < Q9b.grade ~ "Q9a",
    Q9b.grade < Q9a.grade ~ "Q9b",
    TRUE ~ "Q9a and Q9b"
  )

  Q9.evidence <- as_paragraph(
    "Q9 was graded ", Q9.grade, " because ",
    min_quest,
    ifelse(min_quest == "Q9a and Q9b", " were", " was"),
    " graded ", Q9.grade
  )

## Question 12 -----------------

## Calculations ----
  n_bordering_states <- merged_points_sf %>%
    st_drop_geometry() %>%
    summarize(n = n_distinct(state) - 1) %>%
    pull(n)

## Create evidence text ----
  Q12.evidence <- as_paragraph(
    as_i(species_name),
    " is established in ",
    as_chunk(num_to_word(n_bordering_states)),
    " of the five jurisdictions bordering Maryland (Table 1)."
  )
## Create grade ----
    Q12.grade <- merged_points_sf %>%
      st_drop_geometry() %>%
      summarize(
        num_states =n_distinct(state)-1,
        Grade = case_when(
          num_states >= 4  ~ "A",
          num_states >= 2 ~ "B",
          num_states == 1  ~ "C",
          num_states < 1 ~ "D",
          TRUE ~ NA_character_
        )
      ) %>%
      pull(Grade)

 return(list(
   Q5c.evidence = Q5c.evidence,
   Q5c.grade = Q5c.grade,
   Q6.evidence = Q6.evidence,
   Q6.grade = Q6.grade,
   Q7.evidence = Q7.evidence,
   Q7.grade = Q7.grade,
   Q8.evidence = Q8.evidence,
   Q8.grade = Q8.grade,
   Q9a.evidence = Q9a.evidence,
   Q9a.grade = Q9a.grade,
   Q9b.evidence = Q9b.evidence,
   Q9b.grade = Q9b.grade,
   Q9.grade = Q9.grade,
   Q9.evidence = Q9.evidence,
   Q12.evidence = Q12.evidence,
   Q12.grade = Q12.grade))
}
