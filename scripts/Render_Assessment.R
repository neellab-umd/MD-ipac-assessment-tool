## This function gets all the name information for a species that is necessary for the quarto script, and then renders that quarto script to yield a word document.

render_assessment <- function(species_name, mode = "generate", assessor_file = NULL) {
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(here,
                 magrittr,
                 tidyverse,
                 officer,
                 update = FALSE
  )

  ## Read in species name crosswalk table
  all_species_info <- readr::read_csv(here("core_assets", "List of species for MD IPSSA.csv"), show_col_types = FALSE)

  ## Check if this is a genus-level analysis
  genus_species <- all_species_info %>%
    filter(str_detect(species, paste0("^", species_name, " ")))

  genus_check <- genus_species %>%
    pull(genus_only) %>%
    any()

  if (genus_check) {
    # Genus-level analysis
    species_to_use <- genus_species %>%
      slice(1)

    params_list <- list(
      species = species_name,
      species_abb = species_name,
      authority = "",
      family = species_to_use$family,
      common_name = tolower(species_name),
      genus_only = TRUE,
      mode = mode,
      assessor_file = assessor_file
    )

    # For genus-level: combined location files, multiple intro files
    file_safe_name <- str_replace_all(species_name, " ", "_")
    points_file <- paste0(".\\combined_location_data\\Points_sf_", file_safe_name, ".RData")

    # Get all species in this genus and create intro file list
    intro_files <- genus_species %>%
      mutate(file_safe = str_replace_all(species, " ", "_")) %>%
      mutate(intro_path = paste0(".\\combined_location_data\\intro_info_list_", file_safe, ".RData")) %>%
      pull(intro_path)

    # Check that location file exists
    if (!file.exists(points_file)) {
      stop(paste0("\n",
                  "Missing location data file for ", species_name, ":\n",
                  "  ", points_file, "\n",
                  "Run the species location query and try again.\n"),
           call. = FALSE
      )
    }

    # Check that all intro files exist
    missing_intro <- intro_files[!file.exists(intro_files)]
    if (length(missing_intro) > 0) {
      stop(paste0("\n",
                  "Missing intro info files for ", species_name, ":\n",
                  paste0("  ", missing_intro, collapse = "\n"), "\n",
                  "Run the species location query and try again.\n"),
           call. = FALSE
      )
    }

    # Pass the list of intro files to the quarto document
    params_list$intro_files <- intro_files

  } else {
    # Single species analysis
    species_to_use <- all_species_info %>%
      filter(species == species_name)

    params_list <- list(
      species = species_name,
      species_abb = species_to_use$species_abb,
      authority = species_to_use$authority,
      family = species_to_use$family,
      common_name = species_to_use$common_name,
      genus_only = FALSE,
      mode = mode,
      assessor_file = assessor_file
    )

    # For single species: one file for each
    file_safe_name <- str_replace_all(species_name, " ", "_")
    points_file <- paste0(".\\combined_location_data\\Points_sf_", file_safe_name, ".RData")
    intro_file <- paste0(".\\combined_location_data\\intro_info_list_", file_safe_name, ".RData")

    # Check that both files exist
    if (!file.exists(points_file) || !file.exists(intro_file)) {
      stop(paste0("\n",
                  "You do not have location data for ", species_name, ".\n",
                  "Expected files:\n",
                  "  ", points_file, "\n",
                  "  ", intro_file, "\n",
                  "Run the species location query and try again.\n"),
           call. = FALSE
      )
    }

    # Pass the intro file to the quarto document (as single string, not vector)
    params_list$intro_files <- intro_file
  }

  # If generate mode, render the draft assessor template
  if (mode == "generate") {
    output_filename <- paste0(species_name, "_IPAC_Assessor_Draft.docx")
    output_subdir <- "Assessments/Drafts"
  } else {
    output_filename <- paste0(species_name, "_IPAC_Final_Assessment.docx")
    output_subdir <- "Assessments/Finals"
  }

# Clean up desktop.ini files created by Google Drive
  desktop_ini <- list.files(pattern = "^desktop\\.ini$",
                            recursive = TRUE,
                            all.files = TRUE,
                            full.names = TRUE)
  if (length(desktop_ini) > 0) {
    suppressWarnings(file.remove(desktop_ini))
  }


  quarto::quarto_render(
    "IPAC_Assessment_Script_January_2026.qmd",
    output_file = output_filename,
    execute_params = params_list
  )


# Create the appropriate subdirectory and move the file
dir.create(here::here(output_subdir), showWarnings = FALSE, recursive = TRUE)
invisible(file.rename(
  from = here::here(output_filename),
  to = here::here(output_subdir, output_filename)
))

}
#render_assessment(species_name) # default is mode = generate

# Generate mode - creates document for assessor to complete
#render_assessment(species_name, mode = "generate")
# Output:
#   - <species_name>_IPAC_Assessor_Draft.docx


# # Merge mode - creates full document with assessor input
# render_assessment(species_name,
#                          mode = "merge",
#                          assessor_file = paste0("./Assessor_Completed_Ready_to_Merge/",
#                                                 species_name,
#                                                 "_IPAC_Assessor_Draft.docx"))
# Output:
#   - <species_name>_IPAC_Final_Assessment.docx (full with merged data)

#generate_quarto_document("Wisteria")
