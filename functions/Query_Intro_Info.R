## This script includes three functions - one queries Trefle to get synonyms and native status for each species, one queries gbif to get country and state level record counts, and one queries iNat for state level record counts.

## These results are saved to an rData file for use in the text and two maps in the introduction section of the IPAC protocol.  It can be loaded as follows:
##  load(file = paste0("./combined_location_data/intro_info_list_",
#                    file_safe_name, ".RData"))

if (!require("pacman")) install.packages("pacman")
pacman::p_load(here,
               magrittr,
               tidyverse,
               tidyr,  # Added for replace_na
               httr,
               jsonlite,
               sf,
               spData,
               countrycode,
               update = FALSE
)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## define get_species_info() ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Purpose: query Trefle for syntax and native range
## Usage: this function is called in query_distribution_and_native_range() that is defined below

get_species_info <- function(species_name, token) {
  base_url <- "https://trefle.io/api/v1"

  # Step 1: Search for the species
  search_url <- paste0(base_url, "/species/search")

  search_response <- GET(
    search_url,
    query = list(
      token = token,
      q = species_name
    )
  )

  if (status_code(search_response) != 200) {
    stop("Search failed: ", content(search_response, "text"))
  }

  search_data <- content(search_response, "parsed")

  if (length(search_data$data) == 0) {
    stop("No species found for: ", species_name)
  }

  species_id <- search_data$data[[1]]$id
  scientific_name <- search_data$data[[1]]$scientific_name

  cat("Retrieved information for", scientific_name, "in Trefle \n")


  # Step 2: Get detailed information
  detail_url <- paste0(base_url, "/species/", species_id)

  detail_response <- GET(
    detail_url,
    query = list(token = token)
  )

  if (status_code(detail_response) != 200) {
    stop("Trefle query failed: ", content(detail_response, "text"))
  }

  detail_data <- content(detail_response, "parsed")

## Extract and process synonyms

    format_name <- function(name, author) {
      # Detect var./ssp./subsp. and split off
      if (str_detect(name, " var\\. | ssp\\. | subsp\\. ")) {
        # Split at the rank keyword
        parts <- str_split(name, " (var\\.|ssp\\.|subsp\\.) ", simplify = TRUE)
        genus_species <- parts[1]              # "Genista scoparia"
        rank_and_epithet <- str_trim(parts[2]) # "vulgaris" (after "var." or "ssp.")

        # Identify the rank keyword itself
        rank_keyword <- str_extract(name, "var\\.|ssp\\.|subsp\\.")

        paste0("*", genus_species, "* ", rank_keyword, " *", rank_and_epithet, "* ", author)

      } else {
        paste0("*", name, "* ", author)
      }
    }

    raw_synonyms <- detail_data$data$synonyms

    synonyms <- if (is.null(raw_synonyms) || length(raw_synonyms) == 0) {
      "None."
    } else {
      formatted <- purrr::map_chr(raw_synonyms, function(x) {
        if (is.list(x)) {
          format_name(
            x$name,
            x$author %||% ""
          )
        } else {
          # atomic character fallback
          paste0("*", as.character(x), "*")
        }
      })

      if (length(formatted) == 0 || all(is.na(formatted))) {
        "None."
      } else {
        formatted
      }
    }

  # Extract distribution
  distributions <- detail_data$data$distribution

  return(list(
    species_name = scientific_name,
    species_id = species_id,
    synonyms = synonyms,
    distribution = distributions
  ))
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## define check_gbif_occurrences() ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Purpose: get GBIF counts by country and state
## Usage: this function is called in query_distribution_and_native_range() that is defined below.

check_gbif_occurrences <- function(species_name, exclude_cultivated = TRUE) {

  cat("\nQuerying GBIF for global and state record counts...\n")

  base_params <- list(
    scientificName = species_name,
    occurrenceStatus = "PRESENT"
  )
  ## exclude records that have too much spatial uncertainty
  issue = paste0(
    "!",
    paste(
      c(
        "COLLECTION_MATCH_NONE",
        "COLLECTION_MATCH_FUZZY",
        "INSTITUTION_MATCH_FUZZY",
        "COUNTRY_COORDINATE_MISMATCH"
      ),
      collapse = ",!"
    )
  )

  if (exclude_cultivated) {
    base_params$isInCluster <- FALSE
  }

# Helper function for retrying requests
  safe_get <- function(url, query, max_attempts = 3) {
    for (attempt in 1:max_attempts) {
      tryCatch({
        Sys.sleep(1)  # 1 second delay before each request
        response <- GET(url, query = query)
        if (status_code(response) == 200) {
          return(response)
        }
        cat("    Attempt", attempt, "failed with status", status_code(response), "\n")
      }, error = function(e) {
        cat("    Attempt", attempt, "failed:", e$message, "\n")
        if (attempt < max_attempts) {
          cat("    Retrying in 2 seconds...\n")
          Sys.sleep(2)
        }
      })
    }
    stop("Failed after ", max_attempts, " attempts")
  }

  # Get countries
  cat("  Checking countries...\n")

  country_params <- c(base_params, list(
    limit = 0,
    facet = "country",
    facetLimit = 300
  ))

  country_response <- safe_get(
    "https://api.gbif.org/v1/occurrence/search",
    query = country_params
  )

  country_results <- data.frame(
    location = character(),
    location_type = character(),
    record_count = integer(),
    stringsAsFactors = FALSE
  )

  if (status_code(country_response) == 200) {
    country_data <- content(country_response, "parsed")

    if (!is.null(country_data$facets)) {
      country_facet <- country_data$facets[[1]]$counts

      for (country in country_facet) {
        country_results <- rbind(country_results, data.frame(
          location = country$name,
          location_type = "country",
          record_count = country$count,
          stringsAsFactors = FALSE
        ))
      }
    }
  }

# Get numbers of GBIF records for US states
 cat("  Checking US states...\n")

 states_to_check <- c(setdiff(state.name, c("Alaska", "Hawaii")), "District of Columbia")

  state_results <- data.frame(
    location = character(),
    location_type = character(),
    record_count = integer(),
    stringsAsFactors = FALSE
  )


  for (state in states_to_check) {
    state_params <- c(base_params, list(
      country = "US",
      stateProvince = state,
      limit = 1
    ))

    max_tries <- 3
    for (i in 1:max_tries) {
      response <- try(
        GET("https://api.gbif.org/v1/occurrence/search",
            query = state_params,
            config = httr::config(http_version = 1.1)),
        silent = TRUE
      )
      if (!inherits(response, "try-error") && status_code(response) == 200) break
      Sys.sleep(1)  # wait before retrying
    }


    if (status_code(response) == 200) {
      data <- content(response, "parsed")
      count <- data$count

      state_results <- rbind(state_results, data.frame(
        location = state,
        location_type = "US_state",
        record_count = count,
        stringsAsFactors = FALSE
      ))
    }

    Sys.sleep(0.8)
  }

  return(list(
    countries = country_results,
    us_states = state_results
  ))
}
#++++++++++++++++++++++++++++++++++++++++
## define get_inat_state_counts()
#++++++++++++++++++++++++++++++++++++++++
## Purpose: get iNat counts by state
# Usage: this function is called in query_distribution_and_native_range() that is defined below

get_inat_state_counts <- function(species_name, states_to_check = NULL) {

message("Querying iNat for US state counts...\n" )

  ## States have to be place IDs, not state names

  place_ids <- c(
    "Alabama" = 18, "Alaska" = 2, "Arizona" = 3, "Arkansas" = 14,
    "California" = 5, "Colorado" = 6, "Connecticut" = 7,
    "Delaware" = 8, "District of Columbia" = 20, "Florida" = 9,
    "Georgia" = 10, "Hawaii" = 1, "Idaho" = 11, "Illinois" = 12,
    "Indiana" = 13, "Iowa" = 15, "Kansas" = 16, "Kentucky" = 17,
    "Louisiana" = 19, "Maine" = 21, "Maryland" = 22, "Massachusetts" = 23,
    "Michigan" = 24, "Minnesota" = 25, "Mississippi" = 26,
    "Missouri" = 27, "Montana" = 28, "Nebraska" = 29, "Nevada" = 30,
    "New Hampshire" = 31, "New Jersey" = 32, "New Mexico" = 33,
    "New York" = 34, "North Carolina" = 35, "North Dakota" = 36,
    "Ohio" = 37, "Oklahoma" = 38, "Oregon" = 39, "Pennsylvania" = 40,
    "Rhode Island" = 41, "South Carolina" = 42, "South Dakota" = 43,
    "Tennessee" = 44, "Texas" = 45, "Utah" = 46, "Vermont" = 47,
    "Virginia" = 48, "Washington" = 49, "West Virginia" = 50,
    "Wisconsin" = 51, "Wyoming" = 52
  )

  if (is.null(states_to_check)) {
    states_to_check <- names(place_ids)
  }

## 1. Get taxon ID

  ## 1. Get taxon ID with fallback for hybrids
  search_variants <- c(
    species_name,                                    # Original
    gsub(" x ", " ", species_name),                 # Remove " x "
    gsub("x", "", species_name),                    # Remove "x" entirely
    gsub(" x", "", species_name)                    # Remove " x" (no trailing space)
  )

  taxon_id <- NULL
  taxon_name_used <- NULL

  for (variant in unique(search_variants)) {
    taxon_response <- GET(
      "https://api.inaturalist.org/v1/taxa",
      query = list(q = variant, per_page = 1)
    )
    stop_for_status(taxon_response)
    taxon_data <- content(taxon_response, "parsed")

    if (length(taxon_data$results) > 0) {
      taxon_id <- taxon_data$results[[1]]$id
      taxon_name_used <- variant
      break
    }
  }

  if (is.null(taxon_id)) {
    stop("No taxon found for '", species_name, "' or any variant")
  }

  ## 2. Get counts per state
  map_dfr(states_to_check, function(st) {
    pid <- place_ids[st]
    resp <- GET(
      "https://api.inaturalist.org/v1/observations",
      query = list(
        taxon_id = taxon_id,
        place_id = pid,
        per_page = 0,
        quality_grade = "research",
        captive = "false"
      )
    )
    stop_for_status(resp)
    dat <- content(resp, "parsed")
    data.frame(state = st, record_count = dat$total_results)
  })

  }


#+++++++++++++++++++++++++++++++++++++++++++++++
## define query_distribution_and_native_range()
#+++++++++++++++++++++++++++++++++++++++++++++++
## Purpose: Run the three queries above and combine the Trefle native status with GBIF and iNat occurrences
## Usage: This function is called in the Species_Location_Query function.

query_distribution_and_native_range <- function(species_name, trefle_token) {

  # Get Trefle data for native status
  trefle_data <- get_species_info(species_name, trefle_token)

  # Get GBIF occurrence data
  gbif_data <- check_gbif_occurrences(species_name)
  inat_state_data <- get_inat_state_counts(species_name)

  # Extract Trefle native locations (both countries and US states)
  # Trefle returns $native as a simple vector/list, not $native$countries
  trefle_native_all <- unlist(trefle_data$distribution$native)
  trefle_introduced_all <- unlist(trefle_data$distribution$introduced)

  # Separate US states from other locations in Trefle
  us_states <- c(state.name, "District of Columbia")
  trefle_native_us <- base::intersect(trefle_native_all, us_states)
  trefle_native_countries <- setdiff(trefle_native_all, us_states)

  # Crosswalk between Trefle floristic and historical regions to standard modern country names.
  trefle_region_crosswalk <- list(

    # British Isles
    "Great Britain" = c("United Kingdom"),

    # Historical states
    "Czechoslovakia" = c("Czech Republic", "Slovakia"),

    "Yugoslavia" = c(
      "Slovenia", "Croatia", "Bosnia and Herzegovina",
      "Serbia", "Montenegro", "North Macedonia"
    ),

    # Baltic / Fennoscandian region
    "Baltic States" = c(
      "Estonia", "Latvia", "Lithuania", "Finland"
    ),

    # Russia (split regions)
    "Central European Rus" = c("Russia"),
    "Northwest European R" = c("Russia", "Finland"),

    # Balkans
    "NW. Balkan Pen." = c(
      "Slovenia", "Croatia", "Bosnia and Herzegovina",
      "Serbia", "Montenegro"
    ),

    # Russia (explicit floristic regions)
    "Central European Russia" = c("Russia"),

    "Northwest European Russia" = c("Russia", "Finland"),


    # Mediterranean islands / regions
    "Corse"     = "France",
    "Sardegna"  = "Italy",
    "Sicilia"   = "Italy"
  )

# Use the crosswalk above to convert any non-standard raw native regions returned by Trefle into a clean list of native country names suitable for mapping.

  trefle_native_raw <- unique(trefle_native_countries)

  # Expand regions → countries
  trefle_native_standard <- unique(unlist(
    lapply(trefle_native_raw, function(x) {
      if (x %in% names(trefle_region_crosswalk)) {
        trefle_region_crosswalk[[x]]
      } else {
        x
      }
    })
  ))


# Convert Trefle country names to ISO codes automatically to match GBIF format
  trefle_native_iso <- if (length(trefle_native_standard) > 0 && !all(is.na(trefle_native_standard))) {
    countrycode(
      trefle_native_standard,
      origin = "country.name",
      destination = "iso2c",
      warn = FALSE
    )
  } else {
    character(0)  # Return empty character vector if no data
  }

  # Remove NAs (countries that couldn't be matched)
  trefle_native_iso <- trefle_native_iso[!is.na(trefle_native_iso)]

## Uses ISO2 codes to assign native versus non-native to countries with occurrences of the species in GBIF
countries_combined <- gbif_data$countries %>%
  mutate(
    status = case_when(
      location %in% trefle_native_iso ~ "Native",
      record_count > 0 ~ "Non-native",
      TRUE ~ "Not documented"
    )
  )

  # Combine US state data from GBIF and iNat with Trefle native status
  states_combined <- gbif_data$us_states %>%
    left_join(inat_state_data, by = c("location"= "state"),
              suffix = c("_gbif", "_inat")) %>%

    mutate(
      record_count_max = pmax(record_count_gbif,
                              record_count_inat, na.rm = TRUE),
      status = case_when(
        record_count_max == 0 ~ "Not documented",
        # Check if state is in Trefle native list
        location %in% trefle_native_us ~ "Native",
        # Has records but not native = non-native
        TRUE ~ "Non-native"
      )
    )

 intro_info_list <- (list(
    species_name = trefle_data$species_name,
    synonyms = trefle_data$synonyms,
    countries = countries_combined,
    us_states = states_combined,
    trefle_native_countries = trefle_native_countries,
    trefle_native_us = trefle_native_us
  ))

# Create file safe version of the species for file naming
file_safe_name <- str_replace_all(species_name, " ", "_")

save(intro_info_list,
       file = paste0("./combined_location_data/intro_info_list_",
                     file_safe_name, ".RData"))

 return(intro_info_list)
}

# Complete workflow:
# ==================
# # Set your Trefle token
# trefle_token <- "usr-HStZpCPORUKz7eKzpv4hv2YDcw71uCBn5zhKBhqv9X4"
#
# # Combine Trefle + GBIF data
# combined_data <- query_distribution_and_native_range("Wisteria floribunda", trefle_token)

