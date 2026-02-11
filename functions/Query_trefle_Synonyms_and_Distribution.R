# Function to query Trefle API for species information
library(httr)
library(jsonlite)
library(tidyverse)
library(purrr)

# Function to query Trefle API for species information
get_species_info <- function(species_name, token) {

  # Base URL
  base_url <- "https://trefle.io/api/v1"

  # Step 1: Search for the species to get its ID
  search_url <- paste0(base_url, "/species/search")

  search_response <- GET(
    search_url,
    query = list(
      token = token,
      q = species_name
    )
  )

  # Check if search was successful
  if (status_code(search_response) != 200) {
    stop("Search failed: ", content(search_response, "text"))
  }

  search_data <- content(search_response, "parsed")

  # Get the first match (most relevant)
  if (length(search_data$data) == 0) {
    stop("No species found for: ", species_name)
  }

  species_id <- search_data$data[[1]]$id
  scientific_name <- search_data$data[[1]]$scientific_name

  cat("Found species:", scientific_name, "\n")
  cat("Species ID:", species_id, "\n\n")

  # Step 2: Get detailed information including synonyms
  detail_url <- paste0(base_url, "/species/", species_id)

  detail_response <- GET(
    detail_url,
    query = list(token = token)
  )

  if (status_code(detail_response) != 200) {
    stop("Detail query failed: ", content(detail_response, "text"))
  }

  detail_data <- content(detail_response, "parsed")

  # Extract synonyms
  synonyms <- detail_data$data$synonyms

  cat("Synonyms:\n")
  if (length(synonyms) > 0) {
    for (i in seq_along(synonyms)) {
      cat("  -", synonyms[[i]]$name, "\n")
    }
  } else {
    cat("  No synonyms found\n")
  }

  cat("\n")

  # Extract distribution (WGSRPD codes)
  # Note: These are standardized botanical distribution codes, not traditional
  # country names or US state abbreviations
  distributions <- detail_data$data$distribution

  cat("Distribution (WGSRPD codes):\n")
  if (!is.null(distributions$native) && length(distributions$native) > 0) {
    cat("  Native:", paste(distributions$native, collapse = ", "), "\n")
  }
  if (!is.null(distributions$introduced) && length(distributions$introduced) > 0) {
    cat("  Introduced:", paste(distributions$introduced, collapse = ", "), "\n")
  }
  if (is.null(distributions$native) && is.null(distributions$introduced)) {
    cat("  No distribution data available\n")
  }

  cat("\nNote: Distribution codes follow the World Geographical Scheme for Recording Plant Distributions (WGSRPD)\n")

  # Return structured data
  return(list(
    species_name = scientific_name,
    species_id = species_id,
    synonyms = synonyms,
    distribution = distributions
  ))
}

# Your access token
token <- "usr-HStZpCPORUKz7eKzpv4hv2YDcw71uCBn5zhKBhqv9X4"

# Query for Cytisus scoparius
result <- get_species_info("Cytisus scoparius", token)

# You can easily query other species by changing the species name:
# result2 <- get_species_info("Solanum lycopersicum", token)
# result3 <- get_species_info("Rosa canina", token)


synonym_df <- map_df(result$synonyms, ~ data.frame(
  name = .x$name,
  author = .x$author
))
# You can easily query other species by changing the species name:
# result2 <- get_species_info("Solanum lycopersicum", token)
# result3 <- get_species_info("Rosa canina", token)