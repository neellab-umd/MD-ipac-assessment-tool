## Function to pull data from EDDMaps for a particular species across Maryland and surrounding states using county fips numbers. The function includes 3 steps: 1) the API Call, 2) mapping the API call over each county, 3) Cleaning and filtering the data.

##  Called in the Species_location_Query.R script

EDDMapS_Query <- function(species_name) {

## EDDMapS searches are doing by county-level fips codes. You first need to get those codes for all our states from the tidycensus.
# our_states <- c("MD", "DE", "PA", "WV", "VA", "DC")
#
# # Use tidycensus to pull county level geography and FIPS codes:
# all_counties <- map_dfr(our_states, function(st) {
#   counties(state = st, cb = FALSE) %>%
#     dplyr::select(GEOID, NAME, STATEFP)  # GEOID is full 5‐digit FIPS (state + county)
# })
#
# #Extract the GEOID (character) to get the fips codes:
# fips_codes <- all_counties$GEOID

# ## save fips codes so you don't have to query them all the time.
# save(fips_codes,  file = here::here("EDDMaps_location_data",
#                                     "county_fips_codes.RData"))

## This function works for one fips code. To get all counties, it has to be mapped over fips_codes vector - the map block is below the function.

# retrieve saved fips_codes
  load(here::here("EDDMaps_location_data",
                  "county_fips_codes.RData"))

  base_url <- "https://api.bugwoodcloud.org/v2/occurrence"


  fetch_bugwood_data <- function(species_name, fips)   {

    subjectid <- read_csv(here("core_assets", "List of species for MD IPSSA.csv"),
                          show_col_types = FALSE) %>%
      filter(species == species_name) %>%
      pull(.,EddMapsID)

    if (length(subjectid) != 1) {
      stop("Expected exactly one subjectid for ", species_name,
           ", got ", length(subjectid))
    }

    params <- list(
      fips = fips,
      subjectid = subjectid,
      dateenteredstart ="1900-01-01",
      paging = tolower(as.character(TRUE)),
      pagesize = 2000,
      page = 1
    )

    all_data <- list()
    current_url <- base_url
    max_pages <- 50  # Safety limit to prevent infinite loops
    page_count <- 0

    repeat {
      page_count <- page_count + 1

      # Safety check: exit if too many pages
      if (page_count > max_pages) {
        warning("Reached maximum page limit (", max_pages, ") for FIPS ", fips)
        break
      }

      # Add timeout to prevent hanging
      resp <- tryCatch({
        GET(current_url, query = params, timeout(30))
      }, error = function(e) {
        warning("Request failed for FIPS ", fips, ": ", e$message)
        return(NULL)
      })

      # If request failed, return empty result for this FIPS
      if (is.null(resp)) {
        break
      }

      if (status_code(resp) == 200) {
        ## safe parse
        txt <- content(resp, "text", encoding = "UTF-8")

        # Check if response is empty or invalid
        if (is.null(txt) || nchar(txt) == 0) {
          message("Empty response for FIPS ", fips)
          break
        }

        data <- tryCatch({
          jsonlite::fromJSON(txt, simplifyVector = FALSE)
        }, error = function(e) {
          warning("Failed to parse JSON for FIPS ", fips, ": ", e$message)
          return(NULL)
        })

        # If parsing failed, exit
        if (is.null(data)) {
          break
        }

        # Check if there's actual data
        if (!is.null(data$data) && length(data$data) > 0) {
          all_data <- append(all_data, data$data)
        }

        # Check for next page
        next_page <- data$nextpage
        if (is.null(next_page) || next_page == "" || next_page == "null") {
          break
        } else {
          current_url <- paste0("https://api.bugwoodcloud.org", next_page)
          params <- NULL  # Don't resend params when using full nextpage URL
          Sys.sleep(2) #2 second
        }
      } else if (status_code(resp) == 404) {
        # No data for this FIPS/species combination - this is normal
        message("No records found for FIPS ", fips, " (404)")
        break
      } else {
        warning("Failed to fetch data for FIPS ", fips, ", status code: ", status_code(resp))
        break
      }
    }

    if (length(all_data) > 0) {
      message("FIPS ", fips, ": found ", length(all_data), " records")
    }

    return(all_data)
  }

##############################################
## map the fetch function over all fips codes.
##############################################
all_data <- map(fips_codes, ~
                  fetch_bugwood_data(
                    species_name = species_name,
                    fips = .x
                  )
)

########################################
##       Process the Results
########################################
## Each element in the list is a county, and each occurrence is nested within its county.
## Add county fips codes from the list you mapped over to the name slot for each one so you can map over fips codes.

names(all_data) <- fips_codes

## Generate a lookup table to go from county to state
state_lookup <- tibble(
  state_fips = c(
    "01","02","04","05","06","08","09","10","11","12","13","15","16","17","18","19",
    "20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35",
    "36","37","38","39","40","41","42","44","45","46","47","48","49","50","51","53",
    "54","55","56","60","66","69","72","78"
  ),
  state = c(
    "Alabama","Alaska","Arizona","Arkansas","California","Colorado","Connecticut",
    "Delaware","District of Columbia","Florida","Georgia","Hawaii","Idaho","Illinois",
    "Indiana","Iowa","Kansas","Kentucky","Louisiana","Maine","Maryland","Massachusetts",
    "Michigan","Minnesota","Mississippi","Missouri","Montana","Nebraska","Nevada",
    "New Hampshire","New Jersey","New Mexico","New York","North Carolina","North Dakota",
    "Ohio","Oklahoma","Oregon","Pennsylvania","Rhode Island","South Carolina",
    "South Dakota","Tennessee","Texas","Utah","Vermont","Virginia","Washington",
    "West Virginia","Wisconsin","Wyoming","American Samoa","Guam",
    "Northern Mariana Islands","Puerto Rico","Virgin Islands"
  )
)

# Map over the fips codes in the list to pull occurrences out into a dataframe.

raw_occurrences <- map_dfr(names(all_data), function(fips_code) {
  occs <- all_data[[fips_code]]
  if (length(occs) == 0) return(NULL)

  map_dfr(occs, ~tibble(
    species = .x$scientificname %||% NA_character_,
    recordbasis = .x$recordbasis %||% NA_character_,
    managementstatus = .x$managementstatus %||% NA_character_,
    reviewed = .x$reviewed %||% NA,
    verificationmethod = .x$verificationmethod %||% NA_character_,
    identificationcredibility = .x$identificationcredibility %||% NA_character_,
    coordinates = .x$coordinates %||% NA_character_,
    infestationstatus = .x$infestationstatus %||% NA_character_,
    reporter = .x$reporter %||% NA_character_,
    observationdate = .x$observationdate %||% NA_character_,
    projectname = .x$projectname %||% NA_character_,
    location = .x$location %||% NA_character_,
    fips = fips_code
  ))
})

# ADD THIS CHECK - if no records found, return empty sf object
if (is.null(raw_occurrences) || nrow(raw_occurrences) == 0) {
  message("No EDDMapS records found for ", species_name)

  # Return empty sf object with correct structure
  empty_sf <- tibble(
    species = character(0),
    year = integer(0),
    state = character(0),
    Longitude = numeric(0),
    Latitude = numeric(0),
    source = character(0)
  ) %>%
    st_as_sf(
      coords = c("Longitude", "Latitude"),
      crs = 4326,
      remove = FALSE
    )

  return(empty_sf)
}

# NOW continue with the normal processing
## Filter occurrences to ensure data quality

occurrences_clean <- raw_occurrences %>%
  mutate(
    source = "EDDMapS",
    year = if_else(
      !is.na(observationdate) & nchar(observationdate) >= 10,
      lubridate::year(lubridate::ymd(substr(observationdate, 1, 10))),
      NA_integer_
    ),
    state = str_extract(location,
                        "(?<=, )[^,]+(?=, United States)"
    ) %>%
      str_replace_all("D\\. Columbia", "District of Columbia")
  ) %>%
  filter(
    !is.na(coordinates),
    recordbasis != "Living Specimen",
    projectname != "iNaturalist API",
    !is.na(verificationmethod) | identificationcredibility == "Verified"
  )%>%
  tidyr::separate(
    coordinates,
    into = c("Latitude", "Longitude"),
    sep = ",\\s*",
    convert = TRUE
  ) %>%
  filter(!is.na(Longitude), !is.na(Latitude))


#select reduced number of fields to match query results from other databases
eddmaps_occurrences_df_sf <- occurrences_clean %>%
  select(species, year, state, Longitude, Latitude, source) %>%
  st_as_sf(
    coords = c("Longitude", "Latitude"),
    crs = 4326,
    remove = FALSE
  )

# Create file safe version of the species for file naming
file_safe_name <- str_replace_all(species_name, " ", "_")

# Create dynamic file name
file_name <- paste0("eddmaps_occurrences_", file_safe_name, "_DMV.csv")

# Write occurrence data to csv for future use.
write_csv(occurrences_clean, here::here("EDDMaps_location_data", file_name), na = "")

return(eddmaps_occurrences_df_sf)

}
