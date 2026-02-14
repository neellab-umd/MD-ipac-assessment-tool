# Maryland Invasive Plant Status Assessment Protocol Tool

## Table of Contents

-   [Overview](#overview)
-   [Features](#features)
-   [Prerequisites](#prerequisites)
    -   [Software Requirements](#software-requirements)
    -   [Required Data Files](#required-data-files)
-   [Installation](#installation)
-   [Project Structure](#project-structure)
-   [Usage](#usage)
    -   [Graphical Interface](#graphical-interface)
    -   [Command Line](#command-line)
-   [Known Limitations and Post-Processing](#limitations)
    -   [Document Corruption Warning](#corruption)
    -   [Landscape Orientation for Appendix](#landscape-orientation)
    -   [Italics in Scientific Names](#italics)
-   [Data Sources](#data-sources)
-   [Troubleshooting](#troubleshooting)
-   [Contact](#contact)

## Overview

This project automates the Maryland Invasive Plant Status Assessment Protocol by

1. Querying biodiversity databases for georeferenced species records, world wide distributions, native range, and synonyms.
2. Performing spatial analyses on occurrence data.
3. Generating assessments with automated scoring.
4. Providing a template in which assessors manually add evidence from the literature and assign scores.
5. Integrating manual evidence and scores with the automated spatial analysis to assign final I-Ranks in a final assessment document.

## Features

- **Multi-source data integration**: Queries GBIF, iNaturalist, USDA FIA plots, EDDMapS, and Trefle
- **Spatial analysis**: Automated geographic assessment of species distribution
- **Duplicate detection**: Filters and collapses redundant records
- **Report generation**: Creates draft and final assessment documents
- **Evidence integration**: Merges assessor literature review with spatial data
- **I-Rank assignment**: Calculates final invasiveness rankings
- **User-friendly interface**: Graphical application for non-programmers

## Prerequisites

### Required Software

**R version**: 4.5.2 or higher

**Required R packages**:
- `conflicted` - set preferred namespace for functions\
- `magrittr` - `tidyverse`- data wrangling\
- `officer` - `docxtractr` - Word document conversion and extraction\
- `quarto` - report generation\
- `english`, `extrafont`, `flextable`, `gt` - document formatting\
- `ggrepel`, `ggspatial`, `ggtext`, `ggthemes`, `gridE\tra`, `scales`, `viridis`, `paletteer`, `patchwork` - plotting and visualization\
- `lwgeom`, `sf`, `terra`, `raster`, `tidyterra` - spatial data handling\
- `tigris` - census county FIPS codes\
- `spData` - country boundaries for mapping\
- `rgbif`, `rinat` - biodiversity database access\
- `rFIA` - used to get FIA records from the USDA data mart\
- `dbscan` - cluster records into occurrences by spatial proximity\
- `shiny`, `shinyjs` - interactive application interface\
- `glue`, `here`, `showtext` - utilities


Run this block of code at the console prompt to install required R packages from the Comprehensive R Archive Network (CRAN):

```r
if (!require("conflicted")) install.packages("conflicted")
library(conflicted)

conflicts_prefer(
  dplyr::filter,
  dplyr::select,
  terra::extract, 
  terra::area,
  raster::trim,
  flextable::compose,
  flextable::align)

if (!require("pacman")) install.packages("pacman")
pacman::p_load(dbscan, english, extrafont, docxtractr, flextable,
               ggrepel, ggspatial, ggtext, ggthemes, glue, gridExtra,
               gt, here, lwgeom, magrittr, officer, paletteer, patchwork,
               purrr, quarto, raster, scales, sf, shiny, shinyjs, spData,
               showtext, terra, tidyterra, tidyverse, rgbif, rinat, shinyjs, shiny,
               tigris, viridis, update = FALSE)
```

The rFIA package is not on CRAN and needs to be downloaded and installed from github:

```r
  if (!require("rFIA")) remotes::install_github("hunter-stanke/rFIA") 
```

### Required Font

The Windows Aptos font must be installed and available.


## Installation

**Clone this repository:**

```bash
git clone https://github.com/yourusername/md-invasive-plant-protocol.git
cd md-invasive-plant-protocol
```

**Download folders and files from Zenodo:**

Spatial data files and folders that are too large for github need to be downloaded from the [Zenodo repository](https://zenodo.org/records/18641201). The zip file in this repository needs to be unzipped into the IPAC assessment project such that all folders are just below the parent directory, not within the folder IPAC_data_files.  See the project organization below.

**Project Organization:**

This repository combined with the files on Zenodo contain the scripts and required files to run species location queries ant to generate IPAC Assessments.  All scripts run from within the RProject `IPAC_Assessment_Tool.Rproj`. The project must be opened in RStudio once per R session (File → Open Project → IPAC_Assessment_Tool.Rproj) prior to running queries or assessments. 

The project directory structure must remain exactly as follows:

```
project/
├── README.md
├── IPAC_Assessment_Script_January_2026.qmd # Does spatial analyses and Generates assessements
├── Assessment_Interface.R              # Shiny application
├── Assessments/                        # Generated assessments
├── Assessor_Completed_Ready_to_Merge/  # Assessor input files
├── combined_location_data/             # Query results (one set per species)
├── core_assets/                        # Configuration files
│   ├── List of species for MD IPSSA.csv  # REQUIRED: Species info
│   ├── Assessment Questions Ranking Separate.csv
│   └── [other templates and lookup tables]
├── EDDMaps_location_data/  # EDDMapS files - not used directly in the assessment
├── GBIF_location_data/ # GBIF files - not used directly in the assessment
├── FIA_Data/ # FIA files - used in location query
├── functions/                          # Analysis functions
├── scripts/
│   ├── Query_Species_Location.R  # Queries all repositories for each species
│   └── Render_Assessment.R # Renders draft and final assessments
└── spatial_data/                       # REQUIRED: GIS layers and rasters
    ├── State_Boundaries/ #Used in queries and map
    ├── BioNet/ #Polygons ranked for biodiversity conservation (Maryland DNR 2018)
    ├── Maryland_Geology_-_Physiographic_Provinces/  #Physiographic provinces (Maryland DNR 2002)
    ├── Maryland_USGS_Topo_Grids_-_Quad_Grid/  #USGS quad grid (Maryland DNR 1998)
    ├── usa_land_cover_2020v2_30m/ #NLCD raster (CEC 2024)
    └── Forest_Type_Tiffs/#FIA forest type rasters (Burrill et al. 2024)
```

See [Data Sources](#data-sources) section for full citations.

#### Species List

Before running queries or assessments, each species must be listed in the following spreadsheet:

```r
./core_assets/List of species for MD IPSSA.csv
```

**Required fields:**

| Field | Description | Example |
|-------|-------------|---------|
| `species` | Scientific name | *Phyllostachys aurea* |
| `species_abb` | Abbreviation | P. aurea |
| `authority` | Taxonomic authority | Carriere ex Riviere & C.Riviere |
| `year` | Publication year | 1878 |
| `family` | Plant family | Poaceae |
| `common_name` | Common name(s) | Golden bamboo |
| `Tier_2_year` | Tier 2 listing year (if applicable) | 2018 |
| `genus_only` | Multi-species flag | TRUE/NA (see below)|
| `EddMapsID` | EDDMapS identifier | [see below] |

**Obtaining EDDMapS IDs:**

EDDMapS requires an ID for API searches. They have been added for all plants in the spreadsheet. To obtain an ID for plants you are adding to the spreadsheet

1. Navigate to: `https://api.bugwoodcloud.org/v2/subject?search=Genus species`
2. Replace `Genus species` with your target (e.g., `Phyllostachys aurea`)
3. Copy the `subjectid` value from the JSON response (make sure you are looking at the correct taxon - often there are very similar names; you will usually want the record with the full description).
4. Enter the subjectid value in the `EddMapsID` column

**Multi-species Assessments (genus_only):**

When multiple congeneric species cannot be reliably distinguished, they are assessed together:\
1. Create separate rows for each species\
2. Set `genus_only = TRUE` for all species in the group\

Each species is queried separately but the query results are merged into files named with the genus. Species-specific distributions, synonyms, and ranges are saved separately.


## Usage

You can run queries and assessments using a graphical user interface or from the command line.

### Using the Graphical Interface

1. Open the RProject

The RProject `IPAC_Assessment_Tool.Rproj` file is in the IPAC_Assessment_Protocol folder. Open the RProject in RStudio: File → Open Project → `IPAC_Assessment_Tool.Rproj`

2. Launch the user interface application by running this line at the R console prompt
```r
shiny::runApp("Assessment_Interface.R")
```

**Workflow Within the Interface:**

1. **Select Species**
   - Single species: Choose a species from the dropdown menu
   - Multi-species: Select a genus, then check which species to include
   - Click "Confirm Selection"

2. **Query Species Locations** (Left panel)
   - Status indicators show if data exists or if a query is required
   - Click "Query Species Data" if needed (5-10 minutes)
   - Results saved to `./combined_location_data/`

3. **Generate Draft** (Right panel)
   - Select "Draft" mode
   - Yellow warning appears if draft already exists
   - Click "Generate Assessment" (2-5 minutes)
   - Output: `./Assessments/{species_name}_IPAC_Assessor_Draft.docx`

4. **Complete Offline Review** 
   - Conduct literature review
   - Document evidence and assign scores
   - Save the file as follows: `./Assessor_Completed_Ready_to_Merge/{species_name}_IPAC_Assessor_Draft.docx`

5. **Generate Final Assessment** (Return to app when ready, may be months later when the assessor is done)
   - Select same species and confirm
   - (Optional) Re-query data for current records
   - Select "Final" mode
   - Verify green "Ready" status
   - Click "Generate Assessment" (2-5 minutes)
   - Output: `./Assessments/{species_name}_IPAC_Final_Assessment.docx`

### Using the Command Line

Instructions for running species queries and assessments are below.  Before you run either you need to open the RProject in RStudio once per R session: File → Open Project → `IPAC_Assessment_Tool.Rproj`

**Query species locations:**

You only need to run the query once at the before generating a draft assessment and once just before generating a final assessment.  The files are saved to the project; once they are present you can generate an assessment any number of times.


You need to source the query function once per R session
```r
source("scripts/Query_Species_Location.R")

```

Single Species:
Replace the name Berberis thunbergii with the chosen species.

```r
species_location_query("Berberis thunbergii")
```

Multi-species:

Give the name of each congeneric species you want to include in the same query. Note the c() wrapper around the names - you must include it.

```
species_location_query(c("Wisteria floribunda", "Wisteria x formosa", "Wisteria sinensis"))
```

**Generate a Draft Assessment:**

You need to source the function that generates the assessments once for each R session. The same function generates the draft and final assessments:

```r
source("scripts/Render_Assessment.R")

```

Single Species:
Replace the name Berberis thunbergii with the chosen species.

```r
render_assessment("Berberis thunbergii", mode = "generate")
```

Multiple Species:
Replace the name Wisteria with the chosen genus.

```
render_assessment("Wisteria", mode = "generate")
```

**Generate a Final Assessment (after assessor review):**

Remember to update your query before running a final assessment.

You need to source the function that generates the assessments once for each R session. The same function generates the draft and final assessments:

```r
source("scripts/Render_Assessment.R")

```

To generate a final assessment that merges assessor input with an updated spatial analysis for your chosen species, you must have placed the draft word document with the assessor input in the folder `Assessor_Completed_Ready_to_Merge`. 

Make sure you replace the name "Berberis thunbergii" with your chosen species both places the name occurs in the following command:

```r
render_assessment("Berberis thunbergii",
                 mode = "merge",
                 assessor_file = "./Assessor_Completed_Ready_to_Merge/Berberis thunbergii_IPAC_Assessor_Draft.docx")
```

## Known Limitations & Post-Processing

The following manual steps are required after generating assessments due to Quarto/Pandoc/Word limitations:

### Document Corruption Warning (One-time per document)

**Issue:** Word displays a corruption error when first opening generated documents.

**Cause:** [Known Quarto issue with table captions](https://github.com/quarto-dev/quarto-cli/issues/10587). The document is not actually corrupted.

**Fix:** 
- Click "Yes" to proceed opening the file
- Immediately save as the original file name (Ctrl+S)
- Document will open normally thereafter

### Landscape Orientation for Appendix

**Issue:** Appendix table is in portrait orientation.

**Cause:** Quarto/Pandoc cannot set landscape orientation for the final section of Word documents.

**Required steps:**
1. Click at the end of the document (after appendix table)
2. Layout → Breaks → Next Page
3. With cursor in appendix section: Layout → Orientation → Landscape
4. Save

*This must be done for each final assessment.*


## Troubleshooting

**API timeout or rate limiting (Error 429)**
- Wait 1-2 minutes and retry

**Trefle.io timeout**
- Disconnect VPN (corporate VPNs may block API requests)
- Enable split tunneling in VPN may be required

**Missing EDDMapS ID error**
- Verify species has a valid `EddMapsID` in the `List of species for MD IPSSA.csv`.   

**Shiny app hanging during query**
- Check the R console (not not app window) for progress
- EDDMapS can take time for species with many records
- If the query hangs indefinitely (say, >10 minutes) with no console activity, click Stop in RStudio to stop the process.  A bad EDDMapsID is often the culprit.

**"Cannot generate draft" in Shiny**
- Query data missing - click "Query Species Data" first

**GBIF authentication (if needed)**
```r
options(gbif_user = "your_username")
options(gbif_email = "your_email")
options(gbif_pwd = "your_password")
```
## Data Sources

1. **[GBIF](https://www.gbif.org)** - Global occurrence records (API: `rgbif`)
2. **[iNaturalist](https://www.inaturalist.org/)** - Community science observations (API: `rinat`)
3. **[USDA FIA](https://research.fs.usda.gov/products/dataandtools/fia-datamart)** - Forest plot data
4. **[EDDMapS](https://www.eddmaps.org)** - Invasive species mapping (API: Bugwood Cloud)
5. **[Trefle](https://trefle.io/)** - Native range and synonyms

**Spatial data citations:**

- Burrill, E.A. et al. 2024. The Forest Inventory and Analysis Database (version 9.4). USDA Forest Service. Available: https://research.fs.usda.gov/understory/forest-inventory-and-analysis-database-user-guide-nfi
- Maryland DNR. 1998. Maryland USGS Topo Grids. Available: https://geodata.md.gov/imap/rest/services/Location/MD_USGSTopoGrids/FeatureServer/0
- Maryland DNR. 2018. Maryland Biodiversity Conservation Network - BioNet. Available: https://data.imap.maryland.gov
- Maryland DNR. 2002. Maryland Geology - Physiographic Provinces. Available: https://geodata.md.gov/imap/rest/services/Geoscientific/MD_Geology/MapServer/1
- Commission for Environmental Cooperation. 2024. Land Cover of North America at 30 meters (Edition 2.0). Available: http://www.cec.org/nalcms

## Contact

**Project Maintainer:** Maile Neel  
**Email:** mneel@umd.edu  
**Institution:** University of Maryland

**For protocol questions:**  
Office of Plant Protection and Weed Management  
Maryland Department of Agriculture  
50 Harry S. Truman Pkwy  
Annapolis, Maryland 21401  
Phone: (410) 841-5870
