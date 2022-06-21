#' ---
#' title: SGCN Paper Analyses - Set-up
#' ---

#' # Load Packages
my_packages <- c("tidyverse", "plyr", "vegan", "stringdist",
                 "jsonlite", "httr", "natserv", "USAboundaries", ### data download/scraping
                 "sp", "sf", "raster", "rgeos", "maptools", "rgdal", "leaflet", "tmap", "maps", ### geospatial
                 "RColorBrewer", "ggpubr") ### viz
purrr::map(my_packages, require, character.only = TRUE)

#' # Load/Process Data
#' ## US states map
states <- USAboundaries::us_states() %>% 
  dplyr::filter(!(state_name %in% c("Hawaii", "Alaska", "Puerto Rico"))) # Filter to CONUS
st_crs(states) <- 4326 # Set projection as WGS84
#' US state borders
#' # Create shared border across states object
states_buffered <- st_buffer(states %>% dplyr::select(state_name, state_abbr), 1)
states_borders <- st_intersection(states_buffered, states_buffered)
states_borders <- states_borders %>%
  dplyr::filter(purrr::map(1:nrow(states_borders), function(i) !identical(states_borders$state_name[i], states_borders$state_name.1[i])) %>% unlist())
states_borders <- readRDS("data/us_state_borders.rds")
names(states_borders)[1:2] <- c("state_name1", "state_name2")
#'
#' ## SGCN species list
#' ### Original data downloaded from https://www1.usgs.gov/csas/swap/national_list.html (downloaded: 2022/02/05)
SGCN_species_CONUS <- read_csv("data/SGCN_species_CONUS.rds")
#' ### Load SGCN list matched with NatureServe Explorer API data (downloaded: 2022/02/17)
SGCN_species_matched_CONUS <- readRDS("data/SGCN_matched_CONUS.rds")

#' # Create SGCN species lists by CONUS state
#' ## All SGCN species
SGCN_species_byState <- purrr::map(states$state_name, function(st){
  print(st)
  st_abbr <- states$state_abbr[states$state_name == st]
  state_data <- SGCN_species %>% 
    dplyr::filter(grepl(st, States_2015)) 
  state_data
}) %>% set_names(states$state_name)
#' ## Matched SGCN species
SGCN_species_matched_byState <- purrr::map(states$state_name, function(st){
  print(st)
  st_abbr <- states$state_abbr[states$state_name == st]
  state_data <- SGCN_species_matched %>% 
    dplyr::filter(grepl(st, States_2015)) 
  SRank_data <- EST_data_2015 %>% 
    dplyr::filter(SUBNATION_CODE %in% st_abbr,
                  GNAME %in% state_data$scientificName)
  state_data <- state_data %>% 
    dplyr::left_join(SRank_data %>% dplyr::select(GNAME, RND, roundedSrank_2015), by = c("scientificName" = "GNAME"))
  state_data$roundedSrank_2015[grep("\\,", state_data$roundedSrank_2015)] <- state_data$roundedSrank_2015[grep("\\,", state_data$roundedSrank_2015)] %>% strsplit(",") %>% purrr::map(function(x) ifelse(sum(grepl("B", x)) > 0, x[grepl("B", x)], x[grepl("N", x)])) %>% unlist()
  state_data$roundedSrank_2015 <- gsub("B$|N$|M$", "", state_data$roundedSrank_2015)
  state_data
}) %>% set_names(states$state_name)

#' # Identify expected species by CONUS state based on 2015 G ranks
#' ## Expected = Animals, G1/G2, Full Species, Native, Standard Taxonomy plus Nonstandard that have ESA status T, E 
expected_species_byState <- readRDS("data/expected_species_byState.rds")
#' ## Combine all expected species
expected_species <- expected_species_byState %>% 
  bind_rows() %>% 
  distinct(uniqueId, .keep_all = TRUE)

#' # Set up Subgroup Analysis
#' ## Fishes
#' ### Expected
fishes_expected_data <- expected_species_byState %>% 
  purrr::map(function(state_data){
    state_data <- state_data %>% 
      dplyr::filter(grepl("Fishes - Freshwater and Anadromous Bony, Cartilaginous; Lampreys", speciesGlobal$informalTaxonomy))
    state_data <- state_data %>% 
      dplyr::mutate(jurisEndem = state_data %>% pull(uniqueId) %>% purrr::map(ns_id) %>% purrr::map(function(x) x$speciesGlobal$jurisEndem$id) %>% unlist())
    state_data
  })
#' ### SGCN
fishes_SGCN_data <- SGCN_species_matched_byState %>% 
  purrr::map(function(state_data){
    state_data <- state_data %>% 
      dplyr::filter(grepl("Fishes - Freshwater and Anadromous Bony, Cartilaginous; Lampreys", speciesGlobal$informalTaxonomy),
                    speciesGlobal$infraspecies == FALSE,
                    classificationStatus == "Standard" | (classificationStatus %in% c("Nonstandard", "Provisional") & grepl("^E|^T|^E, $|^T, $", speciesGlobal$usesaCode))
      ) 
    state_data <- state_data %>% 
      dplyr::mutate(jurisEndem = state_data %>% pull(uniqueId) %>% purrr::map(ns_id) %>% purrr::map(function(x) ifelse(!is.null(x$speciesGlobal$jurisEndem$id), x$speciesGlobal$jurisEndem$id, NA)) %>% unlist())
    state_data
  })
#' ## Terrestrial Snails
#' ### Expected
snails_expected_data <- expected_species_byState %>% 
  purrr::map(function(state_data){
    state_data <- state_data %>% 
      dplyr::filter(grepl("Terrestrial Snails", speciesGlobal$informalTaxonomy))
    state_data <- state_data %>% 
      dplyr::mutate(jurisEndem = state_data %>% pull(uniqueId) %>% purrr::map(ns_id) %>% purrr::map(function(x) ifelse(!is.null(x$speciesGlobal$jurisEndem$id), x$speciesGlobal$jurisEndem$id, NA)) %>% unlist())
    state_data
  })
#' ### SGCN
snails_SGCN_data <- SGCN_species_matched_byState %>% 
  purrr::map(function(state_data){
    state_data <- state_data %>% 
      dplyr::filter(grepl("Terrestrial Snails", speciesGlobal$informalTaxonomy),
                    speciesGlobal$infraspecies == FALSE,
                    classificationStatus == "Standard" | (classificationStatus %in% c("Nonstandard", "Provisional") & grepl("^E|^T|^E, $|^T, $", speciesGlobal$usesaCode))
      ) 
    state_data <- state_data %>% 
      dplyr::mutate(jurisEndem = state_data %>% pull(uniqueId) %>% purrr::map(ns_id) %>% purrr::map(function(x) ifelse(!is.null(x$speciesGlobal$jurisEndem$id), x$speciesGlobal$jurisEndem$id, NA)) %>% unlist())
    state_data
  })
#' ## Freshwater Mussels
#' ### Expected
mussels_expected_data <- expected_species_byState %>% 
  purrr::map(function(state_data){
    mussels_data <- state_data %>% 
      dplyr::filter(grepl("Freshwater Mussels", speciesGlobal$informalTaxonomy))
    mussels_data <- mussels_data %>% 
      dplyr::mutate(jurisEndem = mussels_data %>% pull(uniqueId) %>% purrr::map(ns_id) %>% purrr::map(function(x) x$speciesGlobal$jurisEndem$id) %>% unlist())
    mussels_data
  })
#' ### SGCN
mussels_SGCN_data <- SGCN_species_matched_byState %>% 
  purrr::map(function(state_data){
    mussels_data <- state_data %>% 
      dplyr::filter(grepl("Freshwater Mussels", speciesGlobal$informalTaxonomy),
                    speciesGlobal$infraspecies == FALSE,
                    classificationStatus == "Standard" | (classificationStatus %in% c("Nonstandard", "Provisional") & grepl("^E|^T|^E, $|^T, $", speciesGlobal$usesaCode))
      ) 
    mussels_data <- mussels_data %>% 
      dplyr::mutate(jurisEndem = mussels_data %>% pull(uniqueId) %>% purrr::map(ns_id) %>% purrr::map(function(x) ifelse(!is.null(x$speciesGlobal$jurisEndem$id), x$speciesGlobal$jurisEndem$id, NA)) %>% unlist())
    mussels_data
  })