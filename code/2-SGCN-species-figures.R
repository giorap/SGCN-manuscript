#' ---
#' title: SGCN Paper Analyses - Statistics and Figures
#' ---
#' 
#' ## Statistics
#' 
#' ### Number of unique species/subspecies names that the USGS SGCN database contains for the 49 states & DC covered in our analysis (2015 SWAP revision).
nrow(SGCN_species_CONUS)
#'
#' ### Percent of names from USGS database that we could match to the NatureServe backbone
100*(nrow(SGCN_species_matched_CONUS)/nrow(SGCN_species_CONUS))
#'
#' ### Number of taxa in Biotics for CONUS
nrow(all_species)
#' ### How many individual taxa in Biotics have rounded G ranks (other than GNR) overall?
100*(nrow(all_species %>% dplyr::filter(!is.na(roundedGRank), roundedGRank != "GNR"))/nrow(all_species))
#'
#' ### How many individual taxa in Biotics have rounded G ranks (other than GNR) among comprehensively assessed groups? 
100*(nrow(all_species %>% dplyr::filter(grepl(paste(comprehensively_assessed_groups, collapse = "|"), speciesGlobal$informalTaxonomy), !is.na(roundedGRank), roundedGRank != "GNR"))/nrow(all_species %>% dplyr::filter(grepl(paste(comprehensively_assessed_groups, collapse = "|"), speciesGlobal$informalTaxonomy))))
#'
#' ### Percent of all 2015 G1s/G2s/G3s included on SGCN list (animals)
length(intersect(SGCN_species_matched_CONUS$scientificName, all_species %>% dplyr::filter(roundedGRank %in% c("G1", "G2", "G3"), speciesGlobal$kingdom == "Animalia") %>% dplyr::pull(scientificName)))/nrow(all_species %>% dplyr::filter(roundedGRank %in% c("G1", "G2", "G3"), speciesGlobal$kingdom == "Animalia"))
#'
#' ### Percent of all 2015 G1s/G2s included on SGCN list (animals)
length(intersect(SGCN_species_matched_CONUS$scientificName, expected_species$scientificName))/nrow(expected_species)
#' ### Percent overlap between RSGCN list and state SGCN lists in the Northeast
RSGCN_list <- readxl::read_xlsx("data/RSGCN_FINAL_10012018_Marine fish still draft.xlsx", sheet = 1)
NE_states <- c("Connecticut", "District of Columbia", "Delaware", "Massachusetts", "Maryland", "Maine", "New Hampshire", "New Jersey", "New York", "Pennsylvania", "Rhode Island", "Virginia", "Vermont", "West Virginia")
NE_states_SGCN_list <- SGCN_species_byState[NE_states] %>% bind_rows()
length(intersect(RSGCN_list$`Scientific Name`, NE_states_SGCN_list$Scientific_Name))/nrow(RSGCN_list)
#'
#' ## Extract non-matching taxa
SGCN_species %>% dplyr::filter(matches_biotics == FALSE) %>% write_csv("output/SGCN_not_matched_in_Biotics-20220225.csv")


#' ## Figures
#' ### Set up output directory
outdir <- paste0("output/", format(Sys.time(), "%m%d%y"))
dir.create(outdir)

#' ### Figure 1
#' #### Top panel: Coverage of plants and selected animal groups in Species of Greatest Conservation Concern (SGCN) lists in 2015 US state wildlife action plans
#' ##### Create output object
coverage_table <- data.frame(taxon = c("Plants", "Mammals", "Reptiles", "Birds", "Amphibians", "Freshwater Fishes", "Bees", "Butterflies and Skippers", "Mussels", "Freshwater/Terrestrial Snails", "Crayfishes", "Stoneflies", "Mayflies", "Caddisflies", "Dragonflies and Damselflies", "Shrimps", "Moths", "Tiger Beetles"),
                             coverage = NA
                             )
#' ##### Calculate coverage (NOTE: coverage is defined as a being included if any species is expected and at least one species is included in the 2015 SWAP)
#' ###### Plants
SGCN_plants_full_richness <- purrr::map_int(SGCN_species_byState, function(state_data) state_data %>% dplyr::filter(Taxonomic_Group == "Plants") %>% nrow())
coverage_table$coverage[coverage_table$taxon == "Plants"] <- sum(ifelse(SGCN_plants_full_richness > 0, 1, 0))/length(SGCN_plants_full_richness)
#' ###### Mammals
SGCN_mammals_full_richness <- purrr::map_int(SGCN_species_byState, function(state_data) state_data %>% dplyr::filter(Taxonomic_Group == "Mammals") %>% nrow())
expected_mammals_richness <- purrr::map_int(expected_species_byState, function(state_data) state_data %>% dplyr::filter(grepl("Mammals$", speciesGlobal$informalTaxonomy)) %>% nrow())
mammals_SGCN_expected_ratio <- (SGCN_mammals_full_richness/expected_mammals_richness)[expected_mammals_richness > 0]
coverage_table$coverage[coverage_table$taxon == "Mammals"] <- sum(mammals_SGCN_expected_ratio > 0)/length(mammals_SGCN_expected_ratio)
#' ###### Reptiles
SGCN_reptiles_full_richness <- purrr::map_int(SGCN_species_byState, function(state_data) state_data %>% dplyr::filter(Taxonomic_Group == "Reptiles") %>% nrow())
expected_reptiles_richness <- purrr::map_int(expected_species_byState, function(state_data) state_data %>% dplyr::filter(grepl("Reptiles$", speciesGlobal$informalTaxonomy)) %>% nrow())
reptiles_SGCN_expected_ratio <- (SGCN_reptiles_full_richness/expected_reptiles_richness)[expected_reptiles_richness > 0]
coverage_table$coverage[coverage_table$taxon == "Reptiles"] <- sum(reptiles_SGCN_expected_ratio > 0)/length(reptiles_SGCN_expected_ratio)
#' ###### Birds
SGCN_birds_full_richness <- purrr::map_int(SGCN_species_byState, function(state_data) state_data %>% dplyr::filter(Taxonomic_Group == "Birds") %>% nrow())
expected_birds_richness <- purrr::map_int(expected_species_byState, function(state_data) state_data %>% dplyr::filter(grepl("Birds$", speciesGlobal$informalTaxonomy)) %>% nrow())
birds_SGCN_expected_ratio <- (SGCN_birds_full_richness/expected_birds_richness)[expected_birds_richness > 0]
coverage_table$coverage[coverage_table$taxon == "Birds"] <- sum(birds_SGCN_expected_ratio > 0)/length(birds_SGCN_expected_ratio)
#' ###### Amphibians
SGCN_amphibians_full_richness <- purrr::map_int(SGCN_species_byState, function(state_data) state_data %>% dplyr::filter(Taxonomic_Group == "Amphibians") %>% nrow())
expected_amphibians_richness <- purrr::map_int(expected_species_byState, function(state_data) state_data %>% dplyr::filter(grepl("Amphibians$", speciesGlobal$informalTaxonomy)) %>% nrow())
amphibians_SGCN_expected_ratio <- (SGCN_amphibians_full_richness/expected_amphibians_richness)[expected_amphibians_richness > 0]
coverage_table$coverage[coverage_table$taxon == "Amphibians"] <- sum(amphibians_SGCN_expected_ratio > 0)/length(amphibians_SGCN_expected_ratio)
#' ###### Fishes
SGCN_fishes_full_richness <- purrr::map_int(SGCN_species_byState, function(state_data) state_data %>% dplyr::filter(Taxonomic_Group == "Fish") %>% nrow())
expected_fishes_richness <- purrr::map_int(expected_species_byState, function(state_data) state_data %>% dplyr::filter(grepl("Fishes - Freshwater and Anadromous Bony, Cartilaginous; Lampreys$", speciesGlobal$informalTaxonomy)) %>% nrow())
fishes_SGCN_expected_ratio <- (SGCN_fishes_full_richness/expected_fishes_richness)[expected_fishes_richness > 0]
coverage_table$coverage[coverage_table$taxon == "Freshwater Fishes"] <- sum(fishes_SGCN_expected_ratio > 0)/length(fishes_SGCN_expected_ratio)
#' ###### Bees
groups_bees <- c("Other Bees", "Digger Bees", "Sweat Bees (in part)", "Leafcutter Bees", "Mason Bees", "Resin Bees (in part)", "Bumble Bees")
SGCN_bees_full_richness <- purrr::map_int(SGCN_species_matched_byState, function(state_data) state_data %>% dplyr::filter(grepl(paste(groups_bees, collapse = "|"), speciesGlobal$informalTaxonomy)) %>% nrow())
expected_bees_richness <- purrr::map_int(expected_species_byState, function(state_data) state_data %>% dplyr::filter(grepl(paste(groups_bees, collapse = "|"), speciesGlobal$informalTaxonomy)) %>% nrow())
bees_SGCN_expected_ratio <- (SGCN_bees_full_richness/expected_bees_richness)[expected_bees_richness > 0]
coverage_table$coverage[coverage_table$taxon == "Bees"] <- sum(bees_SGCN_expected_ratio > 0)/length(bees_SGCN_expected_ratio)
#' ###### Butterflies
SGCN_butterflies_full_richness <- purrr::map_int(SGCN_species_matched_byState, function(state_data) state_data %>% dplyr::filter(grepl("Butterflies and Skippers$", speciesGlobal$informalTaxonomy)) %>% nrow())
expected_butterflies_richness <- purrr::map_int(expected_species_byState, function(state_data) state_data %>% dplyr::filter(grepl("Butterflies and Skippers$", speciesGlobal$informalTaxonomy)) %>% nrow())
butterflies_SGCN_expected_ratio <- (SGCN_butterflies_full_richness/expected_butterflies_richness)[expected_butterflies_richness > 0]
coverage_table$coverage[coverage_table$taxon == "Butterflies and Skippers"] <- sum(butterflies_SGCN_expected_ratio > 0)/length(butterflies_SGCN_expected_ratio)
#' ###### Mussels
SGCN_mussels_full_richness <- purrr::map_int(SGCN_species_matched_byState, function(state_data) state_data %>% dplyr::filter(grepl("Freshwater Mussels$", speciesGlobal$informalTaxonomy)) %>% nrow())
expected_mussels_richness <- purrr::map_int(expected_species_byState, function(state_data) state_data %>% dplyr::filter(grepl("Freshwater Mussels$", speciesGlobal$informalTaxonomy)) %>% nrow())
mussels_SGCN_expected_ratio <- (SGCN_mussels_full_richness/expected_mussels_richness)[expected_mussels_richness > 0]
coverage_table$coverage[coverage_table$taxon == "Mussels"] <- sum(mussels_SGCN_expected_ratio > 0)/length(mussels_SGCN_expected_ratio)
#' ###### Snails
groups_snails <- c("Freshwater Snails", "Terrestrial Snails")
SGCN_snails_full_richness <- purrr::map_int(SGCN_species_matched_byState, function(state_data) state_data %>% dplyr::filter(grepl(paste(groups_snails, collapse = "|"), speciesGlobal$informalTaxonomy)) %>% nrow())
expected_snails_richness <- purrr::map_int(expected_species_byState, function(state_data) state_data %>% dplyr::filter(grepl(paste(groups_snails, collapse = "|"), speciesGlobal$informalTaxonomy)) %>% nrow())
snails_SGCN_expected_ratio <- (SGCN_snails_full_richness/expected_snails_richness)[expected_snails_richness > 0]
coverage_table$coverage[coverage_table$taxon == "Freshwater/Terrestrial Snails"] <- sum(snails_SGCN_expected_ratio > 0)/length(snails_SGCN_expected_ratio)
#' ###### Crayfishes
SGCN_crayfishes_full_richness <- purrr::map_int(SGCN_species_matched_byState, function(state_data) state_data %>% dplyr::filter(grepl("Crayfishes$", speciesGlobal$informalTaxonomy)) %>% nrow())
expected_crayfishes_richness <- purrr::map_int(expected_species_byState, function(state_data) state_data %>% dplyr::filter(grepl("Crayfishes$", speciesGlobal$informalTaxonomy)) %>% nrow())
crayfishes_SGCN_expected_ratio <- (SGCN_crayfishes_full_richness/expected_crayfishes_richness)[expected_crayfishes_richness > 0]
coverage_table$coverage[coverage_table$taxon == "Crayfishes"] <- sum(crayfishes_SGCN_expected_ratio > 0)/length(crayfishes_SGCN_expected_ratio)
#' ###### Stoneflies
SGCN_stoneflies_full_richness <- purrr::map_int(SGCN_species_matched_byState, function(state_data) state_data %>% dplyr::filter(grepl("Stoneflies$", speciesGlobal$informalTaxonomy)) %>% nrow())
expected_stoneflies_richness <- purrr::map_int(expected_species_byState, function(state_data) state_data %>% dplyr::filter(grepl("Stoneflies$", speciesGlobal$informalTaxonomy)) %>% nrow())
stoneflies_SGCN_expected_ratio <- (SGCN_stoneflies_full_richness/expected_stoneflies_richness)[expected_stoneflies_richness > 0]
coverage_table$coverage[coverage_table$taxon == "Stoneflies"] <- sum(stoneflies_SGCN_expected_ratio > 0)/length(stoneflies_SGCN_expected_ratio)
#' ###### Mayflies
SGCN_mayflies_full_richness <- purrr::map_int(SGCN_species_matched_byState, function(state_data) state_data %>% dplyr::filter(grepl("Mayflies$", speciesGlobal$informalTaxonomy)) %>% nrow())
expected_mayflies_richness <- purrr::map_int(expected_species_byState, function(state_data) state_data %>% dplyr::filter(grepl("Mayflies$", speciesGlobal$informalTaxonomy)) %>% nrow())
mayflies_SGCN_expected_ratio <- (SGCN_mayflies_full_richness/expected_mayflies_richness)[expected_mayflies_richness > 0]
coverage_table$coverage[coverage_table$taxon == "Mayflies"] <- sum(mayflies_SGCN_expected_ratio > 0)/length(mayflies_SGCN_expected_ratio)
#' ###### Caddisflies
SGCN_caddisflies_full_richness <- purrr::map_int(SGCN_species_matched_byState, function(state_data) state_data %>% dplyr::filter(grepl("Caddisflies$", speciesGlobal$informalTaxonomy)) %>% nrow())
expected_caddisflies_richness <- purrr::map_int(expected_species_byState, function(state_data) state_data %>% dplyr::filter(grepl("Caddisflies$", speciesGlobal$informalTaxonomy)) %>% nrow())
caddisflies_SGCN_expected_ratio <- (SGCN_caddisflies_full_richness/expected_caddisflies_richness)[expected_caddisflies_richness > 0]
coverage_table$coverage[coverage_table$taxon == "Caddisflies"] <- sum(caddisflies_SGCN_expected_ratio > 0)/length(caddisflies_SGCN_expected_ratio)
#' ###### Dragonflies
SGCN_dragonflies_full_richness <- purrr::map_int(SGCN_species_matched_byState, function(state_data) state_data %>% dplyr::filter(grepl("Dragonflies and Damselflies$", speciesGlobal$informalTaxonomy)) %>% nrow())
expected_dragonflies_richness <- purrr::map_int(expected_species_byState, function(state_data) state_data %>% dplyr::filter(grepl("Dragonflies and Damselflies$", speciesGlobal$informalTaxonomy)) %>% nrow())
dragonflies_SGCN_expected_ratio <- (SGCN_dragonflies_full_richness/expected_dragonflies_richness)[expected_dragonflies_richness > 0]
coverage_table$coverage[coverage_table$taxon == "Dragonflies and Damselflies"] <- sum(dragonflies_SGCN_expected_ratio > 0)/length(dragonflies_SGCN_expected_ratio)
#' ###### Shrimps
SGCN_shrimps_full_richness <- purrr::map_int(SGCN_species_matched_byState, function(state_data) state_data %>% dplyr::filter(grepl("Fairy, Clam, and Tadpole Shrimps$", speciesGlobal$informalTaxonomy)) %>% nrow())
expected_shrimps_richness <- purrr::map_int(expected_species_byState, function(state_data) state_data %>% dplyr::filter(grepl("Fairy, Clam, and Tadpole Shrimps$", speciesGlobal$informalTaxonomy)) %>% nrow())
shrimps_SGCN_expected_ratio <- (SGCN_shrimps_full_richness/expected_shrimps_richness)[expected_shrimps_richness > 0]
coverage_table$coverage[coverage_table$taxon == "Shrimps"] <- sum(shrimps_SGCN_expected_ratio > 0)/length(shrimps_SGCN_expected_ratio)
#' ###### Moths
groups_moths <- c("Giant Silkworm and Royal Moths (G1G3)", "Notodontid Moths (G1G3)", "Papaipema Moths", "Sphinx Moths", "Tiger Moths (G1G3)", "Underwing Moths") 
SGCN_moths_full_richness <- purrr::map_int(SGCN_species_matched_byState, function(state_data) state_data %>% dplyr::filter(grepl(paste(groups_moths, collapse = "|"), speciesGlobal$informalTaxonomy)) %>% nrow())
expected_moths_richness <- purrr::map_int(expected_species_byState, function(state_data) state_data %>% dplyr::filter(grepl(paste(groups_moths, collapse = "|"), speciesGlobal$informalTaxonomy)) %>% nrow())
moths_SGCN_expected_ratio <- (SGCN_moths_full_richness/expected_moths_richness)[expected_moths_richness > 0]
coverage_table$coverage[coverage_table$taxon == "Moths"] <- sum(moths_SGCN_expected_ratio > 0)/length(moths_SGCN_expected_ratio)
#' ###### Beetles
SGCN_beetles_full_richness <- purrr::map_int(SGCN_species_matched_byState, function(state_data) state_data %>% dplyr::filter(grepl("Tiger Beetles$", speciesGlobal$informalTaxonomy)) %>% nrow())
expected_beetles_richness <- purrr::map_int(expected_species_byState, function(state_data) state_data %>% dplyr::filter(grepl("Tiger Beetles$", speciesGlobal$informalTaxonomy)) %>% nrow())
beetles_SGCN_expected_ratio <- (SGCN_beetles_full_richness/expected_beetles_richness)[expected_beetles_richness > 0]
coverage_table$coverage[coverage_table$taxon == "Tiger Beetles"] <- sum(beetles_SGCN_expected_ratio > 0)/length(beetles_SGCN_expected_ratio)

#' ##### Convert to percentage
coverage_table <- coverage_table %>% 
  dplyr::mutate(coverage = ifelse(coverage > 1, 1, coverage),
                percentage = 100*coverage,
                taxon = factor(taxon, levels = coverage_table %>% arrange(percentage, desc(taxon)) %>% pull(taxon))
  )
#' ### Generate plot
SGCN_taxon_completeness <- ggplot() + #assign as object in environment
  geom_bar(data = coverage_table, stat = "identity", color = grey(0.3), fill = "deepskyblue3", position = "dodge", size = 0.3, aes(x = taxon, y = coverage)) +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  theme_bw() +
  theme(rect = element_blank(), 
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major = element_line(color = grey(0.2), size = 0.01),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none", 
        axis.text.x = element_text(size = 9),
        axis.title.x = element_text(size = 11),
        axis.text.y = element_text(size = 9),
        plot.margin = margin(0.1, 1, 0.1, 1, "cm")
  ) +
  ylab("% States That Include Taxon") 
#'
#' #### Bottom panel: Numbers of animal species in each state wildlife action plan
#' ##### Calculate summary
SGCN_richness_actual_animals_full <- purrr::map_int(SGCN_species_byState, function(state_data) state_data %>% dplyr::filter(!(Taxonomic_Group %in% c("Plants", "Fungi"))) %>% nrow())
#' ##### Update states object
states <- states %>% 
  dplyr::mutate(SGCN_richness_actual_animals_full = SGCN_richness_actual_animals_full[states$state_name])
#' ##### Generate map
pal <- RColorBrewer::brewer.pal(11, "Blues")
SGCN_animals <- tm_shape(states) + 
  tm_polygons("SGCN_richness_actual_animals_full", palette = pal, border.col = "#878786", style = "fixed", breaks = c(1, seq(100, 1400, 100)), labels = c("1-100", "101-200", "201-300", "301-400", "401-500", "501-600", "601-700", "701-800", "801-900", "901-1000", "1001-1100", "1101-1200", "1201-1300", "1301-1400"), title = "Number of Animal SGCN") + 
  tm_layout(legend.outside = TRUE, frame = FALSE, title = "", inner.margins = c(0, 0, 0, 0))
SGCN_animals <- tmap_grob(SGCN_animals)
#' #### Write out figure
pdf(file = paste0(outdir, "/Figure1.pdf"), height = 11, width = 10)
ggarrange(SGCN_taxon_completeness, SGCN_animals, nrow = 2, heights = c(.4, .6), labels = c("A)", "B)"))
dev.off()

#' ### Figure 2
#' #### Extract all animal data
SGCN_animals_summary <- SGCN_species_matched_byState %>% 
  bind_rows() %>% 
  dplyr::filter(speciesGlobal$kingdom == "Animalia")

#' #### Panel A
#' ##### Extract S ranks
plot_dat <- SGCN_animals_summary %>% 
  dplyr::pull(roundedSrank_2015) %>% 
  table() %>% 
  stack() %>% 
  set_names(c("n", "roundedSrank")) 
plot_dat <- plot_dat %>% 
  dplyr::mutate(roundedSrank = factor(roundedSrank, levels = c("S1", "S2", "S3", "S4", "S5", "SNR", "SNA", "SU", "SX", "SH")),
                percentage = round(100*(n/sum(n)), 1)
  ) %>% 
  dplyr::filter(complete.cases(.))
#' ##### Generate plot
col_scheme <- c("#db0909", "#f27619", "#fcce0d", "#25bbef", "#003d80", grey(.7), grey(.5), grey(.3), grey(.2), grey(.1))
labs <- plot_dat$roundedSrank %>% levels() %>% rev()
labs <- paste0(labs, "\n ", plot_dat$percentage[match(labs, plot_dat$roundedSrank)], "%")
animal_roundedSRanks_donut <- plot_dat %>% 
  ggdonutchart("n", label = labs, fill = "roundedSrank", palette = col_scheme, lab.pos = "out", lab.font = c(4, "bold", "white")) +
  theme(
    legend.position = "none",
    
  ) 
  # annotate(geom = 'text', x = 0.5, y = 0, label = paste0(plot_dat %>% pull(n) %>% sum() %>% as.character(), "\n Species")) 
animal_roundedSRanks_donut

#' ##### Panel B
#' ###### Extract G ranks
plot_dat <- SGCN_species_matched %>% 
  dplyr::filter(speciesGlobal$kingdom == "Animalia") %>% 
  dplyr::pull(roundedGrank_2015) %>% 
  table() %>% 
  stack() %>% 
  set_names(c("n", "roundedGrank")) %>% 
  dplyr::mutate(roundedGrank_combined = fct_collapse(roundedGrank,  
                                                     G1 = c("G1", "T1"),
                                                     G2 = c("G2", "T2"),
                                                     G3 = c("G3", "T3"),
                                                     G4 = c("G4", "T4"),
                                                     G5 = c("G5", "T5"),
                                                     GNR = c("GNR", "TNR"),
                                                     GU = c("GU", "TU"),
                                                     GH = c("GH", "TH"),
                                                     GX = c("GX", "TX"),
                                                     GNA = c("GNA")
                                                     
  )) %>% 
  dplyr::group_by(roundedGrank_combined) %>% 
  dplyr::summarise(n = sum(n))
plot_dat <- plot_dat %>% 
  dplyr::mutate(roundedGrank_combined = factor(roundedGrank_combined, levels = c("G1", "G2", "G3", "G4", "G5", "GNR", "GU", "GX", "GH")),
                percentage = round(100*(n/sum(n)), 1)
  )
#' ##### Generate plot
col_scheme <- c("#db0909", "#f27619", "#fcce0d", "#25bbef", "#003d80", grey(.7), grey(.5), grey(.3), grey(.1))
labs <- plot_dat$roundedGrank_combined %>% levels() %>% rev()
labs <- paste0(labs, "\n ", plot_dat$percentage[match(labs, plot_dat$roundedGrank_combined)], "%")
animal_roundedGRanks_donut <- plot_dat %>% 
  ggdonutchart("n", label = labs, fill = "roundedGrank_combined", palette = col_scheme) +
  theme(
    legend.position = "none",
    
  ) 
  # annotate(geom = 'text', x = 0.5, y = 0, label = paste0(plot_dat %>% pull(n) %>% sum() %>% as.character(), "\n Species")) 
animal_roundedGRanks_donut
#' #### Write out figure
pdf(file = paste0(outdir, "/Figure2.pdf"), width = 11, height = 6)
ggarrange(animal_roundedSRanks_donut, animal_roundedGRanks_donut, ncol = 2)
dev.off()

#' ### Figure 3: Conservation Status Index scores for animals in 2015 SGCN lists 
#' #### Top panel: Conservation Status Index scores for animals in 2015 SGCN lists
#' #### Extract comprehensively assessed animal groups
SGCN_animals_comprehensive_byState <- SGCN_species_matched_byState %>% 
  purrr::map(function(state_data){
    state_data %>% dplyr::filter(speciesGlobal$kingdom == "Animalia",
                  grepl(paste(comprehensively_assessed_groups, collapse = "|"), speciesGlobal$informalTaxonomy)
    )
  })
#' #### Calculate Status Scores
SGCN_animals_comprehensive_status_scores_byState <- purrr::map(SGCN_animals_comprehensive_byState, function(state_data){
  rank_numbers <- state_data %>% pull(roundedSrank_2015) %>% parse_number()
  rank_numbers_problems <- attr(rank_numbers, "problems")
  if (sum(grepl("SX|SH", rank_numbers_problems$actual)) > 0){
    rank_numbers[rank_numbers_problems$row[grepl("SX|SH", rank_numbers_problems$actual)]] <- "X"
  }
  rank_numbers <- as.factor(rank_numbers)
  rank_numbers <- fct_recode(rank_numbers, "5" = "X", "4" = "1", "3" = "2", "2" = "3", "1" = "4", "0" = "5") %>% as.character() %>% na.omit() %>% as.numeric()
  1 - ((rank_numbers %>% sum(na.rm = TRUE))/(5*sum(complete.cases(rank_numbers))))
}) %>% unlist()
#' #### Generate map
#' ##### Update states object
states <- states %>% 
  dplyr::mutate(SGCN_animals_comprehensive_status_scores = SGCN_animals_comprehensive_status_scores_byState[states$state_name])
#' ##### Generate map
pal <- RColorBrewer::brewer.pal(11, "Spectral")
status_ranks_animals_map <- tm_shape(states) + 
  tm_polygons("SGCN_animals_comprehensive_status_scores", palette = pal, border.col = "#878786", style = "pretty", n = 10, title = "S Rank Status Scores") + 
  tm_layout(legend.outside = TRUE, frame = FALSE, title = "", inner.margins = c(0, 0, 0, 0))
status_ranks_animals_map <- tmap_grob(status_ranks_animals_map)
#' #### Bottom panel: Actual divided by expected numbers of Critically Imperiled (G1) and Imperiled (G2) animals in 2015 SGCN lists 
#' ##### Extract expected for comprehensive animal groups
expected_animals_comprehensive_byState <- expected_species_byState %>% 
  purrr::map(function(state_data){
    state_data %>% dplyr::filter(speciesGlobal$kingdom == "Animalia",
                                 grepl(paste(comprehensively_assessed_groups, collapse = "|"), speciesGlobal$informalTaxonomy)
    )
  })
#' ##### Calculate state ratios
SGCN_expected_G1G2_ratios <- purrr::map(names(SGCN_animals_comprehensive_byState), function(st) length(intersect(SGCN_animals_comprehensive_byState[[st]] %>% pull(scientificName), expected_animals_comprehensive_byState[[st]] %>% pull(scientificName)))/nrow(expected_animals_comprehensive_byState[[st]])) %>% set_names(names(SGCN_animals_comprehensive_byState)) %>% unlist()
#' #### Generate map
#' ##### Update states object
states <- states %>% 
  dplyr::mutate(SGCN_expected_G1G2 = SGCN_expected_G1G2_ratios[states$state_name])
#' ##### Generate map
pal <- RColorBrewer::brewer.pal(11, "Spectral")
SGCN_expected_G1G2_map <- tm_shape(states) + 
  tm_polygons("SGCN_expected_G1G2", palette = pal, border.col = "#878786", style = "pretty", n = 10, title = "Actual/Expected G1-G2") + 
  tm_layout(legend.show = FALSE, legend.outside = TRUE, frame = FALSE, title = "", inner.margins = c(0, 0, 0, 0))
SGCN_expected_G1G2_map <- tmap_grob(SGCN_expected_G1G2_map)
#' #### Write out figure
pdf(file = paste0(outdir, "/Figure3.pdf"), height = 10, width = 9)
ggarrange(status_ranks_animals_map, SGCN_expected_G1G2_map, nrow = 2, labels = c("A)", "B)"))
dev.off()

#' ##### Check completeness of S ranks
number_missing_Sranks_byState <- purrr::map_dbl(purrr::map(SGCN_animals_comprehensive_byState, "roundedSrank_2015"), function(x) length(which(x %in% c("SNR", "SU", NA))))
completeness_Sranks_byState <- number_missing_Sranks_byState/purrr::map_dbl(SGCN_animals_comprehensive_byState, nrow)

#' ### Figure 4: Ratio of actual to expected imperiled state endemic and multistate butterfly, amphibian, and freshwater mussel species in SGCN lists
#' #### Freshwater mussels
#' ##### Endemics ratio
SGCN_mussels_endemics_byState <- mussels_SGCN_data %>% purrr::map(function(x) x %>% dplyr::filter(x$jurisEndem == 1) %>% dplyr::pull(scientificName))
expected_mussels_endemics_byState <- mussels_expected_data %>% purrr::map(function(x) x %>% dplyr::filter(x$jurisEndem == 1) %>% dplyr::pull(scientificName))
SGCN_expected_ratio_mussels_endemics <- purrr::map2_dbl(SGCN_mussels_endemics_byState, expected_mussels_endemics_byState, function(SGCN, expected){
  length(intersect(SGCN, expected))/length(expected)
})
#' ##### Multistate ratio
SGCN_mussels_multistate_byState <- mussels_SGCN_data %>% purrr::map(function(x) x %>% dplyr::filter(x$jurisEndem != 1) %>% dplyr::pull(scientificName))
expected_mussels_multistate_byState <- mussels_expected_data %>% purrr::map(function(x) x %>% dplyr::filter(x$jurisEndem != 1) %>% dplyr::pull(scientificName))
SGCN_expected_ratio_mussels_multistate <- purrr::map2_dbl(SGCN_mussels_multistate_byState, expected_mussels_multistate_byState, function(SGCN, expected){
  length(intersect(SGCN, expected))/length(expected)
})
#' #### Terrestrial Snails
#' ##### Endemics ratio
SGCN_snails_endemics_byState <- snails_SGCN_data %>% purrr::map(function(x) x %>% dplyr::filter(x$jurisEndem == 1) %>% dplyr::pull(scientificName))
expected_snails_endemics_byState <- snails_expected_data %>% purrr::map(function(x) x %>% dplyr::filter(x$jurisEndem == 1) %>% dplyr::pull(scientificName))
SGCN_expected_ratio_snails_endemics <- purrr::map2_dbl(SGCN_snails_endemics_byState, expected_snails_endemics_byState, function(SGCN, expected){
  length(intersect(SGCN, expected))/length(expected)
})
#' ##### Multistate ratio
SGCN_snails_multistate_byState <- snails_SGCN_data %>% purrr::map(function(x) x %>% dplyr::filter(x$jurisEndem != 1) %>% dplyr::pull(scientificName))
expected_snails_multistate_byState <- snails_expected_data %>% purrr::map(function(x) x %>% dplyr::filter(x$jurisEndem != 1) %>% dplyr::pull(scientificName))
SGCN_expected_ratio_snails_multistate <- purrr::map2_dbl(SGCN_snails_multistate_byState, expected_snails_multistate_byState, function(SGCN, expected){
  length(intersect(SGCN, expected))/length(expected)
})
#' #### Freshwater Fishes
#' ##### Endemics ratio
SGCN_fishes_endemics_byState <- fishes_SGCN_data %>% purrr::map(function(x) x %>% dplyr::filter(x$jurisEndem == 1) %>% dplyr::pull(scientificName))
expected_fishes_endemics_byState <- fishes_expected_data %>% purrr::map(function(x) x %>% dplyr::filter(x$jurisEndem == 1) %>% dplyr::pull(scientificName))
SGCN_expected_ratio_fishes_endemics <- purrr::map2_dbl(SGCN_fishes_endemics_byState, expected_fishes_endemics_byState, function(SGCN, expected){
  length(intersect(SGCN, expected))/length(expected)
})
#' ##### Multistate ratio
SGCN_fishes_multistate_byState <- fishes_SGCN_data %>% purrr::map(function(x) x %>% dplyr::filter(x$jurisEndem != 1) %>% dplyr::pull(scientificName))
expected_fishes_multistate_byState <- fishes_expected_data %>% purrr::map(function(x) x %>% dplyr::filter(x$jurisEndem != 1) %>% dplyr::pull(scientificName))
SGCN_expected_ratio_fishes_multistate <- purrr::map2_dbl(SGCN_fishes_multistate_byState, expected_fishes_multistate_byState, function(SGCN, expected){
  length(intersect(SGCN, expected))/length(expected)
})
#' ### Generate plot
#' #### Generate data
SGCN_expected_ratio_plot_dat <- data.frame(
  taxon = rep(c("Freshwater Mussels", "Terrestrial Snails", "Freshwater Fishes"), each = 2*length(SGCN_mussels_endemics_byState)),
  range = rep(rep(c("State endemics", "Multi-state species"), each = length(SGCN_mussels_endemics_byState), 3)),
  ratio = c(SGCN_expected_ratio_mussels_endemics,
            SGCN_expected_ratio_mussels_multistate,
            SGCN_expected_ratio_snails_endemics,
            SGCN_expected_ratio_snails_multistate,
            SGCN_expected_ratio_fishes_endemics,
            SGCN_expected_ratio_fishes_multistate
            ) 
) %>% 
  dplyr::filter(complete.cases(.)) %>% 
  dplyr::mutate(range = factor(range, levels = c("State endemics", "Multi-state species")),
                taxon = factor(taxon, levels = c("Freshwater Mussels", "Freshwater Fishes", "Terrestrial Snails"))
                )
#' #### Run Mann-Whitney two-tailed tests
wilcox.test(x = SGCN_expected_ratio_plot_dat %>% dplyr::filter(taxon == "Freshwater Mussels" & range == "State endemics") %>% pull(ratio),
            y = SGCN_expected_ratio_plot_dat %>% dplyr::filter(taxon == "Freshwater Mussels" & range == "Multi-state species")  %>% pull(ratio),
            alternative = "two.sided"
)

wilcox.test(x = SGCN_expected_ratio_plot_dat %>% dplyr::filter(taxon == "Freshwater Fishes" & range == "State endemics") %>% pull(ratio),
            y = SGCN_expected_ratio_plot_dat %>% dplyr::filter(taxon == "Freshwater Fishes" & range == "Multi-state species")  %>% pull(ratio),
            alternative = "two.sided"
)

wilcox.test(x = SGCN_expected_ratio_plot_dat %>% dplyr::filter(taxon == "Terrestrial Snails" & range == "State endemics") %>% pull(ratio),
            y = SGCN_expected_ratio_plot_dat %>% dplyr::filter(taxon == "Terrestrial Snails" & range == "Multi-state species")  %>% pull(ratio),
            alternative = "two.sided"
)
#' #### Generate plot
pdf(file = paste0(outdir, "/Figure4.pdf"), width = 10, height = 4)
SGCN_expected_ratio_plot <- SGCN_expected_ratio_plot_dat %>% 
  ggplot() +
  geom_boxplot(aes(x = range, y = ratio, fill = range), notch = FALSE, ) +
  facet_wrap(~taxon) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid = element_blank(), 
        axis.text.x = element_text(size = 11)
        ) +
  ylab("Ratio Actual/Expected SGCN") +
  xlab("") +
  geom_text(data = data.frame(range = rep(c("State endemics", "Multi-state species"), 3), ratio = -0.1, taxon = rep(c("Freshwater Mussels", "Terrestrial Snails", "Freshwater Fishes"), 2)), aes(x = range, y = ratio), label = paste0("n = ", c(length(which(!is.na(SGCN_expected_ratio_mussels_endemics))),
                                                                                                                                                                                                                                  length(which(!is.na(SGCN_expected_ratio_mussels_multistate))),
                                                                                                                                                                                                                                  length(which(!is.na(SGCN_expected_ratio_snails_endemics))),
                                                                                                                                                                                                                                  length(which(!is.na(SGCN_expected_ratio_snails_multistate))),
                                                                                                                                                                                                                                  length(which(!is.na(SGCN_expected_ratio_fishes_endemics))),
                                                                                                                                                                                                                                  length(which(!is.na(SGCN_expected_ratio_fishes_multistate)))
                                                                                                                                                                                                                                  ))
            )
SGCN_expected_ratio_plot
dev.off()

#' ### Figure 5: Ratio of actual to expected imperiled state multistate
#' #### Update states_borders object
states_borders <- states_borders %>% 
  dplyr::mutate(
    shared_ratio_mussels = purrr::map_dbl(1:nrow(states_borders), function(border){   ### Mussels
      shared_SGCN <- intersect(mussels_SGCN_data[[states_borders$state_name1[border]]] %>% 
                                 dplyr::pull(scientificName), 
                               mussels_SGCN_data[[states_borders$state_name2[border]]] %>% 
                                 dplyr::pull(scientificName)
      )
      shared_expected <- intersect(mussels_expected_data[[states_borders$state_name1[border]]] %>% 
                                     dplyr::pull(scientificName), 
                                   mussels_expected_data[[states_borders$state_name2[border]]] %>% 
                                     dplyr::pull(scientificName)
      )
      
      (length(intersect(shared_SGCN, shared_expected)))/length(shared_expected)
      
    }),
    shared_ratio_snails = purrr::map_dbl(1:nrow(states_borders), function(border){   ### Snails
      shared_SGCN <- intersect(snails_SGCN_data[[states_borders$state_name1[border]]] %>% 
                                 dplyr::pull(scientificName), 
                               snails_SGCN_data[[states_borders$state_name2[border]]] %>% 
                                 dplyr::pull(scientificName)
      )
      shared_expected <- intersect(snails_expected_data[[states_borders$state_name1[border]]] %>% 
                                     dplyr::pull(scientificName), 
                                   snails_expected_data[[states_borders$state_name2[border]]] %>% 
                                     dplyr::pull(scientificName)
      )
      
      (length(intersect(shared_SGCN, shared_expected)))/length(shared_expected)
      
    }),
    shared_ratio_fishes = purrr::map_dbl(1:nrow(states_borders), function(border){   ### Fishes
      shared_SGCN <- intersect(fishes_SGCN_data[[states_borders$state_name1[border]]] %>% 
                                 dplyr::pull(scientificName), 
                               fishes_SGCN_data[[states_borders$state_name2[border]]] %>% 
                                 dplyr::pull(scientificName)
      )
      shared_expected <- intersect(fishes_expected_data[[states_borders$state_name1[border]]] %>% 
                                     dplyr::pull(scientificName), 
                                   fishes_expected_data[[states_borders$state_name2[border]]] %>% 
                                     dplyr::pull(scientificName)
      )
      
      (length(intersect(shared_SGCN, shared_expected)))/length(shared_expected)
      
    })
    
  )
#' #### Convert object to lines
states_borders_lines <- states_borders %>% st_cast("MULTILINESTRING")

#' #### Generate maps
#' ##### Mussels
pal <- RColorBrewer::brewer.pal(10, "Spectral")
shared_mussels <- tm_shape(states) + 
  tm_borders(col = grey(0.5)) +
  tm_shape(states_borders_lines) + 
  tm_lines(col = "shared_ratio_mussels", palette = pal, style = "pretty", lwd = 2, n = 8, title.col = "",
           legend.size.show = FALSE,
           legend.col.show = FALSE, 
           legend.size.is.portrait = TRUE) + 
  tm_add_legend(type = "fill", 
                labels = c("Low", rep("", 8), "High"),
                col = pal,
                border.lwd = 0.5,
                title = "Shared SGCN"

  )  +
  tm_add_legend(type = "fill", 
                labels = "Missing",
                col = grey(.5),
                border.lwd = 0.5,
                title = ""
                
  )  +
  tm_layout(legend.position = c("right", "bottom"), frame = FALSE, title = "", inner.margins = c(0, 0, 0, 0.08))
shared_mussels <- tmap_grob(shared_mussels)
#' ##### Snails
pal <- RColorBrewer::brewer.pal(10, "Spectral")
shared_snails <- tm_shape(states) + 
  tm_borders(col = grey(0.5)) +
  tm_shape(states_borders_lines) + 
  tm_lines(col = "shared_ratio_snails", palette = pal, style = "pretty", lwd = 2, n = 8, title.col = "",
           legend.size.show = FALSE,
           legend.col.show = FALSE, 
           legend.size.is.portrait = TRUE) + 
  tm_add_legend(type = "fill", 
                labels = c("Low", rep("", 8), "High"),
                col = pal,
                border.lwd = 0.5,
                title = "Shared SGCN"
                
  )  +
  tm_add_legend(type = "fill", 
                labels = "Missing",
                col = grey(.5),
                border.lwd = 0.5,
                title = ""
                
  )  +
  tm_layout(legend.position = c("right", "bottom"), frame = FALSE, title = "", inner.margins = c(0, 0, 0, 0.08))
shared_snails <- tmap_grob(shared_snails)
#' ##### Fishes
pal <- RColorBrewer::brewer.pal(10, "Spectral")
shared_fishes <- tm_shape(states) + 
  tm_borders(col = grey(0.5)) +
  tm_shape(states_borders_lines) + 
  tm_lines(col = "shared_ratio_fishes", palette = pal, style = "pretty", lwd = 2, n = 8, title.col = "",
           legend.size.show = FALSE,
           legend.col.show = FALSE, 
           legend.size.is.portrait = TRUE) + 
  tm_add_legend(type = "fill", 
                labels = c("Low", rep("", 8), "High"),
                col = pal,
                border.lwd = 0.5,
                title = "Shared SGCN"
                
  )  +
  tm_add_legend(type = "fill", 
                labels = "Missing",
                col = grey(.5),
                border.lwd = 0.5,
                title = ""
                
  )  +
  tm_layout(legend.position = c("right", "bottom"), frame = FALSE, title = "", inner.margins = c(0, 0, 0, 0.08))
shared_fishes <- tmap_grob(shared_fishes)
#' #### Write out figure
pdf(file = paste0(outdir, "/Figure5.pdf"), width = 8, height = 12)
ggarrange(shared_mussels, shared_fishes, shared_snails, nrow = 3, labels = c("A) Freshwater Mussels", "B) Freshwater Fishes", "C) Terrestrial Snails"))
dev.off()
