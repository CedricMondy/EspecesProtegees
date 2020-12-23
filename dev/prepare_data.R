
# PACKAGES ----------------------------------------------------------------
if (!require(pacman))
    install.packages("pacman")

pacman::p_load(here, purrr, vroom, janitor, dplyr, lubridate, forcats, glue, usethis, curl)


# FUNCTIONS ---------------------------------------------------------------

prepare_taxa_data <- function(df, condition) {
    df %>% 
        filter({{condition}}) %>% 
        arrange(ordre, famille, genre) %>% 
        mutate(espece = fct_inorder(espece)) %>% 
        group_by(espece, latitude, longitude, annee) %>% 
        arrange(date_debut) %>% 
        slice(1) %>% 
        ungroup() %>% 
        select(libelle_jeu_donnees, observateur, determinateur, nom_scientifique_ref, nom_vernaculaire, regne, classe, ordre, famille, genre, espece, date_debut, latitude, longitude, niveau_precision_localisation, commune, departement, id_sinp_occtax, annee)
}


# DATA IMPORT--------------------------------------------------------------

exportFile <- "records-2020-11-21"

if (!file.exists(here("dev/rawdata", paste0(exportFile, ".csv")))) {
    curl_download(
        url = "https://www.dropbox.com/s/1utoj7dqiocnrgf/records-2020-11-21.zip?raw=1", 
        destfile = here("dev/rawdata", paste0(exportFile, ".zip")), 
        mode = "wb"
    )
    
    unzip(zipfile = here("dev/rawdata", paste0(exportFile, ".zip")),
          exdir = here("dev/rawdata"), overwrite = TRUE)
} 

AllSpeciesRaw <- here("dev/rawdata", paste0(exportFile, ".csv")) %>% 
        vroom()


# DATA PREPARATION --------------------------------------------------------

AllSpecies <- AllSpeciesRaw %>% 
    clean_names() %>% 
    mutate(annee = date_debut %>% 
               as_date() %>% 
               year()) %>% 
    filter(
        across(c(longitude, latitude), function(x) {!is.na(x)}),
        annee >= 2000
    ) %>% 
    mutate(
        niveau_precision_localisation = factor(
            niveau_precision_localisation,
            levels = paste0("XY ", c("point", "centroïde ligne/polygone", "centroïde commune"))
            )) %>%
    select(where(~ !(all(is.na(.))))) %>% 
    mutate(ordre = if_else(
        famille == "Planorbidae",
        "Hygrophila",
        ordre
    )
    )

# DATA EXPORT -------------------------------------------------------------

## INSECTS
insects <- AllSpecies %>%
    prepare_taxa_data(condition = classe == "Hexapoda")

## BIRDS
birds <- AllSpecies %>% 
    prepare_taxa_data(condition = (classe == "Aves" & annee >= 2018))

## MAMMALS
mammals <- AllSpecies %>% 
    prepare_taxa_data(classe == "Mammalia") 

## FISH
fish <- AllSpecies %>% 
    prepare_taxa_data(classe %in% c("Actinopterygii", "Petromyzonti"))

## REPTILES AND AMPHIBIANS
reptiles <- AllSpecies %>% 
    prepare_taxa_data(condition = (classe == "Amphibia" | 
                                       (is.na(classe) & ordre %in% c("Chelonii", "Squamata"))))

## MOLLUSCS AND CRUSTACEANS
molluscs <- AllSpecies %>% 
    prepare_taxa_data(classe %in% c("Bivalvia", "Gastropoda", "Malacostraca"))

use_data(insects, birds, mammals, fish, reptiles, molluscs,
         internal = TRUE, overwrite = TRUE)
