
# PACKAGES ----------------------------------------------------------------
if (!require(pacman))
    install.packages("pacman")

pacman::p_load(here, purrr, vroom, janitor, dplyr, lubridate, forcats, glue, usethis, curl)


# FUNCTIONS ---------------------------------------------------------------
find_taxonomy <- function(cd_ref, taxref) {
    selection <- taxref %>% 
        filter(CD_NOM %in% cd_ref)
    
    selection_es <- filter(selection, RANG == "ES")
    selection_sses <- filter(selection, RANG == "SSES")
    
    selection_sses <- selection_sses %>%
        select(CD_NOM, CD_SUP) %>% 
        left_join(taxref, by = c("CD_SUP" = "CD_NOM"))
    
    left_join(
        tibble(CD_NOM = cd_ref),
        bind_rows(
            selection_es,
            selection_sses
        ),
        by = "CD_NOM"
    ) %>% 
        select(nom_vernaculaire = NOM_VERN,
               espece = LB_NOM,
               famille = FAMILLE,
               ordre = ORDRE,
               classe = CLASSE,
               url_inpn = URL)  %>% 
        mutate(ordre = if_else(
            famille == "Planorbidae",
            "Hygrophila",
            ordre
        )
        )
}

prepare_taxa_data <- function(df, condition, taxref) {
    df %>% 
        filter({{condition}}) %>% 
        select(libelle_jeu_donnees, observateur, determinateur, cd_ref, date_debut, latitude, longitude, niveau_precision_localisation, commune, departement, id_sinp_occtax, annee) %>% 
        bind_cols(find_taxonomy(.$cd_ref, taxref)) %>%
        group_by(espece, latitude, longitude, annee) %>% 
        arrange(date_debut) %>% 
        slice(1) %>% 
        ungroup() 
        
}


# DATA IMPORT--------------------------------------------------------------
## Observations -----
## requête Obenops: https://openobs.mnhn.fr/openobs-hub/occurrences/search?q=(protection%3A%22true%22)%20AND%20(state%3A%C3%8Ele-de-France)%20AND%20(sensitive%3A%22XY%20point%22%20OR%20sensitive%3A%22XY%20centro%C3%AFde%20ligne%2Fpolygone%22%20OR%20sensitive%3A%22XY%20centro%C3%AFde%20commune%22%20OR%20sensitive%3A%22XY%20centro%C3%AFde%20maille%22)#tab_mapView
exportFile <- "records-2021-01-06"

AllSpeciesRaw <- here("dev/rawdata", exportFile, paste0(exportFile, ".csv")) %>% 
        vroom()

## TAXREF ----
taxref <- here("dev/rawdata/TAXREF_v14_2020/TAXREFv14.txt") %>% 
    vroom()

# DATA PREPARATION --------------------------------------------------------
## Corrections ----
AllSpeciesRaw <- AllSpeciesRaw %>% 
    mutate(
        CdRef = case_when(
            # Salamandre tachetée
            CdNom == 111 ~ 965096,
            TRUE ~ CdRef
        )
    )

taxref <- taxref %>% 
    mutate(
        CD_SUP = case_when(
            # rattache Motacilla alba yarrellii à Motacilla alba
            CD_NOM == 3945 ~ 3941, 
            TRUE ~ CD_SUP
        ),
        NOM_VERN = case_when(
            CD_NOM == 77381 ~ "Cistude d'Europe (La)",
            CD_NOM == 444430 ~ "Triton alpestre (Le)",
            CD_NOM == 77619 ~ "Lézard vert, Lézard à deux raies (Le), Lézard vert occidental",
            CD_NOM == 444431 ~ "Triton ponctué (Le)",
            CD_NOM == 851674 ~ "Couleuvre helvétique, Couleuvre à collier, Couleuvre helvétique (La)",
            CD_NOM == 444440 ~ "Grenouille verte (La), Grenouille commune",
            CD_NOM == 444443 ~ "Grenouille rieuse (La)",
            CD_NOM == 8332 ~ "Cicindèle à labre noir",
            TRUE ~ NOM_VERN
        )
    )

## Préparation des données d'observation ----
AllSpecies <- AllSpeciesRaw %>% 
    clean_names() %>% 
    mutate(annee = date_debut %>% 
               as_date() %>% 
               year()) %>% 
    filter(
        across(c(longitude, latitude), function(x) {!is.na(x)}),
        (annee >= 2000 & annee <= 2020)
    ) %>% 
    mutate(
        niveau_precision_localisation = factor(
            niveau_precision_localisation,
            levels = paste0(
                "XY ", 
                c(
                    "point", 
                    "centroïde ligne/polygone",
                    "centroïde commune",
                    "centroïde maille"
                    )
                )
            )) %>%
    select(where(~ !(all(is.na(.)))))

## INSECTS
insects <- AllSpecies %>%
    prepare_taxa_data(condition = classe == "Hexapoda",
                      taxref = taxref)

## BIRDS
birds <- AllSpecies %>% 
    prepare_taxa_data(condition = (classe == "Aves" & annee >= 2018),
                      taxref = taxref)

## MAMMALS
mammals <- AllSpecies %>% 
    prepare_taxa_data(classe == "Mammalia",
                      taxref = taxref) 

## FISH
fish <- AllSpecies %>% 
    prepare_taxa_data(classe %in% c("Actinopterygii", "Petromyzonti"),
                      taxref = taxref)

## REPTILES AND AMPHIBIANS
reptiles <- AllSpecies %>% 
    prepare_taxa_data(condition = (classe == "Amphibia" | 
                                       (is.na(classe) & ordre %in% c("Chelonii", "Squamata"))),
                      taxref = taxref)

## MOLLUSCS AND CRUSTACEANS
molluscs <- AllSpecies %>% 
    prepare_taxa_data(classe %in% c("Bivalvia", "Gastropoda", "Malacostraca"),
                      taxref = taxref)

# DATA EXPORT -------------------------------------------------------------

use_data(insects, birds, mammals, fish, reptiles, molluscs,
         internal = TRUE, overwrite = TRUE)
