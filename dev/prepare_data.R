
# PACKAGES ----------------------------------------------------------------
dependencies <- c("import", "here", "vroom", "janitor", "dplyr", "lubridate", "glue", "usethis", "stringr", "tidyr", "tibble", "tidyselect")
#use function defined in install_dependencies.R
install_dependencies(dependencies)

# FUNCTIONS ---------------------------------------------------------------

import::from(here, here)
import::from(vroom, vroom)
import::from(janitor, clean_names)
import::from(magrittr, `%>%`)
import::from(tibble, tibble)
import::from(dplyr, filter, select, left_join, bind_rows, bind_cols, mutate, group_by, arrange, slice, ungroup, case_when, across, distinct)
import::from(glue, glue)
import::from(lubridate, as_date, year)
import::from(stringr, str_extract, str_remove_all, str_to_sentence, str_replace_na)
import::from(tidyr, pivot_wider)
import::from(usethis, use_data)

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
        mutate(ordre = ifelse(
            famille == "Planorbidae",
            "Hygrophila",
            ordre
        ),
        lien_inpn = glue("<a href='{url_inpn}' target='_blank'>INPN</a>")
        )
}

prepare_taxa_data <- function(df, condition, taxref, fiches_ofb, protection, conservation) {
    df %>% 
        filter({{condition}}) %>% 
        select(libelle_jeu_donnees, observateur, determinateur, cd_ref, date_debut, latitude, longitude, niveau_precision_localisation, commune, departement, id_sinp_occtax, annee) %>% 
        bind_cols(find_taxonomy(.$cd_ref, taxref)) %>%
        select(-cd_ref) %>% 
        group_by(espece, latitude, longitude, annee) %>% 
        arrange(date_debut) %>% 
        slice(1) %>% 
        ungroup() %>% 
        left_join(fiches_ofb, by = "espece") %>% 
        left_join(protection, by = c("espece" = "lb_nom")) %>% 
        left_join(conservation, by = c("espece" = "lb_nom")) %>% 
        mutate(lien_fiche = glue("{lien_inpn}{ifelse(!is.na(lien_ofb), paste0(' | ', lien_ofb), '')}"),
               `Liste rouge mondiale` = str_replace_na(
                   string = `Liste rouge mondiale`,
                   replacement = "Non évaluée"
               ),
               `Liste rouge européenne` = str_replace_na(
                   string = `Liste rouge européenne`,
                   replacement = "Non évaluée"
               ),
               `Liste rouge nationale` = str_replace_na(
                   string = `Liste rouge nationale`,
                   replacement = "Non évaluée"
               ),
               `Liste rouge régionale` = str_replace_na(
                   string = `Liste rouge régionale`,
                   replacement = "Non évaluée"
               )) %>% 
        select(-lien_inpn, -lien_ofb, -url_inpn, -url_ofb)
}

# DATA IMPORT--------------------------------------------------------------
## Observations -----
## requête Obenops: https://openobs.mnhn.fr/openobs-hub/occurrences/search?q=(protection%3A%22true%22)%20AND%20(state%3A%C3%8Ele-de-France)%20AND%20(sensitive%3A%22XY%20point%22%20OR%20sensitive%3A%22XY%20centro%C3%AFde%20ligne%2Fpolygone%22%20OR%20sensitive%3A%22XY%20centro%C3%AFde%20commune%22%20OR%20sensitive%3A%22XY%20centro%C3%AFde%20maille%22)#tab_mapView
exportFile <- "records-2021-01-06"

AllSpeciesRaw <- here("dev/rawdata", exportFile, glue("{exportFile}.csv")) %>% 
    vroom()

## TAXREF ----
taxref <- here("dev/rawdata/TAXREF_v14_2020/TAXREFv14.txt") %>% 
    vroom()

## Statuts ----
statuts <- here("dev/rawdata/BDC-Statuts-v13/BDC_STATUTS_13.csv") %>% 
    vroom()

## Fiches OFB ----
fiches_ofb <- here("dev/fiches_ofb_especes_protegees.csv") %>% 
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
departements_idf <- c(
    "Paris",
    "Seine-et-Marne",
    "Yvelines",
    "Essonne",
    "Hauts-de-Seine",
    "Seine-Saint-Denis",
    "Val-de-Marne",
    "Val-d'Oise"
)

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
            levels = glue("XY {c('point', 'centroïde ligne/polygone', 'centroïde commune', 'centroïde maille')}")
            ),
        departement = factor(
            departement,
            levels = departements_idf
        )) %>%
    select(where(~ !(all(is.na(.)))))

status <- statuts %>%
    filter(
        LB_ADM_TR %in% c("France", "France métropolitaine",
                         "Ile-de-France",
                         departements_idf)
    )

uicn <- status %>% 
    clean_names() %>% 
    filter(regroupement_type == "Liste rouge") %>% 
    # Gère les statuts multiples pour une même espèce et une même liste
    mutate(annee = full_citation %>% 
               str_extract(pattern = "\\d{4}"),
           statut = factor(label_statut,
                           levels = c(
                               "Non applicable",
                               "Non évaluée",
                               "Données insuffisantes",
                               "Préoccupation mineure",
                               "Quasi menacée",
                               "Vulnérable",
                               "En danger",
                               "En danger critique",
                               "Disparue au niveau régional",
                               "On ne sait pas si l'espèce n'est pas éteinte ou disparue",
                               "Eteinte à l'état sauvage",
                               "Eteinte au niveau mondial"
                           ))) %>% 
    mutate(statut_num = as.numeric(statut)) %>% 
    group_by(lb_nom, lb_type_statut) %>% 
    # ne conserve que la liste rouge la plus récente
    filter(annee == max(annee)) %>% 
    # ne conserve que l'évaluation la plus préoccupante
    filter(statut_num == max(statut_num)) %>% 
    slice(1) %>% 
    ungroup() %>% 
    mutate(lien_uicn = glue("<a href='{doc_url}' target='_blank'>{statut}</a>")) %>%
    distinct(lb_nom, lb_type_statut, statut, lien_uicn) %>% 
    (function(df) {
        pivot_wider(
                 df,
                 id_cols = lb_nom,
                 names_from = lb_type_statut,
                 values_from = statut,
                 values_fill = "Non évaluée"
                 ) %>% 
            left_join(
                pivot_wider(
                    df,
                    id_cols = lb_nom,
                    names_from = lb_type_statut,
                    values_from = lien_uicn
                    ),
                by = "lb_nom",
                suffix = c("", "_lien")
                )
    })
   

protections <- status %>% 
    clean_names() %>% 
    filter(regroupement_type == "Protection") %>% 
    mutate(niveau_protection = lb_type_statut %>% 
               str_remove_all(pattern = "Protection ") %>% 
               str_to_sentence()) %>% 
    mutate(lien_protection = glue("<a href='{doc_url}' target='_blank'>{full_citation}</a>")) %>% 
    distinct(lb_nom, niveau_protection, lien_protection)

fiches_ofb <- fiches_ofb %>% 
    left_join(select(taxref, LB_NOM, CD_REF), 
              by = c("espece" = "LB_NOM")) %>% 
    select(-espece) %>% 
    bind_cols(find_taxonomy(cd_ref = .$CD_REF, taxref = taxref)) %>% 
    select(espece, url_ofb = lien_fiche_ofb) %>% 
    mutate(lien_ofb = glue("<a href='{url_ofb}' target='_blank'>OFB</a>"))

## INSECTS
insects <- AllSpecies %>%
    prepare_taxa_data(condition = classe == "Hexapoda",
                      taxref = taxref,
                      fiches_ofb = fiches_ofb,
                      protection = protections,
                      conservation = uicn)

## BIRDS
birds <- AllSpecies %>% 
    prepare_taxa_data(condition = (classe == "Aves" & annee >= 2018),
                      taxref = taxref,
                      fiches_ofb = fiches_ofb,
                      protection = protections,
                      conservation = uicn)

## MAMMALS
mammals <- AllSpecies %>% 
    prepare_taxa_data(classe == "Mammalia",
                      taxref = taxref,
                      fiches_ofb = fiches_ofb,
                      protection = protections,
                      conservation = uicn) 

## FISH
fish <- AllSpecies %>% 
    prepare_taxa_data(classe %in% c("Actinopterygii", "Petromyzonti"),
                      taxref = taxref,
                      fiches_ofb = fiches_ofb,
                      protection = protections,
                      conservation = uicn)

## REPTILES AND AMPHIBIANS
reptiles <- AllSpecies %>% 
    prepare_taxa_data(condition = (classe == "Amphibia" | 
                                       (is.na(classe) & ordre %in% c("Chelonii", "Squamata"))),
                      taxref = taxref,
                      fiches_ofb = fiches_ofb,
                      protection = protections,
                      conservation = uicn)

## MOLLUSCS AND CRUSTACEANS
molluscs <- AllSpecies %>% 
    prepare_taxa_data(classe %in% c("Bivalvia", "Gastropoda", "Malacostraca"),
                      taxref = taxref,
                      fiches_ofb = fiches_ofb,
                      protection = protections,
                      conservation = uicn)

# DATA EXPORT -------------------------------------------------------------

use_data(insects, birds, mammals, fish, reptiles, molluscs,
         internal = TRUE, overwrite = TRUE)
