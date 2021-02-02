
# PACKAGES ----------------------------------------------------------------
dependencies <- c("import", "here", "vroom", "dplyr", "usethis")
#use function defined in install_dependencies.R
install_dependencies(dependencies)

library(EspecesProtegees)

# FUNCTIONS ---------------------------------------------------------------

import::from(here, here)
import::from(vroom, vroom)
import::from(dplyr, `%>%`, mutate, case_when)
import::from(sf, st_transform)
import::from(usethis, use_data)

# DATA IMPORT--------------------------------------------------------------
## Observations -----
## requête Obenops: https://openobs.mnhn.fr/openobs-hub/occurrences/search?q=(protection%3A%22true%22)%20AND%20(state%3A%C3%8Ele-de-France)%20AND%20(sensitive%3A%22XY%20point%22%20OR%20sensitive%3A%22XY%20centro%C3%AFde%20ligne%2Fpolygone%22%20OR%20sensitive%3A%22XY%20centro%C3%AFde%20commune%22%20OR%20sensitive%3A%22XY%20centro%C3%AFde%20maille%22)#tab_mapView
observations <- here("dev/rawdata/records-2021-01-06/records-2021-01-06.csv") %>% 
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

## Limites géographiques ----
### Limites région ----
### données ADMIN-EXPRESS Décembre 2020: ftp://Admin_Express_ext:Dahnoh0eigheeFok@ftp3.ign.fr/ADMIN-EXPRESS_2-4__SHP__FRA_WM_2020-12-15.7z
LimitesRegion <- here("dev/rawdata/ADMIN-EXPRESS_2-4__SHP__FRA_2020-12-15/ADMIN-EXPRESS/1_DONNEES_LIVRAISON_2020-12-15/ADE_2-4_SHP_LAMB93_FR/REGION.shp") %>% 
    preparer_region(code_region = 11)

### Limites communes ----
### données ADMIN-EXPRESS Décembre 2020: ftp://Admin_Express_ext:Dahnoh0eigheeFok@ftp3.ign.fr/ADMIN-EXPRESS_2-4__SHP__FRA_WM_2020-12-15.7z
LimitesCommunes <- here("dev/rawdata/ADMIN-EXPRESS_2-4__SHP__FRA_2020-12-15/ADMIN-EXPRESS/1_DONNEES_LIVRAISON_2020-12-15/ADE_2-4_SHP_LAMB93_FR/COMMUNE.shp") %>% 
    preparer_communes(code_region = 11)


### Grille 10km x 10km INPN ----
### https://inpn.mnhn.fr/docs/Shape/L93_10K.zip
GrilleINPN <- here("dev/rawdata/L93_10K/L93_10X10.shp") %>% 
    preparer_inpn(limites_region = LimitesRegion)

# DATA PREPARATION --------------------------------------------------------
## Corrections ----
observations <- observations %>% 
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

## Préparation des infromations complémentaires ----
idf <-  c(
    "France", "France métropolitaine",
    "Ile-de-France", 
    "Paris",
    "Seine-et-Marne",
    "Yvelines",
    "Essonne",
    "Hauts-de-Seine",
    "Seine-Saint-Denis",
    "Val-de-Marne",
    "Val-d'Oise"
)

uicn <- preparer_uicn(
    statuts,
    zones_administratives = idf
    )
   
protections <- preparer_protection(
    statuts, 
    zones_administratives = idf
    )

fiches_ofb <- preparer_fiches(fiches_ofb, taxref)

## Grille hexagonale ----
grille_5km <- preparer_grille(LimitesRegion, 5000)

## INSECTS
insects <- observations %>%
    preparer_observations(
        condition = classe == "Hexapoda",
        taxref = taxref
        ) %>% 
    ajouter_information_especes(fiches_ofb, protections, uicn) %>% 
    allouer_polygones(
        limites_communes = LimitesCommunes,
        grille_inpn = GrilleINPN
        )

## BIRDS
birds <- observations %>% 
    preparer_observations(
        condition = (classe == "Aves" &
                         annee >= 2018),
        taxref = taxref
        ) %>% 
    ajouter_information_especes(fiches_ofb, protections, uicn) %>% 
    allouer_polygones(
        limites_communes = LimitesCommunes,
        grille_inpn = GrilleINPN
    )

## MAMMALS
mammals <- observations %>% 
    preparer_observations(
        classe == "Mammalia",
        taxref = taxref
        ) %>% 
    ajouter_information_especes(fiches_ofb, protections, uicn) %>% 
    allouer_polygones(
        limites_communes = LimitesCommunes,
        grille_inpn = GrilleINPN
    )

## FISH
fish <- observations %>% 
    preparer_observations(
        classe %in% c("Actinopterygii", "Petromyzonti"),
        taxref = taxref
        ) %>% 
    ajouter_information_especes(fiches_ofb, protections, uicn) %>% 
    allouer_polygones(
        limites_communes = LimitesCommunes,
        grille_inpn = GrilleINPN
    )

## REPTILES AND AMPHIBIANS
reptiles <- observations %>% 
    preparer_observations(
        condition = (classe == "Amphibia" | 
                         (is.na(classe) & 
                              ordre %in% c("Chelonii", "Squamata"))),
        taxref = taxref
        ) %>% 
    ajouter_information_especes(fiches_ofb, protections, uicn) %>% 
    allouer_polygones(
        limites_communes = LimitesCommunes,
        grille_inpn = GrilleINPN
    )

## MOLLUSCS AND CRUSTACEANS
molluscs <- observations %>% 
    preparer_observations(
        classe %in% c("Bivalvia", "Gastropoda", "Malacostraca"),
        taxref = taxref
        ) %>% 
    ajouter_information_especes(fiches_ofb, protections, uicn) %>% 
    allouer_polygones(
        limites_communes = LimitesCommunes,
        grille_inpn = GrilleINPN
    )

LimitesCommunes <- LimitesCommunes %>% 
    st_transform(crs = 4326)
GrilleINPN <- GrilleINPN %>% 
    st_transform(crs = 4326)

# DATA EXPORT -------------------------------------------------------------

use_data(insects, birds, mammals, fish, reptiles, molluscs,
         LimitesCommunes, GrilleINPN,
         internal = TRUE, overwrite = TRUE)
