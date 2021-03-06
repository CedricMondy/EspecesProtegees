
# PACKAGES ----------------------------------------------------------------
dependencies <- c("import", "here", "vroom", "dplyr", "usethis")
#use function defined in install_dependencies.R
install_dependencies(dependencies)

library(EspecesProtegees)

# FUNCTIONS ---------------------------------------------------------------

import::from(here, here)
import::from(vroom, vroom, locale)
import::from(dplyr, `%>%`, mutate, case_when)
import::from(sf, st_transform)
import::from(usethis, use_data)

# DATA IMPORT--------------------------------------------------------------
## Observations -----
## requête Openobs: https://openobs.mnhn.fr/openobs-hub/occurrences/search?q=(protection%3A%22true%22)%20AND%20(state%3A%C3%8Ele-de-France)%20AND%20(sensitive%3A%22XY%20point%22%20OR%20sensitive%3A%22XY%20centro%C3%AFde%20ligne%2Fpolygone%22%20OR%20sensitive%3A%22XY%20centro%C3%AFde%20commune%22%20OR%20sensitive%3A%22XY%20centro%C3%AFde%20maille%22)#tab_mapView
observations <- read_from_zip(
    zipfile = here("dev/rawdata/records-IdF-2021-02-15.zip"),
    file = ".csv",
    fun = vroom
)

## TAXREF ----
taxref <- read_from_zip(
    zipfile = here("dev/rawdata/TAXREF_v14_2020.zip"),
    file = "TAXREFv14.txt",
    fun = vroom
)

## Statuts ----
statuts <- read_from_zip(
    zipfile = here("dev/rawdata/BDC-Statuts-v14.zip"),
    file = ".csv",
    fun = vroom, locale = locale(encoding = 'WINDOWS-1252')
)

## Fiches OFB ----
fiches_ofb <- here("dev/fiches_ofb_especes_protegees.csv") %>% 
    vroom()

## Limites géographiques ----
### données ADMIN-EXPRESS Décembre 2020: ftp://Admin_Express_ext:Dahnoh0eigheeFok@ftp3.ign.fr/ADMIN-EXPRESS_2-4__SHP__FRA_WM_2020-12-15.7z
### Limites région ----
LimitesRegion <- read_from_zip(
    zipfile = here("dev/rawdata/ADMIN-EXPRESS_2-4__SHP__FRA_WM_2020-12-15.7z"),
    file = "REGION.shp",
    fun = preparer_adminexpress, 
    code_region = 11, crs = 2154
)

### Limites départements ----
LimitesDepartements <- read_from_zip(
    zipfile = here("dev/rawdata/ADMIN-EXPRESS_2-4__SHP__FRA_WM_2020-12-15.7z"),
    file = "DEPARTEMENT.shp",
    fun = preparer_adminexpress, 
    code_region = 11, colonne = "NOM_DEP", 
    simplifier = TRUE, crs = 2154
)

### Limites communes ----
LimitesCommunes <- read_from_zip(
    zipfile = here("dev/rawdata/ADMIN-EXPRESS_2-4__SHP__FRA_WM_2020-12-15.7z"),
    file = "COMMUNE.shp",
    fun = preparer_adminexpress,
    code_region = 11, colonne = "ID", simplifier = TRUE,
    crs = 2154
)

### Grille 10km x 10km INPN ----
### https://inpn.mnhn.fr/docs/Shape/L93_10K.zip
GrilleINPN <- read_from_zip(
    zipfile = here("dev/rawdata/L93_10K.zip"),
    file = ".shp",
    fun = preparer_inpn,
    limites_region = LimitesRegion
)

# DATA PREPARATION --------------------------------------------------------

taxref <- taxref %>% 
    mutate(
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
    taxref = taxref,
    zones_administratives = idf
    )
   
protections <- preparer_protection(
    statuts, 
    taxref = taxref,
    zones_administratives = idf
    )

fiches_ofb <- preparer_fiches(fiches_ofb, taxref)

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
        condition = (classe == "Aves" & annee >= 2017),
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

LimitesDepartements <- LimitesDepartements %>% 
    st_transform(crs = 4326)
LimitesCommunes <- LimitesCommunes %>% 
    st_transform(crs = 4326)
GrilleINPN <- GrilleINPN %>% 
    st_transform(crs = 4326)

# DATA EXPORT -------------------------------------------------------------

use_data(insects, birds, mammals, fish, reptiles, molluscs,
         LimitesDepartements, LimitesCommunes, GrilleINPN,
         internal = TRUE, overwrite = TRUE)
