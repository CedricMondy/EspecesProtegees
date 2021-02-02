#' Title
#'
#' @param df 
#' @param zones_administratives 
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom dplyr filter mutate group_by slice ungroup distinct left_join
#' @importFrom glue glue
#' @importFrom janitor clean_names
#' @importFrom stringr str_extract
#' @importFrom tidyr pivot_wider
preparer_uicn <- function(df, zones_administratives) {
    df %>%
        filter(LB_ADM_TR %in% zones_administratives) %>% 
        clean_names() %>% 
        filter(regroupement_type == "Liste rouge") %>% 
        mutate(statut = factor(label_statut,
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
        # Gère les statuts multiples pour une même espèce et une même liste
        mutate(annee = full_citation %>% 
                   str_extract(pattern = "\\d{4}")) %>% 
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
}

#' Title
#'
#' @param cd_ref 
#' @param taxref 
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom dplyr filter select left_join tibble bind_rows mutate
#' @importFrom glue glue
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

#' Title
#'
#' @param df 
#' @param zones_administratives 
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom dplyr filter mutate distinct
#' @importFrom glue glue
#' @importFrom janitor clean_names
#' @importFrom stringr str_remove_all str_to_sentence
preparer_protection <- function(df, zones_administratives) {
    df %>%
        filter(
            LB_ADM_TR %in% zones_administratives
        ) %>% 
        clean_names() %>% 
        filter(regroupement_type == "Protection") %>% 
        mutate(niveau_protection = lb_type_statut %>% 
                   str_remove_all(pattern = "Protection ") %>% 
                   str_to_sentence()) %>% 
        mutate(lien_protection = glue("<a href='{doc_url}' target='_blank'>{full_citation}</a>")) %>% 
        distinct(lb_nom, niveau_protection, lien_protection)
}

#' Title
#'
#' @param df 
#' @param taxref 
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom dplyr left_join select bind_cols mutate
#' @importFrom glue glue
preparer_fiches <- function(df, taxref) {
    df %>% 
        left_join(select(taxref, LB_NOM, CD_REF), 
                         by = c("espece" = "LB_NOM")) %>% 
        select(-espece) %>% 
        bind_cols(find_taxonomy(cd_ref = .$CD_REF, taxref = taxref)) %>% 
        select(espece, url_ofb = lien_fiche_ofb) %>% 
        mutate(lien_ofb = glue("<a href='{url_ofb}' target='_blank'>OFB</a>"))
}

#' Title
#'
#' @param df 
#' @param condition 
#' @param taxref 
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom dplyr mutate filter across select bind_cols
#' @importFrom janitor clean_names
#' @importFrom lubridate as_date year
#' @importFrom stringr str_remove_all
preparer_observations <- function(df, condition, taxref) {
    df %>% 
        clean_names() %>% 
        mutate(annee = date_debut %>% 
                   as_date() %>% 
                   year()) %>% 
        filter(
            across(c(longitude, latitude), function(x) {!is.na(x)}),
            (annee >= 2000 & annee <= 2020)
        ) %>% 
        mutate(
            precision = niveau_precision_localisation %>% 
                str_remove_all(pattern = "XY ") %>% 
                str_remove_all(pattern = "centro.{1}de ")
            ) %>%
        select(where(~ !(all(is.na(.))))) %>% 
        filter({{ condition }}) %>%
        select(libelle_jeu_donnees, observateur, determinateur, cd_ref,
               annee, date_debut,
               latitude, longitude, precision, 
               commune, departement, id_sinp_occtax) %>% 
        bind_cols(find_taxonomy(.$cd_ref, taxref)) %>%
        select(-cd_ref) 
}

#' Title
#'
#' @param df 
#' @param fiches 
#' @param protection 
#' @param conservation 
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom dplyr left_join mutate across starts_with select
#' @importFrom glue glue
#' @importFrom stringr str_replace_na
ajouter_information_especes <- function(df, fiches, protection, conservation){
    df %>% 
        left_join(fiches_ofb, by = "espece") %>% 
        left_join(protection, by = c("espece" = "lb_nom")) %>% 
        left_join(conservation, by = c("espece" = "lb_nom")) %>% 
        mutate(lien_fiche = glue("{lien_inpn}{ifelse(!is.na(lien_ofb), paste0(' | ', lien_ofb), '')}")) %>% 
        mutate(
            across(
                starts_with("Liste rouge"),
                str_replace_na, replacement = "Non évaluée"
                )
            ) %>% 
        select(-lien_inpn, -lien_ofb, -url_inpn, -url_ofb)
}
