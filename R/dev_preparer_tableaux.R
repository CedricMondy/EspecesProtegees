#' Title
#'
#' @param df 
#' @param zones_administratives 
#'
#' @return
#' @export
#'
#' @examples
preparer_uicn <- function(df, zones_administratives) {
    set_levels <- function(x) {
        x %>% 
            unique() %>% 
            janitor::make_clean_names() %>%
            stringr::str_replace_all(pattern = "_", replacement = " ") %>% 
            purrr::set_names(unique(x))
    }
    
    df_uicn <- df %>%
        dplyr::filter(LB_ADM_TR %in% zones_administratives) %>% 
        janitor::clean_names() %>% 
        dplyr::filter(regroupement_type == "Liste rouge")
    
    conservation_levels <- set_levels(df_uicn$lb_type_statut)
    statut_levels <- set_levels(df_uicn$label_statut)
        
    df_uicn  %>% 
        dplyr::mutate(
            lb_type_statut = conservation_levels[lb_type_statut],
            statut = statut_levels[label_statut] %>% 
                factor(levels = c(
                    "non evaluee",
                    "non applicable",
                    "donnees insuffisantes",
                    "preoccupation mineure",
                    "quasi menacee",
                    "vulnerable",
                    "en danger",
                    "en danger critique",
                    "disparue au niveau regional",
                    "on ne sait pas si lespece nest pas eteinte ou disparue",
                    "eteinte a letat sauvage",
                    "eteinte au niveau mondial"
                ))
            ) %>% 
        # Gère les statuts multiples pour une même espèce et une même liste
        dplyr::mutate(annee = full_citation %>% 
                   stringr::str_extract(pattern = "\\d{4}")) %>% 
        dplyr::mutate(statut_num = as.numeric(statut)) %>% 
        dplyr::group_by(lb_nom, lb_type_statut) %>% 
        # ne conserve que la liste rouge la plus récente
        dplyr::filter(annee == max(annee)) %>% 
        # ne conserve que l'évaluation la plus préoccupante
        dplyr::filter(statut_num == max(statut_num)) %>% 
        dplyr::slice(1) %>% 
        dplyr::ungroup() %>% 
        dplyr::mutate(lien_uicn = glue::glue("<a href='{doc_url}' target='_blank'>{statut}</a>"),
               statut = as.character(statut)) %>%
        dplyr::distinct(lb_nom, lb_type_statut, statut, lien_uicn) %>% 
        (function(df) {
            tidyr::pivot_wider(
                df,
                id_cols = lb_nom,
                names_from = lb_type_statut,
                values_from = statut,
                values_fill = "non evaluee"
            ) %>% 
                dplyr::left_join(
                    tidyr::pivot_wider(
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
find_taxonomy <- function(cd_ref, taxref) {
    selection <- taxref %>% 
        dplyr::filter(CD_NOM %in% cd_ref)
    
    selection_es <- dplyr::filter(selection, RANG == "ES")
    selection_sses <- dplyr::filter(selection, RANG == "SSES")
    
    selection_sses <- selection_sses %>%
        dplyr::select(CD_NOM, CD_SUP) %>% 
        dplyr::left_join(taxref, by = c("CD_SUP" = "CD_NOM"))
    
    dplyr::left_join(
        dplyr::tibble(CD_NOM = cd_ref),
        dplyr::bind_rows(
            selection_es,
            selection_sses
        ),
        by = "CD_NOM"
    ) %>% 
        dplyr::select(nom_vernaculaire = NOM_VERN,
               espece = LB_NOM,
               famille = FAMILLE,
               ordre = ORDRE,
               classe = CLASSE,
               url_inpn = URL)  %>% 
        dplyr::mutate(ordre = ifelse(
            famille == "Planorbidae",
            "Hygrophila",
            ordre
        ),
        lien_inpn = glue::glue("<a href='{url_inpn}' target='_blank'>INPN</a>")
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
preparer_protection <- function(df, zones_administratives) {
    df %>%
        dplyr::filter(
            LB_ADM_TR %in% zones_administratives
        ) %>% 
        janitor::clean_names() %>% 
        dplyr::filter(regroupement_type == "Protection") %>% 
        dplyr::mutate(niveau_protection = lb_type_statut %>% 
                   stringr::str_remove_all(pattern = "Protection ") %>% 
                   stringr::str_to_sentence()) %>% 
        dplyr::mutate(lien_protection = glue::glue("<a href='{doc_url}' target='_blank'>{full_citation}</a>")) %>% 
        dplyr::distinct(lb_nom, niveau_protection, lien_protection)
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
preparer_fiches <- function(df, taxref) {
    df %>% 
        dplyr::left_join(
            dplyr::select(taxref, LB_NOM, CD_REF), 
            by = c("espece" = "LB_NOM")) %>% 
        dplyr::select(-espece) %>% 
        dplyr::bind_cols(find_taxonomy(cd_ref = .$CD_REF, taxref = taxref)) %>% 
        dplyr::select(espece, url_ofb = lien_fiche_ofb) %>% 
        dplyr::mutate(lien_ofb = glue::glue("<a href='{url_ofb}' target='_blank'>OFB</a>"))
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
preparer_observations <- function(df, condition, taxref) {
    df %>% 
        janitor::clean_names() %>% 
        dplyr::mutate(annee = date_debut %>% 
                   lubridate::as_date() %>% 
                   lubridate::year()) %>% 
        dplyr::filter(
            dplyr::across(c(longitude, latitude), function(x) {!is.na(x)}),
            (annee >= 2000 & annee <= 2020)
        ) %>% 
        dplyr::mutate(
            precision = niveau_precision_localisation %>% 
                stringr::str_remove_all(pattern = "XY ") %>% 
                stringr::str_remove_all(pattern = "centro.{1}de ")
            ) %>%
        dplyr::select(where(~ !(all(is.na(.))))) %>% 
        dplyr::filter({{ condition }}) %>%
        dplyr::select(libelle_jeu_donnees, observateur, determinateur, cd_ref,
               annee, date_debut,
               latitude, longitude, precision, 
               commune, departement, id_sinp_occtax) %>% 
        dplyr::bind_cols(find_taxonomy(.$cd_ref, taxref)) %>%
        dplyr::select(-cd_ref) 
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
ajouter_information_especes <- function(df, fiches, protection, conservation){
    df %>% 
        dplyr::left_join(fiches_ofb, by = "espece") %>% 
        dplyr::left_join(protection, by = c("espece" = "lb_nom")) %>% 
        dplyr::left_join(conservation, by = c("espece" = "lb_nom")) %>% 
        dplyr::mutate(lien_fiche = glue::glue("{lien_inpn}{ifelse(!is.na(lien_ofb), paste0(' | ', lien_ofb), '')}")) %>% 
        dplyr::mutate(
            dplyr::across(
                dplyr::starts_with("Liste rouge"),
                stringr::str_replace_na, replacement = "non evaluee"
                )
            ) %>% 
        dplyr::select(-lien_inpn, -lien_ofb, -url_inpn, -url_ofb)
}
