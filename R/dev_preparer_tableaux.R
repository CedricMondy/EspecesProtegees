#' Title
#'
#' @param zipfile 
#' @param file 
#' @param fun 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
read_from_zip <- function(zipfile, file, fun, ...) {
    if (!require(archive))
        remotes::install_github("jimhester/archive")
    
    temp <- tempfile()
    
    archive::archive_extract(zipfile, dir = temp)
    
    filepath <- list.files(
        path = temp, 
        pattern = file, 
        full.names = TRUE, 
        recursive = TRUE
    )
    
    obj <- fun(filepath, ...)
    
    unlink(temp, recursive = TRUE)
    
    return(obj)
}

#' Title
#'
#' @param cd_nom 
#' @param taxref 
#'
#' @return
#' @export
#'
#' @examples
find_cdref <- function(cd_nom, taxref) {
    codes <- taxref$CD_REF
    names(codes) <- taxref$CD_NOM
    
    codes[as.character(cd_nom)]
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
find_species <- function(cd_ref, taxref) {
    selection <- taxref %>% 
        dplyr::filter(CD_NOM %in% cd_ref)
    
    selection_es <- dplyr::filter(selection, RANG == "ES")
    selection_sses <- dplyr::filter(selection, RANG == "SSES")
    selection_var <- dplyr::filter(selection, RANG == "VAR")
    
    selection_sses <- selection_sses %>%
        dplyr::distinct(CD_REF, CD_SUP) %>% 
        dplyr::left_join(dplyr::select(taxref, -CD_REF), 
                         by = c("CD_SUP" = "CD_NOM"))
    
    selection_var <- selection_var %>% 
        dplyr::distinct(CD_REF, CD_SUP) %>% 
        dplyr::left_join(dplyr::select(taxref, -CD_REF), 
                         by = c("CD_SUP" = "CD_NOM"))
    
    dplyr::left_join(
        dplyr::tibble(CD_REF = cd_ref),
        dplyr::bind_rows(
            selection_es,
            selection_sses,
            selection_var
        ),
        by = "CD_REF"
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
preparer_uicn <- function(df, taxref, zones_administratives) {
    set_levels <- function(x) {
        x %>% 
            unique() %>% 
            janitor::make_clean_names() %>%
            stringr::str_replace_all(pattern = "_", replacement = " ") %>% 
            purrr::set_names(unique(x))
    }
    
    df_uicn <- df %>%
        dplyr::mutate(CD_REF = find_cdref(CD_NOM, taxref)) %>% 
        dplyr::filter(LB_ADM_TR %in% zones_administratives) %>% 
        janitor::clean_names() %>% 
        dplyr::filter(regroupement_type == "Liste rouge") %>% 
        dplyr::distinct(cd_ref, lb_type_statut, label_statut, full_citation, doc_url) %>% 
        dplyr::bind_cols(
            find_species(cd_ref = .$cd_ref, taxref = taxref)
        )
        
    
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
        dplyr::group_by(espece, lb_type_statut) %>% 
        # ne conserve que la liste rouge la plus récente
        dplyr::filter(annee == max(annee)) %>% 
        # ne conserve que l'évaluation la plus préoccupante
        dplyr::filter(statut_num == max(statut_num)) %>% 
        dplyr::slice(1) %>% 
        dplyr::ungroup() %>% 
        dplyr::mutate(lien_uicn = glue::glue("<a href='{doc_url}' target='_blank'>{statut}</a>"),
               statut = as.character(statut)) %>%
        dplyr::distinct(espece, lb_type_statut, statut, lien_uicn) %>% 
        (function(df) {
            tidyr::pivot_wider(
                df,
                id_cols = espece,
                names_from = lb_type_statut,
                values_from = statut,
                values_fill = "non evaluee"
            ) %>% 
                dplyr::left_join(
                    tidyr::pivot_wider(
                        df,
                        id_cols = espece,
                        names_from = lb_type_statut,
                        values_from = lien_uicn
                    ),
                    by = "espece",
                    suffix = c("", "_lien")
                )
        })
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
preparer_protection <- function(df, taxref, zones_administratives) {
    df %>%
        dplyr::mutate(CD_REF = find_cdref(CD_NOM, taxref)) %>% 
        dplyr::filter(
            LB_ADM_TR %in% zones_administratives
        ) %>% 
        janitor::clean_names() %>% 
        dplyr::filter(regroupement_type == "Protection") %>% 
        dplyr::distinct(cd_ref, lb_type_statut, full_citation, doc_url) %>% 
        dplyr::bind_cols(
            find_species(cd_ref = .$cd_ref, taxref = taxref)
        ) %>% 
        dplyr::mutate(niveau_protection = lb_type_statut %>% 
                   stringr::str_remove_all(pattern = "Protection ") %>% 
                   stringr::str_to_sentence()) %>% 
        dplyr::mutate(lien_protection = glue::glue("<a href='{doc_url}' target='_blank'>{full_citation}</a>")) %>% 
        dplyr::distinct(espece, niveau_protection, lien_protection)
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
        dplyr::bind_cols(find_species(cd_ref = .$CD_REF, taxref = taxref)) %>% 
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
preparer_observations <- function(df, condition, taxref, period = c(2000, 2020)) {
    df %>% 
        janitor::clean_names() %>% 
        dplyr::mutate(annee = date_debut %>% 
                   lubridate::as_date() %>% 
                   lubridate::year()) %>% 
        dplyr::filter(
            dplyr::across(c(longitude, latitude), function(x) {!is.na(x)}),
            (annee >= period[1] & annee <= period[2])
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
        dplyr::mutate(cd_ref = find_cdref(cd_ref, taxref)) %>% 
        dplyr::bind_cols(find_species(.$cd_ref, taxref)) 
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
        dplyr::left_join(protection, by = "espece") %>% 
        dplyr::left_join(conservation, by = "espece") %>% 
        dplyr::mutate(lien_fiche = glue::glue("{lien_inpn}{ifelse(!is.na(lien_ofb), paste0(' | ', lien_ofb), '')}")) %>% 
        dplyr::mutate(
            dplyr::across(
                dplyr::starts_with("Liste rouge"),
                stringr::str_replace_na, replacement = "non evaluee"
                )
            ) %>% 
        dplyr::select(-lien_inpn, -lien_ofb, -url_inpn, -url_ofb)
}
