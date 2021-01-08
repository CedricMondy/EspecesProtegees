#' @importFrom dplyr filter count group_by summarise arrange desc mutate left_join select transmute
#' @importFrom stringr str_remove_all str_to_sentence
#' @importFrom tidyr pivot_wider
generate_taxalist <- function(data, limits, taxa) {
    liste <- data %>% 
        filter_limits(limits = limits) %>% 
        filter_taxa(taxa = taxa) %>% 
        count(ordre, famille, espece, nom_vernaculaire, 
              url_inpn, url_ofb) %>% 
        (function(df) {
            n_ordre <- group_by(df, ordre) %>% 
                summarise(n = sum(n),
                          .groups = "drop") %>% 
                arrange(desc(n))
            
            n_famille <- group_by(df, ordre, famille) %>%
                summarise(n = sum(n),
                          .groups = "drop") %>% 
                mutate(ordre = factor(ordre, levels = n_ordre$ordre)) %>% 
                arrange(ordre, desc(n))
            
            df %>% 
                mutate(
                    ordre = factor(ordre, levels = n_ordre$ordre),
                    famille = factor(famille, levels = n_famille$famille)
                ) %>% 
                arrange(ordre, famille, desc(n))
        }) %>% 
        mutate(
            inpn = paste0("<a href='", url_inpn, "' target='_blank'>INPN</a>"),
            ofb = ifelse(
                !is.na(url_ofb),
                paste0(" | <a href='", url_ofb, "' target='_blank'>OFB</a>"),
                ""
            )
        ) %>% 
        mutate(
            info = paste0(inpn, ofb)
        ) 
    
    protection <- left_join(
        select(liste, espece),
        filter(status, REGROUPEMENT_TYPE == "Protection"),
        by = c("espece" = "LB_NOM")
    ) %>% 
        mutate(LB_TYPE_STATUT = str_remove_all(
            string = LB_TYPE_STATUT,
            pattern = "Protection "
        ) %>% 
            str_to_sentence()
        ) %>% 
        transmute(
            espece = espece,
            protection = LB_TYPE_STATUT,
            lien = paste0(
                "<a href='", 
                DOC_URL,
                "' target='_blank'>",
                FULL_CITATION, 
                "</a>"
            )
        )
    
    conservation <- left_join(
        select(liste, espece),
        filter(status, REGROUPEMENT_TYPE == "Liste rouge"),
        by = c("espece" = "LB_NOM")
        ) %>% 
        transmute(
            espece = espece,
            liste_rouge = LB_TYPE_STATUT,
            statut =  
                ifelse(
                    is.na(CODE_STATUT),
                    NA_character_,
                    paste0("<a href='",
                           DOC_URL,
                           "' target='_blank'>",
                           LABEL_STATUT, 
                           "</a>"
                           )
                    )
            ) %>% 
        mutate(
            liste_rouge = liste_rouge %>% 
                str_remove_all(
                    pattern = "Liste rouge "
                    ) %>% 
                str_to_sentence()
            ) %>%
        pivot_wider(
            id_cols = espece,
            names_from = liste_rouge,
            values_from = statut
        )
    
    liste %>% 
        left_join(
            protection,
            by = "espece"
        ) %>% 
        left_join(
            conservation,
            by = "espece"
        ) %>% 
        select(
            `Ordre`   = ordre,
            `Famille` = famille,
            `Espèce`  = espece,
            `Nom vernaculaire` = nom_vernaculaire,
            `Nombre d'observations` = n,
            `Fiche espèce` = info,
            `Protection` = protection,
            `Texte réglementaire` = lien,
            `Mondiale`,
            `Européenne`,
            `Nationale`,
            `Régionale`
        )
}
