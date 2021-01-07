#' @importFrom dplyr filter count group_by summarise arrange desc mutate left_join select transmute
#' @importFrom stringr str_remove_all str_to_sentence
generate_taxalist <- function(data, limits, taxa) {
    liste <- data %>% 
        filter(
            longitude >= limits$west,
            longitude <= limits$east,
            latitude >= limits$south,
            latitude <= limits$north
        ) %>% 
        (function(df) {
            if (!is.null(taxa)) {
                filter(df,
                       (ordre %in% taxa) |
                           (espece %in% taxa))
            } else {
                df
            }
        }) %>% 
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
                "<a href='", DOC_URL, "' target='_blank'>", FULL_CITATION, "</a>"
            )
        )
    
    liste %>% 
        left_join(
            protection,
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
            `Texte réglementaire` = lien
        )
}
