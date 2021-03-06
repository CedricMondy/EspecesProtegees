#' @importFrom dplyr select matches count across all_of group_by summarise arrange desc mutate
generate_taxalist <- function(data) {
    cols <- select(data,
                   ordre, famille, espece, nom_vernaculaire, 
                   lien_fiche, niveau_protection, lien_protection, 
                   matches("^(liste rouge).+_lien$")) %>% 
        colnames()
    
    data %>% 
        count(across(all_of(cols))) %>% 
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
        select(
            `Ordre`   = ordre,
            `Famille` = famille,
            `Espèce`  = espece,
            `Nom vernaculaire` = nom_vernaculaire,
            `Nombre d'observations` = n,
            `Fiche espèce` = lien_fiche,
            `Protection` = niveau_protection,
            `Texte réglementaire` = lien_protection,
            `Mondiale` = `liste rouge mondiale_lien`,
            `Européenne` = `liste rouge europeenne_lien`,
            `Nationale` = `liste rouge nationale_lien`,
            `Régionale` = `liste rouge regionale_lien`
        )
}
