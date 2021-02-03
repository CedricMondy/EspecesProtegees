#' @importFrom dplyr select contains distinct mutate count case_when arrange desc group_by group_split bind_rows slice
#' @importFrom glue glue
#' @importFrom plotly plot_ly add_pie layout config
#' @importFrom purrr map set_names
#' @importFrom stringr str_wrap str_replace_all str_remove str_to_sentence
#' @importFrom tidyr pivot_longer
generate_redlists <- function(data) {

    conservation_levels <- c(
        `liste rouge mondiale` = "Liste rouge mondiale",
        `liste rouge europeenne` = "Liste rouge européenne",
        `liste rouge nationale` = "Liste rouge nationale",
        `liste rouge regionale` = "Liste rouge régionale"
    )
    
    statut_levels <- c(
         `non evaluee` = "Non évaluée",
         `non applicable` = "Non applicable",
         `donnees insuffisantes` = "Données insuffisantes",
         `preoccupation mineure` = "Préoccupation mineure",
         `quasi menacee` = "Quasi menacée",
         `vulnerable` = "Vulnérable",
         `en danger` = "En danger",
         `en danger critique` = "En danger critique",
         `disparue au niveau regional` = "Disparue au niveau régional",
         `on ne sait pas si lespece nest pas eteinte ou disparue` = "On ne sait pas si l'espèce n'est pas éteinte ou disparue",
         `eteinte a letat sauvage` = "Eteinte à l'état sauvage",
         `eteinte au niveau mondial` = "Eteinte au niveau mondial"
    )
    
    if (nrow(data) > 0) {
        data_conservation <- data %>% 
            select(espece, contains("liste rouge")) %>% 
            select(-contains("_lien")) %>% 
            distinct() %>% 
            pivot_longer(
                cols = -espece,
                names_to = 'conservation',
                values_to = 'statut'
            ) %>% 
            count(conservation, statut) %>% 
            mutate(
                conservation = conservation_levels[conservation] %>% 
                    factor(levels = conservation_levels),
                statut = statut_levels[statut] %>% 
                    factor(levels = statut_levels)) %>% 
            mutate(
                color = case_when(
                    statut %in% statut_levels[1:2] ~ "#FFFFFF",
                    statut == statut_levels[3] ~ "#CCCCCC",
                    statut == statut_levels[4] ~ "#7CCD7C",
                    statut == statut_levels[5] ~ "#CDCD00",
                    statut == statut_levels[6] ~ "#FFD700",
                    statut == statut_levels[7] ~ "#FF8C69",
                    statut == statut_levels[8] ~ "#FF0000",
                    statut %in% statut_levels[9:11] ~ "#68228B",
                    statut == statut_levels[12] ~ "#000000"
                ),
                nombre_especes = glue("{statut}: {n} espèces") %>% 
                    str_wrap(width = 25) %>% 
                    str_replace_all(pattern = "\n", replacement = "<br>"),
                labs = glue("<b>{conservation}</b><br>{nombre_especes}") %>% 
                    map(HTML)
                ) %>% 
            arrange(conservation, desc(statut))
        
        data_conservation %>% 
            group_by(conservation) %>% 
            group_split() %>% 
            map(
                function(df) {
                    df2 <- bind_rows(
                        slice(df, nrow(df)),
                        slice(df, -nrow(df))
                    )
                    
                    plot_ly(
                        data = df2,
                        labels = ~statut,
                        values = ~n,
                        sort = FALSE,
                        textinfo = 'none',
                        text = ~labs,
                        hoverinfo = 'text',
                        marker = list(
                            colors = df2$color,
                            line = list(color = "darkgrey", width = 1)
                        ),
                        height = 160
                    ) %>% 
                        add_pie(
                            hole = .6
                        ) %>% 
                        layout(
                            showlegend = FALSE,
                            annotations = list(
                                text = glue("<b>{str_remove(unique(df2$conservation), pattern = 'Liste rouge ') %>% str_to_sentence()}") %>% 
                                    str_wrap(width = 20),
                                showarrow = FALSE
                            ),
                            margin = list(
                                l = 10, r = 10,
                                b = 10, t = 10,
                                pad = 4
                            )
                        ) %>%
                        config(displayModeBar = FALSE) %>%
                        config(showLink = FALSE)
                }
            ) %>% 
            set_names(c("world", "continent", "country", "region")) 
    }
    
}
