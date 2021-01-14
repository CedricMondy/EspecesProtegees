#' @importFrom dplyr select distinct mutate count case_when arrange desc group_by group_split bind_rows slice
#' @importFrom tidyr pivot_longer
#' @importFrom glue glue
#' @importFrom purrr map set_names
#' @importFrom htmltools HTML
#' @importFrom plotly plot_ly add_pie layout
#' @importFrom stringr str_wrap str_remove str_to_sentence
generate_redlists <- function(data) {

    if (nrow(data) > 0) {
        data_conservation <- data %>% 
            select(espece, contains("Liste rouge")) %>% 
            select(-contains("_lien")) %>% 
            distinct() %>% 
            pivot_longer(
                cols = -espece,
                names_to = 'conservation',
                values_to = 'statut'
            ) %>% 
            mutate(
                conservation = conservation %>% 
                    factor(levels = c(
                        "Liste rouge mondiale", 
                        "Liste rouge européenne", 
                        "Liste rouge nationale",
                        "Liste rouge régionale"
                    )
                    ),
                statut = factor(
                    statut,
                    levels = c(
                        "Non évaluée",
                        "Non applicable",
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
                    )
                )
            ) %>% 
            count(conservation, statut) %>% 
            mutate(color = case_when(
                statut %in% c("Non applicable", "Non évaluée") ~ "#FFFFFF",
                statut == "Données insuffisantes" ~ "#CCCCCC",
                statut == "Préoccupation mineure" ~ "#7CCD7C",
                statut == "Quasi menacée" ~ "#CDCD00",
                statut == "Vulnérable" ~ "#FFD700",
                statut == "En danger" ~ "#FF8C69",
                statut == "En danger critique" ~ "#FF0000",
                statut %in% c(
                    "Disparue au niveau régional",
                    "On ne sait pas si l'espèce n'est pas éteinte ou disparue",
                    "Eteinte à l'état sauvage"
                ) ~ "#68228B",
                statut == "Eteinte au niveau mondial" ~ "#000000"
            ),
            labs = glue("<b>{conservation}</b><br>{statut}: {n} espèces") %>% 
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
                        textinfo='none',
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
                        plotly::layout(
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
                        ) 
                }
            ) %>% 
            set_names(c("world", "continent", "country", "region")) 
    }
    
}
