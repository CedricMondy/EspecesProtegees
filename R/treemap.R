#' @importFrom dplyr ungroup count bind_rows transmute group_by summarise 
#' @importFrom glue glue
#' @importFrom stringr str_wrap
prepare_treemap_data <- function(data) {
    data %>% 
        ungroup() %>% 
        count(ordre, color, espece, nom_vernaculaire) %>% 
        (function(df) {
            bind_rows(
                df %>% 
                    transmute(
                        parents = ordre,
                        labels = glue("<i>{espece}</i><br><br>{str_wrap(nom_vernaculaire, 30)}"),
                        ids = espece,
                        values = n,
                        color = color
                    ),
                df %>% 
                    group_by(ordre, color) %>% 
                    summarise(n = sum(n),
                              .groups = "drop") %>% 
                    transmute(labels = ordre,
                              parents = "&#8617; Retour à la vue initiale",
                              ids = ordre,
                              values = n,
                              color = color)
            )
        })
}

#' @importFrom plotly plot_ly layout
#' @importFrom dplyr rowwise mutate
generate_treemap <- function(data, ...) {
    
    data %>% 
        rowwise() %>% 
        mutate(
            customdata = list(
                list(
                    id     = ids,
                    parent = parents
                    )
                )
            ) %>% 
        plot_ly(
            ids          = ~ids,
            labels       = ~labels,
            parents      = ~parents,
            values       = ~values,
            customdata   = ~customdata,
            marker       = list(colors = .$color),
            type         = "treemap",
            branchvalues = "total",
            ...
        ) %>%
        plotly::layout(
            margin = list(
                l = 0,
                r = 0,
                b = 0,
                t = 20,
                pad = 0
            )
        )
    
}

update_click <- function(click, new_click) {
    if (!is.null(click)) {
        if (new_click$id == click) {
            if (new_click$parent == "&#8617; Retour à la vue initiale") {
                new_value <- NULL
            } else {
                new_value <- new_click$parent
            }
        } else {
            new_value <- new_click$id
        }
    } else {
        new_value <- new_click$id
    }
    new_value
}

