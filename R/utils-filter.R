#' @importFrom dplyr filter
filter_data <- function(data, annees, departements, precisions) {
    data %>% 
        filter( annee >= annees[1] & annee <= annees[2] ) %>% 
        (function(df) {
            if (!is.null(departements))
                df <- df %>% 
                    filter(departement %in% departements)
            
            if (!is.null(precisions))
                df <- df %>% 
                    filter(niveau_precision_localisation %in% precisions)
            
            df
        })
}

filter_limits <- function(data, limits) {
    data %>% 
        filter(
            longitude >= limits$west,
            longitude <= limits$east,
            latitude >= limits$south,
            latitude <= limits$north
        )
}

filter_taxa <- function(data, taxa) {
    if (!is.null(taxa)) {
        data %>% 
            filter(
                (ordre %in% taxa) |
                    (espece %in% taxa)
            )
    } else {
        data
    }
}
