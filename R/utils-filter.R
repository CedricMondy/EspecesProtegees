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
    df <- data
    
    if (!is.null(limits))
        df <- df %>% 
            filter(
                longitude >= limits$west,
                longitude <= limits$east,
                latitude >= limits$south,
                latitude <= limits$north
                )
    
    df
}

filter_taxa <- function(data, taxa) {
    df <- data
    
    if (!is.null(taxa))
        df <- df %>% 
            filter(
                (ordre %in% taxa) |
                    (espece %in% taxa)
            )
    
    df
}
