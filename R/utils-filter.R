#' @importFrom dplyr filter
filter_years <- function(data, years) {
    df <- data
    
    if (all(!is.null(years)))
        df <- df %>% 
            filter(
                annee >= years[[1]] & annee <= years[[2]]
            )
    
    df
}

filter_departments <- function(data, departments) {
    df <- data
    
    if (all(!is.null(departments)))
        df <- df %>% 
            filter(departement %in% departments)
    
    df
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

add_missing_columns <- function(df, columns) {
    missing_columns <- columns[! columns %in% colnames(df)]
    
    df[, missing_columns] <- NA
    
    df
}
