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
    extract_polygon_ids <- function(polygons, bbox) {
        polygons %>% 
            sf::st_transform(crs = 2154) %>% 
            sf::st_intersection(bbox) %>% 
            dplyr::pull(ID)
    }
    
    if (!is.null(limits)) {
        bbox <- sf::st_bbox(c(
            xmin = limits$west, xmax = limits$east,
            ymin = limits$south, ymax = limits$north
        ),
        crs = 4326) %>% 
            sf::st_as_sfc() %>% 
            sf::st_transform(crs = 2154)
        
        communes <- extract_polygon_ids(LimitesCommunes, bbox)
        mailles <- extract_polygon_ids(GrilleINPN, bbox)
        
        data %>% 
            dplyr::filter(
                (precision %in% c("point", "ligne/polygone") &
                     longitude >= limits$west &
                     longitude <= limits$east &
                     latitude >= limits$south &
                     latitude <= limits$north) |
                    (precision %in% c("commune", "maille") & 
                         ID %in% c(communes, mailles))
            )
    } else {
        data
    }
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
