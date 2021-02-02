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

#' @importFrom dplyr filter
filter_departments <- function(data, departments) {
    df <- data
    
    if (all(!is.null(departments)))
        df <- df %>% 
            filter(departement %in% departments)
    
    df
}

#' @importFrom dplyr pull filter
#' @importFrom sf st_transform st_intersection st_bbox st_as_sfc
filter_limits <- function(data, limits) {
    extract_polygon_ids <- function(polygons, bbox) {
        polygons %>% 
            st_transform(crs = 2154) %>% 
            st_intersection(bbox) %>% 
            pull(ID)
    }
    
    if (!is.null(limits)) {
        bbox <- st_bbox(c(
            xmin = limits$west, xmax = limits$east,
            ymin = limits$south, ymax = limits$north
        ),
        crs = 4326) %>% 
            st_as_sfc() %>% 
            st_transform(crs = 2154)
        
        communes <- extract_polygon_ids(LimitesCommunes, bbox)
        mailles <- extract_polygon_ids(GrilleINPN, bbox)
        
        data %>% 
            filter(
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

#' @importFrom dplyr filter
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
