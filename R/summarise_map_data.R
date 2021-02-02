#' @importFrom dplyr distinct
summarise_map_data <- function(data){
    data %>% 
        distinct(departement, commune, precision, ID, longitude, latitude,
                 ordre, colorOrdre, espece, nom_vernaculaire, color) 
}
