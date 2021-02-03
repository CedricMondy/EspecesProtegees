#' Title
#'
#' @param df 
#' @param limites_communes 
#' @param grille_inpn 
#'
#' @return
#' @export
#'
#' @examples
allouer_polygones <- function(df, limites_communes, grille_inpn) {
    
    dplyr::bind_rows(
        df %>% 
            dplyr::filter(precision %in% c("point", "ligne/polygone")) %>% 
            dplyr::mutate(ID = paste0(longitude, "-", latitude)),
        df %>% 
            dplyr::filter(precision == "commune") %>% 
            inpn_to_sf() %>% 
            sf::st_transform(crs = 2154) %>% 
            sf::st_join(limites_communes) %>% 
            sf::st_drop_geometry(),
        df %>% 
            dplyr::filter(precision == "maille") %>% 
            inpn_to_sf() %>% 
            sf::st_transform(crs = 2154) %>% 
            sf::st_join(grille_inpn) %>% 
            sf::st_drop_geometry()
    )
}
