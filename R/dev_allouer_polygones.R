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
#' @importFrom dplyr bind_rows filter mutate
#' @importFrom sf st_transform st_join st_drop_geometry
allouer_polygones <- function(df, limites_communes, grille_inpn) {
    
    bind_rows(
        df %>% 
            filter(precision %in% c("point", "ligne/polygone")) %>% 
            mutate(ID = paste0(longitude, "-", latitude)),
        df %>% 
            filter(precision == "commune") %>% 
            inpn_to_sf() %>% 
            st_transform(crs = 2154) %>% 
            st_join(limites_communes) %>% 
            st_drop_geometry(),
        df %>% 
            filter(precision == "maille") %>% 
            inpn_to_sf() %>% 
            st_transform(crs = 2154) %>% 
            st_join(grille_inpn) %>% 
            st_drop_geometry()
    )
}
