#' Title
#'
#' @param fichier 
#' @param code_region 
#' @param colonne
#' @param simplifier 
#'
#' @return
#' @export
#'
#' @examples
preparer_adminexpress <- function(fichier, code_region, colonne = NULL, simplifier = FALSE, crs = 2154) {
    fichier %>% 
        sf::st_read() %>% 
        dplyr::filter(INSEE_REG %in% code_region) %>% 
        (function(df) {
            if (!is.null(colonne))
                df <- df %>% 
                    dplyr::select(!!colonne)
            
            if (simplifier)
                df <- df %>% 
                    rmapshaper::ms_simplify()
            
            df
        }) %>% 
        sf::st_transform(crs = crs)
}


#' Title
#'
#' @param fichier_grille_inpn 
#' @param limites_region 
#'
#' @return
#' @export
#'
#' @examples
preparer_inpn <- function(fichier_grille_inpn, limites_region) {
    fichier_grille_inpn %>% 
            sf::st_read() %>% 
            sf::st_join(limites_region) %>% 
            dplyr::filter(!is.na(ID)) %>% 
            dplyr::select(ID = CODE_10KM)
}
