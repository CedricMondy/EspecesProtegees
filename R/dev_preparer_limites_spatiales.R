#' Title
#'
#' @param fichier 
#' @param code_region 
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom dplyr filter
#' @importFrom sf st_read
preparer_region <- function(fichier, code_region) {
    fichier %>% 
        st_read() %>% 
        filter(INSEE_REG %in% code_region)
}


#' Title
#'
#' @param fichier_communes 
#' @param code_region 
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom dplyr filter select
#' @importFrom rmapshaper ms_simplify
#' @importFrom sf st_read
preparer_communes <- function(fichier_communes, code_region) {
        fichier_communes %>% 
            st_read() %>% 
            filter(INSEE_REG %in% code_region) %>% 
            select(ID) %>% 
            ms_simplify()
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
#' @importFrom dplyr filter select
#' @importFrom sf st_read st_join
preparer_inpn <- function(fichier_grille_inpn, limites_region) {
    fichier_grille_inpn %>% 
            st_read() %>% 
            st_join(limites_region) %>% 
            filter(!is.na(ID)) %>% 
            select(ID = CODE_10KM)
}
