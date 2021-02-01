#' Title
#'
#' @param numbers 
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom stringr str_sub
format_ID <- function(numbers) {
    n_char <- nchar(max(numbers))
    
    formatted_numbers <- paste0(
        paste(rep(0, 20), collapse = ""),
        numbers
    ) %>% 
        stringr::str_sub(
            string = .,
            start = nchar(.) - (n_char - 1),
            end = nchar(.)
        )
}

#' Title
#'
#' @param taille_maille 
#' @param zone 
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom dplyr mutate n
#' @importFrom sf st_make_grid st_as_sf
preparer_grille <- function(zone, taille_maille) {
    zone %>% 
        sf::st_make_grid(
            cellsize = taille_maille,
            square = FALSE
        ) %>% 
        sf::st_as_sf() %>% 
        dplyr::mutate(
            ID = paste0(taille_maille, format_ID(seq(dplyr::n())))
            ) %>% 
        (function(df) {
            colnames(df)[colnames(df) == "ID"] <- paste0("ID_", taille_maille)
            df
        })
}
