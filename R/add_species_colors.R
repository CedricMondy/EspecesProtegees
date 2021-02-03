#' @importFrom dplyr mutate distinct group_by group_split n_distinct left_join
#' @importFrom leaflet colorFactor
#' @importFrom paletteer paletteer_d
#' @importFrom purrr map_df
#' @importFrom tinter tinter
add_species_colors <- function(data){
    palOrdre <- colorFactor(
        palette = paletteer_d("RColorBrewer::Dark2") %>% 
            as.character(),
        domain = unique(data$ordre)
    )
    
    data <- data %>% 
        mutate(colorOrdre = palOrdre(ordre)) 
    
    colorSpecies <- data %>% 
        distinct(ordre, espece, colorOrdre) %>% 
        group_by(ordre) %>%
        group_split() %>% 
        map_df(function(df) {
            df %>% 
                mutate(
                    color = tinter(
                        x = unique(df$colorOrdre),
                        direction = "tints",
                        steps = 2 * n_distinct(df$espece),
                        crop = 2
                    ) %>% 
                        sample(n_distinct(df$espece))
                )
        }) 
    
    data %>% 
        left_join(colorSpecies, by = c("ordre", "colorOrdre", "espece"))
}
