#' @importFrom dplyr distinct mutate arrange pull
#' @importFrom purrr set_names
#' @importFrom tidyr pivot_longer
search_taxon <- function(data) {
    all_taxa <- data %>% 
        distinct(nom_vernaculaire, espece, famille, ordre) %>% 
        arrange(ordre, famille, espece) %>% 
        pivot_longer(
            cols = c("nom_vernaculaire", "espece", 
                     "famille", "ordre"),
            names_to = "niveau",
            values_to = "taxon"
        ) %>% 
        mutate(niveau = factor(niveau,
                               levels = c(
                                   "nom_vernaculaire", 
                                   "espece", 
                                   "famille",
                                   "ordre"
                               ))) %>% 
        arrange(niveau) %>% 
        pull(taxon)
    
    c("", all_taxa) %>% 
        set_names(c("Rechercher un taxon", rep("", length(all_taxa))))
}
