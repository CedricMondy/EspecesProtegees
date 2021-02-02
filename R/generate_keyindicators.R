#' @importFrom dplyr summarise n_distinct n mutate
#' @importFrom glue glue
generate_keyindicators <- function(data) {
    FormatTaxaNumber <- function(n, level) {
        glue("<b>{n}</b> {ifelse(n == 1, level, paste0(level, 's'))}")
    }
    
    resume <- data %>% 
        summarise(
            n_ordre = n_distinct(ordre),
            n_famille = n_distinct(famille),
            n_espece = n_distinct(espece),
            n_obs = n()
        ) %>% 
        mutate(
            n_ordre = FormatTaxaNumber(n_ordre, "ordre"),
            n_famille = FormatTaxaNumber(n_famille, "famille"),
            n_espece = FormatTaxaNumber(n_espece, "espèce"),
            n_obs = FormatTaxaNumber(n_obs, "observation")
        )
}
