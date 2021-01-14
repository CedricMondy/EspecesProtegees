#' @importFrom dplyr select
generate_observationlist <- function(data) {
    data %>% 
        select(
            `Nom vernaculaire` = nom_vernaculaire,
            `Ordre`   = ordre,
            `Famille` = famille,
            `Espèce`  = espece,
            `Date` = date_debut,
            `Commune` = commune,
            `Département` = departement,
            `Précision géographique` = niveau_precision_localisation,
            `Longitude` = longitude,
            `Latitude` = latitude,
            `Observateur (structure)` = observateur,
            `Jeu de données` = libelle_jeu_donnees,
            `ID SINP occtax` = id_sinp_occtax
        )
}
