#' generate_observationlist UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom DT DTOutput
mod_generate_observationlist_ui <- function(id){
  ns <- NS(id)
  tagList(
    DTOutput(ns("ListeObservations"))
  )
}
    
#' generate_observationlist Server Function
#'
#' @noRd 
#' @importFrom shiny moduleServer reactive req
#' @importFrom dplyr filter select
#' @importFrom DT datatable renderDT
mod_generate_observationlist_server <- function(id, donnees, limites, taxa){
  moduleServer(
    id,
    function(input, output, session){
      liste_observations <- reactive({
        req(donnees(), limites(), taxa)
        
        liste <- donnees() %>% 
          filter(
            longitude >= limites()$west,
            longitude <= limites()$east,
            latitude >= limites()$south,
            latitude <= limites()$north
          ) %>% 
          (function(df) {
            if (!is.null(taxa())) {
              filter(df,
                     (ordre %in% taxa()) |
                       (espece %in% taxa()))
            } else {
              df
            }
          }) %>% 
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
        
        
        liste %>% 
          datatable(
            filter = 'top',
            rownames = FALSE,
            options = list(
              dom = 'itlp',
              scrollX = TRUE, 
              autoWidth = TRUE
            )
          )
      })
      
      output$ListeObservations <- renderDT(liste_observations())
    }
  )
}
    
