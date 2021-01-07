#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom dplyr filter
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
  donnees <- mod_select_data_server(id = "donnees")
  annees <- mod_select_period_server(
    id = "periode",
    raw_data = donnees
  )
  departements <- mod_select_server(id = "departements")
  precisions <- mod_select_server(id = "precisions")
  
  DonneesFiltrees <- reactive({
    req(donnees)

      filter_data(
        data         = donnees(),
        annees       = annees(),
        departements = departements(),
        precisions   = precisions())
  })

  taxa <- mod_generate_treemap_server(
    id = "treemap",
    donnees = DonneesFiltrees,
    limites = limites
  )

  limites <- mod_generate_map_server(
    id = "carte",
    donnees = DonneesFiltrees,
    taxa = taxa
  )
  
  mod_generate_taxalist_server(
    id = "especes",
    donnees = DonneesFiltrees,
    limites = limites,
    taxa    = taxa
  )
  
  mod_generate_observationlist_server(
    id = "observations",
    donnees = DonneesFiltrees,
    limites = limites,
    taxa    = taxa
  )
  
}
