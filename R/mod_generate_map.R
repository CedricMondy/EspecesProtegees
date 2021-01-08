#' generate_map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom leaflet leafletOutput
mod_generate_map_ui <- function(id){
  ns <- NS(id)
  tagList(
    leafletOutput(ns("map"))
  )
}
    
#' generate_map Server Function
#'
#' @noRd 
#' @importFrom shiny moduleServer observe req reactive
#' @importFrom leaflet renderLeaflet
mod_generate_map_server <- function(id, donnees, taxa){
  moduleServer(
    id,
    function(input, output, session){
      output$map <- renderLeaflet(generate_map() )
      
      observe({
        req(donnees, taxa)
        
        data_map <- filter_taxa(data = donnees(), taxa = taxa())
        
        update_map("map", data = data_map)
      })
      
      reactive(input$map_bounds)
    }
  )
}
   
