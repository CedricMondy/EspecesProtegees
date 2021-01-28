#' generate_map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList  
#' @importFrom shiny.semantic checkbox_input
#' @importFrom leaflet leafletOutput
mod_generate_map_ui <- function(id){
  ns <- NS(id)
  tagList(
    leafletOutput(ns("map")),
    checkbox_input(
      input_id = ns("richness"),
      label = "Calculer la richesse par maille de 10km",
      type = "toggle",
      is_marked = FALSE
    )
  )
}
    
#' generate_map Server Function
#'
#' @noRd 
#' @importFrom shiny moduleServer observe observeEvent req reactive
#' @importFrom leaflet renderLeaflet
mod_generate_map_server <- function(id, donnees, taxa){
  moduleServer(
    id,
    function(input, output, session){
      output$map <- renderLeaflet(generate_map() )
      
      observe({
        req(donnees())
        
        update_map_scale("map", data = donnees())
        
      })
      
      observe({
        req(donnees, taxa)
        
        data_map <- filter_taxa(data = donnees(), 
                                taxa = taxa())
        
        update_map("map", data = data_map)
        
        observeEvent(input$richness, {
          if (input$richness) {
            add_hexagons("map", data_map)
            } else {
              clear_hexagons("map")
              }
          })
        })
      
      reactive(input$map_bounds)
    }
  )
  }
   
