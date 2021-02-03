#' generate_map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @import shiny 
#' @importFrom leaflet leafletOutput
#' @importFrom shiny.semantic checkbox_input
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
#' @import shiny 
#' @importFrom leaflet renderLeaflet
mod_generate_map_server <- function(id, donnees, taxa, limites_communes, grille_inpn){
  moduleServer(
    id,
    function(input, output, session){
      
      output$map <- renderLeaflet({
        generate_map(
          data = birds,
          limites_communes = limites_communes,
          grille_inpn = grille_inpn
        )
        })
      
      observe({
        req(donnees())
        
        update_map_scale("map", data = donnees())
        
      })
      
      observeEvent(input$richness, {
        observe({
          req(donnees, taxa)
        
          if (input$richness) {
            add_grid(
              "map",
              data = filter_taxa(data = donnees(), 
                                 taxa = taxa()),
              limites_communes = limites_communes,
              grille_inpn = grille_inpn
              )
            } else {
              clear_grid("map")
              }
        })
        
          })
      
      observe({
        req(donnees, taxa)
        
        data_map <- filter_taxa(data = donnees(), 
                                taxa = taxa())
        
        update_map(
          "map",
          data = data_map,
          limites_communes = limites_communes,
          grille_inpn = grille_inpn
          )
        
        
        })
      
      reactive(input$map_bounds)
    }
  )
  }
   
