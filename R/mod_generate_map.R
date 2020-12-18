#' generate_map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom leafgl leafglOutput
mod_generate_map_ui <- function(id){
  ns <- NS(id)
  tagList(
    leafglOutput(ns("map"))
  )
}
    
#' generate_map Server Function
#'
#' @noRd 
#' @importFrom shiny moduleServer observe req reactive
#' @importFrom leafgl renderLeafgl
mod_generate_map_server <- function(id, donnees){
  moduleServer(
    id,
    function(input, output, session){
      output$map <- generate_map() %>% 
        renderLeafgl()
      
      observe({
        req(donnees)
        
        update_map("map",
                   data = donnees())
      })
      
      reactive(input$map_bounds)
    }
  )
}
   
