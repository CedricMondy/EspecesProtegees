#' generate_chronic UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom plotly plotlyOutput
mod_generate_chronic_ui <- function(id){
  ns <- NS(id)
  tagList(
    p("Nombre d'observations"),
    plotlyOutput(ns("chronic"),
               height = "300px")
  )
}
    
#' generate_chronic Server Function
#'
#' @noRd 
#' @importFrom shiny moduleServer req
#' @importFrom plotly renderPlotly
mod_generate_chronic_server <- function(id, data, years){
  moduleServer(
    id,
    function(input, output, session){
      output$chronic <- renderPlotly({
        req(data(), years())
        
        generate_chronic(data(), years())
      })
    }
  )
}
