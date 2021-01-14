#' generate_redlists UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList fillRow
#' @importFrom htmltools p
#' @importFrom plotly plotlyOutput
mod_generate_redlists_ui <- function(id){
  ns <- NS(id)
  tagList(
    p(style = "padding:0px; margin:0px"),
    p("Listes rouges", style = "font-weight:bold;padding-left:5px; padding-bottom:0px; margin-bottom:0px"),
      fillRow(
      plotlyOutput(ns("uicnWorld")),
      plotlyOutput(ns("uicnContinent")),
      plotlyOutput(ns("uicnCountry")),
      plotlyOutput(ns("uicnRegion")),
      style = "padding: 0px; margin: 0px;"
    )
  )
}
    
#' generate_redlistVis Server Function
#'
#' @noRd 
#' @importFrom shiny reactive req
mod_generate_redlists_server <- function(id, donnees){
  moduleServer(
    id,
    function(input, output, session){
      redlists <- reactive({
        req(donnees())
        
        generate_redlists(donnees())
      })
      
      output$uicnWorld <- renderPlotly({
        req(redlists())
        
        redlists()$world
      })
      
      output$uicnContinent <- renderPlotly({
        req(redlists())

        redlists()$continent
      })
      
      output$uicnCountry <- renderPlotly({
        req(redlists())

        redlists()$country
      })
      
      output$uicnRegion <- renderPlotly({
        req(redlists())

        redlists()$region
      })
    }
  )
}
