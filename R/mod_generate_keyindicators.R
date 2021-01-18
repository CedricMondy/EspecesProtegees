#' generate_keyindicators UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList htmlOutput
#' @importFrom shiny.semantic card cards
mod_generate_keyindicators_ui <- function(id){
  ns <- NS(id)
  
  CardStyle <- "
  height: 45px;
  margin: 2px;
  display: flex;
  align-items: center;
  justify-content: center;
  text-align: center;
  background-color: #8DB6CD;
  "
  
  tagList(
    div(),
    cards(
      class = "one",
      card(htmlOutput(ns("ordre")), style = CardStyle),
      card(htmlOutput(ns("famille")), style = CardStyle),
      card(htmlOutput(ns("espece")), style = CardStyle),
      card(htmlOutput(ns("observations")), style = CardStyle)
    )
    
  )
}
    
#' generate_keyindicators Server Function
#'
#' @noRd 
#' @importFrom shiny moduleServer observe req renderText
mod_generate_keyindicators_server <- function(id, donnees){
  moduleServer(
    id,
    function(input, output, session){
      observe({
        req(donnees())
        
        indicators <- generate_keyindicators(donnees())
        
        output$ordre <- renderText(indicators$n_ordre)
        output$famille <- renderText(indicators$n_famille)
        output$espece <- renderText(indicators$n_espece)
        output$observations <- renderText(indicators$n_obs)
      })
    }
  )
}
