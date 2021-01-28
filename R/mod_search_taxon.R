#' search_taxon UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList selectInput
mod_search_taxon_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    selectInput(
       inputId = ns("taxon"),
       label = "",
       choices = search_taxon(birds)
    )
  )
}
    
#' search_taxon Server Function
#'
#' @noRd 
#' @importFrom dplyr filter
#' @importFrom shiny moduleServer observeEvent updateSelectInput reactive
mod_search_taxon_server <- function(id, data){
  moduleServer(
    id,
    function(input, output, session){

      observeEvent(data(),
                   {
                     updateSelectInput(
                       session = session,
                       inputId = "taxon",
                       choices = search_taxon(data())
                       )
                     })
      
      reactive({
        if (input$taxon != "") {
          data() %>% 
          filter(
            nom_vernaculaire %in% input$taxon |
              espece %in% input$taxon |
              famille %in% input$taxon |
              ordre %in% input$taxon
              
          )
        } else {
          data()
        }
        
      })
      
    }
  )
}
