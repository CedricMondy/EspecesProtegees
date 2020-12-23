#' generate_treemap UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom plotly plotlyOutput
mod_generate_treemap_ui <- function(id){
  ns <- NS(id)
  tagList(
    plotlyOutput(ns("treemap"))
  )
}
    
#' generate_treemap Server Function
#'
#' @noRd 
#' 
#' @importFrom shiny moduleServer observe reactive req reactiveVal observeEvent
#' @importFrom plotly renderPlotly event_data
#' @importFrom dplyr filter
mod_generate_treemap_server <- function(id, donnees, limites){
  moduleServer(
    id,
    function(input, output, session){
      click <- reactiveVal()
      
      observe({
        req(donnees(), limites())
        
        output$treemap <- renderPlotly({
          donnees() %>% 
            filter(
              longitude >= limites()$west,
              longitude <= limites()$east,
              latitude >= limites()$south,
              latitude <= limites()$north
            ) %>% 
            prepare_treemap_data() %>% 
            generate_treemap(source = "treemap")
        })
      })

      new_click <- reactive({
        event_data(
          event  = "plotly_click", 
          source = "treemap",
          priority = "event"
        )$customdata
      })      
      
      observeEvent(
        new_click(),
        {
          click(
            update_click(
              click     = click(),
              new_click = new_click()
            )
          )
          }
        )
      
      observe({
        if (is.null(new_click()))
          click(NULL)
      })
      
      observeEvent(
        limites(),
        {
        click(NULL)
          })
      
      reactive(click())
    }
  )
}
