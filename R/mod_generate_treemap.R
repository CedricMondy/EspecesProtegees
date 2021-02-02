#' generate_treemap UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @import shiny
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
#' @import shiny
#' @importFrom plotly renderPlotly layout event_data
mod_generate_treemap_server <- function(id, donnees, limites, titre = ""){
  moduleServer(
    id,
    function(input, output, session){
      click <- reactiveVal()
      
      observe({
        req(donnees(), limites())
        
        output$treemap <- renderPlotly({
          donnees() %>% 
            filter_limits(limits = limites()) %>% 
            prepare_treemap_data() %>% 
            generate_treemap(source = "treemap") %>% 
            layout(title = list(
              text = titre, 
              x = 0, xanchor = "left", 
              pad = list(t = 0, r = 0, l = 5, b = 5),
              font = list(size = 14)
              ),
                   margin = list(t = 35))
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
