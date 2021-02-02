#' select_period UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @import shiny
mod_select_period_ui <- function(id, min_year, max_year){
  ns <- NS(id)
  tagList(
    sliderInput(
      inputId = ns("year"),
      label = "",
      value = c(min_year, max_year),
      min   = min_year,
      max   = max_year,
      sep   = "",
      ticks = FALSE
    )
  )
}
    
#' select_period Server Function
#'
#' @import shiny
#' @noRd 
#' @importFrom dplyr pull
mod_select_period_server <- function(id, raw_data){
  moduleServer(
    id,
    function(input, output, session){
      observeEvent(raw_data(),
                   {
                     if (nrow(raw_data()) > 0) {
                       min_year <- min(pull(raw_data(), annee))
                     max_year <- max(pull(raw_data(), annee))
      
      updateSliderInput(
                       session,
                       inputId = "year",
                       value = c(min_year, max_year),
                       min = min_year,
                       max = max_year
                     )
      
                     }
                   }
      )
      
      reactive(input$year)
      
    }
  )
}
    
