#' select_period UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList sliderInput 
#' @importFrom lubridate year
mod_select_period_ui <- function(id){
  ns <- NS(id)
  tagList(
    sliderInput(
      inputId = ns("year"),
      label = "Années",
      value = c(2000, year(Sys.Date())),
      min   = 2000,
      max   = year(Sys.Date()),
      sep   = "",
      ticks = FALSE
    )
  )
}
    
#' select_period Server Function
#'
#' @importFrom shiny moduleServer observeEvent reactive updateSliderInput
#' @importFrom dplyr pull
#' @noRd 
mod_select_period_server <- function(id, raw_data){
  moduleServer(
    id,
    function(input, output, session){
      observeEvent(raw_data(),
                   {
        
      updateSliderInput(
                       session,
                       inputId = "year",
                       value = c(2000, max(pull(raw_data(), annee))),
                       min = min(pull(raw_data(), annee)),
                       max = max(pull(raw_data(), annee))
                     )
      
      })
      reactive(input$year)
      
    }
  )
}
    
