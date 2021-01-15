#' select_precision UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_select_ui <- function(id, placeholder, choices){
  select_choices <- c("", choices)
  
  if (length(names(choices)) == 0) {
    names(select_choices) <- c(placeholder, rep("", length(choices)))
  } else {
    names(select_choices) <- c(placeholder, names(choices))
  }
    
  ns <- NS(id)
  tagList(
    selectInput(
      inputId = ns("select"),
      label = "",
      choices = select_choices,
      selected = "",
      multiple = TRUE
    )
  )
}
    
#' select_precision Server Function
#'
#' @importFrom shiny reactive moduleServer
#' @noRd 
mod_select_server <- function(id){
  moduleServer(
    id,
    function(input, output, session){
      reactive({
        input$select
      })
    }
  )
}
    
