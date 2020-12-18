#' choose_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList selectInput
mod_select_data_ui <- function(id){
  ns <- NS(id)
  tagList(
    selectInput(
      inputId = ns("group"),
      label = "Groupe taxonomique",
      choices = list(
        "Oiseaux"                 = "birds",
        "Mammifères"              = "mammals",
        "Reptiles & amphibiens"   = "reptiles",
        "Poissons"                = "fish",
        "Insectes"                = "insects",
        "Crustacés et mollusques" = "molluscs"
      ),
      multiple = FALSE
    )
  )
}
    
#' choose_data Server Function
#'
#' @importFrom shiny reactive req moduleServer
#' @importFrom leaflet colorFactor
#' @importFrom paletteer paletteer_d
#' @importFrom dplyr mutate
#' @noRd 
mod_select_data_server <- function(id){
  moduleServer(
    id,
    function(input, output, session) {
      reactive({
        req(input$group)
        
        raw_data <- switch(
          input$group,
          birds    = EspecesProtegees:::birds,
          fish     = EspecesProtegees:::fish,
          insects  = EspecesProtegees:::insects,
          mammals  = EspecesProtegees:::mammals,
          molluscs = EspecesProtegees:::molluscs,
          reptiles = EspecesProtegees:::reptiles
          )
        
        palOrdre <- colorFactor(
          palette = paletteer_d("RColorBrewer::Dark2") %>% 
            as.character(),
          domain = raw_data$ordre
          )
        
        raw_data %>% 
          mutate(color = palOrdre(ordre))
        })
      }
  )
}
    
