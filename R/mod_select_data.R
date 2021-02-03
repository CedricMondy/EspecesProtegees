#' choose_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @import shiny
mod_select_data_ui <- function(id){
  ns <- NS(id)
  tagList(
    selectInput(
      inputId = ns("group"),
      label = "",
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
#' @noRd 
#' @import shiny
mod_select_data_server <- function(id){
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      reactive({
        req(input$group)
        
        CustomLabel <- switch(
            input$group,
            birds = "oiseaux",
            fish = "poissons",
            insects = "insectes",
            mammals = "mammifères",
            molluscs = "crustacés et mollusques",
            reptiles = "reptiles et amphibiens"
          )
        
        output$GetData <- renderUI({
          downloadButton(
            outputId = ns("download"),
            label = paste0(
              "Télécharger toutes les données ",
              CustomLabel
            ), 
            class = "ui blue button"
            )
        })
        
        switch(
          input$group,
          birds    = birds,
          fish     = fish,
          insects  = insects,
          mammals  = mammals,
          molluscs = molluscs,
          reptiles = reptiles
          ) %>% 
          add_species_colors()
        
        })
      }
  )
}
    
