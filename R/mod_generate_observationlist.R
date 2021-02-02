#' generate_observationlist UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @import shiny
#' @importFrom DT DTOutput
mod_generate_observationlist_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(outputId = ns("GetData")),
    DTOutput(ns("ListeObservations"))
  )
}
    
#' generate_observationlist Server Function
#'
#' @noRd 
#' @import shiny
#' @importFrom DT renderDT datatable
mod_generate_observationlist_server <- function(id, donnees){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns

      liste_observations <- reactive({
        req(donnees())
        
        generate_observationlist(data = donnees())
      })
      
      output$ListeObservations <- renderDT({
        liste_observations() %>% 
          datatable(
            filter = 'top',
            rownames = FALSE,
            options = list(
              dom = 'tlp',
              scrollX = TRUE, 
              autoWidth = TRUE
            )
          )
        }
      )
      
      output$GetData <- renderUI({
        downloadButton(
          outputId = ns("download"),
          label = paste0(
            "Télécharger ",
            nrow(liste_observations()),
            " observations"
          ), 
          class = "ui blue button"
        )
      })
      
      output$download <- downloadHandler(
        filename = "observations_affichees.csv",
        content = function(file) {
            write.csv2(
              x = liste_observations(),
              file = file, 
              row.names = FALSE,
              fileEncoding = "UTF-8"
              )
        }
      )
      
    }
  )
}
    
