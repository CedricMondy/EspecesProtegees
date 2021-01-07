#' generate_taxalist UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList uiOutput 
#' @importFrom DT DTOutput
mod_generate_taxalist_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(outputId = ns("GetData")),
    DTOutput(ns("ListeEspece"))
    )
}
    
#' generate_taxalist Server Function
#'
#' @noRd 
#' @importFrom shiny moduleServer reactive req renderUI downloadButton downloadHandler
#' @importFrom DT datatable renderDT
mod_generate_taxalist_server <- function(id, donnees, limites, taxa){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      
      liste_especes <- reactive({
        req(donnees(), limites(), taxa)
        
        generate_taxalist(
          data = donnees(),
          limits = limites(),
          taxa = taxa()
        )
      })
      
      output$GetData <- renderUI({
        downloadButton(
          outputId = ns("download"),
          label = paste0(
            "Télécharger ",
            nrow(liste_especes()),
            " espèces"
          ), 
          class = "ui blue button"
        )
      })
      
      output$ListeEspece <- renderDT({
        liste_especes() %>% 
          datatable(
            filter = 'top',
            rownames = FALSE,
            escape = FALSE,
            options = list(
              dom = 'tlp',
              scrollX = TRUE, 
              autoWidth = TRUE
            )
          )
        }
        )
      
      output$download <- downloadHandler(
        filename = "especes_affichees.csv",
        content = function(file) {
          write.csv2(
            x = liste_especes(),
            file = file, 
            row.names = FALSE,
            fileEncoding = "UTF-8"
          )
        }
      )
      
    }
  )
}
