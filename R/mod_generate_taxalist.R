#' generate_taxalist UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @import shiny
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
#' @import shiny
#' @importFrom DT renderDT datatable
mod_generate_taxalist_server <- function(id, donnees){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      
      liste_especes <- reactive({
        req(donnees())
        
        generate_taxalist(data = donnees())
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
        entete <- withTags(
          table(
            class = 'display',
            thead(
              tr(
                th(rowspan = 2, 'Ordre'),
                th(rowspan = 2, 'Famille'),
                th(rowspan = 2, 'Espèce'),
                th(rowspan = 2, 'Nom vernaculaire'),
                th(rowspan = 2, "Nombre d'observations"),
                th(rowspan = 2, 'Fiche espèce'),
                th(colspan = 2, 'Protection'),
                th(colspan = 4, 'Liste rouge')
              ),
              tr(
                th('Echelle'),
                th('Texte réglementaire'),
                th('Mondiale'),
                th('Européenne'),
                th('Nationale'),
                th('Régionale')
              )
            )
          )
        )
          
        liste_especes() %>% 
          datatable(
            container = entete,
            filter = 'top',
            rownames = FALSE,
            escape = FALSE,
            options = list(
              dom = 'tlp',
              scrollX = TRUE, 
              autoWidth = TRUE,
              pageLength = 5
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
