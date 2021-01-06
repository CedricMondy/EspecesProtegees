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
#' @importFrom dplyr filter count select mutate arrange group_by summarise
#' @importFrom DT datatable renderDT
mod_generate_taxalist_server <- function(id, donnees, limites, taxa){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      
      liste_especes <- reactive({
        req(donnees(), limites(), taxa)
        
        donnees() %>% 
          filter(
            longitude >= limites()$west,
            longitude <= limites()$east,
            latitude >= limites()$south,
            latitude <= limites()$north
          ) %>% 
          (function(df) {
            if (!is.null(taxa())) {
              filter(df,
                     (ordre %in% taxa()) |
                       (espece %in% taxa()))
            } else {
              df
            }
          }) %>% 
          count(ordre, famille, espece, nom_vernaculaire, 
                url_inpn, url_ofb) %>% 
          (function(df) {
            n_ordre <- group_by(df, ordre) %>% 
              summarise(n = sum(n)) %>% 
              arrange(desc(n))
            
            n_famille <- group_by(df, ordre, famille) %>%
              summarise(n = sum(n)) %>% 
              mutate(ordre = factor(ordre, levels = n_ordre$ordre)) %>% 
              arrange(ordre, desc(n))
            
            df %>% 
              mutate(
                ordre = factor(ordre, levels = n_ordre$ordre),
                famille = factor(famille, levels = n_famille$famille)
              ) %>% 
              arrange(ordre, famille, desc(n))
          }) %>% 
          mutate(
            inpn = paste0("<a href='", url_inpn, "' target='_blank'>INPN</a>"),
            ofb = ifelse(
              !is.na(url_ofb),
              paste0(" | <a href='", url_ofb, "' target='_blank'>OFB</a>"),
              ""
            )
          ) %>% 
          mutate(
            info = paste0(inpn, ofb)
          ) %>% 
          select(
            `Ordre`   = ordre,
            `Famille` = famille,
            `Espèce`  = espece,
            `Nom vernaculaire` = nom_vernaculaire,
            `Nombre d'observations` = n,
            `Fiche espèce` = info
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
