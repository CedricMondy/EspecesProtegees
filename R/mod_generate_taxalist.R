#' generate_taxalist UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList  
#' @importFrom DT DTOutput
mod_generate_taxalist_ui <- function(id){
  ns <- NS(id)
  tagList(
    DTOutput(ns("ListeEspece"))
    )
}
    
#' generate_taxalist Server Function
#'
#' @noRd 
#' @importFrom shiny moduleServer reactive
#' @importFrom dplyr filter count rename mutate arrange group_by summarise
#' @importFrom DT datatable renderDT
mod_generate_taxalist_server <- function(id, donnees, limites, taxa){
  moduleServer(
    id,
    function(input, output, session){
      
      liste_especes <- reactive({
        req(donnees(), limites(), taxa)
        
        liste <- donnees() %>% 
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
          count(ordre, famille, espece, nom_vernaculaire) %>% 
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
          rename(
            `Ordre`   = ordre,
            `Famille` = famille,
            `Espèce`  = espece,
            `Nom vernaculaire` = nom_vernaculaire,
            `Nombre d'observations` = n
          )
        
        
        liste %>% 
          datatable(
            filter = 'top',
            options = list(
              dom = 'itlp'
            )
          )
      })
      
      output$ListeEspece <- renderDT(liste_especes())
    }
  )
}
