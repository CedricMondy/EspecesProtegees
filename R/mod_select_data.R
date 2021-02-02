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
#' @importFrom dplyr mutate distinct group_by group_split n_distinct left_join
#' @importFrom leaflet colorFactor
#' @importFrom paletteer paletteer_d
#' @importFrom purrr map_df
#' @importFrom tinter tinter
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
        
        raw_data <- switch(
          input$group,
          birds    = birds,
          fish     = fish,
          insects  = insects,
          mammals  = mammals,
          molluscs = molluscs,
          reptiles = reptiles
          )
        
        palOrdre <- colorFactor(
          palette = paletteer_d("RColorBrewer::Dark2") %>% 
            as.character(),
          domain = unique(raw_data$ordre)
          )
        
        data <- raw_data %>% 
          mutate(colorOrdre = palOrdre(ordre)) 
        
        colorSpecies <- distinct(data, ordre, espece, colorOrdre) %>% 
            group_by(ordre) %>%
          group_split() %>% 
          map_df(function(df) {
            df %>% 
              mutate(
                color = tinter(
                x = unique(df$colorOrdre),
                direction = "tints",
                steps = 2 * n_distinct(df$espece),
                crop = 2
                ) %>% 
                  sample(n_distinct(df$espece))
              )
          }) 
        
        data %>% 
          left_join(colorSpecies, by = c("ordre", "colorOrdre", "espece"))
        })
      }
  )
}
    
