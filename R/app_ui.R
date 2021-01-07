#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom shiny.semantic semanticPage sidebar_layout sidebar_panel main_panel tabset segment grid grid_template
#' @importFrom shinybusy add_busy_spinner
#' @importFrom purrr set_names
#' @importFrom stringr str_remove_all str_to_sentence
#' @noRd
app_ui <- function(request) {
  my_layout <- grid_template(
    default = list(
      areas = rbind(
        c("map", "treemap")
      ),
      cols_width = c("50%", "50%")
    )
  )
  
  ChoixDepartements <- levels(birds$departement)
  
  ChoixPrecisions <- levels(birds$niveau_precision_localisation) %>% 
    as.list() %>% 
    (function(x) {
      set_names(x = x,
                nm = x %>% 
                  str_remove_all(pattern = "XY ") %>% 
                  str_remove_all(pattern = "centroïde ") %>% 
                  str_to_sentence())
    })
  
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    semanticPage(
      tagList(tags$head(
        tags$style(
          'div.speciesPopup {
               height: 10px;
               max-height: 200px;
               max-width: auto !important;
               opacity: .5;
           }
           
           .leaflet-popup-content {
                margin-top: 10px;
                margin-right: 2px;
                padding-right: 8px;
                margin-left: 10px;
                min-width: 100px !important;
                max-height: 200px;
                overflow: auto;
           }'
        ) 
      )),
      title = "Espèces protégées d'Ile-de-France",
      div(
        class = "raised segment",
        div(
          a(class = "ui green ribbon label", 
            "Espèces protégées d'Ile-de-France")
        ),
        sidebar_layout(
          sidebar_panel(
            mod_select_data_ui(id = "donnees"),
            br(),
            hr(),
            mod_select_period_ui(id = "periode"),
            br(),
            mod_select_ui(
              id = "departements",
              label = "Département(s)",
              placeholder = "Tous les départements",
              choices = ChoixDepartements
              ),
            br(),
            mod_select_ui(
              id = "precisions",
              label = "Niveau de précision géographique",
              placeholder = "Tous les niveaux",
              choices = ChoixPrecisions
              )
          ),
          main_panel(
            add_busy_spinner(spin = "fading-circle"),
            tabset(
              tabs = list(
                list(
                  menu = "Visualisation",
                content = shiny.semantic::grid(
                  my_layout,
                  map = mod_generate_map_ui(id = "carte"),
                  treemap = mod_generate_treemap_ui(id = "treemap")
                )
                ),
                list(
                  menu = "Données affichées",
                  content = tabset(
                    tabs = list(
                      list(
                        menu = "Liste des espèces",
                        content = mod_generate_taxalist_ui(id = "especes")
                      ),
                      list(
                        menu = "Liste des observations",
                        content = mod_generate_observationlist_ui(
                          id = "observations"
                          )
                      )
                    )
                  )
                  
                )
              )
            )
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'EspecesProtegees'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

