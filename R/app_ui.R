#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom shiny.semantic semanticPage sidebar_layout sidebar_panel main_panel tabset grid grid_template segment
#' @importFrom shinybusy add_busy_spinner
#' @noRd
app_ui <- function(request) {
  gridLayout <- grid_template(
    default = list(
      areas = rbind(
        c("map", "map", "treemap"),
        c("numbers", "redlist", "redlist")
      ),
      rows_height = c("auto", "225px"),
      cols_width = c("17%", "33%", "50%")
    )
  )
  
  ChoixDepartements <- levels(birds$departement)
  
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
            h3("Groupe taxonomique"),
            mod_select_data_ui(id = "donnees"),
            h3("Années"),
            mod_select_period_ui(id = "periode",
                                 min_year = min(birds$annee),
                                 max_year = max(birds$annee)),
            br(),
            h3("Départements"),
            mod_select_ui(
              id = "departements",
              placeholder = "Tous les départements",
              choices = ChoixDepartements
              ),
            br(),
            hr(),
            br(),
            mod_generate_chronic_ui(id = "chronic")
          ),
          main_panel(
            add_busy_spinner(spin = "fading-circle"),
            tabset(
              tabs = list(
                list(
                  menu = "Visualisation",
                content = shiny.semantic::grid(
                  gridLayout,
                  map = mod_generate_map_ui(id = "carte"),
                  treemap = mod_generate_treemap_ui(id = "treemap"),
                  numbers = tagList(
                    br(),
                    mod_generate_keyindicators_ui(id = "numbers")
                    ),
                  redlist = segment(mod_generate_redlists_ui(id = "redlist"),
                                    style = "height:195px; padding:0;margin:5px")
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

