#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
  donnees <- mod_select_data_server(id = "donnees")
  
  donnees_taxon <- mod_search_taxon_server(
    id = "recherche_taxon",
    data = donnees
  )
  
  annees <- mod_select_period_server(
    id = "periode",
    raw_data = donnees_taxon
  )
  
  departements <- mod_select_server(id = "departements")

  DonneesFiltrees <- reactive({
    req(donnees_taxon())

    donnees_taxon() %>% 
      filter_years(annees()) %>% 
      filter_departments(departements())
  })
  
  ResumeDonneesFiltrees <- reactive({
    req(DonneesFiltrees())
    
    DonneesFiltrees() %>% 
      summarise_map_data()
  })

  taxa <- mod_generate_treemap_server(
    id = "treemap",
    donnees = ResumeDonneesFiltrees,
    limites = limites,
    titre = "Nombre de localisations"
  )

  limites <- mod_generate_map_server(
    id = "carte",
    donnees = ResumeDonneesFiltrees,
    taxa = taxa
  )
  
  DonneesChronique <- reactive({
      req(donnees_taxon, departements, limites, taxa)
      
    donnees_taxon() %>% 
        filter_departments(departements()) %>% 
        filter_limits(limites()) %>% 
        filter_taxa(taxa())
    })
  
  mod_generate_chronic_server(
      id = "chronic",
      data = DonneesChronique, years = annees
    )
  
  DonneesVisibles <- reactive({
    DonneesFiltrees() %>% 
      filter_limits(limites()) %>% 
      filter_taxa(taxa())
  })
  
  mod_generate_taxalist_server(
    id = "especes",
    donnees = DonneesVisibles
  )
  
  mod_generate_observationlist_server(
    id = "observations",
    donnees = DonneesVisibles
  )
  
  mod_generate_keyindicators_server(
    id = "numbers",
    donnees = DonneesVisibles
  )
  
  mod_generate_redlists_server(
    id = "redlist",
    donnees = DonneesVisibles
  )
  
}
