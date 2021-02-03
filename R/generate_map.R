
#' Title
#'
#' @param inpn 
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom sf st_as_sf
inpn_to_sf <- function(inpn) {
    inpn %>% 
        st_as_sf(
            coords = c("longitude", "latitude"),
            crs = 4326,
            remove = FALSE
        )
}

#' @importFrom dplyr tibble count arrange desc pull
#' @importFrom glue glue
format_species_list <- function(species) {
    tibble(sp = species) %>% 
        count(sp) %>% 
        arrange(desc(n)) %>% 
        pull(sp) %>% 
        as.character() %>% 
        (function(x) {
            glue("<i>{x}</i>") %>% 
                paste(collapse = "<br>")
        })
}

#' @importFrom dplyr inner_join filter rowwise mutate group_by summarise n_distinct select
#' @importFrom glue glue
#' @importFrom purrr map
prepare_polygons <- function(data, polygons, type) {
    inner_join(
        polygons,
        data %>% 
            filter(precision == type) %>% 
            rowwise() %>% 
            mutate(espece = paste0("<i>", espece, "</i>")) %>% 
            group_by(precision, ID, departement, commune) %>% 
            summarise(S = n_distinct(espece),
                      especes = format_species_list(espece),
                      .groups = "drop") %>% 
            mutate(labs = glue("<b>{ifelse(!is.na(commune), commune, '')} ({departement})</b><br><small>{precision}</small><br>{ifelse(S < 10, especes, paste0('<b>', S, ' espèces</b> (cliquer pour les afficher)'))}") %>% 
                       map(HTML),
                   popups = especes) %>% 
            select(ID, labs, popups),
        by = "ID"
    ) 
}

#' @importFrom dplyr ungroup distinct filter mutate
#' @importFrom glue glue
#' @importFrom leaflet colorFactor addPolygons addCircleMarkers
#' @importFrom purrr map
#' @importFrom stringr str_replace_all
add_observations <- function(map, data, limites_communes, grille_inpn) {
    speciesColors <- data %>% 
        ungroup() %>% 
        distinct(espece, color)
    
    palSpColor <- colorFactor(
        palette = speciesColors$color,
        levels = speciesColors$espece
    )
    
    data_points <- data %>% 
        filter(precision %in% c("point", "ligne/polygone")) %>% 
        inpn_to_sf() %>% 
        mutate(
            labs = glue("<b>{ifelse(!is.na(commune), commune, '')} ({departement})<br><small>{precision}</small><br></b><i>{espece}</i><br>{ifelse(!is.na(nom_vernaculaire), paste0('(', str_wrap(nom_vernaculaire, width = 40), ')'), '')}") %>% 
                str_replace_all(pattern = "\n", replacement = "<br>") %>% 
                map(HTML)
        )
    
    data_communes <- prepare_polygons(
        data = data,
        polygons = limites_communes,
        type = "commune"
    )
    
    data_mailles <- prepare_polygons(
        data = data,
        polygons = grille_inpn,
        type = "maille"
    )
    
    map %>% 
        (function(x) {
            if (nrow(data_mailles) > 0) {
                x <- x %>% 
                    addPolygons(
                        data = data_mailles,
                        fillColor = c("#0000FF"),
                        fillOpacity = .1,
                        stroke = TRUE,
                        color = "black",
                        opacity = 1,
                        weight = 1,
                        label = ~labs,
                        popup = ~popups,
                        group = "Mailles"
                    )  
            }
            
            if (nrow(data_communes) > 0) {
                x <- x %>% 
                    addPolygons(
                        data = data_communes,
                        fillColor = c("#0000FF"),
                        fillOpacity = .1,
                        stroke = TRUE,
                        weight = 1,
                        color = "black",
                        opacity = 1,
                        label = ~labs,
                        popup = ~popups,
                        group = "Communes"
                    )  
            }
            
            if (nrow(data_points) > 0) {
                x <- x %>% 
                    addCircleMarkers(
                        data = inpn_to_sf(data_points),
                        fillColor = palSpColor(data_points$espece),
                        fillOpacity = 1,
                        radius = 5,
                        stroke = TRUE,
                        weight = 2,
                        color = "black",
                        label = ~labs,
                        group = "Points"
                    )
            }
            
            x
        })
}

#' @importFrom leaflet leaflet leafletOptions addProviderTiles providerTileOptions addScaleBar addLayersControl fitBounds
#' @importFrom leaflet.extras addResetMapButton addSearchOSM searchOptions
#' @importFrom sf st_bbox
generate_map <- function(data, limites_communes, grille_inpn) {
    
    data <- data %>% 
        add_species_colors()
    
    bbox <- data %>% 
        inpn_to_sf() %>% 
        st_bbox()
    
    leaflet(width = "100%", height = 400,
            options = leafletOptions(preferCanvas = TRUE)) %>% 
        addProviderTiles("GeoportailFrance.orthos",
                         group = "Orthophotos",
                         options = providerTileOptions(
                             updatewhenZooming = TRUE,
                             updateWhenIdle = TRUE
                         )
        ) %>% 
        addProviderTiles("OpenStreetMap.France", 
                         group = "OSM",
                         options = providerTileOptions(
                             updatewhenZooming = TRUE,
                             updateWhenIdle = TRUE
                         )) %>% 
        addScaleBar(position = "bottomright") %>% 
        addResetMapButton() %>% 
        addSearchOSM(
            options = searchOptions(
                autoCollapse = TRUE, 
                minLength = 2,
                position = "topleft",
                hideMarkerOnCollapse = TRUE
            )
        ) %>%
        addLayersControl(baseGroups = c(
            "OSM", "Orthophotos"
        ),
        overlayGroups = c(
            "Mailles", "Communes", "Points"
        ),
        position = "topright") %>% 
        fitBounds(lng1 = bbox[["xmin"]],
                  lat1 = bbox[["ymin"]],
                  lng2 = bbox[["xmax"]],
                  lat2 = bbox[["ymax"]]) %>% 
        add_observations(data = data,
                         limites_communes = limites_communes,
                         grille_inpn = grille_inpn)
    
}


#' @importFrom leaflet leafletProxy clearMarkers clearShapes clearControls addLayersControl
update_map <- function(mapId, data, limites_communes, grille_inpn) {

    if (nrow(data) > 0) {

        
        leafletProxy(mapId) %>% 
            clearMarkers() %>% 
            clearShapes() %>% 
            clearControls() %>% 
            add_observations(data = data,
                             limites_communes = limites_communes,
                             grille_inpn = grille_inpn) %>% 
            addLayersControl(baseGroups = c(
                "OSM", "Orthophotos"
            ),
            overlayGroups = c(
                "Mailles", "Communes", "Points"
            ),
            position = "topright") 
    }
}

#' @importFrom leaflet leafletProxy fitBounds
#' @importFrom sf st_bbox
#' @importfrom dplyr filter
update_map_scale <- function(mapId, departements, limites_departements) {
    if (!is.null(departements)) {
        bbox <- limites_departements %>% 
            filter(NOM_DEP %in% departements) %>% 
            st_bbox()
        
        leafletProxy(mapId) %>% 
            fitBounds(lng1 = bbox[["xmin"]],
                      lat1 = bbox[["ymin"]],
                      lng2 = bbox[["xmax"]],
                      lat2 = bbox[["ymax"]])
    }
}

#' @importFrom dplyr n_distinct select bind_rows filter distinct left_join group_by summarise
#' @importFrom glue glue
#' @importFrom leaflet colorNumeric leafletProxy clearGroup clearControls addLayersControl addPolygons popupOptions
#' @importFrom purrr map
#' @importFrom sf st_transform st_join
add_grid <- function(mapId, data, limites_communes, grille_inpn) {

    if (nrow(data) > 0) {
        palRich <- colorNumeric(
            palette = "viridis",
            domain = c(0, n_distinct(data$espece)),
            reverse = TRUE
        )
        
        grilleL93 <- st_transform(grille_inpn, crs = 2154) %>% 
            select(maille = ID)
        
        cells <- data %>% 
            (function(df) {
                bind_rows(
                    df %>% 
                        filter(precision %in% c("point", "ligne/polygone")) %>%
                        inpn_to_sf() %>%
                        (function(.df) {
                            if (nrow(.df) > 0) {
                                .df %>% 
                                    distinct(espece)
                            } else {
                                .df %>% 
                                    select(espece)
                            }
                        }) %>% 
                        st_transform(crs = 2154) %>% 
                        st_join(grilleL93, .),
                    df %>% 
                        filter(precision %in% c("commune")) %>% 
                        distinct(ID, espece) %>% 
                        left_join(limites_communes, ., by = "ID") %>%
                        filter(!is.na(espece)) %>% 
                        select(-ID) %>% 
                        st_transform(crs = 2154) %>% 
                        st_join(grilleL93, .),
                    df %>% 
                        filter(precision %in% c("maille")) %>% 
                        left_join(grilleL93, ., by = c("maille" = "ID")) %>% 
                        filter(!is.na(espece)) %>% 
                        (function(.df) {
                            if (nrow(.df) > 0) {
                                .df %>% 
                                    distinct(maille, espece)
                            } else {
                                .df %>% 
                                    select(maille, espece)
                            }
                        })
                )
            }) %>%  
            filter(!is.na(espece)) %>% 
            st_transform(crs = 4326) %>% 
            group_by(maille) %>% 
            summarise(S = ifelse(length(espece) == 0, 0, n_distinct(espece)),
                      species = ifelse(length(espece) == 0, "", format_species_list(espece)),
                      .groups = "drop")
        
        leafletProxy(mapId, data = inpn_to_sf(data)) %>%
            clearGroup(group = "Richesse") %>%
            clearControls() %>% 
            addLayersControl(baseGroups = c(
                "OSM", "Orthophotos"
            ),
            overlayGroups = c(
                "Mailles", "Communes", "Points", "Richesse"
            ),
            position = "topright") %>% 
            addPolygons(data = cells,
                        fillColor = ~palRich(S),
                        fillOpacity = .75,
                        stroke = FALSE,
                        label = ~glue("{ifelse(S < 10, paste0('<b>', S, ' espèces</b><br>', species), paste0('<b>', S, ' espèces</b> (cliquer pour les afficher)'))}") %>% map(HTML),
                        popup = ~species,
                        group = "Richesse",
                        options = popupOptions(className = "speciesPopup",
                                               closeButton = FALSE))
    }
    
}

#' @importFrom leaflet leafletProxy clearGroup clearControls addLayersControl
clear_grid <- function(mapId) {
    leafletProxy(mapId) %>% 
        clearGroup(group = "Richesse") %>%
        clearControls() %>% 
        addLayersControl(baseGroups = c(
            "OSM", "Orthophotos"
        ),
        overlayGroups = c(
            "Mailles", "Communes", "Points"
        ))
}
