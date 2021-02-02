
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

#' @importFrom leaflet leaflet leafletOptions addProviderTiles providerTileOptions addScaleBar addLayersControl fitBounds
#' @importFrom leaflet.extras addResetMapButton addSearchOSM searchOptions
#' @importFrom sf st_bbox
generate_map <- function() {
    
    bbox <- birds %>% 
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
        addProviderTiles("GeoportailFrance.ignMaps",
                         group = "IGN",
                         options = providerTileOptions(
                             updatewhenZooming = TRUE,
                             updateWhenIdle = TRUE
                         )) %>% 
        addProviderTiles("GeoportailFrance.parcels",
                         group = "Parcelles",
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
            "OSM", "IGN", "Orthophotos", "Parcelles"
        ),
        position = "topright") %>% 
        fitBounds(lng1 = bbox[["xmin"]],
                  lat1 = bbox[["ymin"]],
                  lng2 = bbox[["xmax"]],
                  lat2 = bbox[["ymax"]])
    
}

#' @importFrom dplyr ungroup distinct mutate filter inner_join rowwise
#' @importFrom glue glue
#' @importFrom leaflet colorFactor leafletProxy clearMarkers clearShapes clearControls addCircleMarkers addLayersControl
#' @importFrom purrr map
#' @importFrom stringr str_replace_all
update_map <- function(mapId, data) {
    prepare_polygons <- function(data, polygons, type) {
        inner_join(
            polygons,
            data %>% 
                filter(precision == type) %>% 
                rowwise() %>% 
                mutate(espece = paste0("<i>", espece, "</i>")) %>% 
                group_by(precision, ID, departement, commune) %>% 
                summarise(especes = ifelse(length(espece) > 10, paste(c(espece[seq(9)], '...'), collapse = ',<br>'),paste(espece, collapse = ',<br>')),
                          .groups = "drop") %>% 
                mutate(labs = glue("<b>{ifelse(!is.na(commune), commune, '')} ({departement})</b><br><small>{precision}</small><br>{especes}") %>% 
                              map(HTML)) %>% 
                select(ID, labs),
            by = "ID"
        ) 
    }
    
    if (nrow(data) > 0) {

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
            polygons = LimitesCommunes,
            type = "commune"
        )
        
        data_mailles <- prepare_polygons(
            data = data,
            polygons = GrilleINPN,
            type = "maille"
        )
        
        leafletProxy(mapId) %>% 
            clearMarkers() %>% 
            clearShapes() %>% 
            clearControls() %>% 
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
            }) %>%
            addLayersControl(baseGroups = c(
                "OSM", "IGN", "Orthophotos", "Parcelles"
            ),
            overlayGroups = c(
                "Mailles", "Communes", "Points"
            ),
            position = "topright") 
    }
}

#' @importFrom sf st_bbox
#' @importFrom leaflet leafletProxy fitBounds
update_map_scale <- function(mapId, data) {
    if (nrow(data) > 0) {
        bbox <- data %>% 
        inpn_to_sf() %>% 
        st_bbox()
    
    leafletProxy(mapId) %>% 
        fitBounds(lng1 = bbox[["xmin"]],
                  lat1 = bbox[["ymin"]],
                  lng2 = bbox[["xmax"]],
                  lat2 = bbox[["ymax"]])
    }
}

#' @importFrom dplyr n_distinct tibble count arrange desc pull mutate n group_by summarise
#' @importFrom glue glue
#' @importFrom leaflet colorNumeric leafletProxy clearShapes clearControls clearGroup addLayersControl addPolygons popupOptions
#' @importFrom sf st_transform st_make_grid st_as_sf st_join
add_hexagons <- function(mapId, data) {
    if (nrow(data) > 0) {
        palRich <- colorNumeric(
            palette = "viridis",
            domain = c(0, n_distinct(data$espece)),
            reverse = TRUE
        )
        
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
        
        hexagons <- inpn_to_sf(data) %>% 
            st_transform(crs = 2154) %>% 
            st_make_grid(
                cellsize = 10000,
                square = FALSE
            ) %>% 
            st_as_sf() %>% 
            mutate(hexids = seq(n())) %>% 
            st_join(st_transform(inpn_to_sf(data), crs = 2154)) %>% 
            st_transform(crs = 4326) %>% 
            group_by(hexids) %>% 
            summarise(S = n_distinct(espece),
                      species = format_species_list(espece),
                      .groups = "drop")
        
        leafletProxy(mapId, data = inpn_to_sf(data)) %>%
            clearGroup(group = "Richesse") %>%
            clearControls() %>% 
            addLayersControl(baseGroups = c(
                "OSM", "IGN", "Orthophotos", "Parcelles"
            ),
            overlayGroups = c(
                "Mailles", "Communes", "Points", "Richesse"
            ),
            position = "topright") %>% 
            addPolygons(data = hexagons,
                        fillColor = ~palRich(S),
                        fillOpacity = .75,
                        stroke = FALSE,
                        label = ~glue("{S} espèces"),
                        popup = ~species,
                        group = "Richesse",
                        options = popupOptions(className = "speciesPopup",
                                               closeButton = FALSE))
    }
    
}

#' @importFrom leaflet leafletProxy clearShapes clearControls addLayersControl
clear_hexagons <- function(mapId) {
    leafletProxy(mapId) %>% 
        clearGroup(group = "Richesse") %>%
        clearControls() %>% 
        addLayersControl(baseGroups = c(
            "OSM", "IGN", "Orthophotos", "Parcelles"
        ),
        overlayGroups = c(
            "Mailles", "Communes", "Points"
        ))
}
