#' @importFrom sf st_as_sf
inpn_to_sf <- function(inpn) {
    inpn %>% 
        st_as_sf(
            coords = c("longitude", "latitude"),
            crs = 4326,
            remove = FALSE
        )
}

#' @importFrom sf st_bbox
#' @importFrom dplyr distinct ungroup
#' @importFrom leaflet leaflet leafletOptions addProviderTiles providerTileOptions addScaleBar addLayersControl fitBounds
#' @importFrom leaflet.extras addResetMapButton addSearchOSM searchOptions 
generate_map <- function() {
    
    bbox <- mammals %>% 
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
            "OSM", "IGN", "Orthophotos"
        ),
        position = "topright") %>% 
        fitBounds(lng1 = bbox[["xmin"]],
                  lat1 = bbox[["ymin"]],
                  lng2 = bbox[["xmax"]],
                  lat2 = bbox[["ymax"]])
    
}

#' @importFrom dplyr distinct ungroup
#' @importFrom leaflet colorFactor leafletProxy clearShapes clearControls addLayersControl clearMarkers addCircleMarkers
#' @importFrom glue glue
#' @importFrom htmltools HTML
#' @importFrom purrr map
#' @importFrom stringr str_wrap str_replace_all
update_map <- function(mapId, data) {
    orderColors <- data %>% 
        ungroup() %>% 
        distinct(ordre, color)
    
    palOrderColor <- colorFactor(
        palette = orderColors$color,
        levels = orderColors$ordre
    )
    
    data <- data %>% 
        mutate(
            labs = glue("<b><i>{espece}</i></b><br><b>{ifelse(!is.na(nom_vernaculaire), paste0('(', str_wrap(nom_vernaculaire, width = 40), ')'), '')}</b><br>{ifelse(!is.na(commune), commune, '')} ({departement})<br>{date_debut}<br><i>({str_wrap(libelle_jeu_donnees, width = 40)})</i><br><small>{niveau_precision_localisation}</small>") %>% 
                str_replace_all(pattern = "\n", replacement = "<br>") %>% 
        map(HTML)
        )
    data_points <- data %>% 
        filter(niveau_precision_localisation == "XY point") 
    
    data_zones <- data %>% 
        filter(niveau_precision_localisation != "XY point")
    

    leafletProxy(mapId) %>% 
        clearMarkers() %>% 
        clearShapes() %>% 
        clearControls() %>% 
        (function(x) {
            if (nrow(data_zones) > 0) {
                x %>% 
                  addCircleMarkers(
                      data = inpn_to_sf(data_zones),
                      fillColor = palOrderColor(data_zones$ordre),
                      fillOpacity = 1,
                      radius = 8,
                      stroke = FALSE,
                      label = ~labs,
                      group = "Observations"
                      )  
                } else {
                    x
                    }
            }) %>%
        (function(x) {
            if (nrow(data_points) > 0) {
                x %>% 
                    addCircleMarkers(
                        data = inpn_to_sf(data_points),
                        fillColor = palOrderColor(data_points$ordre),
                        fillOpacity = 1,
                        radius = 5,
                        stroke = TRUE,
                        weight = 2,
                        color = "black",
                        label = ~labs,
                        group = "Observations"
                        )
                } else {
                    x
                    }
            }) %>%
        addLayersControl(baseGroups = c(
            "OSM", "IGN", "Orthophotos"
        ),
        overlayGroups = c(
            "Observations"
        ),
        position = "topright") 
}
