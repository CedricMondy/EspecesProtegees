#' @importFrom dplyr count mutate
#' @importFrom plotly plot_ly layout config
generate_chronic <- function(data, annees) {

    if (nrow(data) > 0) {
        data %>% 
        count(annee) %>% 
        mutate(
            fillColor = ifelse(
                (annee >= annees[1] & annee <= annees[2]),
                "#104E8B",
                "#CAE1FF"
            )
        ) %>% 
        plot_ly(
            x = ~annee,
            y = ~n,
            color = ~fillColor,
            colors = ~fillColor,
            type = 'bar',
            text = ~glue("<b>{annee}: </b><br>{n} observations"),
            hoverinfo = 'text'
        ) %>% 
        layout(showlegend = FALSE,
               xaxis = list(title = "",
                            range = c(annees[1] - .5, annees[2] + .5),
                            fixedrange = TRUE),
               yaxis = list(title = "",
                            fixedrange = TRUE)) %>%
        config(displayModeBar = FALSE) %>%
        config(showLink = FALSE)
    }
}
