# Tema de Ggplot
#' Title
#'
#' @return
#' @export
#'
#' @examples
theme_INE_intenso <- function(){
  theme(
    # Texto
    text = element_text(family = "Helvetica Neue", color="white"),
    # Panel
    panel.background = element_rect(fill = "white"),
    panel.grid.major.y = element_line(colour = "#CE0182",
                                      # size = .1,
                                      linetype = 3),
    panel.grid.minor.y = element_line(colour = "#CE0182",
                                      # size = .08,
                                      linetype = 3),
    # Ejes
    axis.ticks = element_blank(),
    axis.line.y = element_line(color="#CE0182"),
    axis.text = element_text(family = "Helvetica Neue", color="white"),
    # Fondo
    plot.background = element_rect(fill = "#CE0182")
    )
}

theme_INE_light <- function(){
  theme(
    # Texto
    text = element_text(family = "Helvetica Neue"),
    # Panel
    panel.background = element_rect(fill = "#ffebf7"),
    panel.grid.major.y = element_line(colour = "#CE0182",
                                      # size = .1,
                                      linetype = 3),
    panel.grid.minor.y = element_line(colour = "#CE0182",
                                      # size = .08,
                                      linetype = 3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    # Ejes
    axis.ticks = element_blank(),
    # axis.line.y = element_line(color="#CE0182"),
    axis.text = element_text(family = "Helvetica Neue"),
    # Fondo
    plot.background = element_rect(fill = "white")
  )
}
