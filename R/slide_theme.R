## Custom slide theme


#' Slide theme
#'
#' Set up slides theme
#'
#' @return Environment vars set and theme set etc
#' @details Sets up the slide theme with Tenso font, Okabe-Ito colors, and element_markdown()
#' @export
kjh_set_slide_theme <- function() {

  # Reordered Okabe-Ito
  slide_colors_opt <- unname(palette.colors()[c(2:4,6:8,5,1,9)])

  ## NB UK spelling of colour here
  options(ggplot2.discrete.colour = slide_colors_opt,
          ggplot2.discrete.fill = slide_colors_opt)


  ggplot2::theme_set(theme_tenso())
}


#' Slide colors
#'
#' @return Vector of named slide colors
#' @export
#'
kjh_slide_colors <- function() {
  slide_colors <- c(
    `slate` = "#242E3D",
    `pink` = "#FC2290",
    `red` =  "#FB0307",
    `orange` =  "#EF9B2D",
    `yellow` =  "#FFFF54",
    `lightgrey` = "#F6F6F6",
    #  `grey` =  "#CBCBCB",
    `grey` =  "#E8E8E8",
    `darkgrey` =  "#C0C0C0",
    `blue` =  "#212E3E",
    `lblue` =  "#4EAFF0",
    `green` = "#1BB71C"
  )
  slide_colors
}

#' Theme Tenso
#'
#' @param base_size Font size
#' @param base_family Font Family (Tenso Slide)
#'
#' @return Slide ggplot Theme
#' @export
#'
theme_tenso <- function (base_size = 12, base_family = "Tenso Slide") {
  (ggthemes::theme_foundation(base_size = base_size, base_family = base_family) +
     ggplot2::theme(line = ggplot2::element_line(colour = kjh_slide_colors()["slate"]),
                    rect = ggplot2::element_rect(fill = kjh_slide_colors()["lightgrey"],
                                                 linetype = 0, colour = NA),
                    text = ggplot2::element_text(colour = kjh_slide_colors()["slate"]),
                    axis.text = ggplot2::element_text(size = ggplot2::rel(1.35)),
                    axis.title = ggplot2::element_text(size = ggplot2::rel(1.5)),
                    strip.text = ggplot2::element_text(size = ggplot2::rel(1.35),
                                                       face = "bold"),
                    legend.text = ggplot2::element_text(size = ggplot2::rel(1.5)),
                    legend.title = ggplot2::element_text(size = ggplot2::rel(1.5),
                                                         face = "bold"),
                    axis.ticks = ggplot2::element_line(),
                    axis.line = ggplot2::element_line(),
                    legend.background = ggplot2::element_rect(),
                    legend.position = "top",
                    legend.direction = "horizontal",
                    legend.box = "vertical",
                    panel.grid = ggplot2::element_line(colour = NULL),
                    panel.grid.major = ggplot2::element_line(colour = kjh_slide_colors()["grey"]),
                    panel.grid.minor = ggplot2::element_blank(),
                    plot.title = ggtext::element_markdown(hjust = 0,
                                                          size = ggplot2::rel(1.5),
                                                          face = "bold"),
                    plot.subtitle = ggtext::element_markdown(hjust = 0,
                                                             size = ggplot2::rel(1.25),
                                                             face = "plain"),
                    plot.caption = ggtext::element_markdown(hjust = 0,
                                                            size = ggplot2::rel(0.8),
                                                            face = "plain"),
                    plot.margin = grid::unit(c(5.5,12,5.5,5.5), "pt"),
                    strip.background = ggplot2::element_rect()
     )
  )

}

#' Generate ggplot default colors
#'
#' @param n Number of colors
#' @param h Hue
#'
#' @return n colors every 15 degrees around the color wheel
#' @export
#'
get_ggplot_colors <- function(n = 6, h = c(0, 360) + 15){
  hcl(h = (seq(h[1], h[2], length = n)), c = 100, l = 65)
}


#' Set to classic theme
#'
#' @param n N colors for color theme
#'
#' @return Sets the theme to the ggplot theme_classic
#' @export
#'
kjh_set_classic_theme <- function(n = 6){
  ## NB UK spelling of colour here
  options(ggplot2.discrete.colour = get_ggplot_colors(n = n),
          ggplot2.discrete.fill = get_ggplot_colors(n = n))

  ggplot2::theme_set(ggplot2::theme_classic())
}


#' Turn on Myriad
#'
#' @return Sets the slide theme to myriad semicondensed theme
#' @export
#'
kjh_set_myriad_theme <- function() {
  kjh_register_myriad()
  kjh_set_showtext()

  myriad::import_myriad_semi()
  ggplot2::theme_set(myriad::theme_myriad_semi())
}


