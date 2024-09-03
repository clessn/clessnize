#' Check if font files are accessible
#'
#' This function checks if the specified font files are accessible via system.file().
#' @return A message indicating whether each font file is found or not.
#' @export
check_fonts_access <- function() {
  fonts <- c("PixelOperatorSC.ttf", "000webfont.ttf")
  font_paths <- sapply(fonts, function(font) system.file(paste0("fonts/", font), package = "clessnize"))

  # Vérifier si les chemins ne sont pas vides
  results <- sapply(font_paths, function(path) if (path == "") "Not found" else "Found")
  
  # Créer un message récapitulatif
  message(paste(names(results), ":", results, font_paths, collapse = "\n"))
  
  return("check_fonts_access is done")
}

#' Theme Datagotchi Light
#'
#' A custom ggplot2 theme that applies the PixelOperatorSC or WebFont fonts,
#' depending on the user's selection, with various aesthetic adjustments.
#'
#' @param base_size Numeric, the base font size. Default is 11.
#' @param base_family Character, the base font family. Default is "PixelOperatorSC".
#' @param base_line_size Numeric, the base line size. Default is `base_size / 22`.
#' @param base_rect_size Numeric, the base rectangle size. Default is `base_size / 22`.
#' @param half_line Numeric, half the line size. Default is `base_size / 2`.
#' @param base_margin Numeric, the base margin size. Default is `base_size`.
#' @param primary_colour Character, the primary color used in the theme. Default is "black".
#' @param secondary_colour Character, the secondary color used in the theme. Default is "grey30".
#' @param minor_colour Character, the color for minor elements like grid lines. Default is "#f7f7f7".
#' @param bg_colour Character, the background color. Default is "white".
#' @param strip_colour Character, the strip background color. Default is "white".
#'
#' @details
#' This theme is part of the `clessnize` package and is designed to integrate custom fonts
#' into ggplot2 visualizations with minimal configuration. The function automatically checks
#' if the required fonts are available, and loads them if necessary. The `showtext` package
#' is used to ensure that the fonts render correctly.
#'
#' @return A ggplot2 theme object.
#'
#' @examples
#' \dontrun{
#'   library(ggplot2)
#'   data <- mtcars
#'   ggplot(data, aes(x = mpg, y = wt)) +
#'     geom_point() +
#'     theme_datagotchi_light()
#' }
#'
#' @export
theme_datagotchi_light <- function(base_size = 11,
                              base_family = "PixelOperatorSC",
                              base_line_size = base_size / 22,
                              base_rect_size = base_size / 22,
                              half_line = base_size / 2,
                              base_margin = base_size,

                              primary_colour = "black",
                              secondary_colour = "grey30",
                              minor_colour = "#f7f7f7",
                              bg_colour = "white",
                              strip_colour = "white") {
  # Vérifier si les fonts sont déjà chargés
  if (!("PixelOperatorSC" %in% sysfonts::font_families())) {
    sysfonts::font_add(family = "PixelOperatorSC", regular = system.file("fonts/PixelOperatorSC.ttf", package = "clessnize"))
  }
  if (!("WebFont" %in% sysfonts::font_families())) {
    sysfonts::font_add(family = "WebFont", regular = system.file("fonts/000webfont.ttf", package = "clessnize"))
  }
  
  # Activer showtext
  showtext::showtext_auto()
  
  # Charger les logos
  logo1_path <- system.file("clessnize/inst/extdata/Logo_Partenaire_-51.png", package = "clessnize") 
 # logo2_path <- system.file("images/logo2.png", package = "clessnize") # Modifier le chemin d'accès
  
  logo1 <- png::readPNG(logo1_path)
  logo1_grob <- grid::rasterGrob(logo1, x = unit(0, "npc"), y = unit(1, "npc"), 
                                 hjust = -0.1, vjust = 1.1, 
                                 width = unit(1, "inches"), height = unit(1, "inches"))
  
  #logo2 <- png::readPNG(logo2_path)
  #logo2_grob <- grid::rasterGrob(logo2, x = unit(1, "npc"), y = unit(1, "npc"), 
  #                               hjust = 1.1, vjust = 1.1, 
  #                               width = unit(1, "inches"), height = unit(1, "inches"))
  
  
  # Créer le thème en utilisant la font spécifiée
  ggplot2::theme_classic() +
    ggplot2::theme(
      text = ggplot2::element_text(size = base_size, family = base_family, colour = secondary_colour),
      axis.text = ggplot2::element_text(colour = secondary_colour),
      axis.ticks.x = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.line.y = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_text(
        margin = ggplot2::margin(r = half_line, t = half_line),
        hjust = 0.5
      ),
      axis.title.y = ggplot2::element_text(
        margin = ggplot2::margin(r = half_line, b = half_line),
        hjust = 0.5
      ),
      axis.line.x = ggplot2::element_blank(),
      legend.position = "bottom",
      legend.title = ggplot2::element_blank(),
      legend.background = ggplot2::element_rect(fill = NA),
      panel.grid.major.y = ggplot2::element_line(colour = minor_colour),
      plot.caption = ggplot2::element_text(hjust = 0, face = "italic"),
      plot.title = ggplot2::element_text(
        face = "bold",
        colour = primary_colour,
        size = base_size * 1.5,
        hjust = 0.5
      ),
      plot.background = ggplot2::element_rect(fill = bg_colour, colour = bg_colour),
      panel.background = ggplot2::element_rect(fill = NA),
      strip.background = ggplot2::element_blank()
    )
}

#' Datagotchi Green Light
#'
#' A color code for the Datagotchi theme.
#'
#' @return A character string representing the color code "#1BF640".
#' @export
datagotchi_green_light <- "#1BF640"

#' Datagotchi Green Dark
#' 
#' A color code for the Datagotchi theme.
#' 
#' @return A character string representing the color code "#26E92E".
#' @export
datagotchi_green_dark <- "#26E92E"






