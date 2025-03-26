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
#' library(ggplot2)
#' data <- mtcars
#' ggplot(data, aes(x = mpg, y = wt)) +
#'   geom_point() +
#'   theme_datagotchi_light()
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

#' Theme Datagotchi Dark
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
#' library(ggplot2)
#' data <- mtcars
#' ggplot(data, aes(x = mpg, y = wt)) +
#'   geom_point() +
#'   theme_datagotchi_light()
#' }
#'
#' @export
theme_datagotchi_dark <- function(
    base_size = 11,
    base_family = "PixelOperatorSC",
    base_line_size = base_size / 22,
    base_rect_size = base_size / 22,
    half_line = base_size / 2,
    base_margin = base_size,
    primary_colour = "white",
    secondary_colour = "#f2f2f2",
    minor_colour = "#525252",
    bg_colour = "#494949",
    strip_colour = "grey80") {
  # Vérifier si les fonts sont déjà chargés
  if (!("PixelOperatorSC" %in% sysfonts::font_families())) {
    sysfonts::font_add(family = "PixelOperatorSC", regular = system.file("fonts/PixelOperatorSC.ttf", package = "clessnize"))
  }
  if (!("WebFont" %in% sysfonts::font_families())) {
    sysfonts::font_add(family = "WebFont", regular = system.file("fonts/000webfont.ttf", package = "clessnize"))
  }

  # Activer showtext
  showtext::showtext_auto()
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

#' Party Colors
#'
#' A named vector containing the colors associated with the USA political parties (2024).
#'
#' @format A named character vector with color codes.
#' @export
party_colors <- c(
  "democrat" = "#0076CE",
  "republican" = "#FF0000",
  "rfk" = "#FFD700"
)


#' Add a logo to a ggplot
#'
#' This function takes a ggplot object, adds a logo to it, and saves the result as a PNG file.
#'
#' @param plot A ggplot object to which the logo will be added.
#' @param logo_path A character string specifying the path or URL of the logo image.
#' Default is `"https://raw.githubusercontent.com/clessn/img/refs/heads/main/Logo.PNG"`.
#' @param output_path A character string specifying the file path for saving the output plot with the logo.
#' Default is `"plot_with_logo.png"`.
#' @param logo_width A numeric value specifying the width of the logo as a proportion of the plot width.
#' Default is `0.1`.
#' @param logo_position A character string specifying the position of the logo.
#' Accepts `"topright"`, `"topleft"`, `"bottomright"`, or `"bottomleft"`. Default is `"topright"`.
#' @param margin A numeric value specifying the margin between the logo and the plot edges as a proportion of the plot width.
#' Default is `0.01`.
#'
#' @return A character string representing the path of the saved image file.
#'
#' @details This function saves the provided ggplot as a temporary PNG file, reads both the plot and the logo,
#' resizes the logo, calculates its position on the plot, and overlays the logo. The final image is saved
#' as a PNG file to the specified output path.
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' plot <- ggplot(mtcars, aes(mpg, hp)) +
#'   geom_point()
#' add_logo_to_plot(plot, logo_path = "logo.png", output_path = "plot_with_logo.png")
#' }
#'
#' @import ggplot2
#' @import magick
#' @export
add_logo_to_plot <- function(plot,
                             logo_path = "https://raw.githubusercontent.com/clessn/img/refs/heads/main/Logo.PNG",
                             output_path = "plot_with_logo.png",
                             logo_width = 0.1,
                             logo_position = "topright",
                             margin = 0.01) {
  # Save the ggplot as a PNG file
  ggsave("temp_plot.png", plot, width = 12, height = 8, dpi = 300)

  # Read the plot and logo images
  plot_img <- image_read("temp_plot.png")
  logo_img <- image_read(logo_path)

  # Get dimensions
  plot_width <- image_info(plot_img)$width
  plot_height <- image_info(plot_img)$height

  # Resize logo
  logo_new_width <- as.integer(plot_width * logo_width)
  logo_img <- image_resize(logo_img, paste0(logo_new_width, "x"))

  # Calculate position
  margin_px <- as.integer(plot_width * margin)
  logo_info <- image_info(logo_img)

  if (logo_position == "topright") {
    x_pos <- plot_width - logo_info$width - margin_px
    y_pos <- margin_px
  } else if (logo_position == "topleft") {
    x_pos <- margin_px
    y_pos <- margin_px
  } else if (logo_position == "bottomright") {
    x_pos <- plot_width - logo_info$width - margin_px
    y_pos <- plot_height - logo_info$height - margin_px
  } else if (logo_position == "bottomleft") {
    x_pos <- margin_px
    y_pos <- plot_height - logo_info$height - margin_px
  } else {
    stop("Invalid logo_position. Use 'topright', 'topleft', 'bottomright', or 'bottomleft'.")
  }

  # Add logo to the plot
  plot_with_logo <- image_composite(plot_img, logo_img, offset = paste0("+", x_pos, "+", y_pos))

  # Save the final image
  image_write(plot_with_logo, path = output_path)

  # Remove temporary file
  file.remove("temp_plot.png")

  # Return the path of the saved image
  return(output_path)
}
