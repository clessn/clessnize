#' Add image overlay to bars or histograms
#'
#' @param plot The ggplot object
#' @param data The data frame containing the counts
#' @param image_path The path to the image to overlay
#' @param geom The type of plot: "bar" or "hist"
#' @return The ggplot object with the image overlay
#' @noRd
add_image_bars <- function(plot, data, image_path, geom) {
  # Load the image
  image <- png::readPNG(image_path)
  
  if (geom == "bar") {
    for (i in 1:nrow(data)) {
      plot <- plot + ggplot2::annotation_custom(
        grid::rasterGrob(image, width = grid::unit(1, "npc"), height = grid::unit(1, "npc"), interpolate = TRUE),
        xmin = i - 0.5, xmax = i + 0.5, ymin = 0, ymax = data$n[i]
      )
    }
  } else if (geom == "hist") {
    breaks <- hist(data$y, plot = FALSE)$breaks
    for (i in 1:(length(breaks) - 1)) {
      plot <- plot + ggplot2::annotation_custom(
        grid::rasterGrob(image, width = grid::unit(1, "npc"), height = grid::unit(1, "npc"), interpolate = TRUE),
        xmin = breaks[i], xmax = breaks[i + 1], ymin = 0, ymax = sum(data$y >= breaks[i] & data$y < breaks[i + 1])
      )
    }
  }
  
  return(plot)
}

#' Create a ggplot with image overlay on bars or histograms
#'
#' @param data The data frame to use for the plot
#' @param mapping Default list of aesthetic mappings to use for plot
#' @param image_path The path to the image to overlay on bars or histograms
#' @param geom The type of plot: "bar" or "hist"
#' @param ... Other arguments passed to ggplot
#' @return A ggplot object with the image overlay on bars or histograms
#' @export
ggboule <- function(data, mapping = aes(), image_path = system.file("extdata/boule.png", package = "clessnize"), geom = "bar", ...) {
  # Create the base ggplot object
  plot <- ggplot2::ggplot(data, mapping, ...)
  
  # Add the appropriate geom to the plot
  if (geom == "bar") {
    plot <- plot + ggplot2::geom_bar(stat = "identity", fill = NA, color = "black")
  } else if (geom == "hist") {
    plot <- plot + ggplot2::geom_histogram(fill = NA, color = "black")
  }
  
  # Apply the image overlay
  plot <- add_image_bars(plot, data, image_path, geom)
  
  return(plot)
}

#' Print ASCII portrait of JML
#'
#' This function prints an ASCII art portrait of our boss that fits nicely in the RStudio console.
#'
#' @return No return value, called for side effect (printing ASCII art)
#' @export
#'
#' @examples
#' jml()
jml <- function() {
  ascii_art <- "
                                +@%#+=*%@#%%@#                                    
                             *%@@@%@@@@@%%@@@@@@@@@                               
                           +%%@@@@##*+##@%%%%%%@@@@@#                             
                          %@@@@@#+++==+*+*++-+*#%@@@@@                            
                         @@@@@#+++===---=-==---==+@@@@@                           
                        +@@@#*+*++==-------::----==+%@@@                          
                        @@@#++*++===---::::::::---==+##@@                         
                        @@@++++++==---:::::::::---==+++@*                         
                        @%+=*+++==----::::::::----==+++#*                         
                        =*+=++++==---::::::::::----=+++*=                         
                        ++==+*++====+*+=-----------==+==                          
                       =%+=-+*=+=@*@*-=+%=:-%*#*%#@+*+==                          
                       =*#*=**=---=----===:==:=-==--=+-=                          
                        *+##*#+------:--==-==-------=+=                           
                        ++***#+=---:---=*=--=--:::--=*=                           
                        =-#**+++=--=--+=+=:-+===----=+*                           
                         *%##*=++==---==#@@%+=======++*                           
                           #%+===---===--::------==*=+                            
                           @@*=-+---=@+-::::-%%==-=-=*                            
                            %@+===---==--:----=---==*                             
                            @%@+=#=---------=---=++#                              
                             %%@+-*+=----::----=++#                               
                           ++%%#%*=+*==-----==+*+=                                
                          %@::-%*****#*++***%#**#                                 
                         %@%*::::.+++*+*++**#***=                                 
                       *@@%%%*:::....==+*+===++++:                                
                    *@@%%%%%%%+::.:....======+=+-=.                               
                 %@@@@@@@@%%%%%+::........======--..@%                            
             *@@@@@@@%%%@@@%%%%%%:..::......===--:-.:@@@%+                        
          %@@@@@@@@%%%%%%@@@%%%%%%..::.....-:.:=-:.=-:@@%%%%%#                    
     *@@@@@@@@@@@%%%%%%%%@@@@%%%%%%.....--..::.::+-:.+%%%%%%%%%@%%                
  @@@@@@@@@@@@@%%%%%%%%%%%%@%@%%%%%%..::::::..:--:::@@%%%%%%%%%%%@@#              
*@@@@@@@@%%%%%%%%%%%%%%%%%%%%%%%%%%%+.........:-:..%%%%%%%%%%%%%%%@@@             
@@@@@@@@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%....:....::.:.=%%%%%%@%%%%%%@@@@%            
@@@@@@@@@@@%%%%%%%%%%%%%%%%%%%%%%%%%%%.:.::...::.:..%%%%%%%%%%%@@@@@%@            
@@@@@@@@%@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%.:.::.::..:..%%%%%%%%%@%@@@@@@@+           
@@@@@@@@@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*.:.::..:.::.:%%%%%%%%@%@@@@@@@%           
@@@@@@@@@@%%%%%%%%%%%%%%%%%%%%%%@%%%%%%%....:::::.:..%%%%%%%%@@@@@@@@%@           
@@@@@@@@%%%%%%%%%%%%%%%%%%%%%%%%%@@%%%%%%.:..:.:..::.%%%%%%%%%@@@@@@@%@@          
@@@@@@@@%%%%%%%%%%%%%%%%%%%%%%%%%%@@%%%%%@.:.::.:..:.:%%%%%%%%@@@@@@@@%@          
@@@@@@@@%%%%%%%%%%%%%%%%%%%%%%%%%%@@@%%%%%:.:.::...:..%%%%%%%%@@@@@@@@%@%         
"
  cat(ascii_art)
  invisible(NULL)
}
