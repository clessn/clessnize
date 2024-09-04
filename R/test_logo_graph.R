# Charger les bibliothèques nécessaires
library(ggplot2)
library(dplyr)
library(grid)
library(png)

# Chargez votre jeu de données
data <- read.csv("/home/alexab/Dropbox/CLESSN/omnibus/_SharedFolder-omnibus/AllMonths/omnibusMain.csv")

# Charger le logo
logo_path1 <- "/home/alexab/Dropbox/CLESSN/clessnize/inst/extdata/CLESSN-UL-NOIR.png"
logo_path2 <- "/home/alexab/Dropbox/CLESSN/clessnize/inst/extdata/CAPP_2023_NB2.png"

# Lire le fichier PNG et créer un rasterGrob pour le logo
logo1 <- png::readPNG(logo_path1)
logo2 <- png::readPNG(logo_path2)

# Créer un rasterGrob pour le logo avec redimensionnement
logo_grob1 <- rasterGrob(logo1, interpolate = TRUE, 
                        width = unit(3, "inches"), height = unit(1.5, "inches"))
logo_grob2 <- rasterGrob(logo2, interpolate = TRUE, 
                         width = unit(2.5, "inches"), height = unit(1.5, "inches"))

# Calculer les comptages pour chaque niveau de religion
dataGraph <- data |>
  group_by(ses_religion) |>
  summarise(count = n())

# Créer le graphique principal
graph <- ggplot(data = dataGraph, aes(x = ses_religion, y = count)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "TEST",
       x = "Religion",
       y = "Nombre de répondants") +
  coord_cartesian(clip = "off") +  # Permet d'afficher le logo en dehors des marges du graphique
  theme(plot.margin = unit(c(1, 1, 6, 1), "lines"))

# Ajouter le logo au graphique
graph_with_logos <- graph +
  annotation_custom(logo_grob1, xmin = 10, xmax = 15, ymin = -400, ymax = -650) +
  annotation_custom(logo_grob2, xmin = 8, xmax = 13, ymin = -250, ymax = -650)

# Afficher le graphique final avec le logo
print(graph_with_logo)


