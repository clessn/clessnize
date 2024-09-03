library(ggplot2)
library(dplyr)
library(ggimage)

# Chargez votre jeu de données
data <- read.csv("/home/alexab/Dropbox/CLESSN/omnibus/_SharedFolder-omnibus/AllMonths/omnibusMain.csv")

# Charger le logo
logo_path <- "/home/alexab/Dropbox/CLESSN/clessnize/inst/extdata/Logo_Partenaire_-51.png"

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
  annotation_custom(t, xmin = 6.5, xmax = 8.5, ymin = -5, ymax = -8.5) +
  coord_cartesian(clip = "off") +
  theme(plot.margin = unit(c(1, 1, 3, 1), "lines"))

# Afficher le graphique final avec le logo
print(graph)

