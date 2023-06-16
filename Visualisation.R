library(geojsonR)
library(dplyr)
library(ggplot2)

# récupération de préparation
source("Préparation.R")

# 1.Créer des représentations graphiques pour:
  #Nombre d’accidents en fonction des conditions atmosphériques
    #Marzhin
acc_atmo <- function(donnee){
  hist(donnee$descr_athmo,
          xlab = "Conditions Atmosphériques", ylab = "Nombre d'Accidents",
          main = "Nombre d'Accidents en fonction des Conditions Atmosphériques",
          col = "skyblue", las = 2)
}
acc_atmo(donnee = data)

  # Nombre d’accidents en fonction de la description de la surface
    #Marzhin
acc_surf <- function(donnee){
  hist(donnee$descr_etat_surf,
       xlab = "Conditions de surface", ylab = "Nombre d'Accidents",
       main = "Nombre d'Accidents en fonction des conditions de surface",
       col = "skyblue", las = 2)
}
acc_surf(data)

  # Nombre d’accidents selon la gravité
    #Marzhin
acc_grav <- function(donnee){
  hist(donnee$descr_grav,
       xlab = "Gravité", ylab = "Nombre d'Accidents",
       main = "Nombre d'Accidents en fonction de la gravité",
       col = "skyblue", las = 2)
}
acc_surf(donnee = data)

  # Nombre d’accidents par tranches d’heure
    #Clément
# histogramme par heure
hist(as.numeric(strftime(data$date, format = "%H")), main = "Histogramme d'accident par heure")

  # Nombre d’accidents par ville
    #Clément
#hist(data$id_code_insee, labels = TRUE, xlab = data$ville)
ville_plus_touche <- head(sort(table(data$ville), decreasing = TRUE), 12)
barplot(ville_plus_touche, las=2) # las met les nom des villes à la verticale


#2.Créer des histogrammes
# Quantité d’accidents en fonction des tranches d’âges
  #Chloé

data <- read.csv("stat_acc_V3.csv", sep=";")

# Convertir la colonne "age" en type numérique
data$age <- as.numeric(data$age)
# Créer des tranches d'âge
data <- data %>%
  mutate(age_group = cut(age, breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, Inf), labels = c("0-10", "11-20", "21-30", "31-40", "41-50", "51-60", "61-70", "71-80", "81-90", "91-100", "100+"), include.lowest = TRUE))
# Calculer la quantité d'accidents par tranche d'âge
age_summary <- data %>%
  group_by(age_group) %>%
  summarise(total_accidents = n())
# Afficher l'histogramme de la quantité d'accidents par tranche d'âge
ggplot(age_summary, aes(x = age_group, y = total_accidents)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Tranche d'âge", y = "Quantité d'accidents", title = "Quantité d'accidents en fonction des tranches d'âge")


# Moyenne mensuelle des accidents
  #Chloé
data <- read.csv("stat_acc_V3.csv", sep=";")
data$date <- as.Date(data$date)
# Créer la série chronologique par mois
monthly_data <- data %>%
  mutate(month = format(date, "%Y-%m")) %>%
  group_by(month) %>%
  summarise(total_accidents = n())
# Calculer la moyenne mensuelle des accidents
monthly_mean <- monthly_data %>%
  group_by(month) %>%
  summarise(mean_accidents = mean(total_accidents))
# Tracer l'histogramme de la moyenne mensuelle des accidents
ggplot(monthly_mean, aes(x = month, y = mean_accidents)) +
  geom_bar(stat = "identity", fill = "steelblue", color = "black") +
  labs(x = "Mois", y = "Moyenne mensuelle des accidents", title = "Histogramme de la moyenne mensuelle des accidents")


# Proposer une représentation sous formes de carte de la quantité d’accidents enregistrés par
  #Clément
  # voir carte.R