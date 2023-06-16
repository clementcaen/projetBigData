

# 1.Créer des représentations graphiques pour:

 
  #Nombre d’accidents en fonction des conditions atmosphériques
    #Marzhin
 


  # Nombre d’accidents en fonction de la description de la surface
    #Marzhin



  # Nombre d’accidents selon la gravité
    #Marzhin



  # Nombre d’accidents par tranches d’heure
    #Clément



  # Nombre d’accidents par ville
    #Clément



#2.Créer des histogrammes 


# Quantité d’accidents en fonction des tranches d’âges
  #Chloé

data <- read.csv("C:/Users/chloe OBIANG/Downloads/stat_acc_V3.csv", sep=";")
library(dplyr)
library(ggplot2)
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

library(dplyr)
library(ggplot2)
data <- read.csv("C:/Users/chloe OBIANG/Downloads/stat_acc_V3.csv", sep=";")
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

  #région 



  #départements



# Même chose avec les taux d’accidents graves
  #Clément

  #région


  #départements



