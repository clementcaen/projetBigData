# Etude des relations entre variables qualitatives
  # Faire des tableaux croisées et des tests d’indépendance du chi2 sur les tableaux entre les différentes variables
   #Marzhin


  # Représenter graphiquement ces tableaux (mosaicplot) et les analyser
    #Marzhin


# Calculer les régressions linéaires de l’évolution du nombre d’accidents par mois, puis par semaine.
 # Comparer les résultats obtenus par les deux régressions mentionnées ci-dessus

  # Analyser les performances de la régression (proportion de la variabilité due aux résidus et aux variables ex.)
   #Chloé    

library(caTools)
library(Metrics)

# Division de la base de données en ensembles d'entraînement et de test pour l'agrégation mensuelle
train_monthly <- monthly_data[1:round(0.7 * nrow(monthly_data)), ]
test_monthly <- monthly_data[(round(0.7 * nrow(monthly_data)) + 1):nrow(monthly_data), ]

# Division de la base de données en ensembles d'entraînement et de test pour l'agrégation hebdomadaire
train_weekly <- weekly_data[1:round(0.7 * nrow(weekly_data)), ]
test_weekly <- weekly_data[(round(0.7 * nrow(weekly_data)) + 1):nrow(weekly_data), ]

# Régression linéaire pour l'agrégation mensuelle
lm_monthly <- lm(total_accidents ~ as.Date(paste0(month, "-01")), data = train_monthly)
predicted_monthly <- predict(lm_monthly, newdata = test_monthly) # Prédictions mensuelles

# Régression linéaire pour l'agrégation hebdomadaire
lm_weekly <- lm(total_accidents ~ as.Date(paste0(week, "-1"), format = "%Y-%U-%u"), data = train_weekly)
predicted_weekly <- predict(lm_weekly, newdata = test_weekly) # Prédictions hebdomadaires

# Calcul de la racine carrée de l'erreur quadratique moyenne (RMSE)
rmse_monthly <- rmse(test_monthly$total_accidents, predicted_monthly)
rmse_weekly <- rmse(test_weekly$total_accidents, predicted_weekly)

# Affichage des résultats
print("Résultats pour l'agrégation mensuelle:")
print(rmse_monthly)
print("Résultats pour l'agrégation hebdomadaire:")
print(rmse_weekly)

  # Analyser des erreurs types associés aux estimateurs
   #Chloé

#erreur absolue moyenne mensuelle 

# Charger les bibliothèques nécessaires
library(dplyr)
library(ggplot2)
library(forecast)

# Charger les données mensuelles (remplacez "votre_fichier.csv" par le nom de votre fichier de données)
data <- read.csv("C:/Users/chloe OBIANG/Downloads/stat_acc_V3.csv", sep=";")

# Convertir la colonne "date" en type "Date"
data$date <- as.Date(data$date)

# Créer la série chronologique par mois
monthly_data <- data %>%
  mutate(month = format(date, "%Y-%m")) %>%
  group_by(month) %>%
  summarise(total_accidents = n())

# Ajuster un modèle de régression linéaire pour les données mensuelles
lm_monthly <- lm(total_accidents ~ as.numeric(as.Date(paste0(month, "-01"))), data = monthly_data)

# Prédictions du modèle pour les données mensuelles
predictions_monthly <- predict(lm_monthly)

# Calcul de l'erreur absolue moyenne (MAE) pour les données mensuelles
mae_monthly <- mean(abs(monthly_data$total_accidents - predictions_monthly))

# Afficher l'erreur absolue moyenne pour les données mensuelles
cat("Erreur absolue moyenne (MAE) pour les données mensuelles :", mae_monthly)


# erreur absolue moyenne hebdomadaire


# Charger les bibliothèques nécessaires
library(dplyr)
library(ggplot2)
library(forecast)

# Charger les données mensuelles (remplacez "votre_fichier.csv" par le nom de votre fichier de données)
data <- read.csv("C:/Users/chloe OBIANG/Downloads/stat_acc_V3.csv", sep=";")

# Convertir la colonne "date" en type "Date"
data$date <- as.Date(data$date)

weekly_data <- data %>%
  mutate(week = format(date, "%Y-%W")) %>%
  group_by(week) %>%
  summarise(total_accidents = n())


# Régression linéaire pour l'agrégation hebdomadaire
lm_weekly <- lm(total_accidents ~ as.Date(paste0(week, "-1"), format = "%Y-%U-%u"), data = train_weekly)
predicted_weekly <- predict(lm_weekly, newdata = test_weekly) # Prédictions hebdomadaires


# Calcul de l'erreur absolue moyenne (MAE) pour les données mensuelles
mae_weekly <- mean(abs(weekly_data$total_accidents - predicted_weekly))

# Afficher l'erreur absolue moyenne pour les données mensuelles
cat("Erreur absolue moyenne (MAE) pour les données hebdomadaires :", mae_weekly)

# erreur relative hebdomadaire 

# Charger les bibliothèques nécessaires
library(dplyr)
library(ggplot2)
library(forecast)

# Charger les données mensuelles (remplacez "votre_fichier.csv" par le nom de votre fichier de données)
data <- read.csv("C:/Users/chloe OBIANG/Downloads/stat_acc_V3.csv", sep=";")

# Convertir la colonne "date" en type "Date"
data$date <- as.Date(data$date)

# Créer la série chronologique par semaine
weekly_data <- data %>%
  mutate(week = format(date, "%Y-%W")) %>%
  group_by(week) %>%
  summarise(total_accidents = n())

# Fractionner les données en ensembles d'entraînement et de test
train_weekly <- weekly_data[1:40, ]  # Remplacez les indices appropriés selon votre configuration de données
test_weekly <- weekly_data[41:52, ]  # Remplacez les indices appropriés selon votre configuration de données

# Régression linéaire pour l'agrégation hebdomadaire
lm_weekly <- lm(total_accidents ~ as.Date(paste0(week, "-1"), format = "%Y-%U-%u"), data = train_weekly)
predicted_weekly <- predict(lm_weekly, newdata = test_weekly) # Prédictions hebdomadaires

# Calcul de l'erreur relative pour les données hebdomadaires
relative_error_weekly <- abs((weekly_data$total_accidents - predicted_weekly) / weekly_data$total_accidents)

# Afficher l'erreur relative pour les données hebdomadaires
cat("Erreur relative pour les données hebdomadaires :", mean(relative_error_weekly))

# erreur relative mensuelle

# Charger les bibliothèques nécessaires
library(dplyr)
library(ggplot2)
library(forecast)

# Charger les données mensuelles (remplacez "votre_fichier.csv" par le nom de votre fichier de données)
data <- read.csv("C:/Users/chloe OBIANG/Downloads/stat_acc_V3.csv", sep=";")

# Convertir la colonne "date" en type "Date"
data$date <- as.Date(data$date)

# Créer la série chronologique par mois
monthly_data <- data %>%
  mutate(month = format(date, "%Y-%m")) %>%
  group_by(month) %>%
  summarise(total_accidents = n())

# Fractionner les données en ensembles d'entraînement et de test
train_monthly <- monthly_data[1:6, ]  # Remplacez les indices appropriés selon votre configuration de données
test_monthly <- monthly_data[7:12, ]  # Remplacez les indices appropriés selon votre configuration de données

# Régression linéaire pour l'agrégation mensuelle
lm_monthly <- lm(total_accidents ~ as.Date(paste0(month, "-01"), format = "%Y-%m-%d"), data = train_monthly)
predicted_monthly <- predict(lm_monthly, newdata = test_monthly) # Prédictions mensuelles
# Calcul de l'erreur relative pour les données mensuelles
relative_error_monthly <- abs((monthly_data$total_accidents - predicted_monthly) / monthly_data$total_accidents)
# Afficher l'erreur relative pour les données mensuelles
cat("Erreur relative mensuelle :", mean(relative_error_monthly))


   # Calculer les intervalles de confiance à 95% pour ces estimateurs
   #Chloé


library(dplyr)
library(ggplot2)
library(forecast)
# Calculer les intervalles de confiance à 95% pour les données mensuelles
monthly_ci <- predict(lm_monthly, interval = "confidence", level = 0.95)
# Imprimer les intervalles de confiance à 95% pour les données mensuelles
cat("Intervalles de confiance à 95% pour les données mensuelles:\n")
print(monthly_ci)

# Calculer les intervalles de confiance à 95% pour les données hebdomadaires
library(dplyr)
library(ggplot2)
library(forecast)
weekly_ci <- predict(lm_weekly, interval = "confidence", level = 0.95)
# Imprimer les intervalles de confiance à 95% pour les données hebdomadaires
cat("Intervalles de confiance à 95% pour les données hebdomadaires:\n")
print(weekly_ci)


   # Calculer les R2 et R2 ajusté pour les deux modèles. 
   #Chloé


# Mensuel 
library(dplyr)
library(ggplot2)
data <- read.csv("C:/Users/chloe OBIANG/Downloads/stat_acc_V3.csv", sep=";")
data$date <- as.Date(data$date)
monthly_data <- data %>%
  mutate(month = format(date, "%Y-%m")) %>%
  group_by(month) %>%
  summarise(total_accidents = n())
# Ajuster un modèle de régression linéaire pour les données mensuelles
lm_monthly <- lm(total_accidents ~ as.numeric(as.Date(paste0(month, "-01"))), data = monthly_data)
# Calculer le coefficient de détermination (R²) pour chaque modèle
r_squared_monthly <- summary(lm_monthly)$r.squared
# Afficher les résultats
cat("R² pour les données mensuelles :", r_squared_monthly, "\n")
# Tracer les graphiques de régression pour les deux modèles
ggplot(monthly_data, aes(x = as.numeric(as.Date(paste0(month, "-01"))), y = total_accidents)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "Mois", y = "Nombre d'accidents", title = "Régression linéaire - Données mensuelles")

#semaine
library(dplyr)
library(ggplot2)
data <- read.csv("C:/Users/chloe OBIANG/Downloads/stat_acc_V3.csv", sep=";")
data$date <- as.Date(data$date) 
weekly_data <- data %>%
  mutate(week = format(date, "%Y-%W")) %>%
  group_by(week) %>%
  summarise(total_accidents = n())
# Ajuster un modèle de régression linéaire pour les données hebdomadaires
lm_weekly <- lm(total_accidents ~ as.numeric(as.Date(paste0(week, "-1"), "%Y-%U-%u")), data = weekly_data)
# Calculer le coefficient de détermination (R²) pour chaque modèle
r_squared_weekly <- summary(lm_weekly)$r.squared
# Afficher les résultats
cat("R² pour les données hebdomadaires :", r_squared_weekly, "\n")
# Tracer les graphiques de régression pour les deux modèles

ggplot(weekly_data, aes(x = as.numeric(as.Date(paste0(week, "-1"), "%Y-%U-%u")), y = total_accidents)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "Semaine", y = "Nombre d'accidents", title = "Régression linéaire - Données hebdomadaires")

# Mensuel sur intervalle de confiance

library(dplyr)
library(ggplot2)
library(forecast)

data <- read.csv("C:/Users/chloe OBIANG/Downloads/stat_acc_V3.csv", sep=";")
data$date <- as.Date(data$date)

# Créer la série chronologique par mois
monthly_data <- data %>%
  mutate(month = format(date, "%Y-%m")) %>%
  group_by(month) %>%
  summarise(total_accidents = n())

# Ajuster un modèle de régression linéaire pour les données mensuelles
lm_monthly <- lm(total_accidents ~ as.numeric(as.Date(paste0(month, "-01"))), data = monthly_data)

# Calculer les intervalles de confiance à 95% pour les données mensuelles
monthly_ci <- predict(lm_monthly, interval = "confidence", level = 0.95)

# Créer les dataframes pour le tracé
monthly_plot_data <- data.frame(x = as.numeric(as.Date(paste0(monthly_data$month, "-01"))),
                                y = monthly_data$total_accidents,
                                lower_bound = monthly_ci[, "lwr"],
                                upper_bound = monthly_ci[, "upr"])


# Tracer le graphique avec les intervalles de confiance pour les données mensuelles
ggplot(monthly_plot_data, aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound), alpha = 0.3, fill = "blue") +
  labs(x = "Mois", y = "Nombre d'accidents", title = "Régression linéaire - Données mensuelles")

# Hebdomadaire sur intervalle de confiance

library(dplyr)
library(ggplot2)
library(forecast)

data <- read.csv("C:/Users/chloe OBIANG/Downloads/stat_acc_V3.csv", sep=";")
data$date <- as.Date(data$date)

# Créer la série chronologique par semaine
weekly_data <- data %>%
  mutate(week = format(date, "%Y-%W")) %>%
  group_by(week) %>%
  summarise(total_accidents = n())

# Ajuster un modèle de régression linéaire pour les données hebdomadaires
lm_weekly <- lm(total_accidents ~ as.numeric(as.Date(paste0(week, "-1"), "%Y-%U-%u")), data = weekly_data)

# Calculer les intervalles de confiance à 95% pour les données hebdomadaires
weekly_ci <- predict(lm_weekly, interval = "confidence", level = 0.95)

weekly_plot_data <- data.frame(x = as.numeric(as.Date(paste0(weekly_data$week, "-1"), "%Y-%U-%u")),
                               y = weekly_data$total_accidents,
                               lower_bound = weekly_ci[, "lwr"],
                               upper_bound = weekly_ci[, "upr"])

# Tracer le graphique avec les intervalles de confiance pour les données hebdomadaires
ggplot(weekly_plot_data, aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound), alpha = 0.3, fill = "blue") +
  labs(x = "Semaine", y = "Nombre d'accidents", title = "Régression linéaire - Données hebdomadaires")




