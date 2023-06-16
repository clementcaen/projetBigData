source("Préparation.R")

library(forecast)
library(dplyr)
library(ggplot2)
library(caTools)
library(Metrics)

#Etude des relations entre variables qualitatives
  # Faire des tableaux croisées et des tests d’indépendance du chi2 sur les tableaux entre les différentes variables
   #Marzhin
# Et Représenter graphiquement ces tableaux (mosaicplot) et les analyser
    #Marzhin
athmo_grav <- function(donnee){
  tableau_croise <- table(donnee$descr_athmo, donnee$descr_grav)
  print(tableau_croise)
  couleurs <- c("#B3D9FF", "#B3FFD9","#FFD9B3" , "#FFB3D9", "#D9B3FF", "#D9FFB3")
  mosaicplot(tableau_croise, main = "Tableau croisé de l'athmosphère et de la gravité", xlab = "atmosphère", ylab = "gravité", color=couleurs)
  resultats_chi2 <- chisq.test(tableau_croise)
}

surf_grav <- function(donnee){
  tableau_croise <- table(donnee$descr_etat_surf, donnee$descr_grav)
  print(tableau_croise)
  couleurs <- c("#B3D9FF", "#B3FFD9","#FFD9B3" , "#FFB3D9", "#D9B3FF", "#D9FFB3")
  mosaicplot(tableau_croise, main = "Tableau croisé de la surface et de la gravité", xlab = "surface", ylab = "gravité", color=couleurs)
  resultats_chi2 <- chisq.test(tableau_croise)
}

lum_grav<- function(donnee){
  tableau_croise <- table(donnee$descr_lum, donnee$descr_grav)
  print(tableau_croise)
  couleurs <- c("#B3D9FF", "#B3FFD9","#FFD9B3" , "#FFB3D9", "#D9B3FF", "#D9FFB3")
  mosaicplot(tableau_croise, main = "Tableau croisé de la lûmière et de la gravité", xlab = "athmosphère", ylab = "gravité", color=couleurs)
  resultats_chi2 <- chisq.test(tableau_croise)
}

secu_grav<- function(donnee){
  tableau_croise <- table(donnee$descr_dispo_secu, donnee$descr_grav)
  print(tableau_croise)
  couleurs <- c("#B3D9FF", "#B3FFD9","#FFD9B3" , "#FFB3D9", "#D9B3FF", "#D9FFB3")
  mosaicplot(tableau_croise, main = "Tableau croisé des éléments de sécurité et de la gravité", xlab = "éléments de sécurité", ylab = "gravité", color=couleurs)
  resultats_chi2 <- chisq.test(tableau_croise)
}
vehicule_agglo<- function(donnee){
  tableau_croise <- table(donnee$descr_cat_veh, donnee$descr_agglo)
  print(tableau_croise)
  couleurs <- c("#B3D9FF", "#B3FFD9","#FFD9B3" , "#FFB3D9", "#D9B3FF", "#D9FFB3")
  mosaicplot(tableau_croise, main = "Tableau croisé des véhicules et de l'agglo", xlab = "véhicule", ylab = "agglo", color=couleurs)
  resultats_chi2 <- chisq.test(tableau_croise)
}
col_grav<- function(donnee){
  tableau_croise <- table(donnee$descr_type_col, donnee$descr_grav)
  print(tableau_croise)
  couleurs <- c("#B3D9FF", "#B3FFD9","#FFD9B3" , "#FFB3D9", "#D9B3FF", "#D9FFB3")
  mosaicplot(tableau_croise, main = "Tableau croisé du type de collision et de la gravité", xlab = "type de collision", ylab = "gravité", color=couleurs)
  resultats_chi2 <- chisq.test(tableau_croise)
}

#athmo_grav(donnee = data)
#surf_grav(donnee = data)
#lum_grav(donnee = data)
#secu_grav(donnee = data)
#vehicule_agglo(donnee = data)
#col_grav(donnee = data)


# Calculer les régressions linéaires de l’évolution du nombre d’accidents par mois, puis par semaine.
 # Comparer les résultats obtenus par les deux régressions mentionnées ci-dessus
  # Analyser les performances de la régression (proportion de la variabilité due aux résidus et aux variables ex.)
   #Chloé
performance_regression <- function(data){
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
}
#performance_regression(data)


  # Analyser des erreurs types associés aux estimateurs
   #Chloé
# Créer la série chronologique par mois
mensuel <- function (data){
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

  # Créer la série chronologique par mois
  monthly_data <- data %>%
    mutate(month = format(date, "%Y-%m")) %>%
    group_by(month) %>%
    summarise(total_accidents = n())

  # Fractionner les données en ensembles d'entraînement et de test
  train_monthly <- monthly_data[1:6, ]
  test_monthly <- monthly_data[7:12, ]

  # Régression linéaire pour l'agrégation mensuelle
  lm_monthly <- lm(total_accidents ~ as.Date(paste0(month, "-01"), format = "%Y-%m-%d"), data = train_monthly)
  predicted_monthly <- predict(lm_monthly, newdata = test_monthly) # Prédictions mensuelles
  # Calcul de l'erreur relative pour les données mensuelles
  relative_error_monthly <- abs((monthly_data$total_accidents - predicted_monthly) / monthly_data$total_accidents)
  # Afficher l'erreur relative pour les données mensuelles
  cat("Erreur relative mensuelle :", mean(relative_error_monthly))

  # Calculer les intervalles de confiance à 95% pour les données mensuelles
  monthly_ci <- predict(lm_monthly, interval = "confidence", level = 0.95)
  # Imprimer les intervalles de confiance à 95% pour les données mensuelles
  cat("Intervalles de confiance à 95% pour les données mensuelles:\n")
  print(monthly_ci)

}
#mensuel(data)

hebdomadaire <- function (data){
  # Créer la série chronologique par semaine
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

  # Pour l'erreur relative
  weekly_data <- data %>%
    mutate(week = format(date, "%Y-%W")) %>%
    group_by(week) %>%
    summarise(total_accidents = n())

  # Fractionner les données en ensembles d'entraînement et de test
  train_weekly <- weekly_data[1:40, ]
  test_weekly <- weekly_data[41:52, ]

  # Régression linéaire pour l'agrégation hebdomadaire
  lm_weekly <- lm(total_accidents ~ as.Date(paste0(week, "-1"), format = "%Y-%U-%u"), data = train_weekly)
  predicted_weekly <- predict(lm_weekly, newdata = test_weekly) # Prédictions hebdomadaires

  # Calcul de l'erreur relative pour les données hebdomadaires
  relative_error_weekly <- abs((weekly_data$total_accidents - predicted_weekly) / weekly_data$total_accidents)

  # Afficher l'erreur relative pour les données hebdomadaires
  cat("Erreur relative pour les données hebdomadaires :", mean(relative_error_weekly))


# Calculer les intervalles de confiance à 95% pour les données hebdomadaires
weekly_ci <- predict(lm_weekly, interval = "confidence", level = 0.95)
# Imprimer les intervalles de confiance à 95% pour les données hebdomadaires
cat("Intervalles de confiance à 95% pour les données hebdomadaires:\n")
print(weekly_ci)

}
#hebdomadaire(data)

   # Calculer les R2 et R2 ajusté pour les deux modèles. 
   #Chloé
r2_sansintervale_confiance_mensuel <- function (data){
  # Mensuel
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
}
#r2_sansintervale_confiance_mensuel(data)

#semaine
r2_sansintervale_confiance_hebdo <- function (data){
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
}
#r2_sansintervale_confiance_hebdo(data)

# Mensuel sur intervalle de confiance
r2_avecintervale_confiance_mensuel <- function (data){
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
}
#r2_avecintervale_confiance_mensuel(data)

# Hebdomadaire sur intervalle de confiance
r2_avecintervale_confiance_hebdo <- function (data){
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

  # Graphique avec les intervalles de confiance pour les données hebdomadaires
  ggplot(weekly_plot_data, aes(x = x, y = y)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, color = "blue") +
    geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound), alpha = 0.3, fill = "blue") +
  labs(x = "Semaine", y = "Nombre d'accidents", title = "Régression linéaire - Données hebdomadaires")
}
#r2_avecintervale_confiance_hebdo(data)
