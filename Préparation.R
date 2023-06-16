library(datasets)
stat_acc_V3 <- read.csv("stat_acc_V3.csv", sep=';')
data <- stat_acc_V3



# Clément 1.2
# Mettre les variables numériques sous format numériques, date sous format date
convertDate <- function (colonneCsv){
  new_date <- NULL
  for (each_date in colonneCsv){
    new_date <- append(new_date, as.POSIXct(each_date, format = "%Y-%m-%d %H:%M:%S")) # format represente les dates venant du csv ex:2009-10-04 20:50:00
  }
  return(new_date)
}
convertNumeric <- function (colonneCsv){
  new_num <- NULL
  for (each_number in colonneCsv){
    new_num <- append(new_num, as.double(each_number))
  }
  return(new_num)
}
# conversion colonnes non multi-modales
data["Num_Acc"] <- convertNumeric(data["Num_Acc"])
data["id_usa"] <- convertNumeric(data["id_usa"])
data["date"] <- convertDate(data["date"])
#data["id_code_insee"] <- convertNumeric(data["id_code_insee"]) # on ne les convertis pas à cause de la Corse (2A)
data["latitude"] <- convertNumeric(data["latitude"])
data["longitude"] <- convertNumeric(data["longitude"])
data["an_nais"] <- convertNumeric(data["an_nais"])
data["age"] <- convertNumeric(data["age"])
data["place"] <- convertNumeric(data["place"])

# Marzhin 1.3
convert_tableau<- function(donnee){
  # conversion colonnes multimodales
  donnee$num_veh <- factor(donnee$num_veh)
  donnee$num_veh <- as.numeric(donnee$num_veh)
  
  donnee$descr_cat_veh <- factor(donnee$descr_cat_veh)
  donnee$descr_cat_veh <- as.numeric(donnee$descr_cat_veh)
  
  donnee$descr_agglo <- factor(donnee$descr_agglo)
  donnee$descr_agglo <- as.numeric(donnee$descr_agglo)
  
  donnee$descr_athmo <- factor(donnee$descr_athmo)
  donnee$descr_athmo <- as.numeric(donnee$descr_athmo)
  
  donnee$descr_lum <- factor(donnee$descr_lum)
  donnee$descr_lum <- as.numeric(donnee$descr_lum)
  
  donnee$descr_etat_surf <- factor(donnee$descr_etat_surf)
  donnee$descr_etat_surf <- as.numeric(donnee$descr_etat_surf)
  
  donnee$description_intersection <- factor(donnee$description_intersection)
  donnee$description_intersection <- as.numeric(donnee$description_intersection)
  
  donnee$descr_dispo_secu <- factor(donnee$descr_dispo_secu)
  donnee$descr_dispo_secu <- as.numeric(donnee$descr_dispo_secu)
  
  donnee$descr_grav <- factor(donnee$descr_grav, levels = c("Indemne", "Blessé léger", "Blessé hospitalisé", "Tué"))
  donnee$descr_grav <- as.numeric(donnee$descr_grav)
  
  donnee$descr_motif_traj <- factor(donnee$descr_motif_traj)
  donnee$descr_motif_traj <- as.numeric(donnee$descr_motif_traj)

  donnee$descr_type_col <- factor(donnee$descr_type_col)
  donnee$descr_type_col <- as.numeric(donnee$descr_type_col)
 
  #write.xlsx(donnee, file="dataV2.xlsx")
  return (donnee)
}


data <- convert_tableau(data)

#Chloé 1.4

#Evolution par mois du nombre d’accident

library(dplyr) # manipulation de donnée
library(ggplot2) # réalisation de graphique

data <- read.csv("stat_acc_V3.csv", sep=";")
# Convertir la colonne "date" en type "Date"
data$date <- as.Date(data$date)
# Créer la série chronologique par mois
monthly_data <- data %>%
  mutate(month = format(date, "%Y-%m")) %>%
  group_by(month) %>%
  summarise(total_accidents = n())
# Tracer la série chronologique par mois
ggplot(monthly_data, aes(x = as.Date(paste0(month, "-01")), y = total_accidents)) +
  geom_line() +
  labs(x = "Mois", y = "Nombre d'accidents", title = "Évolution mensuelle du nombre d'accidents")

#Evolution par semaine du nombre d’accident

library(dplyr)
library(ggplot2)

# Créer la série chronologique par semaine
weekly_data <- data %>%
  mutate(week = format(date, "%Y-%W")) %>%
  group_by(week) %>%
  summarise(total_accidents = n())
# Tracer la série chronologique par semaine
ggplot(weekly_data, aes(x = as.Date(paste0(week, "-1"), "%Y-%U-%u"), y = total_accidents)) +
  geom_line() +
  labs(x = "Semaine", y = "Nombre d'accidents", title = "Évolution hebdomadaire du nombre d'accidents")

# Agréger les données par mois
monthly_aggregated <- aggregate(total_accidents ~ format(date, "%Y-%m"), data = data, FUN = sum)
print(monthly_aggregated)

# Agréger les données par semaine
weekly_aggregated <- aggregate(total_accidents ~ format(date, "%Y-%U"), data = data, FUN = sum)
print(weekly_aggregated)


# Clément 1.5 (bonus)
# on creer un tableau avec les données du nombre d'habitant par commune https://www.insee.fr/fr/statistiques/2044741
stat_pop <- read.csv("base-cc-evol-struct-pop-2009.csv", sep=';')
# P09_POP: population | CODGEO: code insee | REG: id_region

# On groupe par région avec la sommme des habitants des villes (total d'habitant par région) et les divisant par 100 000 pour avoir les données pour 100 000 habitant
total_pop <- aggregate(P09_POP ~ REG, data = stat_pop, FUN = sum)
pop100000_by_reg <- NULL
pop100000_by_reg <- data.frame(total_pop$REG, total_pop$P09_POP /100000)
#print(pop100000_by_reg)

# On trouve tous les accidents dans la région en mergeant les 2 tableaux

# on rajoute les code insee correspondnt au code geo (CODGEO : Code du département suivi du numéro de commune ou du numéro d'arrondissement municipal /source:https://www.insee.fr/fr/statistiques/2044741#dictionnaire)
# Le code insee de chaque commune/ville/village est constitué de 5 chiffres, les 2 premiers sont le numéro du département (codé sur 2 chiffres) à laquelle la ville est rattachée et les 3 autres chiffres sont un code donné à la commune (codée sur 3 chiffres).
codegeo_to_insee <- function (codegeo){
  code_departement <- substr(codegeo, 1, 2)
  code_commune <- substr(codegeo, 3, nchar(codegeo))

  return (paste0(code_departement, code_commune))
}
stat_pop$id_code_insee <- NULL
for (codegeo in stat_pop$CODGEO){
  print(codegeo)
  print(codegeo_to_insee(codegeo))
}

merging_tableau <- merge(total_pop, data, by="id_code_insee")

# on groupe par région les accidents
accid_by_reg <- aggregate(place ~ REG, data = merging_tableau, FUN = sum) # on somme les personnes touchés par l'accidents (place) pour connaitre le nombre d'affecté par les accident par région
#print(accid_by_reg)

# On trie par gravité ? (histogramme)



# clément
# historgramme par heure
hist(as.numeric(strftime(data$date, format = "%H")), main = paste("Histogramme ", "d'accident par heure"))

#hist(data$id_code_insee, labels = TRUE, xlab = data$ville)
ville_plus_touche <- head(sort(table(data$ville), decreasing = TRUE), 10)
barplot(ville_plus_touche, las=2) # las met les nom des villes à la verticale

# dico regions:
regions <- list(
  alsace = c(67, 68),
  aquitaine = c(24, 33, 40, 47, 64),
  auvergne = c(3, 15, 43, 63),
  bourgogne = c(21, 58, 71, 89),
  bretagne = c(22, 29, 35, 56),
  centre = c(18, 28, 36, 37, 41, 45),
  champagne = c(8, 10, 51, 52),
  #corse = c(2A, 2B),
  franche_comte = c(25, 39, 70, 90),
  ile_de_france = c(75, 77, 78, 91, 92, 93, 94, 95),
  languedoc_roussillon = c(11, 30, 34, 48, 66),
  limousin = c(19, 23, 87),
  lorraine = c(54, 55, 57, 88),
  midi_pyrenees = c(9, 12, 31, 32, 46, 65, 81, 82),
  nord_pas_de_calais = c(59, 62),
  basse_normandie = c(14, 50, 61),
  haute_normandie = c(27, 76),
  pays_de_la_loire = c(44, 49, 53, 72, 85),
  picardie = c(2, 60, 80),
  poitou_charentes = c(16, 17, 79, 86),
  provence_alpes_cote_d_azur = c(4, 5, 6, 13, 83, 84),
  rhone_alpes = c(1, 7, 26, 38, 42, 69, 73, 74)
)

#Chloé visualisation.2

#histogrammes

#Quantité d'accident en fonction des tranches d'ages


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

#moyenne mensuelle des accidents

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


#Chloé Analyse.2

#Analyser les performances de la régression (proportion de la variabilité due aux résidus et aux variables explicatives)

#RMSE

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

#Analyser des erreurs types associés aux estimateurs 

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
# mensuel

library(dplyr)
library(ggplot2)
library(forecast)
# Calculer les intervalles de confiance à 95% pour les données mensuelles
monthly_ci <- predict(lm_monthly, interval = "confidence", level = 0.95)
# Imprimer les intervalles de confiance à 95% pour les données mensuelles
cat("Intervalles de confiance à 95% pour les données mensuelles:\n")
print(monthly_ci)

#hebdomadaire

library(dplyr)
library(ggplot2)
library(forecast)
# Calculer les intervalles de confiance à 95% pour les données hebdomadaires
weekly_ci <- predict(lm_weekly, interval = "confidence", level = 0.95)
# Imprimer les intervalles de confiance à 95% pour les données hebdomadaires
cat("Intervalles de confiance à 95% pour les données hebdomadaires:\n")
print(weekly_ci)


#Calculer les R2 et R2 ajusté pour les deux modèles. Qu’en déduire ? 
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

