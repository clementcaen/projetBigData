library(datasets)
library(dplyr)
library(ggplot2)

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

# Evolution par mois du nombre d'accidents

library(dplyr) # manipulation de données
library(ggplot2) # réalisation de graphiques


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
# Créer la série chronologique par semaine
weekly_data <- data %>%
  mutate(week = format(date, "%Y-%U")) %>%
  group_by(week) %>%
  summarise(total_accidents = n())

# Tracer la série chronologique par semaine
ggplot(weekly_data, aes(x = as.Date(paste0(week, "-1"), "%Y-%U-%u"), y = total_accidents)) +
  geom_line() +
  labs(x = "Semaine", y = "Nombre d'accidents", title = "Évolution hebdomadaire du nombre d'accidents")

# Agréger les données par mois
monthly_aggregated <- aggregate(total_accidents ~ month, data = monthly_data, FUN = length)
#print(monthly_aggregated)

# Agréger les données par semaine
weekly_aggregated <- aggregate(total_accidents ~ week, data = weekly_data, FUN = length)
#print(weekly_aggregated)


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
  #print(codegeo)
  #print(codegeo_to_insee(codegeo))
}

#merging_tableau <- merge(total_pop, data, by="id_code_insee")

# on groupe par région les accidents
#accid_by_reg <- aggregate(place ~ REG, data = merging_tableau, FUN = sum) # on somme les personnes touchés par l'accidents (place) pour connaitre le nombre d'affecté par les accident par région
#print(accid_by_reg)

# On trie par gravité ? (histogramme)


# récpapitulatif des variables préparées
# data : Tableau d'Accidents organisés
# monthly_aggregated :
# weekly_aggregated :
# total_pop : population par région
# accid_by_reg : accident pour 100k habitant
