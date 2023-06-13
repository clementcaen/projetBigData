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
data["id_code_insee"] <- convertNumeric(data["id_code_insee"])
data["latitude"] <- convertNumeric(data["latitude"])
data["longitude"] <- convertNumeric(data["longitude"])
data["an_nais"] <- convertNumeric(data["an_nais"])
data["age"] <- convertNumeric(data["age"])
data["place"] <- convertNumeric(data["place"])

# Marzhin
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

print(data)


# Clément 1.5 (bonus)
# on creer un tableau avec les données du nombre d'habitant par commune https://www.insee.fr/fr/statistiques/2044741
#stat_pop <- read.csv("base-cc-evol-struct-pop-2009.csv", sep=';')
# P09_POP: population | CODGEO: code insee | REG: id_region

# On groupe par région avec la sommme des habitants des villes (total d'habitant par région) et les divisant par 100 000 pour avoir les données pour 100 000 habitant
#total_pop <- aggregate(P09_POP ~ REG, data = stat_pop, FUN = sum)
#pop100000_by_reg <- NULL
#pop100000_by_reg <- data.frame(total_pop$REG, total_pop$P09_POP /100000)
#print(pop100000_by_reg)

# On trouve tous les accidents dans la région en mergeant les 2 tableaux
#stat_pop$id_code_insee <- stat_pop$CODGEO
#merging_tableau <- merge(total_pop, data, by="id_code_insee")

#print(merging_tableau)

# on groupe par région les accidents
#accid_by_reg <- aggregate(place ~ REG, data = merging_tableau, FUN = sum) # on somme les personnes touchés par l'accidents (place) pour connaitre le nombre d'affecté par les accident par région
#print(accid_by_reg)

# On trie par gravité ? (histogramme)



# clément

hist(as.numeric(strftime(data$date, format = "%H")))
