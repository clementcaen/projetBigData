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

#print(data)


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
  corse = c(2A, 2B),
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