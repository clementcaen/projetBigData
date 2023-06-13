

data = stat_acc_V3

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

convert_tableau<- function(donnee){
  
  # conversion colonnes non multi-modales
  donnee["Num_Acc"] <- convertNumeric(donnee["Num_Acc"])
  donnee["id_usa"] <- convertNumeric(donnee["id_usa"])
  donnee["date"] <- convertDate(donnee["date"])
  donnee["id_code_insee"] <- convertNumeric(donnee["id_code_insee"])
  donnee["latitude"] <- convertNumeric(donnee["latitude"])
  donnee["longitude"] <- convertNumeric(donnee["longitude"])
  donnee["an_nais"] <- convertDate(donnee["an_nais"])
  donnee["age"] <- convertNumeric(donnee["age"])
  donnee["place"] <- convertNumeric(donnee["place"])
  
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
 
  write.xlsx(donnee, file="dataV2.xlsx")
}


print(convert_tableau(data))

