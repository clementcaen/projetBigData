# Etude des relations entre variables qualitatives

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

print(col_grav(dataV2))

