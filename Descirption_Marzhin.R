data = stat_acc_V3

vehicule=unique(data["num_veh"])
print(vehicule)
print(n=58, vehicule)

categorie_vehicule=unique(data["descr_cat_veh"])
print(n=24, categorie_vehicule)

agglo=unique(data["descr_agglo"])
print(agglo)
athmo=unique(data["descr_athmo"])
print(athmo)
luminosite=unique(data["descr_lum"])
print(luminosite)
etat_surf=unique(data["descr_etat_surf"])
print(etat_surf)
intersection=unique(data["description_intersection"])
print(intersection)
secu=unique(data["descr_dispo_secu"])
print(secu)
gravite=unique(data["descr_grav"])
print(gravite)
typr_col=unique(data["descr_type_col"])
print(typr_col)
trajet=unique(data["descr_motif_traj"])
print(trajet)


#les numéros associés:

#Athmospérique

# Autre = 1
# Brouillard - fumée = 2
# Neige - grêle = 3
# Normale = 4
# Pluie forte = 5
# Pluie légère = 6
# Temps couvert = 7
# Temps éblouissant = 8
# Vent fort - Tempête = 9

#Gravité

# Indemne = 1
# Blessé léger = 2
# Blessé hospitalisé = 3
# Tué = 4

#Surface 

# Autre = 1
# Boue = 2
# Corps gras - Huile = 3
# Enneigée = 4
# Flaques = 5
# Inondée = 6
# Mouillée = 7
# Normale = 8
# Verglacée = 9


