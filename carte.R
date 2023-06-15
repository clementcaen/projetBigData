#install.packages("sf")
#install.packages("leaflet")
#install.packages("magrittr")

library(sf)
library(leaflet)
library(geojsonio)
# représentation sous formes de carte de la quantité d’accidents enregistrés par région puis par départements

isin <- function(x, values) {
  return (x %in% values)
}

regions <- list(
    alsace = c("67", "68"),
    aquitaine = c("24", "33", "40", "47", "64"),
    auvergne = c("03", "15", "43", "63"),
    bourgogne = c("21", "58", "71", "89"),
    bretagne = c("22", "29", "35", "56"),
    centre = c("18", "28", "36", "37", "41", "45"),
    champagne = c("08", "10", "51", "52"),
    corse = c("2A", "2B"),
    franche_comte = c("25", "39", "70", "90"),
    ile_de_france = c("75", "77", "78", "91", "92", "93", "94", "95"),
    languedoc_roussillon = c("11", "30", "34", "48", "66"),
    limousin = c("19", "23", "87"),
    lorraine = c("54", "55", "57", "88"),
    midi_pyrenees = c("09", "12", "31", "32", "46", "65", "81", "82"),
    nord_pas_de_calais = c("59", "62"),
    basse_normandie = c("14", "50", "61"),
    haute_normandie = c("27", "76"),
    pays_de_la_loire = c("44", "49", "53", "72", "85"),
    picardie = c("2", "60", "80"),
    poitou_charentes = c("16", "17", "79", "86"),
    provence_alpes_cote_d_azur = c("04", "05", "06", "13", "83", "84"),
    rhone_alpes = c("01", "07", "26", "38", "42", "69", "73", "74")
)
# Clément (aide Marzhin)
j <- 0
acc_region <- function(donnee, nb_ligne){
  # on fait la carte
  donnee$region <- ""
  for (i in 1:nb_ligne){
    j <- j + 1
    each_code <- donnee$id_code_insee[i]
    dep <- substring(each_code, 1, 2) # on récupère les 2 premier caractère du code insee
    for (reg_name in names(regions)){
      each_region <- regions[[reg_name]]
      if(isin(dep, each_region)){ # regarde si le departement(dep) est dans cette region(each_region)
        #print("Met la regions")
        #print(each_region)
        donnee$region[i] <- reg_name
      }
    }
    donnee$dep[i] <- dep
  }
  return (donnee)
}

# import de préparation
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

data <- acc_region(data,  nrow(data))

# Pour chaque region on fait la somme des accidents
acc_by_reg <- table(data$region)

# Carte avec fleatlet
# Documentation pour R: https://rstudio.github.io/leaflet/json.html
geo_region_json <- geojson_read("regions.geojson")
geo_dep_json <- geojson_read("departements.geojson")
data_gouv_dep_csv <- read.csv("points-extremes-des-departements-metropolitains-de-france.csv")

# histogramme accident par region
middle_dep_lat <- NULL
middle_dep_lon <- NULL
#pour toutes les régions
middle_dep_lat[data_gouv_dep_csv$Departement] <- (data_gouv_dep_csv$Latitude.la.plus.au.nord + data_gouv_dep_csv$Latitude.la.plus.au.sud) / 2
middle_dep_lon[data_gouv_dep_csv$Departement] <- (data_gouv_dep_csv$Longitude_est + data_gouv_dep_csv$Longitude_ouest) / 2

dep_list <- data_gouv_dep_csv$Departement

tableau_data_reg_stat <- NULL
tableau_data_dep_stat <- NULL

lon_reg <- c(5.619444, 0.197778, 4.538056, 4.809167, -2.838611,
                         1.685278, 9.105278, 2.504722, 2.137222, 2.775278,
                         0.106667, -0.823889, 6.053333)
lat_reg <- c(48.68917, 45.19222, 45.51583, 47.23528, 48.17972,
                        47.48056, 42.14972, 48.70917, 43.70222, 49.96611,
                        49.12111, 47.47472, 43.95500)
# construction des tableaux de données pour la carte qui contient lon | lat | nb d'accident
i <- 0
for (reg_name in names(regions)){
  i <- i + 1
  each_region <- regions[[reg_name]]
  # region
  tableau_data_reg_stat$lon <- append(tableau_data_reg_stat$lon, lon_reg[i])
  tableau_data_reg_stat$lat <- append(tableau_data_reg_stat$lat, lat_reg[i])
  tableau_data_reg_stat$nb <- append(tableau_data_reg_stat$nb, sum(data$region == reg_name))
}
for (dep in dep_list){
  # departement
  tableau_data_dep_stat$lon <- append(tableau_data_dep_stat$lon, middle_dep_lon[dep])
  tableau_data_dep_stat$lat <- append(tableau_data_dep_stat$lat, middle_dep_lat[dep])
  tableau_data_dep_stat$nb <- append(tableau_data_dep_stat$nb, sum(data$dep == dep))
}
#print(tableau_data_reg_stat)

#map <- leaflet() %>% setView(lng = 2, lat = 48, zoom = 5) %>% # se place sur le point de view de la France
#  addTiles() %>%
#  addGeoJSON(geo_region_json) %>%
#  addMarkers(data = tableau_data_reg_stat, lng = ~lon, lat = ~lat, label = ~nb) # donne les points pour chaque region
#map


# departement
map <- leaflet() %>% setView(lng = 2, lat = 48, zoom = 5) %>%
  addTiles() %>%
  addGeoJSON(geo_dep_json) %>%
  addMarkers(data = tableau_data_dep_stat, lng = ~lon, lat = ~lat, label = ~nb) # donne les points pour chaque region
map
