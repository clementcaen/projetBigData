#install.packages("sf")
#install.packages("leaflet")
#install.packages("magrittr")

library(sf)
library(leaflet)
library(geojsonio)
# représentation sous formes de carte de la quantité d’accidents enregistrés par région puis par départements

# fonction isin venant d'elsa
isin <- function(x, values) {
  match(x, values, nomatch = 0) > 0
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
acc_region <- function(donnee){
  # on fait la carte
  data$region <- ""
  for (i in seq_along(donnee)){
    each_code <- donnee$id_code_insee[i]
    dep <- substring(each_code, 1, 2) # on récupère les 2 premier caractère du code insee
    for (reg_name in names(regions)){
      each_region <- regions[[reg_name]]
      if(isin(dep, each_region)){ # regarde si le departement(dep) est dans cette region(each_region)
        print("Met la regions")
        print(each_region)
        donnee$region[i] <- reg_name
      }
    }
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

data <- acc_region(data)

# Pour chaque region on fait la somme des accidents
acc_by_reg <- table(data$region)

# Carte avec fleatlet
# Documentation pour R: https://rstudio.github.io/leaflet/json.html
geo_region_json <- geojson_read("regions.geojson")

data_gouv_dep_csv <- read.csv("")

# histogramme accident par region


# moyenne des latitudes, longitudes pour toute les départements des regions


map <- leaflet() %>% setView(lng = 2, lat = 48, zoom = 5) %>%
  addTiles() %>%
  addGeoJSON(geo_region_json)
  addMarkers(data = , lng = ~(), lat = ~(), label = ~Freq) # donne les points pour chaque region
map


# departement
map <- leaflet() %>% setView(lng = 2, lat = 48, zoom = 5) %>%
  addTiles() %>%
  addGeoJSON(geo_dep_json)
  addMarkers(data = , lng = ~(data_gouv_csv$LongitudeNord-data_gouv_csv$LongitudeSud), lat = ~(data_gouv_csv$LatitudeNord-data_gouv_csv$LatitudeSud), label = ~Freq) # donne les points pour chaque region
map
