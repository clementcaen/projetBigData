#install.packages("sf")
#install.packages("leaflet")
#install.packages("magrittr")

library(sf)
library(leaflet)
library(geojsonio)
library(datasets)

# idee de Elsa de recoder la fonction isin
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
# 2 liste contenant dans cette ordre les longitude ou latitude de ces régions :
# "Alsace Champagne-Ardenne Lorraine", "Aquitaine Limousin Poitou-Charentes",  "Auvergne Rhône-Alpes", "Bourgogne Franche-Comté", "Bretagne",
#             "Centre-Val de Loire", "Corse", "Île-de-France", "Languedoc-Roussillon Midi-Pyrénées", "Nord-Pas-de-Calais Picardie",
#             "Normandie", "Pays de la Loire", "Provence-Alpes-Côte d'Azur")
lon_reg <- c(5.619444, 0.197778, 4.538056, 4.809167, -2.838611,
                         1.685278, 9.105278, 2.504722, 2.137222, 2.775278,
                         0.106667, -0.823889, 6.053333)
lat_reg <- c(48.68917, 45.19222, 45.51583, 47.23528, 48.17972,
                        47.48056, 42.14972, 48.70917, 43.70222, 49.96611,
                        49.12111, 47.47472, 43.95500)

# Coordonnees_centre_departement
# donnees trouvées par Paul-Adrien Source: https://www.data.gouv.fr/fr/datasets/coordonnees-geographiques-extremes-des-departements-metropolitains-de-france/
data_gouv_dep_csv <- read.csv("points-extremes-des-departements-metropolitains-de-france.csv")
middle_dep_lat <- NULL
middle_dep_lon <- NULL
middle_dep_lat[data_gouv_dep_csv$Departement] <- (data_gouv_dep_csv$Latitude.la.plus.au.nord + data_gouv_dep_csv$Latitude.la.plus.au.sud) / 2
middle_dep_lon[data_gouv_dep_csv$Departement] <- (data_gouv_dep_csv$Longitude_est + data_gouv_dep_csv$Longitude_ouest) / 2
dep_list <- data_gouv_dep_csv$Departement[order(data_gouv_dep_csv$Departement)]

#importation des données préparés : fichier préparation.R
#source("Préparation.R")

# import de préparation
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

# traitement pour la carte sur les données
# Clément (aide Marzhin)
acc_region <- function(donnee, nb_ligne){
  # on fait la carte
  donnee$region <- ""
  for (i in 1:nb_ligne){
    each_code <- donnee$id_code_insee[i]
    dep <- substring(each_code, 1, 2) # on récupère les 2 premiers caractères du code insee
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

data <- acc_region(data,  nrow(data))

# construction des tableaux de données pour la carte qui contient lon | lat | nb d'accident
datatableau_pour_nb_accident_region <- function (data, lon_reg, lat_reg, regions){
  tableau_data_reg_stat <- NULL
  i <- 0
  tableau_data_reg_stat$lon <- NULL
  tableau_data_reg_stat$lat <- NULL
  tableau_data_reg_stat$nb <- NULL
  for (reg_name in names(regions)){
    i <- i + 1
    #each_region <- regions[[reg_name]]
    # region
    tableau_data_reg_stat$lon <- append(tableau_data_reg_stat$lon, lon_reg[i])
    tableau_data_reg_stat$lat <- append(tableau_data_reg_stat$lat, lat_reg[i])
    tableau_data_reg_stat$nb <- append(tableau_data_reg_stat$nb, sum(data$region == reg_name))
  }
  return(tableau_data_reg_stat)
}

datatableau_pour_nb_accident_departement <- function (data, middle_dep_lon, middle_dep_lat, dep_list){
  tableau_data_dep_stat <- NULL
  tableau_data_dep_stat$lon <- NULL
  tableau_data_dep_stat$lat <- NULL
  tableau_data_dep_stat$nb <- NULL
  tableau_data_dep_stat$dep <- NULL

  for (dep in dep_list){# on trie les département dans l'ordre alphabetique (c'est déjà le cas sauf pour la corse qui est à la fin (2A et 2B)) pour avoir le même ordre que le trie dans le geojson
    # departement
    tableau_data_dep_stat$lon <- append(tableau_data_dep_stat$lon, middle_dep_lon[dep])
    tableau_data_dep_stat$lat <- append(tableau_data_dep_stat$lat, middle_dep_lat[dep])
    tableau_data_dep_stat$nb <- append(tableau_data_dep_stat$nb, sum(data$dep == dep))
    tableau_data_dep_stat$dep <- append(tableau_data_dep_stat$dep, dep)
    #print(tableau_data_dep_stat$dep)
  }
  return (tableau_data_dep_stat)
}


# récupitulatif des données :
# regions : dictionnaire des regions avec leurs départements
# lon_reg et lat_reg :listes des longitudes et latitudes centrales des régions
# middle_dep_lat et middle_dep_lon : listes des longitude et lattitude centrale des départements obtenu à partir d'un csv (data.gouv)
# dep_list: liste des regions
# data : dataframe contenant toutes les données + ajout de 2 colonnes: dep et régions qui contient respectivement le numero du departement et le nom de la region

# Pour chaque region on fait la somme des accidents
#print(table(data$region))

# on récupère des données complete du nombre d'accidents par regions et par départements
tableau_data_reg_stat <- datatableau_pour_nb_accident_region(data, lon_reg, lat_reg, regions)
tableau_data_dep_stat <- datatableau_pour_nb_accident_departement(data, middle_dep_lon, middle_dep_lat, dep_list)

# Cartes avec fleatlet
# Documentation pour R: https://rstudio.github.io/leaflet/json.html
geo_region_json <- geojson_read("regions.geojson")
geo_dep_json <- geojson_read("departements.geojson")

map_region <- function (geo_region_json, tableau_data_reg_stat){
  map <- leaflet() %>% setView(lng = 2, lat = 48, zoom = 5) %>% # se place sur le point de view de la France
    addTiles() %>%
    addGeoJSON(geo_region_json) %>%
    addMarkers(data = tableau_data_reg_stat, lng = ~lon, lat = ~lat, label = ~nb) # donne les points pour chaque region
  map
}

map_departement <- function (geo_dep_json, tableau_data_dep_stat){
  # departement
  map <- leaflet() %>% setView(lng = 2, lat = 48, zoom = 5) %>%
    addTiles() %>%
    addGeoJSON(geo_dep_json) %>%
    addMarkers(data = tableau_data_dep_stat, lng = ~lon, lat = ~lat, label = ~nb) # donne les points pour chaque region
  map
}


# Création d'un map avec layer coloré en fonction du nombre d'acccident
#https://leafletjs.com/examples/choropleth/ https://rstudio.github.io/leaflet/choropleths.html
map_depart_better <- function (tableau_data_dep_stat, title_legend){
  bins <- c(0, 1, 100, 200, 500, 1000, Inf)
  #legend_label <- c("Données inconnus", "- de 100", "100 à 200", "200 à 500", "500 à 1000", "+ de 1000")
  pal <- colorBin("YlOrRd", domain = tableau_data_dep_stat$nb, bins = bins)

  departements <- geojsonio::geojson_read("departements.geojson", what = "sp")

  # Chaque departement possède : { code, name }
  departements <- departements[order(departements$code), ] # on trie les donnée geojson pour les avoir dans le même ordre que le tableau des accidents ( la virgule déclare que l'on souhaite garder les colonnes)

  #print(departements$code)
  departements$density_acc <- tableau_data_dep_stat$nb
  departements$dep <- names(tableau_data_dep_stat$lon)
  labels <- sprintf(
    "<strong>N° du département: %s</strong><br/>%g Accidents en 2009",
    departements$dep, departements$density_acc
  ) %>% lapply(htmltools::HTML)

  m <- leaflet(departements) %>%
    setView(lng = 2, lat = 48, zoom = 5) %>%
    addProviderTiles("MapBox", options = providerTileOptions(
      id = "mapbox.light",
      accessToken = Sys.getenv('pk.eyJ1IjoiY2xlbWVudGoiLCJhIjoiY2xoYWh4NTYwMDc3azNjbnM2em9veHdxbCJ9.xyrycPiUE7fNHDMvZN3zDw'))) %>%
    addPolygons(
      fillColor = ~pal(density_acc),
      weight = 2,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.7,
      highlightOptions = highlightOptions(
        weight = 5,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE),
      label = labels,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto")
      ) %>%
    addControl(
      html = paste0(
        '<div id="legend" class="info legend">',
        '   <h4>', title_legend, '</h4>',
        '   <ul style="list-style: none; color: black;  ">',
        '     <li style="background-color:', pal(bins[1]), ';">Données Inconnues</li>',
        '     <li style="background-color:', pal(bins[2]), ';">- de 100</li>',
        '     <li style="background-color:', pal(bins[3]), ';">100 à 200</li>',
        '     <li style="background-color:', pal(bins[4]), ';">200 à 500</li>',
        '     <li style="background-color:', pal(bins[5]), ';">500 à 1000</li>',
        '     <li style="background-color:', pal(bins[6]), ';">+ de 1000</li>',
        '   </ul>',
        '</div>'
      ),
      position = "topright"
    )
  m
}

# affichage des maps du nombre d'accidents par régions et département
map_depart_better(tableau_data_dep_stat, 'Nombre d\'accidents')
#map_region(geo_region_json, tableau_data_reg_stat)
#map_departement(geo_dep_json, tableau_data_reg_stat)

# Pour uniquement les accidents graves ( blessés, grave ou leger, ou tués,
# filtrage des accidents indemnes (1)
data_graves <- subset(data, descr_grav != 1)
# récuperation des nouvelles données par région & departement
tableau_data_reg_stat_graves <- datatableau_pour_nb_accident_region(data_graves, lon_reg, lat_reg, regions)
tableau_data_dep_stat_graves <- datatableau_pour_nb_accident_departement(data_graves, middle_dep_lon, middle_dep_lat, dep_list)

map_depart_better(tableau_data_dep_stat_graves, "Nombre d\'accidents graves")
#map_region(geo_region_json, tableau_data_reg_stat_graves)
#map_departement(geo_dep_json, tableau_data_reg_stat_graves)