regions <- c("Alsace Champagne-Ardenne Lorraine", "Aquitaine Limousin Poitou-Charentes",
             "Auvergne Rhône-Alpes", "Bourgogne Franche-Comté", "Bretagne",
             "Centre-Val de Loire", "Corse", "Île-de-France",
             "Languedoc-Roussillon Midi-Pyrénées", "Nord-Pas-de-Calais Picardie",
             "Normandie", "Pays de la Loire", "Provence-Alpes-Côte d'Azur")

longitude <- c("5°37'10\" E", "0°11'52\" E", "4°32'17\" E", "4°48'33\" E", "2°50'19\" O",
               "1°41'07\" E", "9°06'19\" E", "2°30'17\" E", "2°08'14\" E", "2°46'31\" E",
               "0°06'24\" E", "0°49'26\" O", "6°03'12\" E")
longitude_numerique <- c(5.619444, 0.197778, 4.538056, 4.809167, -2.838611,
                         1.685278, 9.105278, 2.504722, 2.137222, 2.775278,
                         0.106667, -0.823889, 6.053333)

latitude <- c("48°41'21\"", "45°11'32\"", "45°30'57\"", "47°14'07\"", "48°10'47\"",
              "47°28'50\"", "42°08'59\"", "48°42'33\"", "43°42'08\"", "49°57'58\"",
              "49°07'16\"", "47°28'29\"", "43°57'18\"")
latitude_numerique <- c(48.68917, 45.19222, 45.51583, 47.23528, 48.17972,
                        47.48056, 42.14972, 48.70917, 43.70222, 49.96611,
                        49.12111, 47.47472, 43.95500)


data <- data.frame(Regions = regions,
                   "longitude" = longitude,
                   "latitude" = latitude)
