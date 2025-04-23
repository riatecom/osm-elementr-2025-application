-------------------------------------------------------------------------------
# Utiliser OSM avec R ----
# Script associé au notebook
# Par Louis Laurian et Ronan Ysebaert, Mai 2025
-------------------------------------------------------------------------------

# 0. Note méthodologique et objectifs ----  
--------------------------------------------------------------------------------
# Cette analyse est reproductible en n'importe quel lieu, avec une adresse (adr)
# et un nom de ville (lib). Néanmoins les temps de calcul utilisant le serveur
# de démonstration ont été paramétrées pour ne pas le surcharger. L'exécution
# de ces blocs de code prennent du temps. En conséquence, 6 jeux de données
# ont été préparés en amont et les blocs de codes relevant des calculs
# d'itinéraires / temps de trajets sont placés en commentaires, mais sont
# fonctionnels. 
#
# Objectif : identifier les secteurs accessibles à pieds pour prendre un verre 
# ou manger lorsque l'on est en transit dans une gare SNCF donnée
#
# Prérequis : une connexion internet pour charger les points OSM  
-------------------------------------------------------------------------------  
      
# 1. Initialiser l'analyse ----  
  
## 1.1 Packages nécessaires (OSM) ----
library(tidygeocoder) # géocodage
library(maptiles) # import de tuiles OSM (raster)
library(osmdata) # import de données OSM (vecteur)
library(maposm) # import de données OSM (couches géo)
library(osrm) # calcul d'itinéraires

## 1.2 Autres packages utilitaires ----
library(sf) # manipulation de données vectorielles
library(terra) # manipulation de données raster
library(mapsf) # cartographie thématique
library(maplegend) # légendes
library(spatstat) # analyse de semis de points
library(stplanr) # segmentiser des lignes
library(mapview) # cartographie interactive
library(lwgeom)

## 1.3 Import des adresses ---- 
# Clermont, Grenoble, Toulouse, Compiègne  
cs <- read.csv("data/case_studies.csv", encoding = "UTF-8", sep = ",")
cs
cs <- cs[cs$lib == "Compiegne",]
adr <- cs$adr
lib <- cs$lib

#----#

# 2. Géocodage ----
pt <- data.frame(address = adr)
pt <- geocode(.tbl = pt, address = "address", quiet = TRUE) |>
  st_as_sf(coords = c("long", "lat"), crs = 4326)

#---#

# 3. Import de tuiles OSM ----
# Définir une emprise autour du point (3 km)
emprise <- st_buffer(pt, 3000) |>
  st_bbox()

# Charger les tuiles avec le package maptiles
tiles <- get_tiles(x = emprise,
                   project = FALSE,
                   crop = TRUE,
                   cachedir = "cache",
                   zoom = 14)

# Reprojeter pour affichage en coordonnées projetées
pt_3857 <- st_transform(pt, crs = "EPSG:3857")

# Cartographie
mf_raster(tiles, expandBB = c(rep(-.2,4)))
mf_map(pt_3857, pch = 24, cex = 1.3, col = "darkblue", lwd = 2, add = TRUE)
mf_annotation(pt_3857, txt = "Gare")
mf_title(paste0(lib, " / Tuiles OSM - Défaut"))

# Autre exemple de fournisseur
tiles <- get_tiles(x = emprise,
                    project = FALSE,
                    crop = TRUE,
                    provider = "OpenStreetMap.HOT",
                    zoom = 14)

# Cartographie
mf_raster(tiles, expandBB = c(rep(-.2,4)))
mf_map(pt_3857, pch = 24, cex = 1.3, col = "darkblue", lwd = 2, add = TRUE)
mf_annotation(pt_3857, txt = lib)
mf_title(paste0(lib, " / Tuiles OSM - OpenStreetmap.HOT"))

#---#

# 4. Import de couches géographiques d'habillage avec maposm ----
# Charger les couches (2 km autour du point)
res <- om_get(x = c(st_coordinates(pt)[1],
                    st_coordinates(pt)[2]),
              r = 2000)

# Cartographie (styles par défaut)
mf_map(res$zone, col = "#f2efe9", border = NA, add = FALSE)
mf_map(res$urban, col = "#e0dfdf", border = "#e0dfdf", lwd = .5, add = TRUE)
mf_map(res$green, col = "#c8facc", border = "#c8facc", lwd = .5, add = TRUE)
mf_map(res$water, col = "#aad3df", border = "#aad3df", lwd = .5, add = TRUE)
mf_map(res$railway, col = "grey50", lty = 2, lwd = .2, add = TRUE)
mf_map(res$road, col = "white", border = "white", lwd = .5, add = TRUE)
mf_map(res$street, col = "white", border = "white", lwd = .5, add = TRUE)
mf_map(res$building, col = "#d9d0c9", border = "#c6bab1", lwd = .5, add = TRUE)
mf_map(pt_3857, pch = 24, cex = 1.3, col = "darkblue", lwd = 2, add = TRUE)
mf_map(res$zone, col = NA, border = "#c6bab1", lwd = 4, add = TRUE)
mf_title(paste0(lib, " / Autour de la gare"))
mf_arrow()
mf_scale(size = 500, scale_units = "m")
mf_credits("OpenStreetMap contributors, 2025", pos = "bottomleft")

#---#

# 4. Import de points avec osmdata ----
# Définir l'emprise et le type d'objets d'intérêt
q <- opq(bbox = emprise, osm_types = "node")

# Extraction des restaurants, bars et cafés
req <- add_osm_feature(opq = q,
                       key = 'amenity',
                       value = c("bar", "cafe", "restaurant"))

# Transformer en objet sf
poi <- osmdata_sf(req)
poi <- poi$osm_points

# Sélectionner les variables
poi <- poi[, c("osm_id", "name", "amenity", "cuisine")]

# Intersection avec espace d'étude
poi <- st_intersection(poi, st_transform(res$zone, crs = 4326))

# Reprojection
poi_3857  <- st_transform(poi , crs = "EPSG:3857")

# Cartographie
om_map(res, title = paste0(lib, " / Autour de la gare"))
mf_map(poi_3857, type = "typo", var = "amenity", cex = .7, pch = 15, 
       leg_pos = "topright", leg_val_cex = .9, leg_title_cex = .9,
       add = TRUE)
mf_map(pt_3857, pch = 24, cex = 1.3, lwd = 2, col = "darkblue", add = TRUE)

#---#

# 6. Cartographie interactive avec mapview
mapview(res$zone, alpha.regions = 0) + mapview(poi)

# 7. Carroyage ----
# Création d'une grille régulière hexagonale de 300m de résolution
grid <- st_make_grid(res$zone, cellsize = 300, square = FALSE)
grid <- st_intersection(grid, res$zone)
grid <- st_sf(ID = 1:length(grid), geom = grid)

# Compter les POI dans les carrreaux
inter <- st_intersects(grid, poi_3857, sparse = TRUE)
grid$n_poi <- lengths(inter)

# Cartographie
om_map(res, title = paste0(lib, " / Autour de la gare"))
mf_map(grid, var = "n_poi", type = "choro", breaks = c(0,1,3,5,10,max(grid$n_poi)),
       alpha = .6, border = NA, leg_title = "Nombre \nd'aménités",
       leg_val_rnd = 0, leg_pos = "topright", add = TRUE)
mf_title("Aménités gustatives - données carroyées")

#---#

# 8. Lissage (KDE) ----

## 8.1 Préparation KDE ----
# Création du lissage avec spatstat
p <- as.ppp(X = st_coordinates(poi_3857), W = as.owin(res$zone))
ds <- density.ppp(x = p, sigma = 100, eps = 10, positive = TRUE)
r <- rast(ds) * 100 * 100
crs(r) <- st_crs(poi_3857)$wkt

## 8.2 Carto KDE ----
# Définir une palette pour les cartes à venir
pal <- c( "#418e40", "#a5be6a", "#e2cf6a", "#c19849", "#ab6c32", "#842f1a")

mf_map(res$zone, col = "#f2efe9", border = NA, expandBB = c(0,.3,0,0))
mf_raster(r, leg_title = "N. restaurants / ha.", leg_pos = "left", 
          pal = pal, add = TRUE)
mf_map(res$water, col = "#aad3df", border = "#aad3df", lwd = .5, add = TRUE)
mf_map(res$railway, col = "#ffffff60", border = "white", lwd = .2, add = TRUE)
mf_map(res$road, col = "#ffffff60", border = "white", lwd = .5, add = TRUE)
mf_map(res$street, col = "#ffffff60", border = "white", lwd = .5, add = TRUE)
mf_map(res$zone, col = NA, border = "#c6bab1", lwd = 4, add = TRUE)
mf_map(poi_3857, pch = 20, cex = .3, col = "red", add = TRUE)
mf_map(pt_3857, pch = 24, cex = 1.3, lwd = 2, col = "darkblue", add = TRUE)
mf_title(paste0(lib, " / La montagne alimentaire"))

## 8.3 Carto / KDE discrétisé ----

# Discrétiser le raster
bks <- c(0, 0.01, 0.1, 0.5, 1, 2, minmax(r)[2])

# Cartographie
mf_map(res$zone, col = "#f2efe9", border = NA)
mf_raster(r, type = "interval", breaks = bks, pal = pal, rev = TRUE,
          leg_val_rnd = 2, leg_pos = "topleft", leg_title = "Densité\n (n/ha)")
mf_map(res$water, col = "#aad3df", border = "#aad3df", lwd = .5,
       add = TRUE)
mf_map(res$railway, col = "#ffffff60", border = "white", lwd = .2, add = TRUE)
mf_map(res$road, col = "#ffffff60", border = "white", lwd = .5, add = TRUE)
mf_map(res$street, col = "#ffffff60", border = "white", lwd = .5, add = TRUE)
mf_map(res$zone, col = NA, border = "#c6bab1", lwd = 4, add = TRUE)
mf_map(poi_3857, pch = 20, cex = .3, col = "red", add = TRUE)
mf_map(pt_3857, pch = 24, cex = 1.3, lwd = 2, col = "darkblue", add = TRUE)
mf_title(paste0(lib, " / La montagne alimentaire vectorisée"))

## 8.4 Carto /restos et cafés/bars séparés ----
# Regrouper cafés/bars
poi_3857$amenity[poi_3857$amenity %in% c("bar", "cafe")] <- "bar-cafe"

# Préparation de la boucle
p <- list()
ds <- list()
r <- list()
sel <- levels(as.factor(poi_3857$amenity))

# Produire le KDE pour cafés/bars puis restaurants
for(i in 1:length(sel)){
  p[[i]] <- as.ppp(
    X = st_coordinates(poi_3857[poi_3857$amenity == sel[i] ,]),
    W = as.owin(res$zone))
  ds[[i]] <- density.ppp(x = p[[i]], sigma = 100, eps = 10,
                         positive = TRUE)
  r[[i]] <- rast(ds[[i]]) * 100 * 100
  crs(r[[i]]) <- st_crs(poi_3857)$wkt
}

# Cartographie cafés/bars discrétisée
par(mfrow = c(1,2))
mf_map(res$zone, col = "#f2efe9", border = NA)
mf_raster(r[[1]], type = "interval", breaks = bks, pal = pal, rev = TRUE,
          leg_val_rnd = 2, leg_pos = "topleft", leg_title = "Densité cafés-bars\n (n/ha)",
          add = TRUE)
mf_map(res$water, col = "#aad3df", border = "#aad3df", lwd = .5,
       add = TRUE)
mf_map(res$railway, col = "#ffffff60", border = "white", lwd = .2, add = TRUE)
mf_map(res$road, col = "#ffffff60", border = "white", lwd = .5, add = TRUE)
mf_map(res$street, col = "#ffffff60", border = "white", lwd = .5, add = TRUE)
mf_map(res$zone, col = NA, border = "#c6bab1", lwd = 4, add = TRUE)
mf_map(poi_3857[poi_3857$amenity == "bar-cafe",], pch = 20, cex = .3,
       col = "red", add = TRUE)
mf_map(pt_3857, pch = 24, cex = 1.3, lwd = 2, col = "darkblue", add = TRUE)
mf_title(paste0("Boire un verre à ", lib))

# Cartographie restaurants discrétisée
mf_map(res$zone, col = "#f2efe9", border = NA)
mf_raster(r[[2]], type = "interval", breaks = bks, pal = pal, rev = TRUE,
          leg_val_rnd = 2, leg_pos = "topleft", leg_title = "Densité restaurants\n (n/ha)", 
          add = TRUE)
mf_map(res$water, col = "#aad3df", border = "#aad3df", lwd = .5,
       add = TRUE)
mf_map(res$railway, col = "#ffffff60", border = "white", lwd = .2, add = TRUE)
mf_map(res$road, col = "#ffffff60", border = "white", lwd = .5, add = TRUE)
mf_map(res$street, col = "#ffffff60", border = "white", lwd = .5, add = TRUE)
mf_map(res$zone, col = NA, border = "#c6bab1", lwd = 4, add = TRUE)
mf_map(poi_3857[poi_3857$amenity == "restaurant",], pch = 20, cex = .3,
       col = "red", add = TRUE)
mf_map(pt_3857, pch = 24, cex = 1.3, lwd = 2, col = "darkblue", add = TRUE)
mf_title(paste0("Aller manger à ", lib))

# 9. Depuis la gare----
# Uniquement les restaurants
poi <- poi[poi$amenity == "restaurant",]

## 9.1 Calcul isochrones ----
## ! Ce bloc de code (fonctionnel) a été préparé en amont ! ##
# o <- data.frame(X = st_coordinates(pt)[, 1],
#                 Y = st_coordinates(pt)[, 2])
# row.names(o) <- 1:nrow(o)
# 
# isos <- osrmIsochrone(loc = o,
#                       breaks = seq(0,30,5),
#                       res = 60,
#                       osrm.profile = "foot")
# 
# st_write(isos, paste0("data/", lib, ".gpkg"), layer = "isos")

## 9.2 Calcul temps de trajets gare/ restaurants ----
## ! Ce bloc de code (fonctionnel) a été préparé en amont ! ##
# Création de la matrice O/D
# d <- data.frame(X = st_coordinates(poi)[, 1],
#                 Y = st_coordinates(poi)[, 2])
# 
# # 1 requête : 1 point d'origine, 92 points de destination
# # Distance et temps de trajet
# mat_dist <- osrmTable(src = o,
#                       dst = d,
#                       measure = "distance",
#                       osrm.profile = 'foot')
# mat_dur <- osrmTable(src = o,
#                      dst = d,
#                      measure = "duration",
#                      osrm.profile = 'foot')

# Mise en forme des données
# dist <- t(mat_dist$distances)
# dur <- t(mat_dur$durations)
# poi$dist <- dist
# poi$dur <- dur
# st_write(poi, paste0("data/", lib, ".gpkg"), layer = "poi")

## 9.3 Carto / Isochrones et restos autour de la gare ----
poi <- st_read(paste0("data/", lib, ".gpkg"), layer = "poi")
isos <- st_read(paste0("data/", lib, ".gpkg"), layer = "isos")
isos <- st_transform(isos, crs = "EPSG:3857")
isos <- st_intersection(isos, res$zone)
poi <- st_transform(poi, crs = "EPSG:3857")

par(mfrow = c(1,1))

om_map(res, title = paste0(lib, " / Isochrones autour de la gare"))
mf_map(
    x = isos, var = "isomin", type = "choro",
    breaks = sort(unique(c(isos$isomin, isos$isomax))),
    pal = "Geyser", alpha = .6, border = NA, leg_val_rnd = 0, 
    leg_title = "Isochrones\n(minutes à pieds)", leg_pos = "topright",
    add = TRUE)
mf_map(x = poi, var = "dur", type = "choro", 
       breaks = sort(unique(c(isos$isomin, isos$isomax))),
       pal = "Geyser", pch = 21, border = "white", leg_pos = NA,
       add = TRUE)
mf_map(pt_3857, pch = 24, cex = 1.3, lwd = 2, col = "darkblue", add = TRUE)

# Liste des restaurants en moins de 10 minutes à pieds
poi_10 <- poi[poi$dur < 10,]
poi_10$name

#---#

# 10 The place to be ? ----
### 10.1 Calcul des temps de trajet depuis la grille ----
## ! Ce bloc de code (fonctionnel) a été préparé en amont ! ##
# Coordonnées des points d'origine : centroïdes des cellules de la grille
# o <- st_centroid(grid) |>
#   st_transform(crs = 4326)
# 
# # Création de la matrice O/D
# o$X <- st_coordinates(o)[, 1]
# o$Y <- st_coordinates(o)[, 2]
# o <- st_set_geometry(o[, c("X", "Y")], NULL)
# matfoot <- list()
# 
# # Routage
# for(i in 1:nrow(d)){
#   matfoot[[i]] <- osrmTable(src = o[, c("X", "Y")],
#                         dst = d[i , c("X", "Y")],
#                         measure = "duration",
#                         osrm.profile = 'foot')
#   Sys.sleep(5)
#   matfoot[[i]]$ID_ORIG <- c(1:nrow(o))
#   matfoot[[i]]$ID_DEST <- rep(i, nrow(o))
#   matfoot[[i]] <- data.frame("ID_ORIG" = matfoot[[i]]$ID_ORIG,
#                          "ID_DEST" = matfoot[[i]]$ID_DEST,
#                          "duration" = matfoot[[i]]$durations)
#   colnames(matfoot[[i]]) <- c("ID_ORIG", "ID_DEST", "dur_foot")
# }
# 
# matfoot <- do.call(rbind, matfoot)
# write.csv(matfoot, paste0("data/matfoot_", lib, ".csv"), row.names = FALSE)

## 10.2 Carto / temps d'accès au restaurant le plus proche ----
# Préparation des données
mat <- read.csv(paste0("data/matfoot_", lib, ".csv"))

# Restaurant le plus proche pour chaque point de grille
poi_min <- aggregate(dur_foot ~ ID_ORIG, mat, FUN = min, na.rm = TRUE)

# Récupérer l'identifiant de destination associé
poi_min <- do.call(rbind,
                   lapply(split(mat, mat$ID_ORIG),
                          function(x) x[which.min(x$dur_foot), ]))
poi_min_sf <- merge(grid, poi_min,
                    by.x = "ID", by.y = "ID_ORIG", all.x = TRUE)
# Cartographie
om_map(res, title = "Temps d'accès au restaurant le plus proche")
mf_map(poi_min_sf, var = "dur_foot", type = "choro", breaks = seq(0,15,3),
       alpha = .6, border = NA, leg_title = "Temps de trajet\n(minutes à pieds)",
       leg_val_rnd = 0, pal = "Greens", leg_pos = "topright",
       col_na = "#8B000060", leg_no_data = "Plus de 15 minutes", add = TRUE)

## 10.3 Carto / nombre de POI à moins de 5 minutes ----
# Préparation des données
mat5 <- mat[mat$dur_foot < 5 ,]
mat5$n <- 1
mat5 <- aggregate(list(n_resto = mat5$n),
                  list(ID_ORIG = mat5$ID_ORIG),
                  FUN = sum)
grid <- merge(grid[, "ID"], mat5,
              by.x = "ID", by.y = "ID_ORIG", all.x = TRUE)
grid$n_resto[is.na(grid$n_resto)] <- 0

# Cartographie
om_map(res, title = paste0(lib," / Restaurants à moins de 5 minutes"))
mf_map(grid, var = "n_resto", type = "choro", breaks = c(0,1,5,10,30,max(grid$n_resto)),
       alpha = .6, border = NA, leg_title = "Nb de\nrestaurants",
       leg_val_rnd = 0, pal = "Reds", leg_pos = "topright", add = TRUE)

# 11. Itinéraires ----
# On ne s'intéresse qu'aux pizzerias
poi <- st_read(paste0("data/", lib, ".gpkg"), layer = "poi")
st_geometry(poi) <- "geometry"
pizz <- poi[!is.na(poi$cuisine) & poi$cuisine == "pizza" , "geometry"]

## 11.1 Calcul des itinéraires gare / pizzerias ----
## ! Ce bloc de code (fonctionnel) a été préparé en amont ! ##
# o <- data.frame(X = st_coordinates(pt)[, 1],
#                 Y = st_coordinates(pt)[, 2])
# row.names(o) <- 1
# 
# d <- data.frame(X = st_coordinates(pizz)[, 1],
#                 Y = st_coordinates(pizz)[, 2])
# 
# routes <- list()
# for(i in 1:nrow(d)){
#   routes[[i]] <- osrmRoute(src = o,
#                            dst = d[i,],
#                            overview = "full",
#                            osrm.profile = 'foot')
#    Sys.sleep(5)
# }
# routes <- do.call(rbind, routes)
# st_write(routes, paste0("data/", lib, ".gpkg"), layer = "routes")

## 11.2 Carto / La route des pizzas ---- 
routes <- st_read(paste0("data/", lib, ".gpkg"), layer = "routes")
routes <- st_transform(routes, crs = "EPSG:3857")
routes$n <- 1
routes$distance <- routes$distance * 1000

# La fonction overline permet de compter le nombre d'occurence des tronçons
seg <- overline(routes, attrib = "n", fun = sum)

routes <- routes[order(routes$distance, decreasing = TRUE), ]
l_seg <- list()

# La fonction line_segment découpe un objet sf LINESTRING en segments réguliers
for(i in 1:nrow(routes)){
  l_seg[[i]] <- line_segment(
    routes[i, ],
    segment_length = max(routes$distance) / 20)
}

pal <- mf_get_pal(nrow(l_seg[[1]]), rev = TRUE, palette = "Blues 3")
om_map(res, title = paste0(lib, " / Les routes des pizzas"), theme = "dark")
for(i in nrow(routes):1){
  mf_map(l_seg[[i]], col = pal, lwd = 1.6, add = TRUE)
}
leg(type = "cont", val = c(min(routes$distance), 1000, max(routes$distance)),
    pal = pal, pos = "left", title = "Distance (m)")

## 11.3 Calcul d'un trip ----
## ! Ce bloc de code (fonctionnel) a été préparé en amont ! ##
# # Ajouter le point de départ : la gare
# pizz <- rbind(pt[1, "geometry"], pizz)
# 
# trip <- osrmTrip(loc = pizz,
#                  overview = "full",
#                  osrm.profile = "foot")
# 
# trip <- trip[[1]]$trip
# st_write(trip, paste0("data/", lib, ".gpkg"), layer = "trip")

## 11.4 Carto / la tournée des pizzas ----
trip <- st_read(paste0("data/", lib, ".gpkg"), layer = "trip")
trip <- st_transform(trip, crs = "EPSG:3857")

# Pour les labels : récupérer le début de la ligne
start <- lwgeom::st_startpoint(trip)
start <- do.call(rbind, start)
start <- data.frame(id = rownames(trip), X = start[,1], Y = start[,2])
start <- st_as_sf(start, coords = c("X", "Y"), crs = "EPSG:3857")

pizz <- poi[!is.na(poi$cuisine) & poi$cuisine == "pizza" ,]|>
  st_transform(crs = "EPSG:3857")

mf_map(res$urban, col = "#a83c0a", border = "#e0dfdf", lwd = .5)
mf_map(res$green, col = "#569128", border = "#569128", lwd = .5, add = TRUE)
mf_map(res$water, col = "#aad3df", border = "#aad3df", lwd = .5, add = TRUE)
mf_map(res$railway, col = "grey80", lty = 2, lwd = .2, add = TRUE)
mf_map(res$building, col = "#942222", border = "#942222", lwd = .5, add= TRUE)
mf_map(res$road, col = "grey80", border = NA, lwd = .2, add = TRUE)
mf_map(res$street, col = "grey80", border = NA, lwd = .2, add = TRUE)
mf_map(res$zone, col = "#a83c0a33", alpha = .1, border = NA, add = TRUE)
mf_map(res$zone, col = NA, border = "#d7b578", lwd = 15, add = TRUE) 
mf_map(trip, col = "black", lwd = 3, lty = 3, add = TRUE)
mf_map(start[start$id != 1, ], pch = 20, cex = 2, col = "black", add = TRUE)
mf_map(pt_3857, pch = 24, cex = 1.3, col = "darkblue", lwd = 2, add = TRUE)
mf_label(start, var = "id", cex = .8, overlap = FALSE, pos = 4, col = "black", halo = TRUE)
mf_title(paste0(lib, " / La route de la pizza en ", 
                round(sum(trip$duration, 0)), " minutes à pieds"))
mf_arrow()
mf_scale(size = 500, scale_units = "m")
mf_credits("OpenStreetMap contributors, 2025", pos = "bottomleft")
