# Utiliser OpenStreetMap avec R - Mise en pratique

Réalisé par Louis Laurian, Ronan Ysebaert, Timothée Giraud et Matthieu Viry (UAR RIATE pour le groupe [ElementR](https://elementr.gitpages.huma-num.fr/website/apropos.html) en mai 2025.

## Contenu

Cette chaîne de traitements reproductible a pour vocation de mettre en pratique les éléments plus théoriques abordés en amont. 
Elle peut utilement être remobilisée dans d’autres contextes spatiaux ou thématiques. 


## Accéder aux données




<div align="center">
  <a href="https://github.com/JosManoel">
    <img src="https://image_1.png" width="412px"/> 
  </a>
  <a href="https://github.com/JosManoel">
    <img src="https://image_2.png" width="412px"/>
  </a>
</div>

```
# Import adresses
cs <- read.csv("data/case_studies.csv", encoding = "UTF-8", sep = ",")

for (i in 1:nrow(cs)){
  tmp <- cs[i,]
  adr <- tmp$adr
  lib <- tmp$lib
  
  # Géocodage ----
  pt <- data.frame(address = adr)
  pt <- geocode(.tbl = pt, address = "address", quiet = TRUE) |>
    st_as_sf(coords = c("long", "lat"), crs = 4326)
  
  # Emprise
  emprise <- st_buffer(pt, 3000) |>
    st_bbox()
  
  # Import de couches géographiques d'habillage avec maposm ----
  res <- om_get(x = c(st_coordinates(pt)[1],
                      st_coordinates(pt)[2]),
                r = 2000)
  
  # Import de points avec osmdata ----
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
  
  # Ne garder que les pizzas
  pizz <- poi[!is.na(poi$cuisine) & poi$cuisine == "pizza" , "geometry"]
  
  ## Calcul d'un trip ----
  # Ajouter le point de départ : la gare
  pizz <- rbind(pt[1, "geometry"], pizz)
  
  trip <- osrmTrip(loc = pizz,
                   overview = "full",
                   osrm.profile = "foot")
  
  trip <- trip[[1]]$trip
  trip <- st_transform(trip, crs = "EPSG:3857")
  
  # Pour les labels : récupérer le début de la ligne
  start <- lwgeom::st_startpoint(trip)
  start <- do.call(rbind, start)
  start <- data.frame(id = rownames(trip), X = start[,1], Y = start[,2])
  start <- st_as_sf(start, coords = c("X", "Y"), crs = "EPSG:3857")
  
  pizz <- poi[!is.na(poi$cuisine) & poi$cuisine == "pizza" ,]|>
    st_transform(crs = "EPSG:3857")
  
  mf_export(x = res$zone, filename = paste0("img/", lib, "_pizza.png"),
          width = 500)
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
                round(sum(trip$duration, 0)), " minutes à pieds"), pos = "center")
  mf_arrow()
  mf_scale(size = 500, scale_units = "m")
  mf_credits("OpenStreetMap contributors, 2025", pos = "bottomleft")
  dev.off()
}
```