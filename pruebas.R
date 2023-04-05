library(leaflet)



# Cargar el archivo .shp de los circuitos
#circuits_sf <- st_read("circuits.shp")

circuitos <- circuitos %>% 
  st_transform(crs = 4326)

datos_shp_nulos <- datos_shp_nulos %>% 
  st_transform(crs = 4326)

datos_shp_nulos_ <- datos_shp_nulos %>% 
  st_set_geometry(NULL) %>% 
  left_join(centroides_radios_censales)


centroides_radios_censales_circuito <- centroides_radios_censales_circuito %>% 
  st_transform(crs = 4326)


#filtro_na <- is.na(datos_shp_nulos_$geometry)
radios_censales_sin_na <- centroides_radios_censales_circuito[!filtro_na, ]

filtro_na <- is.na(datos_shp_nulos$geometry)
datos_shp_nulos_ <- datos_shp_nulos %>% 
  filter(geometry != "c(NaN, NaN)")



# Crear el mapa interactivo con los puntos
mapa <- leaflet(datos_shp_nulos) %>%
  addProviderTiles("CartoDB.Positron")

# Agregar la capa de polígonos de los circuitos
mapa <- addPolygons(mapa, data = circuitos)

mapa <- addCircleMarkers(mapa, 
                         data = centroides_radios_censales_circuito[is.na(centroides_radios_censales_circuito$muni_subci),], 
                         color = "red", 
                         radius = 5)



# Mostrar el mapa
mapa
