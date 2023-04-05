library(sf)
library(tidyverse)
library(dplyr)
library(mapview)

#limites ciudad

ciudad <- st_read("data/poligonos_edicion/Polygon_layer.shp") %>% 
  filter(ciudad$nombre_pol == 'puntos_ciudad_cba')

#df facebook
poblacion_radios_facebook <- st_read("arg_general_2020_csv/arg_general_2020.csv")

# Cargamos el shp de seccionales electorales
circuitos <- read_sf("data/Seccionales_Electorales/Seccionales_Electorales.shp")

#calculamos el área que representa cada circuito electoral del total de la ciudad
circuitos$area <- st_area(circuitos)
area_total <- sum(circuitos$area)
circuitos$porcentaje_area <- (circuitos$area / area_total) * 100




# Cargamos el archivo SHP de radios censales
radios_censales <- st_read("data/precenso/areas_tot.shp") %>% 
  mutate(id = 1:n())

precenso <- st_read("data/precenso1/areas_tot.shp")

#poblacion proyectada 2020

poblacion_radio_censal <- readxl::read_xlsx("data/precensal_datos2.xlsx")

# Transformamos ambos archivos a la misma proyección cartográfica (si es necesario)
circuitos <- st_transform(circuitos, crs = 4326)
radios_censales <- st_transform(radios_censales, crs = 4326)


#geometry de los radios censales

radios_censales_geometry <- radios_censales %>% 
  select(c('codigo', 'geometry')) 



# Asigna a cada radio censal el atributo del circuito electoral al que pertenece
#radios_censales$circ_electoral <- circuitos$muni_subci[st_contains(circuitos, radios_censales)]


# Calcula el centroide de cada radio censal y conserva la columna 'id'
# centroides_radios_censales <- st_centroid(radios_censales) %>% 
#   cbind(radios_censales$id)

centroides_radios_censales <- st_centroid(radios_censales)




# Asigna a cada centroide el circuito electoral correspondiente
centroides_radios_censales_circuito <- st_join(centroides_radios_censales, circuitos, join = st_within)


centroides_radios_censales_circuito$lon <- st_coordinates(centroides_radios_censales_circuito)[,1]
centroides_radios_censales_circuito$lat <- st_coordinates(centroides_radios_censales_circuito)[,2]
centroides_radios_censales_circuito <- centroides_radios_censales_circuito %>%  st_set_geometry(NULL)

filtro_na <- is.na(centroides_radios_censales$lon)
sin_geo <- centroides_radios_censales_circuito[!filtro_na, ]

sin_geo <- centroides_radios_censales_circuito %>% 
  filter(lon == 'NaN')

sin_geo <- sin_geo %>% 
  select('codigo')

sin_geo <- left_join(sin_geo, radios_censales_geometry, by = "codigo")


#trabajo con los q no encontre circuito

centroides_radios_censales_circuito_sinna <- centroides_radios_censales_circuito %>% 
  filter(lon != 'NaN')


centroides_radios_censales_sincircuito <- centroides_radios_censales_circuito_sinna %>% 
  filter(is.na(muni_subci))

centroides_radios_censales_sincircuito <- centroides_radios_censales_sincircuito %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) 
mapview(centroides_radios_censales_sincircuito) #segun el mapa determino a donde corresponden

centroides_radios_censales_sincircuito <- centroides_radios_censales_sincircuito %>%
  mutate(muni_subci = case_when(
    codigo == 140140707 ~ 8,
    codigo > 140140707 & codigo < 140140712 ~ 6,
    codigo >  140140603 & codigo < 140140611 ~ 13,
    TRUE ~ 0))

centroides_radios_censales_sincircuito <- centroides_radios_censales_sincircuito %>% 
  select(c('codigo', 'muni_subci', 'ttlvvnd')) %>% 
  st_set_geometry(NULL)

centroides_radios_censales_circuito_sinna <- centroides_radios_censales_circuito_sinna %>% 
  filter(!is.na(muni_subci))

centroides_radios_censales_circuito_sinna <- centroides_radios_censales_circuito_sinna %>% 
  select(c('codigo', 'muni_subci', 'ttlvvnd'))


centroides_radios_censales_total <- centroides_radios_censales_circuito_sinna %>% 
  rbind(centroides_radios_censales_sincircuito)



# matcheo población y radios censales c seccionales

poblacion_radio_censal$CODIGO <- as.numeric(poblacion_radio_censal$CODIGO)
centroides_radios_censales_total$codigo <- as.numeric(centroides_radios_censales_total$codigo)

poblacion_seccionales <- left_join(poblacion_radio_censal, centroides_radios_censales_total, by = c('CODIGO' = 'codigo'))

centroides_radios_censales_total_sinpob <- anti_join(centroides_radios_censales_total, poblacion_radio_censal, by = c('codigo' = 'CODIGO'))

# centroides_radios_censales_circuito <- centroides_radios_censales_circuito %>% 
#   st_transform(crs = 4326)
#   
# st_write(centroides_radios_censales_circuito, "data/circuitos_seccionales/circuitosseccionales.shp", delete_layer = T)
# 
#   
# 
# mapview::mapview(centroides_radios_censales_circuito) 
# 
# datos_shp_nulos <- centroides_radios_censales_circuito[is.na(centroides_radios_censales_circuito$muni_subci), ]
# mapview(datos_shp_nulos)
# 
# datos_shp_nulos <- st_transform(datos_shp_nulos, crs = st_crs(circuitos))
# datos_shp_nulos_ <- st_join(datos_shp_nulos, circuitos, join = st_within)
# 
# 
# mapview(datos_shp_nulos) %>%
#   addPolylines(data = circuitos)
# 
# 
# mapview(centroides_radios_censales_circuito, zcol = "muni_subci")
# 


# centroides_radios_censales_circuito %>% 
#   group_by(muni_subci) %>% 
#   summarise(cantidad = n())
# 
# centroides_radios_censales_circuito <- centroides_radios_censales_circuito %>% 
#   st_set_geometry(NULL)


# centroides_radios_censales_circuito <- centroides_radios_censales_circuito %>% 
#   select(c(
#            "vv_r_22",
#            "vv_c_22",
#            "vv_d_22",
#            "ttlvvnd",
#            "pdcreet",
#            "muni_subci"
#            )) %>% 
#   mutate(across(c(vv_r_22, vv_c_22, vv_d_22, ttlvvnd, pdcreet), as.numeric))



poblacion_seccionales1 <- poblacion_seccionales %>% 
  select(c("Total Viviendas.x",
           "var_viv_2020",
           "Total Viviendas.y", 
           "Total Personas", 
           "Cantidad promedio de personas por vivienda habitada en cada radio",
           "Participación de cada radio en el total.y",
           "muni_subci"))


centroides_radios_censales_total_sinpob <- centroides_radios_censales_total_sinpob %>% 
  mutate(`Total Viviendas.x` = 0)


poblacion_radio_censal_agregada <- centroides_radios_censales_total_sinpob %>%
  group_by(muni_subci) %>%
  summarize(total_viviendas)


# Agrupar por circuito electoral y calcular sumas
sumas_circuitos <- poblacion_seccionales1 %>%
  group_by(muni_subci) %>%
  summarize(total_viviendas_x = sum(`Total Viviendas.x`),
            var_viv_2020 = mean(var_viv_2020),
            total_viviendas_y= sum(`Total Viviendas.y`),
            total_personas = sum(`Total Personas`),
            prom_personas_xvivienda = mean(`Cantidad promedio de personas por vivienda habitada en cada radio`),
            `Participación de cada radio en el total.y` = sum(`Participación de cada radio en el total.y`))


sumas_circuitos <- sumas_circuitos %>% 
  mutate(porcentaje_pob_ciudad = total_personas/1565112)

## Df facebook

poblacion_radios_facebook <- poblacion_radios_facebook %>% 
  st_as_sf(coords = c('longitude', 'latitude'), crs = 4326)

poblacion_radios_facebook <- poblacion_radios_facebook %>% 
  st_transform(st_crs(ciudad)) %>% 
  st_intersection(ciudad)

# # Calcula el área de intersección entre cada radio censal y cada circuito electoral
# area_interseccion <- st_area(interseccion)
# 
# # Encuentra el índice del circuito electoral que comparte más área con cada radio censal
# ind_max_area <- apply(area_interseccion, 1, which.max)
# 
# # Crea una nueva variable en la capa de datos de intersección con el atributo del circuito electoral correspondiente
# interseccion$circ_electoral <- circuitos$nom_circuito[ind_max_area]

# Agrega las columnas correspondientes de los circuitos electorales a la capa de datos de los radios censales
radios_censales_con_circuitos$nom_circuito <- interseccion$circ_electoral

# Muestra las primeras filas de la tabla con las columnas correspondientes de los circuitos electorales
head(radios_censales_con_circuitos[, c("cod_radio", "nom_circuito")])


