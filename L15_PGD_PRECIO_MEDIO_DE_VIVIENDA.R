rm(list = ls())

if(!require ('pacman')) install.packages('pacman')
pacman::p_load(tidyverse, dplyr, sf, readr)


base_datos <- read_csv("C:/Users/brenp/Desktop/CARTOGRAFIA PGD/Vivienda/cdmx_avaluos.csv",
                       col_types = NULL, 
                       show_col_types = FALSE)|>
  st_as_sf(coords = c("longitud", "latitud"), crs = 4326)|>
  st_transform(32614)|>
  mutate(id = row_number())

st_write(base_datos,"C:/Users/brenp/Desktop/CARTOGRAFIA PGD/Vivienda/cdmx_avaluos.geojson", delete_layer = TRUE)

hexagonos<- st_read("C:/Users/brenp/Desktop/CARTOGRAFIA PGD/H3_8_CDMX/H3_8_CDMX.shp")|>
  st_transform(32614)|>
  select(h3_id)


base_datos_hexa<- base_datos|>
  st_intersection(hexagonos)|>
  st_drop_geometry()|>
  as.data.frame()


library(dplyr)

resultado <- base_datos_hexa|>
  group_by(h3_id)|>
  summarise(mediana_precio_m2 = median(precio_m2, na.rm = TRUE))



hexagonos_precio_viv <- hexagonos|>
  left_join(resultado, by = "h3_id")



st_write(hexagonos_precio_viv,"C:/Users/brenp/Desktop/CARTOGRAFIA PGD/Vivienda/hexagonos_precio_viv.geojson")
