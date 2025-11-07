#********************************************************************************
#    INSTITUTO DE PLANEACIÓN DEMOCRÁTICA Y PROSPECTIVA DE LA CIUDAD DE MÉXICO   *
#********************************************************************************
#********************************************************************************             
#*                                                                              * 
#*    Metodología para el mapa de precio por m² de vivienda 2018-2024           *
#*    Línea XV. Proveedora de Vivienda Sostenible y Adecuada                    *                                                   
#*    Plan General de Desarrollo de la Ciuad de México                          *
#*                                                                              * 
#********************************************************************************
#Limpiar la zona de trabajo----
rm(list = ls())
#Librerias----
if(!require ('pacman')) install.packages('pacman')
pacman::p_load(tidyverse, dplyr, sf, readr, googledrive, tmap, paletteer)

##Consideraciones----
options(timeout = 100000)
#Crear directorio
dir.create("D:/BASES Y DATOS GENERALES")
setwd(directorio_base)

#Cargar la información sobre los avalúos de la SHF----
jp_file <- as_id("https://drive.google.com/file/d/1uGSLaZbLKH7qUuIi9RK_4l30CN4XLa2p/view?usp=drive_link")
drive_download(jp_file, overwrite = TRUE)

base_datos <- read_csv("cdmx_avaluos.csv",
                       col_types = NULL, 
                       show_col_types = FALSE)|>
  st_as_sf(coords = c("longitud", "latitud"), crs = 4326)|>
  st_transform(32614)|>
  mutate(id = row_number())

#Cargar la capa de exágonos para hacer el agrupado de los datos----
jp_file <- as_id("https://drive.google.com/file/d/1btYonN0jWimM19WFMmloh853Asb1jihu/view?usp=sharing")
drive_download(jp_file, overwrite = TRUE)
zip::unzip('H3_8_CDMX.zip',  exdir = "H3_8_CDMX")

hexagonos<- st_read("H3_8_CDMX/H3_8_CDMX.shp")|>
  st_transform(32614)|>
  select(h3_id)

#Agrupar la información por unidad de análisis (hexágonos)-----
base_datos_hexa<- base_datos|>
  st_intersection(hexagonos)|>
  st_drop_geometry()|>
  as.data.frame()

#Obtener el precio por m² de vivienda (mediana)---- 
resultado <- base_datos_hexa|>
  group_by(h3_id)|>
  summarise(mediana_precio_m2 = median(precio_m2, na.rm = TRUE))

#Agregar la información del precio al archivo vectorial para su representación----
hexagonos_precio_viv <- hexagonos|>
  left_join(resultado, by = "h3_id")

#Ecportar el resultado----
#st_write(hexagonos_precio_viv,"hexagonos_precio_viv.gpkg")

hexagonos_precio_viv<-hexagonos_precio_viv|>
  filter(mediana_precio_m2 > 0)
#Mapear----
breaks_precio <- c(4607, 6000, 20000, 35000, 50000, 75589)

tmap_mode("view")
tm_basemap("OpenStreetMap") +
  
  tm_shape(hexagonos_precio_viv) +
  tm_polygons("mediana_precio_m2",
              style = "fixed",
              breaks = breaks_precio,
              palette = paletteer_c("ggthemes::Orange", 5), 
              alpha = 0.8,
              title = "Precio por m² de vivienda (mediana)") +
  
  tm_layout(legend.position = c("left", "bottom"))



