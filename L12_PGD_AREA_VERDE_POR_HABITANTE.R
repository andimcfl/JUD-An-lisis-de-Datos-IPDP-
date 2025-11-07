#********************************************************************************
#    INSTITUTO DE PLANEACIÓN DEMOCRÁTICA Y PROSPECTIVA DE LA CIUDAD DE MÉXICO   *
#********************************************************************************
#********************************************************************************             
#*                                                                              * 
#*    Metodología para el mapa de área de distribución de área de               *
#*    espacio público cubierta por árboles y arbustos por habitante,            *
#*    por alcaldía.                                                             *
#*    Línea XII. Habitable, con equilibrio ecológico y ambiental                *                                                    
#*    Plan General de Desarrollo de la Ciuad de México                          *
#*                                                                              * 
#********************************************************************************
#Limpiar la zona de trabajo----
rm(list=ls())
#Librerias----
if (require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse,dplyr,sf,classInt, googledrive, tmap, paletteer)

##Consideraciones
options(timeout = 100000)
#Crear directorio----
dir.create("D:/BASES Y DATOS GENERALES")
setwd(directorio_base)

##Marco Geoestadistico Nacional 2024----
url <-"https://www.inegi.org.mx/contenidos/productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/marcogeo/794551132173/09_ciudaddemexico.zip"
download.file(url, '09_ciudaddemexico.zip', mode = "wb")
zip::unzip("09_ciudaddemexico.zip", exdir = "09_ciudaddemexico")

####Cargar localidades urbanas####
loc_ur_mgn_2024 <- st_read("09_ciudaddemexico/conjunto_de_datos/09l.shp")|>
  filter(AMBITO == "Urbana")|>
  st_transform(crs = 32614)|>
  mutate(CVE_MUN = paste0(CVE_ENT,CVE_MUN))|>
  group_by(CVE_MUN)|>
  summarise(cuenta = n())|>
  select(CVE_MUN)

##Datos del censo de población y vivienda (principales resultados por localidad)----
url <- "https://www.inegi.org.mx/contenidos/programas/ccpv/2020/microdatos/iter/iter_09_2020_xlsx.zip"
options(timeout = 500)
download.file(url, 'iter_09_2020_xlsx.zip', mode = "wb")

iter_09_2020_xlsx <- unzip('iter_09_2020_xlsx.zip', files = "ITER_09XLSX20.xlsx")

####Cargar datos de población por localidad####
iter_09_2020_xlsx <- readxl::read_excel("ITER_09XLSX20.xlsx")|>
  mutate(CVEGEO = paste0(ENTIDAD, MUN, LOC),
         CVE_MUN = paste0(ENTIDAD, MUN))|>
  mutate_all(~ gsub("\\*", "", .)) |> 
  mutate(across(9:230, ~ as.numeric(., na.rm = TRUE)))|>
  select(CVEGEO, CVE_MUN,LOC, POBTOT)|>
  filter(!LOC %in% c("0000", "9998", "9999"))|>
  group_by(CVE_MUN)|>
  summarise(PTOT = sum(POBTOT))

####Asignar la información de población al archivo vectorial de las localidadades####
loc_ur_mgn_2024<-loc_ur_mgn_2024|>
  left_join(iter_09_2020_xlsx, by = "CVE_MUN")
  

#Área cubierta por arboles y arbustos----
url <- "http://www.conabio.gob.mx/informacion/gis/maps/ccl/acacdmxgw_c.zip"
download.file(url, 'acacdmxgw_c.zip', mode = "wb")
ruta <- unzip('acacdmxgw_c.zip', files = "acacdmxuw.sqlite")

averdesss <- st_read(ruta, layer = "acacdmxuw")|>
  st_transform(32614)|>
  select(gridcode)


#Espacios públicos----
jp_file <- as_id("https://drive.google.com/file/d/1wm8hIsMkGrKwjNZ7d7V-TEEFWCWuel4A/view?usp=drive_link")
drive_download(jp_file, overwrite = TRUE)
zip::unzip('espacio_publico_2025.zip',  exdir = "espacio_publico_2025")

epublicos <- st_read("espacio_publico_2025/espacio_publico_2025/espacio_publico_2025.shp")|>
  st_transform(32614)|>
  mutate(tipo = "Espacio público")

#Cortar las áreas verdes a los espacios públicos----
averdesss <- st_read(ruta, layer = "acacdmxuw")|>
  st_transform(32614)|>
  select(gridcode)|>
  st_intersection(epublicos)

####Exportar si se quiere la superficie de espacio público cubierta por árboles y arbustos####
#st_write(averdesss,"acacdmxu_averdesss_corte_epub.gpkg", delete_layer = TRUE)

#Obtener el área de espacio cubierta por árboles y arbustos por habitante agrupado por alcaldia----
##Obtener el área de espacio cubierta por árboles y arbusto por alcaldía----
averdesss_1<-averdesss|>
  select(esp_pub)|>
  st_intersection(loc_ur_mgn_2024)|>
  mutate(area = as.numeric(st_area(Shape)))|>
  group_by(CVE_MUN)|>
  summarise(area_t = sum(area))|>
  st_drop_geometry()|>
  as.data.frame()

##Área de espacio cubierta por árboles y arbustos por habitante agrupado por alcaldia----
loc_ur_mgn_2024<- loc_ur_mgn_2024|>
  left_join(averdesss_1, by = "CVE_MUN")|>
  mutate(sphab = area_t/PTOT)

#Exportar los datos de Área de espacio cubierta por árboles y arbustos por habitante agrupado por alcaldia----
#st_write(loc_ur_mgn_2024,"acacdmxu_epub_habitante_loc.gpkg", delete_layer = TRUE)


##MAPEAR----
tmap_mode("view")

tm_basemap("OpenStreetMap") +   
  tm_shape(loc_ur_mgn_2024) +
  tm_polygons("sphab",
              style = "jenks",
              n = 4,
              palette = paletteer_c("ggthemes::Classic Area Green", 4),
              alpha = 0.7,
              title = "m² de vegetación por habitante") +
  tm_layout(legend.position = c("left", "bottom"))+
  tm_shape(epublicos) +
  tm_borders(col = "grey60", lwd = 1)





