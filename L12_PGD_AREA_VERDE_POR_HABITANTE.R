#Limpiar la zona de trabajo
rm(list=ls())
#Librerias
if (require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse,dplyr,sf,classInt)

##MARCO GEOESTADÍSTICO NACIONAL 2024----
url <-"https://www.inegi.org.mx/contenidos/productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/marcogeo/794551132173/09_ciudaddemexico.zip"
options(timeout = 1000)
download.file(url, '09_ciudaddemexico.zip', mode = "wb")

zip::unzip("09_ciudaddemexico.zip", exdir = "09_ciudaddemexico")

loc_ur_mgn_2024 <- st_read("09_ciudaddemexico/conjunto_de_datos/09l.shp")|>
  filter(AMBITO == "Urbana")|>
  st_transform(crs = 32614)|>
  mutate(CVE_MUN = paste0(CVE_ENT,CVE_MUN))|>
  group_by(CVE_MUN)|>
  summarise(cuenta = n())|>
  select(CVE_MUN)

##DATOS DEL CENSO DE POBLACIÓN Y VIVIENDA (PRINCIPALES RESULTADOS POR LOCALIDAD)----
url <- "https://www.inegi.org.mx/contenidos/programas/ccpv/2020/microdatos/iter/iter_09_2020_xlsx.zip"
options(timeout = 500)
download.file(url, 'iter_09_2020_xlsx.zip', mode = "wb")

iter_09_2020_xlsx <- unzip('iter_09_2020_xlsx.zip', files = "ITER_09XLSX20.xlsx")

iter_09_2020_xlsx <- readxl::read_excel("ITER_09XLSX20.xlsx")|>
  mutate(CVEGEO = paste0(ENTIDAD, MUN, LOC),
         CVE_MUN = paste0(ENTIDAD, MUN))|>
  mutate_all(~ gsub("\\*", "", .)) |> 
  mutate(across(9:230, ~ as.numeric(., na.rm = TRUE)))|>
  select(CVEGEO, CVE_MUN,LOC, POBTOT)|>
  filter(!LOC %in% c("0000", "9998", "9999"))|>
  group_by(CVE_MUN)|>
  summarise(PTOT = sum(POBTOT))

loc_ur_mgn_2024<-loc_ur_mgn_2024|>
  left_join(iter_09_2020_xlsx, by = "CVE_MUN")
  

#ARES VERDES POR HABITANTE----
ruta <- "C:/Users/brenp/Desktop/CARTOGRAFIA PGD/Habitable/acub_arbol_arbus_2024/acacdmxuw.sqlite"
st_layers(ruta)

averdesss <- st_read(ruta, layer = "acacdmxuw")|>
  st_transform(32614)|>
  select(gridcode)|>
  st_intersection(loc_ur_mgn_2024)

#st_write(averdesss,"C:/Users/brenp/Desktop/CARTOGRAFIA PGD/Habitable/acub_arbol_arbus_2024/acacdmxu_averdesss_corte.gpkg", delete_layer = TRUE)

averdesss_1<-averdesss|>
  mutate(area = as.numeric(st_area(Shape)))|>
  group_by(CVE_MUN)|>
  summarise(area_t = sum(area))|>
  mutate(CVE_MUN = paste0("09",CVE_MUN))|>
  st_drop_geometry()|>
  as.data.frame()


loc_ur_mgn_2024<- loc_ur_mgn_2024|>
  left_join(averdesss_1, by = "CVE_MUN")|>
  mutate(sphab = area_t/PTOT)


#st_write(loc_ur_mgn_2024,"C:/Users/brenp/Desktop/CARTOGRAFIA PGD/Habitable/acub_arbol_arbus_2024/acacdmxu_habitante_loc.gpkg", delete_layer = TRUE)


#Espacios públicos----
epublicos <- st_read("C:/Users/brenp/Downloads/espacio_publico_2025 (2)/espacio_publico_2025/espacio_publico_2025.shp")|>
  st_transform(32614)

averdesss <- st_read(ruta, layer = "acacdmxuw")|>
  st_transform(32614)|>
  select(gridcode)|>
  st_intersection(epublicos)

st_write(averdesss,"C:/Users/brenp/Desktop/CARTOGRAFIA PGD/Habitable/acub_arbol_arbus_2024/acacdmxu_averdesss_corte_epub.gpkg", delete_layer = TRUE)


averdesss_1<-averdesss|>
  select(esp_pub)|>
  st_intersection(loc_ur_mgn_2024)|>
  mutate(area = as.numeric(st_area(Shape)))|>
  group_by(CVE_MUN)|>
  summarise(area_t = sum(area))|>
  st_drop_geometry()|>
  as.data.frame()

loc_ur_mgn_2024<- loc_ur_mgn_2024|>
  left_join(averdesss_1, by = "CVE_MUN")|>
  mutate(sphab = area_t/PTOT)

st_write(loc_ur_mgn_2024,"C:/Users/brenp/Desktop/CARTOGRAFIA PGD/Habitable/acub_arbol_arbus_2024/acacdmxu_epub_habitante_loc.gpkg", delete_layer = TRUE)

