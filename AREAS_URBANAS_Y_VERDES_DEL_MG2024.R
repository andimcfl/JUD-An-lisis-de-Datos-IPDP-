pacman::p_load(sf, tidyverse, dplyr)

##MARCO GEOESTADÍSTICO NACIONAL 2024----
url <-"https://www.inegi.org.mx/contenidos/productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/marcogeo/794551132173/09_ciudaddemexico.zip"
options(timeout = 5000)
download.file(url, '09_ciudaddemexico.zip', mode = "wb")

zip::unzip("09_ciudaddemexico.zip", exdir = "09_ciudaddemexico")


ageb_ur_mgn_2024 <- st_read("09_ciudaddemexico/conjunto_de_datos/09a.shp")|>
  mutate(area = st_area(geometry))|>
  st_drop_geometry()|>
  as.data.frame()|>
  group_by(CVE_MUN)|>
  summarise(area_alcal = sum(area))


view(ageb_ur_mgn_2024)
write.csv(ageb_ur_mgn_2024, "area_por_alcaldia.csv", row.names = FALSE)


sia_ur_mgn_2024 <- st_read("09_ciudaddemexico/conjunto_de_datos/09sia.shp")|>
  filter(TIPO %in% c("Camellón" , "Estación de Metrobus" , "Estación de Tren Ligero",
                     "Glorieta" , "Jardín" , "Monumento u Obelisco" , "Parque" , "Área Verde" , "Áreas Verdes"))|>
  mutate(area = st_area(geometry))|>
  st_drop_geometry()|>
  as.data.frame()|>
  group_by(CVE_MUN)|>
  summarise(area_alcal = sum(area))

view(sia_ur_mgn_2024)

write.csv(sia_ur_mgn_2024, "sia_ur_mgn_2024.csv", row.names = FALSE)
