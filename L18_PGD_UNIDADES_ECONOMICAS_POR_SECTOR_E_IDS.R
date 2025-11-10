#********************************************************************************
#    INSTITUTO DE PLANEACIÓN DEMOCRÁTICA Y PROSPECTIVA DE LA CIUDAD DE MÉXICO   *
#********************************************************************************
#********************************************************************************             
#*                                                                              * 
#*    Metodología para el mapa de área de distribución de área de               *
#*    espacio público cubierta por árboles y arbustos por habitante,            *
#*    por alcaldía.                                                             *
#*    Línea XVIII. De Economía Dinámica, Innovadora y de Prosperidad            * 
#*                 Compartida                                                   *
#*    Plan General de Desarrollo de la Ciuad de México                          *
#*                                                                              * 
#********************************************************************************
#Limpiar la zona de trabajo----
rm(list=ls())
#Librerias----
if (!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse,dplyr,sf,classInt, googledrive, tmap, paletteer)
#Crear directorio----
dir.create("BASE")
directorio_base <- "BASE"
setwd(directorio_base)
getwd()

#Cargar la capa de exágonos para hacer el agrupado de los datos----
jp_file <- as_id("https://drive.google.com/file/d/1btYonN0jWimM19WFMmloh853Asb1jihu/view?usp=sharing")
drive_download(jp_file, overwrite = TRUE)
zip::unzip('H3_8_CDMX.zip',  exdir = "H3_8_CDMX")

hexagonos<- st_read("H3_8_CDMX/H3_8_CDMX.shp")|>
  st_transform(32614)|>
  select(h3_id)|>
  mutate(area = as.numeric(st_area(geometry)) / 10000)

##IDS por manzana----
url<- "https://www.evalua.cdmx.gob.mx/storage/app/media/2024/die/ids/mza_shp.zip"
options(timeout = 5000)
download.file(url, 'mza_shp.zip', mode = "wb")
zip::unzip("mza_shp.zip")

ids_mza <- st_read("mza_shp/ids_mza_cdmx.shp")|>
  st_transform(32614)|>
  select(ids)|>
  st_centroid()|>
  st_intersection(hexagonos)|>
  st_drop_geometry()|>
  as.data.frame()|>
  group_by(h3_id)|>
  summarise(ids_p = mean(ids, na.rm = TRUE))


##Cargar unidades económicas y seleccionar las de los sectores servicios, comercio e industria----
url<- "https://www.inegi.org.mx/contenidos/masiva/denue/denue_09_shp.zip"
download.file(url, 'denue_09_shp.zip', mode = "wb")
zip::unzip("denue_09_shp.zip", exdir = "denue_09_shp")

##Agregar las unidades económicas a la malla hexagonal para obtener la densidad de unidades por sector en cada hexágono----
denue_25<- st_read("denue_09_shp/denue_09.shp")|>
  st_transform(32614)|>
  mutate(sector = substr(codigo_act, 1,2), 
         actividad = )|>
  filter(actividad %in% c())
  select(actividad) |>
    st_intersection(hexagonos)|>
    st_drop_geometry()|>
    as.data.frame()|>
  group_by(h3_id,actividad)|>
  summarise(u = n(), .groups = "drop")|>
  pivot_wider(names_from = actividad,
              values_from = u,
              values_fill = 0)|>
  select(h3_id,Comercio, Industria, Servicios)

#Obtener la sendidad de unidades por cada sector----
hex_class <- hexagonos|>
  left_join(ids_mza, by = "h3_id")|>
  mutate(ids_p = case_when(is.na(ids_p) ~ 0,
                           TRUE ~ ids_p))|>
  left_join(denue_25s_, by = "h3_id")|>
  mutate(dens_com = Comercio/area , 
         dens_indus = Industria/area, 
         dens_serv =  Servicios/area)|>
  mutate(dens_com = case_when(is.na(dens_com) ~ 0, 
                              TRUE ~ dens_com), 
         dens_indus = case_when(is.na(dens_indus) ~ 0, 
                              TRUE ~ dens_indus),
         dens_serv = case_when(is.na(dens_serv) ~ 0, 
                                TRUE ~ dens_serv))
  
  
#Función para la correlación bivariada entre el IDS y la densidad de unidades económicas----
correlacion_bivariada <- function(df, var1, var2, nombre) {
  
  # Filtrar solo donde ambos valores sean > 0
  filtro_valido <- df[[var1]] > 0 & df[[var2]] > 0
  
  # Calcular correlación de Pearson
  cor_result <- cor.test(df[[var1]][filtro_valido], df[[var2]][filtro_valido], method = "pearson")
  print(paste("Correlación entre", var1, "y", var2, ":", round(cor_result$estimate, 3)))
  
  # --- Clasificación para var1 ---
  clase1 <- rep(NA_integer_, nrow(df))
  
  if (var1 == "ids_p") {
    # Clasificación personalizada con rangos fijos con base en la metodología de EVALÚA
    clase1[filtro_valido] <- cut(
      df[[var1]][filtro_valido],
      breaks = c(-Inf, 0.784, 0.860, 1),
      labels = 1:3,
      right = TRUE
    ) |> as.integer()
  } else {
    # Clasificación por Jenks si no es ids_p
    jenks1 <- classInt::classIntervals(df[[var1]][filtro_valido], n = 3, style = "jenks")
    clase1[filtro_valido] <- findInterval(df[[var1]][filtro_valido], jenks1$brks, all.inside = TRUE)
  }
  
  # --- Clasificación para var2 (siempre Jenks) ---
  jenks2 <- classInt::classIntervals(df[[var2]][filtro_valido], n = 3, style = "jenks")
  clase2 <- rep(NA_integer_, nrow(df))
  clase2[filtro_valido] <- findInterval(df[[var2]][filtro_valido], jenks2$brks, all.inside = TRUE)
  
  # Etiquetas combinadas
  clase_txt <- rep(NA_character_, nrow(df))
  clase_num <- rep(NA_integer_, nrow(df))
  clase_txt[filtro_valido] <- paste0("C", clase1[filtro_valido], "-C", clase2[filtro_valido])
  clase_num[filtro_valido] <- (clase1[filtro_valido] - 1) * 3 + clase2[filtro_valido]
  
  # Etiquetas de nivel
  etiquetas <- c("bajo", "medio", "alto")
  clase_var1_txt <- rep(NA_character_, nrow(df))
  clase_var2_txt <- rep(NA_character_, nrow(df))
  clase_var1_txt[filtro_valido] <- etiquetas[clase1[filtro_valido]]
  clase_var2_txt[filtro_valido] <- etiquetas[clase2[filtro_valido]]
  
  # Armar data.frame de resultados
  resultado <- data.frame(
    clase_txt = clase_txt,
    clase_num = clase_num
  )
  colnames(resultado)[1:2] <- paste0("clase_", nombre, c("_txt", "_num"))
  
  # Agregar clasificaciones univariadas
  resultado[[paste0(nombre, "_", var1, "_nivel")]] <- clase_var1_txt
  resultado[[paste0(nombre, "_", var2, "_nivel")]] <- clase_var2_txt
  
  return(resultado)
}

# Aplicar la función por cada par de variables----
biv_com  <- correlacion_bivariada(hex_class, "ids_p", "dens_com",   "com")
biv_ind  <- correlacion_bivariada(hex_class, "ids_p", "dens_indus", "indus")
biv_ser  <- correlacion_bivariada(hex_class, "ids_p", "dens_serv",  "serv")

# Unir resultados a la capa original----
hex_class <- cbind(hex_class, biv_com, biv_ind, biv_ser)|>
  select(h3_id, com_ids_p_nivel,com_dens_com_nivel,
         indus_ids_p_nivel,indus_dens_indus_nivel,serv_ids_p_nivel,serv_dens_serv_nivel,
         clase_com_txt,clase_indus_txt,clase_serv_txt)

# Guardar----
st_write(hex_class, "ids_ueconomicas_classes_completas.gpkg", delete_layer = TRUE)


#Mapear----

tmap_mode("view")
tm_basemap("OpenStreetMap") +
  
  tm_shape(hex_class) +
  tm_polygons("clase_com_txt",
              style = "fixed",
              breaks = breaks_precio,
              palette = paletteer_c("ggthemes::Orange", 5), 
              alpha = 0.8,
              title = "Precio por m² de vivienda (mediana)") +
  
  tm_layout(legend.position = c("left", "bottom"))




