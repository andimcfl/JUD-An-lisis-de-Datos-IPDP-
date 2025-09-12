#Limpiar la zona de trabajo
rm(list=ls())
#Librerias
if (require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse,dplyr,sf,classInt)

hexagonos<- st_read("C:/Users/brenp/Desktop/CARTOGRAFIA PGD/H3_8_CDMX/H3_8_CDMX.shp")|>
  st_transform(32614)|>
  mutate(area = as.numeric(st_area(geometry)) / 10000)

##AGREGAR EL IDS POR MANZANA
ids_mza <- st_read("C:/Users/brenp/Desktop/CARTOGRAFIA PGD/linea economia/mza_shp_ids_evalua/ids_mza_cdmx.shp")|>
  st_transform(32614)|>
  select(ids)|>
  st_centroid()

ids_mza_j<-ids_mza|>
  st_intersection(hexagonos)|>
  st_drop_geometry()|>
  as.data.frame()

ids_mza_j_datos <- ids_mza_j|>
  group_by(h3_id)|>
  summarise(ids_p = mean(ids, na.rm = TRUE))


##DENUE
denue_25 <- st_read("C:/Users/brenp/Desktop/CARTOGRAFIA PGD/linea economia/2025")|>
  st_transform(32614)|>
  select(actividad)

denue_25s<-denue_25|>
  st_intersection(hexagonos)|>
  st_drop_geometry()|>
  as.data.frame()


denue_25s_ <- denue_25s|>
  group_by(h3_id,actividad)|>
  summarise(u = n(), .groups = "drop") |>
  pivot_wider(names_from = actividad,
              values_from = u,
              values_fill = 0)|>
  select(h3_id,Comercio, Industria, Servicios)


hex_class <- hexagonos|>
  left_join(ids_mza_j_datos, by = "h3_id")|>
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
  
  

correlacion_bivariada <- function(df, var1, var2, nombre) {
  
  # Filtrar solo donde ambos valores sean > 0
  filtro_valido <- df[[var1]] > 0 & df[[var2]] > 0
  
  # Calcular correlación de Pearson
  cor_result <- cor.test(df[[var1]][filtro_valido], df[[var2]][filtro_valido], method = "pearson")
  print(paste("Correlación entre", var1, "y", var2, ":", round(cor_result$estimate, 3)))
  
  # --- Clasificación para var1 ---
  clase1 <- rep(NA_integer_, nrow(df))
  
  if (var1 == "ids_p") {
    # Clasificación personalizada con rangos fijos
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

# Aplicar la función a tus pares de variables
biv_com  <- correlacion_bivariada(hex_class, "ids_p", "dens_com",   "com")
biv_ind  <- correlacion_bivariada(hex_class, "ids_p", "dens_indus", "indus")
biv_ser  <- correlacion_bivariada(hex_class, "ids_p", "dens_serv",  "serv")

# Unir resultados a la capa original
hex_class <- cbind(hex_class, biv_com, biv_ind, biv_ser)|>
  select(h3_id,u_servicio,area,ids_p,Comercio,Industria,Servicios,
         dens_com,dens_indus,dens_serv,com_ids_p_nivel,com_dens_com_nivel,
         indus_ids_p_nivel,indus_dens_indus_nivel,serv_ids_p_nivel,serv_dens_serv_nivel,
         clase_com_txt,clase_indus_txt,clase_serv_txt)

# Guardar como shapefile
st_write(hex_class, "C:/Users/brenp/Desktop/CARTOGRAFIA PGD/ids_ueconomicas_classes_completas.gpkg", delete_layer = TRUE)


