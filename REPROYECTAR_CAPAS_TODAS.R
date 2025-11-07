#Borrar entorno
rm(list=ls())

# Librerías necesarias====
if(!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse, dplyr, sf)

manejo_capas <- function(geobase, salida){
  capas <- st_layers(geobase)$name
  
  for (capa in capas) {
    # Intentar leer la capa
    datos <- try(st_read(geobase, layer = capa, quiet = TRUE))

    datos <- st_transform(datos, 4326)
    
    # Escribir en el GPKG de salida
    st_write(datos, salida, layer = capa, delete_layer = TRUE, quiet = TRUE)
  }

}

# Llamada a la función usando archivo original como entrada y otro como salida
entrada <- "C:/Users/brenp/Desktop/CARTOGRAFIA PGD/CAPAS PARA SIEG/capas_para_sieg_mafa.gpkg"
salida  <- "C:/Users/brenp/Desktop/CARTOGRAFIA PGD/CAPAS PARA SIEG/capas_para_sieg_mafa.gpkg"

manejo_capas(entrada, salida)

 
 