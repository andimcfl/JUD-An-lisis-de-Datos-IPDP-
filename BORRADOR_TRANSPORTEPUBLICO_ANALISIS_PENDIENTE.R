#Borrar entorno
rm(list=ls())

# Librerías necesarias====
if(!require('pacman')) install.packages('pacman')
pacman::p_load(modeest, tidyverse, srvyr, showtext,survey,sf)



# #metro
url<- "https://datos.cdmx.gob.mx/dataset/1b014317-ddb1-46c7-ac79-7330c652abe3/resource/288b10dd-4f21-4338-b1ed-239487820512/download/stcmetro_shp.zip"

download.file(url, 'stcmetro_shp.zip', mode = "wb")

zip::unzip("stcmetro_shp.zip", exdir = "stcmetro_shp")

metro_punto <- st_read("stcmetro_shp/stcmetro_shp/STC_Metro_estaciones_utm14n.shp") |> 
  st_transform(crs = 4326)|>
  select(EST)|>
  mutate(SISTEMA = "METRO")

# #metrobus
url<-"https://datos.cdmx.gob.mx/dataset/63a08f93-e959-430f-8dc4-bf07182401e6/resource/66cd5773-d83f-4a80-8c15-8ab34851e046/download/mb_shp.zip"

download.file(url, 'mb_shp.zip', mode = "wb")

zip::unzip("mb_shp.zip", exdir = "mb_shp")

metrobus_punto <- st_read("mb_shp/mb_shp/Metrobus_estaciones.shp")|>
  st_transform(crs = 4326)|>
  select(EST)|>
  mutate(SISTEMA = "METROBUS")

# #electrico 
url <- "https://datos.cdmx.gob.mx/dataset/6a3bc652-a775-41a0-aeb6-05d31ee40b59/resource/c7a1b906-92ff-4525-9b0c-f166ea731f95/download/ste_shp.zip"

download.file(url, 'ste_shp.zip', mode = "wb")

zip::unzip("ste_shp.zip", exdir = "ste_shp")

cablebus<- st_read("ste_shp/ste_shp/ste_cablebus_shp/ste_cablebus_shp/STE_Cablebus_estaciones.shp")|>
  st_transform(crs = 4326)|>
  select(EST)|>
  mutate(SISTEMA = "CABLEBUS")

tren<- st_read("ste_shp/ste_shp/ste_tren_ligero_shp/ste_tren_ligero_shp/STE_TrenLigero_estaciones_utm14n.shp")|>
  st_transform(crs = 4326)|>
  select(EST)|>
  mutate(SISTEMA = "TRENLIGERO")

trole<- st_read("ste_shp/ste_shp/ste_trolebus_shp/ste_trolebus_shp/STE_Trolebus_Paradas.shp")|>
  st_transform(crs = 4326)|>
  rename(EST = TIPO)|>
  select(EST)|>
  mutate(SISTEMA = "TROLEBUS")


#Caminoes concesionados----
url <- "https://datos.cdmx.gob.mx/dataset/e5e67126-8964-4457-a88b-ed0194bb5eb5/resource/391b1ea2-d5e6-4a60-a1fe-114ccb99ee82/download/rtp_shp.zip"

download.file(url, 'rtp_shp.zip', mode = "wb")

zip::unzip("rtp_shp.zip", exdir = "rtp_shp")

rtp<- st_read("rtp_shp/rtp_shp/RTP_paradas.shp")|>
  st_transform(crs = 4326)|>
  rename(EST = CORREDOR)|>
  select(EST)|>
  mutate(SISTEMA = "RTP")



url <- "https://datos.cdmx.gob.mx/dataset/7c2d9504-3d22-4204-977f-92806e73fce2/resource/6e0ec632-49bd-4f7b-a8f8-2afb44464ae1/download/concesionado_shp.zip"

# download.file(url, 'concesionado_shp.zip', mode = "wb")

# zip::unzip("concesionado_shp.zip", exdir = "rtp_shp")

trole<- st_read("concesionado_shp/concesionado_shp/concesionado_paradas.shp")|>
  st_transform(crs = 4326)|>
  mutate(SISTEMA = "CONCESIONADO")


sistemas_tpm<-cablebus|>
  bind_rows(tren,trole, metrobus_punto, metro_punto)|>
  mutate(no = row_number(),
         id_est = paste0(no, "-", SISTEMA))