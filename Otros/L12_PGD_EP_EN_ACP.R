#Limpiar la zona de trabajo
rm(list=ls())
#Librerias
if (require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse,dplyr,sf,classInt)

#Areas de conservación patrimonial
acp<- st_read("https://datos.cdmx.gob.mx/dataset/f3d8a8f0-fda5-495b-b174-ab760a7a3df4/resource/5b726162-026d-4f57-8a7d-488b8dc2ac42/download/reas-de-conservacin-patrimonial.json")|>
  st_transform(32614)


tabla_acp_mun<-acp|>
  group_by(alcaldia)|>
  summarise(acp = n())|>
  st_drop_geometry()|>
  as.data.frame()


#Espacios públicos----
epublicos <- st_read("C:/Users/brenp/Downloads/espacio_publico_2025 (2)/espacio_publico_2025/espacio_publico_2025.shp")|>
  st_transform(32614)|>
  st_centroid()|>
  mutate(id = row_number())

plot(epublicos)



puntos_ep_acp <- epublicos|>
  st_intersection(acp)

tabla_ep_acp <- puntos_ep_acp|>
  group_by(alcaldia)|>
  summarise(cuenta = n())|>
  st_drop_geometry()|>
  as.data.frame()|>
  left_join(tabla_acp_mun, by = "alcaldia")


plot(puntos_ep_acp)






