#Borrar entorno
rm(list=ls())

# Librerías necesarias====
if(!require('pacman')) install.packages('pacman')
pacman::p_load(modeest, tidyverse, srvyr, showtext,survey,sf)

#CONSIDERACIONES
#Tiempo de espera para la descarga
options(timeout = 8000)
#Ajuste para la UPM
options(survey.lonely.psu = "certainty")

# URL de la base ENSU 2024
url <- "https://www.inegi.org.mx/contenidos/programas/ensu/microdatos/ensu_bd_2024_csv.zip"

# Descargar y descomprimir
download.file(url, destfile = "ensu_2024.zip", mode = "wb")
unzip("ensu_2024.zip", exdir = "ensu_2024")

unzip("ensu_2024/ensu_bd_junio_2024_csv.zip", exdir = "ensu_2024")

list.files("ensu_2024", recursive = TRUE) 

datos_conflictos <- read_csv("ensu_2024/ENSU_CB_0624.csv",
                             col_types = cols(),
                             show_col_types = FALSE) |>
  janitor::clean_names()|>
  filter(edad >= 18)|>
  filter(cve_ent == "09")|>
  mutate(efectividad = ifelse(bp3_2 %in% c(1,2),1,0)) 


colnames(datos_conflictos)
unique(datos_conflictos$cve_ent)
unique(datos_conflictos$efectividad)

# Definir diseño de encuesta
dm <- datos_conflictos |>
  as_survey_design(ids = upm_dis,
                   strata = est_dis,
                   weights = fac_sel, 
                   nest = TRUE)
# Calcular porcentaje con diseño muestral
porcentaje <- dm |>col_names = TRUE,
  group_by(cve_mun,efectividad) |> #bp2_1
  summarise(personas = survey_total(vartype = "cv"),
            prc_efectivo = round(survey_prop(vartype = "cv") * 100,1))|>
  filter(efectividad == 1)|>
  mutate(CVEGEO = paste0("09", cve_mun))|>
  select(CVEGEO, prc_efectivo)

porcentaje


municipios <- st_read("C:/Users/brenp/Desktop/CARTOGRAFIA PGD/municipios.shp")|>
  st_transform(32614)

head(municipios)

municipios

municipioss<-municipios |>
  left_join(porcentaje, by = "CVEGEO")


plot(municipioss)

st_write(municipioss, "C:/Users/brenp/Desktop/CARTOGRAFIA PGD/gobierno/percep_efectivi_gobierno_municipios.gpkg")
