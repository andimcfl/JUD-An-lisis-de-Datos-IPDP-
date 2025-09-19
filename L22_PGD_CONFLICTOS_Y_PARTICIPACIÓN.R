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
  mutate(
    min_un_conflicto = ifelse(
      bp2_2_01 == 1 |
        bp2_2_02 == 1 |
        bp2_2_03 == 1 |
        bp2_2_04 == 1 |
        bp2_2_05 == 1 |
        bp2_2_06 == 1 |
        bp2_2_07 == 1 |
        bp2_2_08 == 1 |
        bp2_2_09 == 1 |
        bp2_2_10 == 1 |
        bp2_2_11 == 1 |
        bp2_2_12 == 1 |
        bp2_2_13 == 1 |
        bp2_2_14 == 1 |
        bp2_2_15 == 1 |
        bp2_2_16 == 1 |
        bp2_2_17 == 1,
      1, 0))|>
  filter(cve_ent == "09")

colnames(datos_conflictos)
unique(datos_conflictos$cve_ent)

# Definir diseño de encuesta
dm <- datos_conflictos |>
  as_survey_design(ids = upm_dis,
                   strata = est_dis,
                   weights = fac_sel, 
                   nest = TRUE)
# Calcular porcentaje con diseño muestral
porcentaje <- dm |>
  group_by(cve_mun,min_un_conflicto) |> #bp2_1
  summarise(personas = survey_total(vartype = "cv"),
            prc_conflict = round(survey_prop(vartype = "cv") * 100,1))|>
  filter(min_un_conflicto == 1)|>
  mutate(CVEGEO = paste0("09", cve_mun))|>
  select(CVEGEO, prc_conflict)

porcentaje


municipios <- st_read("C:/Users/brenp/Desktop/CARTOGRAFIA PGD/Participación/municipios.shp")
head(municipios)


municipioss<-municipios |>
  left_join(porcentaje, by = "CVEGEO")

municipioss

participativo <- openxlsx::read.xlsx("C:/Users/brenp/Desktop/CARTOGRAFIA PGD/Participación/parti_presupuestoparti_1.xlsx")|>
  janitor::clean_names()|>
  mutate(CVEGEO = paste0("09", cve_mun))|>
  select(CVEGEO, parti23)

municipiosss <- municipioss |>
  left_join(participativo, by = "CVEGEO")|>
  st_transform(crs = 32614)

municipiosss

st_write(municipiosss, "C:/Users/brenp/Desktop/CARTOGRAFIA PGD/participacion/participacion_municipios.gpkg")
