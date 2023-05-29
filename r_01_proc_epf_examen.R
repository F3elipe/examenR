# Examen final: Encuesta de Presupuestos Familiares(julio 2016-junio 2017)

# 1. Cargar librerías
pacman::p_load(haven,
               tidyverse,
               dplyr,
               sjmisc)
# 2. Cargar datos
epf_personas <- haven::read_sav("input/data/base-personas-viii-epf-(spss).sav")

# 3. Explorar datos
str(epf_personas)
names(epf_personas)
names(epf_personas)

options(scipen = 999) # Evitar notación cientifica

# 4. Selección y cambio de nombre de variables a utilizar
epf_personasmod <- epf_personas %>% 
  dplyr::select(ZONA,FE,
                VARSTRAT, VARUNIT,
                GASTOT_HD, NPERSONAS,
                ingresostot = ING_TOTAL_HOG_HD,
                tipotenenciahog = TVP,
                tipovivienda = VP)

# 5. Recodificación variables
epf_personasmod %>% 
  mutate_at(vars(ZONA, tipotenenciahog, tipovivienda), ~(as.factor(.))) %>% 
  mutate(ZONA = car::recode(.$ZONA, recodes = c("1 = 'Gran santiago'; 2 = 'Resto de capitales regionales'"))) %>% 
  mutate(tipotenenciahog = car::recode(.$tipotenenciahog, recodes = c("1 = 'Propia,totalmente pagada'; 2 = 'Propia, pagándose'; 3 = 'Arrendada(con contrato)';
                                              4 = 'Arrendada'; 5 = 'Cedida por trabajo o servicio'; 6 = 'Cedida por un pariente o amigo';
                                              7 = 'Ocupada de hecho'; 8 = 'Propiedad en litigio'; 9 = 'Herencia o sucesión compartida';
                                              10 = 'Arrendada(no se sabe si es con contrato o no)'; c(-99,-88,-77) = NA"))) %>% 
  mutate(tipovivienda = car::recode(.$tipovivienda, recodes = c("1 = 'Casa no pareada(independiente)'; 2 = 'Casa pareada por un lado'; 3 = 'Departamento en edificio con ascensor';
                                                                5 = 'Departamento en edificio sin ascensor'; 6 = 'Pieza en casa antigua o en conventillo'; 7 = 'Vivienda tradicional indígena(Ruka, Pae Pae,etc)';
                                                                8 = 'Mediagua o mejora'; 9 = 'Rancho, Choza'; 10 = 'Vivienda precaria de materiales reutilizados'; 11 = 'Móvil';
                                                                12 = 'Otro tipo de vivienda particular'; -88 = 'NA'")))

# 6. Creación objeto
epf_personasmod <- epf_personasmod %>% 
  mutate_at(vars(ZONA, tipotenenciahog, tipovivienda), ~(as.factor(.))) %>% 
  mutate(ZONA = car::recode(.$ZONA, recodes = c("1 = 'Gran santiago'; 2 = 'Resto de capitales regionales'"))) %>% 
  mutate(tipotenenciahog = car::recode(.$tipotenenciahog, recodes = c("1 = 'Propia,totalmente pagada'; 2 = 'Propia, pagándose'; 3 = 'Arrendada(con contrato)';
                                              4 = 'Arrendada'; 5 = 'Cedida por trabajo o servicio'; 6 = 'Cedida por un pariente o amigo';
                                              7 = 'Ocupada de hecho'; 8 = 'Propiedad en litigio'; 9 = 'Herencia o sucesión compartida';
                                              10 = 'Arrendada(no se sabe si es con contrato o no)'; c(-99,-88,-77) = NA"))) %>% 
  mutate(tipovivienda = car::recode(.$tipovivienda, recodes = c("1 = 'Casa no pareada(independiente)'; 2 = 'Casa pareada por un lado'; 3 = 'Departamento en edificio con ascensor';
                                                                5 = 'Departamento en edificio sin ascensor'; 6 = 'Pieza en casa antigua o en conventillo'; 7 = 'Vivienda tradicional indígena(Ruka, Pae Pae,etc)';
                                                                8 = 'Mediagua o mejora'; 9 = 'Rancho, Choza'; 10 = 'Vivienda precaria de materiales reutilizados'; 11 = 'Móvil';
                                                                12 = 'Otro tipo de vivienda particular'; -88 = 'NA'")))
# 6.1 Recodificación variables para usar en modelos
epf_personasmod %>% 
  mutate_at(vars(tipotenenciahog, tipovivienda), ~(as.numeric(.)))


# 7. Creación objeto final
epf_personasmod <- epf_personasmod %>% 
  mutate_at(vars(tipotenenciahog, tipovivienda), ~(as.numeric(.)))

# 8. Exportar datos

saveRDS(epf_personasmod, file = "output/data/datos_proc.rds")