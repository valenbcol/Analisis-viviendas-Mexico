rm(list=ls())

library(tidyverse)
library(ggplot2)
library(readxl)

setwd("C:/Documents/ME-UNLP/Curso Inicial/Instrumentos Computacionales/R/")

################################Punto 1#########################################

## Punto 1 ---- a)
# Cargo la base de datos en un dataframe, exploro el dataset y los nombres variables

viviendas <- read.csv("viviendas.csv", sep=",")
view(viviendas)
names(viviendas)

# Selecciono un grupo de variables 
viviendas_sub <- viviendas %>% 
                 select(1:renta, ubica_geo, factor)
names(viviendas_sub)

## Punto 1 ---- b)
# Descomprimo el archivo para individuos 

unzip("enigh2016_ns_poblacion_csv.zip", exdir = "C:/Documents/ME-UNLP/Curso Inicial/Instrumentos Computacionales/Examen final/R/data")

poblacion <- read.csv("poblacion.csv", sep=",")

poblacion_sub <- poblacion %>%
                 select(1:45)

## Punto 1 ---- c) Junto las bases de viviendas e individuos
enigh_all <- inner_join(x=viviendas_sub, y=poblacion_sub, by="folioviv")

# Verificar cantidad de filas y columnas


## Punto 1 ---- d)

# Creo una varable categórica de edad

enigh_all <- enigh_all %>%
             mutate(rango_edad = case_when(
               edad %in% (0:4) ~ "0-4 AÑOS",
               edad %in% (5:11) ~ "5-11 AÑOS",
               edad %in% (12:19) ~ "12-19 AÑOS",
               edad %in% (20:29) ~ "20-29 AÑOS",
               edad %in% (30:39) ~ "30-39 AÑOS",
               edad %in% (40:49) ~ "40-49 AÑOS",
               edad %in% (50:59) ~ "50-59 AÑOS",
               edad > 60 ~ "60 Y MÁS AÑOS",
               TRUE ~ NA_character_
             ))

# table(enigh_all$rango_edad[enigh_all$rango_edad==NA])

## Punto 1 ---- e)

# Creo una tabla con la cantidad de población en cada rango de edad por sexo

tabla_edad_sexo <- enigh_all %>%
                   count(rango_edad, sexo, wt = factor) %>%
                   pivot_wider(names_from = sexo, values_from = n)
            
print(tabla_edad_sexo)

## Punto 1 ---- f)

# Agrego total y nombres de columnas a la tabla

tabla_edad_sexo2 <- tabla_edad_sexo %>%
            rename(hombre = `1`, mujer = `2`) %>%
            mutate(total = hombre + mujer)

print(tabla_edad_sexo2)

## Punto 1 ---- g)
tabla_edad_prop <-  enigh_all %>%
              count(rango_edad, wt = factor) %>%
              rename(population = n) %>%
              mutate(total_population = sum(population)) %>%
              transmute(rango_edad, porcentaje = (population / total_population) * 100)

print(tabla_edad_prop)
  


## Punto 1 ---- h)  
  

tabla_sexo_prop <- tabla_edad_sexo2 %>%
                   mutate(totalh = sum(hombre, na.rm = TRUE),
                    totalm = sum(mujer, na.rm = TRUE)) %>%
                   group_by(rango_edad) %>%
                   mutate(prop_hombre = (sum(hombre, na.rm = TRUE) / totalh) * 100,
                    prop_mujer = (sum(mujer, na.rm = TRUE) / totalm) * 100) %>%
                   ungroup() %>%
                   select(rango_edad, prop_hombre, prop_mujer)

print(tabla_sexo_prop)


## Punto 1 ---- i)

sexo_prop_long <- tabla_sexo_prop %>%
                  pivot_longer(cols = c(prop_hombre, prop_mujer), 
                   names_to = "sexo", 
                   values_to = "proporcion") %>%
                  mutate(sexo = recode(sexo, 
                   prop_hombre = "Hombre", 
                   prop_mujer = "Mujer"))


## Grafico la estructura poblacional paara cada género 

plot_prop_sexo <- ggplot(sexo_prop_long, aes(x = "", y = proporcion, fill = rango_edad)) +
                  geom_bar(stat = "identity", width = 0.8) +
                  geom_text(aes(label = paste0(round(proporcion, 1), "%")),
                   position = position_stack(vjust = 0.5),
                   size = 3, color = "white", fontface = "bold") +
                  coord_polar("y", start = 0) +
                  facet_wrap(~ sexo) +
                  scale_fill_brewer(palette = "Dark2") + 
                  theme_void() +
                  theme(legend.position = "right",
                   plot.title = element_text(hjust = 0.5)) +
                  labs(title = "Estructura poblacional por género",
                   fill = "Rango etario")

plot_prop_sexo




################################Punto 2#########################################

## Punto 2 ---- j) Extraigo el primero o los primeros dos digitos de la variable ubica_geo 

enigh_all <- enigh_all %>% 
             mutate(entidad = ifelse(nchar(ubica_geo) == 8,
                          substr(ubica_geo, 1, 1),
                          substr(ubica_geo, 1, 2)))

# Verifico que hayan 32 valores que pueda tomar la nueva variable

table(enigh_all$entidad[nchar(enigh_all$ubica_geo) == 8])
table(enigh_all$entidad[nchar(enigh_all$ubica_geo) == 9])

## Punto 2 ---- k) Calculo para cada estado la media, mediana, sd, min y max de renta

alquileres <- enigh_all %>%
              filter(tenencia == 1) %>%
              group_by(entidad) %>%
              summarise(
              prom_renta = mean(renta, na.rm = TRUE),
              mediana_renta = median(renta, na.rm = TRUE),
              sd_renta = sd(renta, na.rm = TRUE),
              min_renta = min(renta, na.rm = TRUE),
              max_renta = max(renta, na.rm = TRUE))

# Verifico missings

print(sum(is.na(alquileres)))

## Punto 2 ---- l) Cargo una nueva tabla y la uno con la anterior usando el código de estado


regionestados <- read_excel("region_inc_estados.xlsx")

regionestados <- regionestados %>%
                 rename(entidad = id_estado) %>%
                 mutate(entidad = as.character(entidad))


alquileres_ext <- inner_join(x=alquileres, y=regionestados, by="entidad")

print(alquileres_ext)


## Punto 2 ---- m) 

plot_alquileres <- ggplot(alquileres_ext, aes(x = ing_mean, y = prom_renta, color = region, size = propiedad)) +
                   geom_point(alpha = 0.5) +
                   scale_size(range = c(1, 30)) +  
                   labs(
                   x = "Ingreso promedio",
                   y = "Alquiler promedio",
                   title = "Relacion entre ingreso promedio y alquiler promedio por region",
                   size = "Propiedad",
                   color = "Region") +
                   theme_minimal() +
                   theme(
                   legend.position = "right",
                   legend.title = element_text(size = 12),
                   legend.text = element_text(size = 10),
                   plot.title = element_text(size = 14, face = "bold"))
  

plot_alquileres




################################Punto 3#########################################

## Punto 3 ---- a) Elijo el estado de Queretaro y cargo los datos correspondientes

queretaro <- read_excel("resloc_22xls14.xls")

## Punto 3 ---- b)

queretaro_sub <- queretaro %>% 
  select(starts_with("ENT"), starts_with("MUN"), starts_with("LOC"), ALUMBCOB) %>% 
  rename_with(tolower)

## Punto 3 ---- c) Genero variable numérica para la cobertura de alumbrado público


queretaroalum <-queretaro_sub %>%
                mutate(alumnum = case_when(
                  is.na(alumbcob) ~ NA_real_,
                  alumbcob == "Toda la localidad" ~ 5,
                  alumbcob == "La mayor parte de la localidad" ~ 4,
                  alumbcob == "Aproximadamente la mitad de la localidad" ~ 3,
                  alumbcob == "Menos de la mitad de la localidad" ~ 2,
                  alumbcob == "No hay alumbrado público" ~ 1,
                  TRUE ~ NA_real_))
                

print(queretaroalum)


## Punto 3 ---- e) Creo loop para descargar los datos de los primeros 5 estados


for(j in seq(1:5)){
  url <- paste0("https://www.inegi.org.mx/contenidos/programas/ccpv/iter/zip/resloc2014/resloc_0", j, "xls14.xls")
  destfile <- paste0("C:/Documents/ME-UNLP/Curso Inicial/Instrumentos Computacionales/Examen final/R/data/base_estado_", j, ".xls")
  download.file(url, destfile = destfile, mode = "wb", quiet = FALSE)
  print(paste("Descargando archivo entidad:", j))
}


## Punto 3 ---- f)

# Creo una lista para almacenar los dataframes por estado que descargaré posteriormente

lista_estados <- list()

options(timeout=120)


# Creo loop para los primeros 9 estados, que también haga todas las transformaciones anteriores para cada uno 

for(j in seq(1:9)){
  url <- paste0("https://www.inegi.org.mx/contenidos/programas/ccpv/iter/zip/resloc2014/resloc_0", j, "xls14.xls")
  destfile <- paste0("C:/Documents/ME-UNLP/Curso Inicial/Instrumentos Computacionales/Examen final/R/data/base_estado_", j, ".xls")
  download.file(url, destfile = destfile, mode = "wb", quiet = FALSE, timeout = 250)
  print(paste("Descargando archivo entidad:", j))
  
  datos_estado <- read_excel(destfile) %>% 
    select(starts_with("ENT"), starts_with("MUN"), starts_with("LOC"), ALUMBCOB) %>% 
    rename_with(tolower) %>%
    mutate(alumnum = case_when(
      is.na(alumbcob) ~ NA_real_,
      alumbcob == "Toda la localidad" ~ 5,
      alumbcob == "La mayor parte de la localidad" ~ 4,
      alumbcob == "Aproximadamente la mitad de la localidad" ~ 3,
      alumbcob == "Menos de la mitad de la localidad" ~ 2,
      alumbcob == "No hay alumbrado público" ~ 1,
      TRUE ~ NA_real_
    ))
  assign(paste0("datos_estado_", j), datos_estado)
  
  lista_estados[[j]] <- datos_estado
  
}


## Punto 3 ---- g) 


# Genero un vector que contenga las bases descargadas en mi carpeta de destino
files <- list.files("C:/Documents/ME-UNLP/Curso Inicial/Instrumentos Computacionales/Examen final/R/data/", pattern = "*.xls", full.names = TRUE)

# Creo un dataframe vacío donde voy a appendear el df de cada estado
appended_states <- data.frame()


for (file in files) {
    datos_estado <- read_excel(file) %>% 
    select(starts_with("ENT"), starts_with("MUN"), starts_with("LOC"), ALUMBCOB) %>% 
    rename_with(tolower) %>%
    mutate(alumnum = case_when(
      is.na(alumbcob) ~ NA_real_,
      alumbcob == "Toda la localidad" ~ 5,
      alumbcob == "La mayor parte de la localidad" ~ 4,
      alumbcob == "Aproximadamente la mitad de la localidad" ~ 3,
      alumbcob == "Menos de la mitad de la localidad" ~ 2,
      alumbcob == "No hay alumbrado público" ~ 1,
      TRUE ~ NA_real_
    ))
  
  
  appended_states <- bind_rows(appended_states, datos_estado)
}


print(appended_states) 

## Punto 3 ---- h) 


# Genero un vector que contenga las bases descargadas en mi carpeta de destino
files <- list.files("C:/Documents/ME-UNLP/Curso Inicial/Instrumentos Computacionales/Examen final/R/data/", pattern = "*.xls", full.names = TRUE)

# Creo un dataframe vacío donde voy a appendear el df de cada estado
appended_states <- data.frame()


for (file in files) {
  datos_estado <- read_excel(file) %>% 
    select(starts_with("ENT"), starts_with("MUN"), starts_with("LOC"), ALUMBCOB) %>% 
    rename_with(tolower) %>%
    mutate(alumnum = case_when(
      is.na(alumbcob) ~ NA_real_,
      alumbcob == "Toda la localidad" ~ 5,
      alumbcob == "La mayor parte de la localidad" ~ 4,
      alumbcob == "Aproximadamente la mitad de la localidad" ~ 3,
      alumbcob == "Menos de la mitad de la localidad" ~ 2,
      alumbcob == "No hay alumbrado público" ~ 1,
      TRUE ~ NA_real_ )) %>%
     group_by(ent_nom, mun_nom)%>%
     summarise(mean_alumb=mean(alumbcob, na.rm = TRUE), .groups = 'drop')%>%
     select(mean_alumb, matches("^(ent|mun)") )
  
  
  appended_states <- bind_rows(appended_states, datos_estado)
}


print(appended_states) 

# El dataframe del último apartado tiene menos filas porque esta desagregado
# hasta el nivel municipio, mientras que el del apartado anterior hasta localidad. 
