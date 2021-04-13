###Script #30DayChartChallenge
#Día 12
#Strips

##Borrar datos del entorno
rm(list=ls())


#Directorio de trabajo
setwd("C:/Users/ALIENWARE/Documents/GitHub/30DayChartChallenge")

#Fuente: https://sig.cdmx.gob.mx/datos/


# Librerías ====
if(!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse, extrafont)

# Parámetros previos ====
url<-"https://sig.cdmx.gob.mx/documents"

# User defined functions ====
descarga_y_unzip <- function(index_archivo) {
  temp <- tempfile()
  download.file(glue::glue("{url}/{index_archivo}/download"),
                mode = "wb",
                destfile = temp)
  unzip(temp, exdir = "catastro") 
  unlink(temp) 
}


# Lectura y limpieza de datos ====
# Descarga masiva
walk(75:90,  ~ descarga_y_unzip(.x))
archivos <- list.files(path = "catastro", pattern = "sig_cdmx_")

# Parse del directorio
catastro <- purrr::map_dfr(
  archivos,
  ~ data.table::fread(glue::glue("catastro/{.x}"), na.strings = "*") %>%
    tibble() %>% 
    select(uso_construccion,
           anio_construccion,
           alcaldia_cumplimiento) %>%
    # Filtrar por sector
    filter(uso_construccion == "Habitacional"))

# Limpieza                    
catastro <- catastro %>% 
  mutate(anio_construccion = na_if(anio_construccion, 0))%>%
  # Eliminar observaciones con NAs y año de construcción menor a 1900
  filter(anio_construccion>=1900 )


#Gráfico
catastro%>%
  #Acentos a los nombres de alcaldías
  mutate(alcaldia_cumplimiento =
                  case_when(alcaldia_cumplimiento=="CUAUHTEMOC" ~
                              "CUAUHTÉMOC",  
                            alcaldia_cumplimiento=="ALVARO OBREGON" ~
                              "ÁLVARO OBREGÓN",
                            alcaldia_cumplimiento=="COYOACAN" ~
                              "COYOACÁN",
                            alcaldia_cumplimiento=="BENITO JUAREZ" ~
                              "BENITO JUÁREZ",
                            alcaldia_cumplimiento=="TLAHUAC" ~
                              "TLÁHUAC",
                            TRUE ~ alcaldia_cumplimiento))%>%

  ggplot(., aes(x=anio_construccion)) +
  geom_histogram(fill="#006d2c",
                 color="black",
    alpha=0.5,
                 position="identity" )+
  scale_y_continuous(labels=scales::comma)+ 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
  theme_bw()+
  labs(
    title ="Cuentas catastrales por alcaldía. Año de construcción o última remodelación",
    subtitle = "Uso habitacional",
    caption = "    
Fuente: @claudiodanielpc con datos del Gobierno de la Ciudad de México. 
Agencia Digital de Innovación Pública (ADIP). Sistema Abierto de Información Geográfica",
    x="Año de construcción o última remodelación",
    y="Número"
  )+
  theme(plot.title = element_text(hjust = 0, size=25,face="bold"),
        plot.subtitle = element_text(hjust = 0, size=15, face="italic"),
        plot.caption = element_text(hjust = 0,size=12),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        text=element_text("Dubai",size=20),
        strip.background = element_rect(
          color="black", fill="#edf8b1", 
          size=1.5, linetype="solid"
        ),
        strip.text = element_text(
          size = 20, color = "black", 
          face = "bold.italic"))+
  facet_wrap(~alcaldia_cumplimiento, scale = "free_y")


ggsave("day12.png", height=10, width=20, units='in', dpi=300)
