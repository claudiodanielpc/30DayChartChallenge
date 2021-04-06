###Script #30DayChartChallenge
#Día 6
#Experimental

##Borrar datos del entorno
rm(list=ls())


#Directorio de trabajo
setwd("C:/Users/ALIENWARE/Documents/GitHub/30DayChartChallenge")

#Librerías
if(!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse, extrafont, lubridate, geofacet)


#Datos====

##Lectura y limpieza de datos del Censo 2020 para obtener datos de población====

censo2020<-read.csv("C:/Users/ALIENWARE/Documents/censo2020/iter_00_cpv2020/conjunto_de_datos/conjunto_de_datos_iter_00CSV20.csv",
                    encoding="UTF-8",
                    header=TRUE,
                    check.names=FALSE)%>%
  janitor::clean_names()%>%
  filter(nom_loc=="Total de la Entidad")%>%
  select(entidad,nom_ent, pobtot)%>%
  rename(code=entidad)


#Datos COVID19 México====
url<-"http://datosabiertos.salud.gob.mx/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip"

##Creación de directorio temporal

td<- tempdir()

# Descarga del archivo temporal


tf = tempfile(tmpdir=td, fileext=".zip")

download.file(url, tf)

# Obtener el nombre del primer archivo

fname = unzip(tf, list=TRUE)$Name[1]

# unzip

unzip(tf, files=fname, exdir=td, overwrite=TRUE)

# dirección del archivo
fpath = file.path(td, fname)
unlink(td)

covid = read.csv(fpath, header=TRUE, row.names=NULL, 
                 stringsAsFactors=FALSE)


##Trabajar la base de datos
##Base general
covid<-covid%>%
  #Todo a minúsculas
  janitor::clean_names()%>%
  #Dejar casos confirmados
  filter(clasificacion_final==1 | clasificacion_final==2 |
           clasificacion_final==3)%>%
  ##Se crea variable de clave de entidad
  mutate(code=entidad_res)%>%
  left_join(censo2020)


###Gráfica----
covid%>%
  ##Se cambia a formato fecha 
  mutate(fecha_sintomas=as.Date(fecha_sintomas, 
                                format="%Y-%m-%d"))%>%
  group_by(fecha_sintomas, code, pobtot)%>%
  #Colapsar
  tally()%>%
  ungroup()%>%
  #Obtener media móvil y casos por cada 100 mil habitantes
  mutate(ma=zoo::rollmean(n,7, fill=NA),
    rate=ma/pobtot*100000)%>%
  ##Generar variable de fecha de hoy
  mutate(hoy=Sys.Date())%>%
  ##Generar diferencia para identificar activos
  mutate(difecha=hoy-fecha_sintomas)%>%
  ##Identificar activos e inactivos
  mutate(difecha=ifelse(difecha<14,"Activo","Inactivo"))%>%
  ggplot(.,aes(fecha_sintomas,rate, fill=difecha))+
  geom_bar(stat="identity",width=.8)+
  scale_x_date("Fecha de inicio de síntomas")+
  scale_fill_manual("Estatus", values = c("#cb181d","#bdbdbd"),
                    labels=c("Casos activos",
                             "Casos inactivos"))+
  #Forma de mapa
  facet_geo(~ code, 
            grid = mx_state_grid2, 
            label = "name",
            scales = "free_y")+
theme_minimal() +
  labs(
    title = "México. Casos confirmados de COVID-19 por 100 mil habitantes por entidad federativa",
    subtitle = "27 de febrero de 2020 al 5 de abril de 2021
(Promedio móvil 7 días)",
    caption = "
Fuente: @claudiodanielpc con datos de la Secretaría de Salud.",
  y="Casos confirmados por 100,000 habitantes")+
  
  
  theme(plot.title = element_text(hjust = 0,
                                  size=25,face="bold"),
        plot.subtitle = element_text(hjust = 0, 
                                     size=20, face="italic"),
        plot.caption = element_text(hjust = 0,size=15),
    axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
    strip.text = element_text(size = 9),
    #Fuente del gráfico
        text=element_text("Consolas"))

##Salvar gráfico
ggsave("day6.png", width = 16.5, height = 11.5)