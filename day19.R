###Script #30DayChartChallenge
#Día 19
#Global change

##Borrar datos del entorno
rm(list=ls())


#Directorio de trabajo
setwd("C:/Users/ALIENWARE/Documents/GitHub/30DayChartChallenge")

#Librerías
if(!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse, extrafont, ggthemes)


#Datos Banco Mundial====
url<-"https://api.worldbank.org/v2/en/indicator/SP.DYN.LE00.IN?downloadformat=csv"

##Creación de directorio temporal

td<- tempdir()

# Descarga del archivo temporal


tf = tempfile(tmpdir=td, fileext=".zip")

download.file(url, destfile=tf, mode = "wb")

# Obtener el nombre del primer archivo

fname = unzip(tf, list=TRUE)$Name[2]

# unzip

unzip(tf, files=fname, exdir=td, 
       overwrite=TRUE)

# dirección del archivo
fpath = file.path(td, fname)
unlink(td)



##Lectura y limpieza de datos====
lifeexp<- read.csv(fpath, header = FALSE)%>%
  #Eliminar primeras dos filas
  slice(-(1:2))%>%
  ##Primera fila a encabezado
  janitor::row_to_names(row_number = 1)%>%
  janitor::clean_names()%>%
  #Filtro de datos a nivel mundial
  filter(country_name=="World")%>%
  ##Variables seleccionadas
  select(country_name, x1960:x2020)%>%
  pivot_longer(cols = !country_name,
               names_to="year",
               values_to="lifeexp")%>%
  drop_na()%>%
  #Quitar x a años y transformar a numérico
  mutate(year=as.numeric(str_remove(year,"x")))


##Gráfico====

lifeexp%>%
  ggplot(.,aes(year,lifeexp))+
  geom_line(size=2, color="#c51b8a")+
  theme_economist(base_family = "sans")+
  #Formato de número del eje y
  scale_y_continuous("Years",labels = scales::comma)+
  scale_x_continuous("Year",
    breaks = seq(from = 1960, to = 2018, by = 2)
  )+
  labs(
    title = "World. Life expectancy at birth, 1960-2018",
    subtitle="Total (years)",
    caption = "
Source: @claudiodanielpc using data from World Bank."
  )+
  theme(plot.title = element_text(hjust = 0, size=25,face="bold"),
        plot.subtitle = element_text(hjust = 0, size=20, face="italic"),
        plot.caption = element_text(hjust = 0,size=15),
        legend.position = "none",
        axis.text.x=element_text(size=12, angle=90))


        #Guardar gráfico
ggsave("day19.png", height=10, 
       width=20, units='in', dpi=300)
