###Script #30DayChartChallenge
#Día 10
#Abstract

##Borrar datos del entorno
rm(list=ls())


#Directorio de trabajo
setwd("C:/Users/ALIENWARE/Documents/GitHub/30DayChartChallenge")

#Librerías
if(!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse, extrafont)


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

unzip(tf, files=fname, exdir=td, 
      overwrite=TRUE)

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
           clasificacion_final==3,
         #Filtrar defunciones
         fecha_def!="9999-99-99")%>%
  #Variables a utilizar
  select(sexo, fecha_def)%>%
  mutate(sexo=ifelse(sexo==1, "Mujer", "Hombre"))%>%
  mutate(fecha_def=as.Date(fecha_def))%>%
  group_by(fecha_def,sexo)%>%
  tally()






##Gráfico
covid%>%
ggplot(., aes(fecha_def, n)) +
  geom_jitter(aes(color=sexo), alpha=0.5, width = 0.9, 
              height = 0.9, size=1)+
  scale_x_date(date_breaks = "1 month", 
               date_labels = "%B\n%Y") +
  scale_color_manual("Género", 
                     values=c("#31a354", "#fdae6b"))+
  coord_flip()+
  theme_minimal()+
  theme(text=element_text("Century Gothic"))+
  labs(title="Defunciones confirmadas 
por COVID-19 
al 10 de abril 2021",
       subtitle=paste0(format(sum(covid$n), big.mark = ","), " defunciones"),
       y = NULL, 
       x = NULL,
       legend= NULL,
       caption="@claudiodanielpc con información 
de la Secretaría de Salud.")+
  theme(plot.title = element_text(face="bold", size=23),
        plot.subtitle = element_text(face="bold", size=20),
        plot.caption = element_text(hjust=0, size=15),
        panel.grid = element_blank(),
        #axis.text.x = element_blank(),
        plot.margin = unit(c(0.5,0.5,0.3,0.5), "cm"))

ggsave("day10.png", width = 5, 
       height = 5* (16/9)* (16/9),
       dpi = 300)