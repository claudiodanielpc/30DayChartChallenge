###Script #30DayChartChallenge
#Día 3
#Historical

##Borrar datos del entorno
rm(list=ls())


#Directorio de trabajo
setwd("C:/Users/ALIENWARE/Documents/GitHub/30DayChartChallenge")

#Librerías
if(!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse, extrafont)

#Datos
movi = read.csv("https://datos.cdmx.gob.mx/datastore/dump/5d33f9c7-e033-4676-a02d-9e2129017acf?bom=True", 
                   header=TRUE, row.names=NULL, 
                   stringsAsFactors=FALSE)%>%
  #Renombrar nombre de variables a minúsculas
  janitor::clean_names()%>%
  ##Filtrar Metro CDMX
  filter(organismo=="STC")%>%
#Renombrar variable de afluencia
    rename(afluencia=afluencia_total_preliminar)%>%
###Cambiar variable de caracter a formato de fecha  
  mutate(fecha=as.Date.character(fecha))%>%
  #Ordenar por fecha
  arrange(fecha)%>%
  ##Ordenar las líneas de metro
  mutate(linea_servicio = factor(linea_servicio, levels=rev(c("L1", "L2", 
                                            "L3", "L4", "L5", "L6", "L7", "L8",
                                            "L9","LA","LB","L12"))))



##Heatmap
ggplot(movi, 
       aes(x = fecha, y = linea_servicio, fill = afluencia)) + 
  geom_tile()+
  #Jornada Nacional de Sana Distancia
  geom_vline(xintercept = ymd("2020-03-23"), linetype="dashed", 
             color = "black", size=1.5)+
  
  geom_vline(xintercept = ymd("2020-05-30"), linetype="dashed", 
             color = "black", size=1.5)+
  annotate(geom = "text", x = ymd("2020-03-01"), y = 13.5, 
           label = "Movilidad previa\na COVID-19",
           hjust = 0, vjust=1, fontface="bold", color="#31a354",
           size=4)+
  
  annotate(geom = "text", x = ymd("2020-03-27"), y = 13.5, 
           label = "Jornada Nacional de Sana Distancia",
           hjust = 0, vjust=1, fontface="bold", color="#636363",
           size=6)+
  ##Semáforo epidemiológico de CDMX pasa de rojo a naranja
  geom_vline(xintercept = ymd("2020-06-29"), linetype="dashed", 
             color = "black", size=1.5)+
  annotate(geom = "text", x = ymd("2020-06-03"), y = 13.5, 
           label = "Semáforo rojo",
           hjust = 0, vjust=1, fontface="bold", color="red",
           size=6)+
  ##Semáforo epidemiológico de CDMX pasa de naranja a rojo
  geom_vline(xintercept = ymd("2020-12-21"), linetype="dashed", 
             color = "black", size=1.5)+
  annotate(geom = "text", x = ymd("2020-09-01"), y = 13.5, 
           label = "Semáforo naranja",
           hjust = 0, vjust=1, fontface="bold", color="#d95f0e",
           size=6)+
  annotate(geom = "text", x = ymd("2021-01-05"), y = 13.5, 
           label = "Semáforo rojo",
           hjust = 0, vjust=1, fontface="bold", color="red",
           size=6)+
  ##Semáforo epidemiológico de CDMX pasa de rojo a naranja
  geom_vline(xintercept = ymd("2021-02-15"), linetype="dashed", 
             color = "black", size=1.5)+
  annotate(geom = "text", x = ymd("2021-02-24"), y = 13.5, 
           label = "Semáforo naranja",
           hjust = 0, vjust=1, fontface="bold", color="#d95f0e",
           size=6)+
  theme_minimal()+
  ##Títulos, subtítulos y fuente
  labs(title = "Afluencia diaria de pasajeros en el Sistema de Transporte Colectivo (Metro)",
       subtitle = "(Cifras preliminares)",
       y = "Líneas",
       caption = "Notas: 
La Jornada Nacional de Sana Distancia fue el nombre que utilizó el Gobierno de México para definir el confinamiento.
Por otra parte, El semáforo epidemiológico es un sistema de monitoreo para la regulación del uso del espacio público de acuerdo con el riesgo de contagio de COVID-19.
Fuente: @claudiodanielpc con datos de la Agencia Digital de Innovación Pública de la Ciudad de México (ADIP) y de la Secretaría de Salud del Gobierno de México.",
    fill = "Afluencia \n(personas)")+
  ##Formato del eje de la fecha
  scale_x_date("Fecha",expand = c(0, 0),
               date_labels = "%b %d %Y", date_breaks = "30 days")+
  ##Colores del heatmap
  scale_fill_distiller(palette = "YlOrBr",direction=1,labels=comma)+
  ##Temita
  theme(legend.position="right",legend.direction="vertical",
        legend.title=element_text(colour="black"),
        legend.margin=margin(grid::unit(0,"cm")),
        legend.title.align = 0.5,
        legend.text=element_text(colour="black",size=14,face="bold"),
        legend.key.height=grid::unit(0.8,"cm"),
        legend.key.width=grid::unit(0.2,"cm"),
        axis.text.x=element_text(colour="black", size=13,angle=90),
        axis.text.y=element_text(colour="black",size=13),
        axis.ticks=element_blank(),
        plot.background=element_blank(),
        panel.border=element_blank(),
        plot.margin=margin(0.7,0.4,0.1,0.2,"cm"),
        strip.text.x = element_text(size=20, color="black",
                                    face="bold"),
        plot.title=element_text(colour="black",hjust=0,size=25,face="bold"),
        plot.subtitle = element_text(hjust = 0, size=20, face="italic"),
        plot.caption = element_text(hjust = 0,size=15),
        text=element_text("Century Gothic",size=20))


##Salvar el gráfico
ggsave("day3.png",height = 10,width = 30, units="in",dpi=300)