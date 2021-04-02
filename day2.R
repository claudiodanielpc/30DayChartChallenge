###Script #30DayChartChallenge
#Día 2
#Pictogram

##Borrar datos del entorno
rm(list=ls())

#Directorio de trabajo
setwd("C:/Users/ALIENWARE/Documents/GitHub/30DayChartChallenge")

##Se cargan las librerías necesarias

if(!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse, extrafont, ggimage)

#Imagen para gráfico====
imagen="https://eurokol.hu/wp-content/uploads/2018/03/800px_COLOURBOX20208210.jpg"



#Lectura y limpieza====
censo2020<-read.csv("C:/Users/ALIENWARE/Documents/censo2020/iter_00_cpv2020/conjunto_de_datos/conjunto_de_datos_iter_00CSV20.csv",
                    encoding="UTF-8",
                    header=TRUE,
                    check.names=FALSE)%>%
  ##Nombres a minúsculas
  janitor::clean_names()%>%
  #Datos de entidad federativa
  filter(nom_loc=="Total de la Entidad")%>%
  #Variables a ocupar
  select(nom_ent,tvivparhab,vivpar_des)%>%
  #Obtener porcentaje
  mutate(pct=as.numeric(vivpar_des)/as.numeric(tvivparhab)*100)%>%
  select(nom_ent,pct)



#Gráfico
censo2020%>%
  arrange(pct)%>%
  mutate(nom_ent=factor(nom_ent,levels = nom_ent))%>%
  ggplot(.,aes(nom_ent, pct)) +
  #Imagen de casa
  geom_image(aes(image=imagen), size=0.03)+
  #Línea para conectar la casa
  geom_segment(aes(y=0,
                   x=nom_ent,
                   yend=pct-.1,
                   xend=nom_ent),
               color="black")+
  coord_flip()+
  #Modificar límites de eje y
  scale_y_continuous(limits=c(0, 30))+
  #Etiquetas de valor
    geom_text(aes(label=format(round(pct,1))), vjust=0.5
            ,hjust=-1, color="#35978f",
            size=5,fontface="bold")+
theme_minimal()+
  labs(
    title = "México. Viviendas particulares deshabitadas por cada 100 particulares habitadas",
    y = "Viviendas particulares deshabitadas por cada 100 particulares habitadas",
    x="Entidad federativa",
    caption = "
Fuente: @claudiodanielpc con datos de INEGI. Censo de Población y Vivienda 2020."
  )+
  theme(plot.title = element_text(hjust = 0, size=25,face="bold"),
        plot.subtitle = element_text(hjust = 0, size=20, face="italic"),
        plot.caption = element_text(hjust = 0,size=15),
        legend.position = "none",
        text=element_text("Tahoma",
                          size=20),
        axis.text.x=element_text(size=12))



##Salvar gráfico
ggsave("day2.png",height=10,width=20)

  