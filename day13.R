###Script #30DayChartChallenge
#Día 13
#Correlation

##Borrar datos del entorno
rm(list=ls())

#Directorio de trabajo
setwd("C:/Users/ALIENWARE/Documents/GitHub/30DayChartChallenge")

##Se cargan las librerías necesarias

if(!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse, extrafont)


#Lectura y limpieza====
censo2020<-read.csv("C:/Users/ALIENWARE/Documents/censo2020/iter_00_cpv2020/conjunto_de_datos/conjunto_de_datos_iter_00CSV20.csv",
                    encoding="UTF-8",
                    header=TRUE,
                    check.names=FALSE)%>%
  ##Nombres a minúsculas
  janitor::clean_names()%>%
  #Datos de entidad federativa
  filter(nom_loc=="Total del Municipio")%>%
  #Variables a ocupar
  select(nom_ent, nom_mun, pobtot,p12ym_solt,
         tvivparhab, vph_cvj)%>%
  mutate(solteros=as.numeric(p12ym_solt)/as.numeric(pobtot)*100,
         vj=as.numeric(vph_cvj)/as.numeric(tvivparhab)*100)


#Gráfico
censo2020%>%
  ggplot(.,aes(solteros, vj)) +
  geom_point(shape=16, alpha=.3, color="firebrick")+
  geom_smooth(method = "lm", se=F, color="#bdbdbd")+
  scale_y_continuous(name = "Porcentaje de viviendas particulares habitadas con consola de videojuegos",
                     limits = c(0,45))+
  scale_x_continuous("Porcentaje de población de 12 años y más soltera o nunca unida",
                     limits = c(0,45))+
  annotate(geom = "text", x = 26, y = 43, 
           label = paste0("Correlación: ",
                          round(cor(censo2020$solteros,censo2020$vj),4)),
           hjust = 0, vjust=1, fontface="bold", color="#d95f0e",
           size=10)+
  theme_minimal()+
  labs(
    title = "México. Población soltera y viviendas con consola de videojuegos
por municipio",
    caption = "
Fuente: @claudiodanielpc con datos de INEGI. Censo de Población y Vivienda 2020."
  )+
  theme(plot.title = element_text(hjust = 0, size=25,face="bold"),
        plot.subtitle = element_text(hjust = 0, size=20, face="italic"),
        plot.caption = element_text(hjust = 0,size=15),
        legend.position = "right",
        text=element_text("Corbel",
                          size=20))


##Salvar gráfico
ggsave("day13.png", width = 11, height = 11)