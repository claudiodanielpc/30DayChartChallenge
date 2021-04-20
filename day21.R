###Script #30DayChartChallenge
#Día 21
#Downwards

##Borrar datos del entorno
rm(list=ls())


#Directorio de trabajo
setwd("C:/Users/ALIENWARE/Documents/GitHub/30DayChartChallenge")


# Librerías ====
if(!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse, extrafont, ggthemes)

#Url de lectura de datos
url<-"http://www.conapo.gob.mx/work/models/CONAPO/Datos_Abiertos/Proyecciones2018/pob_mit_proyecciones.csv"


#Lectura y limpieza de datos====
pob<-read.csv(url)%>%
  janitor::clean_names()%>%
  filter(entidad=="Ciudad de México")%>%
  rename(year=2)%>%
group_by(year, sexo)%>%
  summarise(poblacion=sum(poblacion))


#Gráfico====
pob%>%
  mutate(poblacion=poblacion/1000000)%>%
  ggplot(.,aes(year, poblacion))+
           geom_line(aes(color=sexo), size=2)+
  theme_stata()+
  scale_colour_stata("Género", scheme = "s2color")+
  scale_y_continuous("Población (millones de personas)",
                     labels = scales::comma)+
  scale_x_continuous("Año",
                     breaks = seq(from = 1970, 
                                  to = 2050, by = 5)
  )+
  labs(
    title = "Ciudad de México. Población por género, 1970-2050",
    subtitle="Millones de personas",
    caption = "
Fuente: @claudiodanielpc con datos del Consejo Nacional de Población (CONAPO).
Proyecciones de la Población de México y de las Entidades Federativas."
  )+
  theme(plot.title = element_text(hjust = 0, size=30,face="bold"),
        plot.subtitle = element_text(hjust = 0, size=25, face="italic"),
        plot.caption = element_text(hjust = 0,size=20),
        axis.ticks = element_blank(),
        legend.text = element_text(size=15),
        legend.title = element_text(size=15),
        axis.title = element_text(size=15),
        axis.text.x=element_text(size=15, angle=90, 
                                 hjust=0.5,vjust=0.5),
        axis.text.y = element_text(size=15, angle = 0,
                                   hjust=0.5,vjust=0.5))

##salvar gráfico

ggsave("day21.png", height=10, 
       width=20, units='in', dpi=300)