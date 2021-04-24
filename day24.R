###Script #30DayChartChallenge
#Día 24
#Monochrome 

##Borrar datos del entorno
rm(list=ls())


#Directorio de trabajo
setwd("C:/Users/ALIENWARE/Documents/GitHub/30DayChartChallenge")

#Librerías
if(!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse, extrafont, viridis)

#URL de los datos
url<-"http://dsiappsdev.semarnat.gob.mx/datos/indicadores/dambiental/Biodiversidad/Oce%C3%A1nicos/captura%20pesquera%20nacional.csv"

#Lectura y limpieza de datos====
pesca<-read.csv(url, encoding = "latin1")%>%
  janitor::clean_names()%>%
  rename(year=1)



#Gráfico====

pesca%>%
  mutate(captura_toneladas=
           captura_toneladas/1000000)%>%
  ggplot(., aes(x=litoral, y=captura_toneladas, fill=litoral)) +
  
  geom_histogram(
                 position="identity", stat="sum")+
  scale_fill_grey()+
  coord_flip()+
  facet_wrap(~year, ncol=3)+
  geom_text(aes(label=format(round(captura_toneladas,2))),
            hjust=0,
            color="black",
            size=6.5,fontface="bold")+
  scale_y_continuous("Millones de toneladas",
                     limits =c(0,3),
                     breaks=c(0,3))+
  
  theme_grey()+
  labs(title = "Captura pesquera nacional",
       subtitle = "1991-2017",
       x="Litoral",
       caption = "
Fuente: @claudiodanielpc con datos de la Secretaría de Medio Ambiente y Recursos Naturales (SEMARNAT).")+
  theme(plot.title = element_text(hjust = 0, 
                                  size=30,
                                  face="bold"),
        plot.subtitle = element_text(hjust = 0,
                                     size=25, 
                                     face="italic"),
        plot.caption = element_text(hjust = 0,
                                    size=18),
        legend.position = "none",
        text=element_text("Consolas",
                          size=20),
        axis.text.x=element_text(size=15),
        strip.background = element_rect(
          color="black", fill="transparent", 
          size=1.5, linetype="solid"
        ),
        strip.text = element_text(
          size = 20, color = "black", 
          face = "bold.italic")
        )


##Salvar gráfico
ggsave("day24.png", width = 20, height = 15)