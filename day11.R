###Script #30DayChartChallenge
#Día 11
#Circular

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
  group_by(year)%>%
  mutate(pct=captura_toneladas/sum(captura_toneladas)*100,
         acum=cumsum(pct)-0.5*pct,
         etiqueta=ifelse(litoral=="Litoral del Pacifico" | 
                           litoral== "Litoral del Golfo y Mar Caribe",
                         format(round(pct,1))," "))%>%
  ggplot(., aes(x="", y=pct, fill=litoral))+
  geom_bar(stat="identity", width=1, color="white") +
  geom_text(aes(y = acum, label = etiqueta),
            color = "white",fontface="bold",
            size=3)+
  coord_polar("y", start=0) +
  theme_void() + 
  facet_wrap(~year, ncol=3)+
  scale_fill_brewer("Litoral", 
                    palette="Set1")+
  
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
        legend.position = "bottom",
        text=element_text("Consolas",
                          size=20),
        
        strip.background = element_rect(
          color="black", fill="#9ecae1", 
          size=1.5, linetype="solid"
        ),
        strip.text = element_text(
          size = 20, color = "black", 
          face = "bold.italic")
  )


##Salvar gráfico
ggsave("day11.png", width = 20, height = 15)