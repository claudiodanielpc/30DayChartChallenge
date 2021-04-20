###Script #30DayChartChallenge
#Día 18
#Connections

##Borrar datos del entorno
rm(list=ls())


#Directorio de trabajo
setwd("C:/Users/ALIENWARE/Documents/GitHub/30DayChartChallenge")

#Librerías
if(!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse, extrafont, rtweet,
               geomnet)

##Datos de Twitter====
timeline<-get_timeline("claudiodanielpc", n=4000)

#Limpieza de datos de Twitter====
datos<-timeline%>%
  select(screen_name,reply_to_screen_name)%>%
  drop_na()%>%
  filter(reply_to_screen_name!="claudiodanielpc")%>%
  group_by(screen_name,reply_to_screen_name)%>%
  summarise(cuenta=n())%>%
  filter(cuenta>=5)




ggplot(data = datos
       ) +
  geom_net(layout.alg = "fruchtermanreingold",
    aes(fontsize = cuenta/5,
        from_id = reply_to_screen_name, 
        to_id = screen_name),
    curvature=0.1,    
    ealpha = 0.5,  
     labelon = TRUE,
    labelcolour="#253494",
           ecolour = "#d9d9d9", arrowsize = 0.5,
           linewidth = 2) +
  theme_void(base_family = "Consolas") +
labs(title="Cuentas con las que tengo mayor interacción en Twitter",
     subtitle="Cuentas con las que tengo 5 o más réplicas en los últimos 4,000 tuits",
     caption="
Fuente: @claudiodanielpc usando Rtweet")+
  theme(plot.title = element_text(hjust = 0, size=35,face="bold"),
        plot.subtitle = element_text(hjust = 0, size=30, face="italic"),
        plot.caption = element_text(hjust = 0,size=25),
        plot.background=element_rect(fill = "#99d8c9"))


#Guardar gráfico
ggsave("day18.png", height=10, 
       width=22, units='in', dpi=300)