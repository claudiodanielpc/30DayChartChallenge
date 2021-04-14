###Script #30DayChartChallenge
#Día 14
#Space

##Borrar datos del entorno
rm(list=ls())


#Directorio de trabajo
setwd("C:/Users/ALIENWARE/Documents/GitHub/30DayChartChallenge")

#Librerías
if(!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse, extrafont)

#Los datos se descargan de Kaggle
#https://www.kaggle.com/arashnic/soalr-wind?select=solar_wind.csv

solar<-read.csv("C:/Users/ALIENWARE/Downloads/solar_wind.csv")


#Gráfico
solar%>%
ggplot(.,aes(x=speed, y=temperature, 
             size=density))+
geom_point()+
  theme_minimal()+
  labs(
    title = "Solar wind. Speed, temperature and density",
    caption = "
Fuente: @claudiodanielpc using Kaggle Datasets "
  )+
  theme(plot.title = element_text(hjust = 0, size=25,face="bold"),
        plot.subtitle = element_text(hjust = 0, size=20, face="italic"),
        plot.caption = element_text(hjust = 0,size=15),
        legend.position = "right",
        text=element_text("Consolas",
                          size=20))



ggsave("day14.png", height=10, width=20, units='in', dpi=300)
