###Script #30DayChartChallenge
#Día 14
#Space

##Borrar datos del entorno
rm(list=ls())


#Directorio de trabajo
setwd("C:/Users/ALIENWARE/Documents/GitHub/30DayChartChallenge")

#Librerías
if(!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse, extrafont, viridis)

#Los datos se descargan de Kaggle
#https://www.kaggle.com/arashnic/soalr-wind?select=solar_wind.csv

solar<-read.csv("C:/Users/ALIENWARE/Downloads/solar_wind.csv")



#Gráfico
solar%>%
  #MUestra aleatoria
  #sample_n(100)%>%
ggplot(.,aes(x=speed, y=temperature, size=density,
             fill=density))+
ggrastr::rasterise(
  geom_point(alpha=0.5, shape=21, color="#bdbdbd"),
  dpi=72)+
  scale_size("Density\nSolar wind\nproton density (N/cm^3)")+
    scale_fill_viridis("Density\nSolar wind\nproton density (N/cm^3)",
                       option="viridis",
                       guide=F)+

    scale_y_continuous("Solar wind ion temperature (Kelvin)", 
                       labels = scales::comma)+
  theme_minimal()+
  labs(
    title = "Solar wind. Speed, temperature and density",
    #subtitle = "Sample size= 1 million observations",
    x="Solar wind bulk speed (km/s)",
    caption = "
Source: @claudiodanielpc using Kaggle Datasets "
  )+
  theme(plot.title = element_text(hjust = 0, size=25,face="bold"),
        plot.subtitle = element_text(hjust = 0, size=20, face="italic"),
        plot.caption = element_text(hjust = 0,size=15),
        legend.position = "right",
        legend.title = element_text(size=10),
        text=element_text("Consolas",
                          size=20))



ggsave("day14x.png", height=10, width=20, units='in', dpi=300)