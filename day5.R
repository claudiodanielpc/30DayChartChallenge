###Script #30DayChartChallenge
#Día 5
#Slope

##Borrar datos del entorno
rm(list=ls())
read.de

#Directorio de trabajo
setwd("C:/Users/ALIENWARE/Documents/GitHub/30DayChartChallenge")

#Librerías
if(!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse, extrafont, lubridate)



arg<-read.csv("https://infra.datos.gob.ar/catalog/sspm/dataset/175/distribution/175.1/download/tipos-de-cambio-historicos.csv")%>%
#Variables seleccionadas
    select("indice_tiempo", "dolar_referencia_com_3500")%>%
  #Quitar NAs
  drop_na()%>%
    ###Cambiar variable de caracter a formato de fecha  
  mutate(indice_tiempo=as.Date.character(indice_tiempo))


##Gráfica

arg%>%
  filter(indice_tiempo>=ymd("2003-05-25"))%>%
  #Agregar nombres de presidentes
  mutate(pres=ifelse(
    indice_tiempo>=ymd("2003-05-10") & indice_tiempo<=ymd("2007-12-10"),
    "Nestor Kirchner",
    ifelse(indice_tiempo>=ymd("2007-12-11") & indice_tiempo<=ymd("2015-12-09"),
           "Cristina Fernández de Kirchner",
    ifelse(
    indice_tiempo>=ymd("2015-12-10") & 
                       indice_tiempo<=ymd("2019-12-10"), 
                     "Mauricio Macri","Alberto Fernández"))))%>%
  ggplot(.,aes(indice_tiempo,dolar_referencia_com_3500, 
               colour=pres))+
  #Puntos
  geom_point(size=0.5)+
  #Colores
  scale_color_manual("Presidente",
                     values=c("#9ecae1", "#fdae6b",
                              "#bdbdbd",
                              "#31a354"))+
  ##Línea de regresión
  geom_smooth(method = "lm",
              size=1.5)+
  ##Formato del eje de la fecha
  scale_x_date("Fecha",expand = c(0, 0),
               date_labels ="%Y", date_breaks = "365 days")+
  labs(
    title = "Argentina. Tipo de cambio diario, 2003-2021",
    subtitle = "(pesos por dólar)",
    y= "Pesos por dólar",
    caption = "
Fuente: @claudiodanielpc con información de Datos Argentina: datos.gob.ar"
  )+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0, size=25,face="bold"),
        plot.subtitle = element_text(hjust = 0, size=20, face="italic"),
        plot.caption = element_text(hjust = 0,size=15),
        legend.position = "right",
        text=element_text("Tahoma",
                          size=20),
        axis.text.x=element_text(size=12))


##Salvar el gráfico
ggsave("day5.png",height = 10,width = 20, units="in",dpi=300)