###Script #30DayChartChallenge
#Hombres y mujeres en México


##Borrar datos del entorno
rm(list=ls())

#Directorio de trabajo
setwd("C:/Users/ALIENWARE/Documents/GitHub/30DayChartChallenge")

##Se cargan las librerías necesarias

if(!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse, extrafont)


##Lectura y limpieza de datos del Censo 2020====

censo2020<-read.csv("C:/Users/ALIENWARE/Documents/censo2020/iter_00_cpv2020/conjunto_de_datos/conjunto_de_datos_iter_00CSV20.csv",
                    encoding="UTF-8",
                    header=TRUE,
                    check.names=FALSE)%>%
  janitor::clean_names()%>%
  filter(nom_loc=="Total de la Entidad")%>%
  select(nom_ent, pobtot, pobfem,pobmas)%>%
  mutate(Mujeres=as.numeric(pobfem)/pobtot*100,
         Hombres=as.numeric(pobmas)/pobtot*100)%>%
  select(nom_ent,Mujeres, Hombres)%>%
  gather(., sex,value, -nom_ent)



#Gráfico====
censo2020%>%
  ggplot(., aes(fct_rev(nom_ent), value, fill = sex)) +
  geom_bar(stat = "identity") +
  coord_flip()+
  scale_fill_manual("Género",values=c("#fdae61","#35978f"))+
  #Línea a la mitad
  geom_hline(yintercept = 50, size=2, linetype="solid")+
  theme_light()+
  labs(
    title = "México. Hombres y mujeres por entidad federativa",
    subtitle = "(%)",
    y = "%",
    x="Entidad federativa",
    caption = "
Fuente: @claudiodanielpc con datos de INEGI. Censo de Población y Vivienda 2020."
  )+
  theme(plot.title = element_text(hjust = 0, size=25,face="bold"),
        plot.subtitle = element_text(hjust = 0, size=20, face="italic"),
        plot.caption = element_text(hjust = 0,size=15),
        legend.position = "bottom",
        axis.text.x = element_text(vjust=0.5),
        text=element_text("Tahoma",
                          size=20))

##Salvar gráfico
ggsave("day1.png",height=10,width=20)

