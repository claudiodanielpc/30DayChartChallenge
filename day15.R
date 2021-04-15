###Script #30DayChartChallenge
#Día 13
#Correlation

##Borrar datos del entorno
rm(list=ls())

#Directorio de trabajo
setwd("C:/Users/ALIENWARE/Documents/GitHub/30DayChartChallenge")

##Se cargan las librerías necesarias

if(!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse, extrafont, viridis)


#Lectura y limpieza====
#datos de créditos INFONAVIT
cred<-read.csv("https://raw.githubusercontent.com/claudiodanielpc/vivienda/infonavit/cred_infonavit_zmvm.csv")%>%
  janitor::clean_names()%>%
  select(!mun)%>%
  pivot_longer(!cvegeo,
               names_to = "year", values_to = "cred")%>%
  group_by(cvegeo)%>%
  summarise(cred=sum(cred))%>%
  mutate(cvegeo=ifelse(nchar(cvegeo)==4,
                       paste0("0",cvegeo),
                       cvegeo))




#Datos del censo

#Lista de municipios de Zona Metropolitana del Valle de México (ZMVM) 
#para filtrar 

lista<-c("09002" ,  "09003" ,
         "09004" , "09005" ,
         "09006" , "09007" ,
         "09008" , "09009" ,
         "09010" , "09011" ,
         "09012" , "09013" ,
         "09014" , "09015" ,
         "09016" , "09017" ,
         "13069" , "15002" ,
         "15092" , "15108" ,
         "15091" , "15099" ,
         "15025" , "15016" ,
         "15069" , "15057" ,
         "15060" , "15015" ,
         "15029" , "15095" ,
         "15112" , "15046" ,
         "15030" , "15044" ,
         "15036" , "15053" ,
         "15100" , "15038" ,
         "15024" , "15121" ,
         "15084" , "15010" ,
         "15122" , "15094" ,
         "15050" , "15011" ,
         "15020" , "15059" ,
         "15023" , "15028" ,
         "15033" , "15017" ,
         "15093" , "15083" ,
         "15031" , "15070" ,
         "15096" , "15109" ,
         "15125" , "15061" ,
         "15058" , "15022" ,
         "15068" , "15081" ,
         "15089" , "15037" ,
         "15013" , "15065" ,
         "15120" , "15039" ,
         "15104" , "15075" ,
         "15035" , "15103" ,
         "15009" , "15034")
         
         

censo2020<-read.csv("C:/Users/ALIENWARE/Documents/censo2020/iter_00_cpv2020/conjunto_de_datos/conjunto_de_datos_iter_00CSV20.csv",
                    encoding="UTF-8",
                    header=TRUE,
                    check.names=FALSE)%>%
  ##Nombres a minúsculas
  janitor::clean_names()%>%
  #Datos de entidad federativa
  filter(nom_loc=="Total del Municipio")%>%
  #Variables a ocupar
  select(entidad, mun, nom_mun,pobtot,
         tvivparhab, vivpar_des)%>%

    #Crear clave geo para filtrar
  ##Agregar cero a entidad
  mutate(entidad=ifelse(nchar(entidad)==1,
                    paste0("0",entidad),
                    entidad))%>%
  ##Agregar ceros a municipio
  mutate(mun=ifelse(nchar(mun)==1,
                    paste0("00",mun),
                    ifelse(nchar(mun)==2,
                           paste0("0",mun),
                           mun)))%>%
  ##Crear clave geoestadística  
  mutate(cvegeo=paste0(entidad,mun))%>%
#Filtrar municipios de la ZMVM
filter(cvegeo %in% lista)%>%
  #Transformar a numérico excepto cvegeo y nom_mun
  mutate_at(vars(-cvegeo, -nom_mun), 
            as.numeric)%>%
  left_join(cred)


#Gráfico====
censo2020%>%
ggplot(.,aes(x=cred, y=vivpar_des, size=pobtot,
             fill=pobtot)) +
  geom_point(shape=16, alpha=.5, color="#2c7fb8")+
  geom_text( 
    data=censo2020 %>% filter(vivpar_des>24000 & cred>20000), # Filter data first
    aes(label=nom_mun),
    check_overlap = T
  )+
  scale_size("Población total",
             labels = scales::comma)+
  scale_y_continuous(name = "Créditos INFONAVIT para adquisición de vivienda nueva, 2010-2019",
                     limits = c(0,60000), 
                     labels = scales::comma)+
  scale_x_continuous("Viviendas particulares deshabitadas (Censo 2020)",
                     limits = c(0,60000),
                     labels=scales::comma)+
  scale_fill_continuous("Población total",
                    guide=F)+
  
  theme_minimal()+
  annotate(geom = "text", x = 25000, y = 50000, 
           label = paste0("Correlación: ",
                          round(cor(censo2020$vivpar_des,
                                    censo2020$cred,
                                    use="na.or.complete"),4)),
           hjust = 0, vjust=1, fontface="bold", color="#d95f0e",
           size=10)+
labs(
  title = "Zona Metropolitana del Valle de México
Viviendas particulares deshabitadas y créditos INFONAVIT 
para adquisición de vivienda nueva
  ",
  caption = "
Fuente: @claudiodanielpc con datos de INEGI. Censo de Población y Vivienda 2020 y
CONAVI. Sistema Nacional de Información e Indicadores de Viviedna (SNIIV)"
)+
  theme(plot.title = element_text(hjust = 0, size=25,face="bold"),
        plot.subtitle = element_text(hjust = 0, size=20, face="italic"),
        plot.caption = element_text(hjust = 0,size=15),
        legend.position = "right",
        text=element_text("Montserrat"),
        axis.text = element_text(size=10),
        axis.title = element_text(size=15))


ggsave("day15.png", height=10, width=20, units='in', dpi=300)



