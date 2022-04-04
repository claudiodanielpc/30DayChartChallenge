###Script #30DayChartChallenge
#Día 4
#Flora

##Borrar datos del entorno
rm(list=ls())

#Directorio de trabajo
setwd("D:/Documentos/GitHub/30DayChartChallenge/2022")
#creamos carpeta en donde almacenaremos los archivos
dir.create("day_4", showWarnings = F)

##Se cargan las librerías necesarias

if(!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse, extrafont)

#Descarga de archivo del SIAP====
url<-"http://infosiap.siap.gob.mx/gobmx/datosAbiertos/ProduccionAgricola/Cierre_agricola_mun_2020.csv"
rosas<-read.csv(url)%>%
  janitor::clean_names()%>%
  filter(nomcultivo_sin_um=="Rosa")%>%
  group_by(nomestado,nommunicipio)%>%
  summarise(cosechada=sum(cosechada))%>%
  ungroup()
  
  #Gráfico====
  graf<-rosas%>%
    mutate(nommunicipio = fct_reorder(nommunicipio, cosechada))%>% 
    ggplot(.,aes(nommunicipio,cosechada))+
    geom_bar(stat="identity",width = 0.5)+
    geom_text(aes(x=nommunicipio, y=cosechada, label=cosechada, 
                  hjust=1), 
              position = position_dodge(width=1), color="white", 
              face="bold", size=3.5)+
    coord_flip()+
    labs(title="México. Superficie cosechada de rosas por entidad y municipio, 2020",
         subtitle = "(hectáreas)",
         x="Municipio",
         y="Hectáreas",
         caption = "Fuente: @claudiodanielpc con datos del Servicio de Información Agroalimentaria y Pesquera (SIAP)")+
    facet_wrap(~nomestado, scale="free", nrow=2)+
    theme_bw() +
    theme(text=element_text(family="Century Gothic"),
          panel.background = element_blank(),
          panel.grid =  element_blank(),
          
          panel.border = element_blank(),
          axis.ticks = element_blank(),
          plot.title = element_text(hjust = 0, size=35,face="bold", color="white"),
          plot.subtitle = element_text(hjust = 0, size=20, face="italic", color="white"),
          plot.caption = element_text(hjust = 0,size=18, color="white", face="bold"),
          axis.title = element_text(color="white"),
          axis.title.x = element_blank(),
          axis.text = element_text(face="bold", size=12),
          axis.text.y = element_text(color="white"),
          axis.text.x=element_blank(),
          strip.background = element_rect(
            color="black", fill="#9d2449", 
            size=1.5, linetype="solid"
          ),
          strip.text = element_text(
            size = 15, color = "white", 
            face = "bold.italic")
          
          
          
    )
    
    #Imagen de fondo====
  
  img="https://cdn.computerhoy.com/sites/navi.axelspringer.es/public/styles/1200/public/media/image/2020/02/rosa-1857455.jpg?itok=t2w4IRtG"
  
  ggimage::ggbackground(graf,img)
  
  
    #Salvar gráfico====
  ggsave("day_4/day4.png",
         height=7,width=25,bg = "white")
  