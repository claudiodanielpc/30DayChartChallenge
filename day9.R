###Script #30DayChartChallenge
#Día 9
#Statistics

##Borrar datos del entorno
rm(list=ls())


#Directorio de trabajo
setwd("C:/Users/ALIENWARE/Documents/GitHub/30DayChartChallenge")

#Librerías
if(!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse, extrafont)


#Datos COVID19 México====
url<-"http://datosabiertos.salud.gob.mx/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip"

##Creación de directorio temporal

td<- tempdir()

# Descarga del archivo temporal


tf = tempfile(tmpdir=td, fileext=".zip")

download.file(url, tf)

# Obtener el nombre del primer archivo

fname = unzip(tf, list=TRUE)$Name[1]

# unzip

unzip(tf, files=fname, exdir=td, 
      overwrite=TRUE)

# dirección del archivo
fpath = file.path(td, fname)
unlink(td)




covid = read.csv(fpath, header=TRUE, row.names=NULL, 
                 stringsAsFactors=FALSE)


##Trabajar la base de datos
##Base general
covid<-covid%>%
  #Todo a minúsculas
  janitor::clean_names()%>%
  #Dejar casos confirmados
  filter(clasificacion_final==1 | clasificacion_final==2 |
           clasificacion_final==3,
         #Filtrar defunciones
         fecha_def!="9999-99-99")%>%
  #Variables a utilizar
  select(sexo, edad)







#Gráfico
covid%>%
  mutate(sexo=ifelse(sexo==1,"Mujer", "Hombre"))%>%
  ggplot(., 
         aes(sexo, edad)) +
  geom_violin(aes(fill = sexo, color=sexo),alpha = 0.5, size=1.5)+
  geom_boxplot(width=0.1, size=1.5)+
  scale_fill_manual(values=c("#999999", "#E69F00"))+
scale_color_manual(values=c("#999999", "#E69F00"))+
  theme_minimal()+
  labs(
    title = "México. Defunciones confirmadas por COVID-19 por edad y género",
    subtitle = "Datos al 8 de abril de 2021",
    y="Edad",
  x="Género")+
  theme(plot.title = element_text(hjust = 0,
                                  size=25,face="bold"),
        plot.subtitle = element_text(hjust = 0, 
                                     size=20, face="italic"),
        plot.caption = element_text(hjust = 0,size=15),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text = element_text(size=15),
        legend.position = "none",
        #Fuente del gráfico
        text=element_text("Book Antiqua"))->viol

media<-covid%>%
  mutate(sexo=ifelse(sexo==1,"Mujer", "Hombre"))%>%
  group_by(sexo)%>%
  summarise(media=mean(edad))%>%
  ungroup()
  

covid%>%
  mutate(sexo=ifelse(sexo==1,"Mujer", "Hombre"))%>%
  ggplot(., 
         aes(edad, color=sexo))+
  geom_density(aes(fill=sexo), alpha=0.5)+
  scale_fill_manual("Género",values=c("#999999", "#E69F00"))+
  
  scale_color_manual("Género",values=c("#999999", "#E69F00"))+
    theme_minimal()+
  labs(x="Edad",
      y="Densidad",
    caption = "
Fuente: @claudiodanielpc con datos de la Secretaría de Salud.")+
    theme(
         plot.caption = element_text(hjust = 0,size=15),
         axis.ticks.x = element_blank(),
         axis.text = element_text(size=15),
         legend.position = c(0.8,0.4),
         legend.title=element_text(colour="black",
                                   size=20,face="bold"),
         legend.text=element_text(colour="black",
                                  size=18,face="bold"),
         #Fuente del gráfico
         text=element_text("Book Antiqua"))->hist
  

ggpubr::ggarrange(viol,hist,
                  nrow = 2)

##Salvar gráfico
ggsave("day9.png", width = 15, height = 15)




  