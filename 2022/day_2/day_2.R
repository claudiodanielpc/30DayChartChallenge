###Script #30DayChartChallenge
#Día 2
#Pictogram

##Borrar datos del entorno
rm(list=ls())

#Directorio de trabajo
setwd("D:/")

##Se cargan las librerías necesarias

if(!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse, extrafont, ggimage, srvyr)




#Imagen para gráfico====
imagen="https://eurokol.hu/wp-content/uploads/2018/03/800px_COLOURBOX20208210.jpg"


#Descarga de catálogos
cat<-"https://www.inegi.org.mx/contenidos/app/ageeml/catun_municipio.zip"
temp<-tempfile()
download.file(cat,destfile = temp)
unzip(temp,
      exdir="Documentos/catálogos",
      overwrite = TRUE
)
unlink(temp)


#Lectura y limpieza de datos=====
cat<-data.table::fread("Documentos/catálogos/AGEEML_20211271318711.csv",
                      #Seleccionar variables
                      select=c(
                        "cve_ent",
                        "nom_ent",
                        "cve_mun",
                        "nom_mun"))%>%
                      slice(1:n()-1)%>%
  #Construcción de clave geoestadística
  mutate(cvegeo=paste0(ifelse(nchar(cve_ent)==1,
                              paste0("0",cve_ent),
                              cve_ent),
                       ifelse(nchar(cve_mun)==1,
                              paste0("00", cve_mun),
                              ifelse(nchar(cve_mun)==2,
                              paste0("0",cve_mun),
                              cve_mun))))%>%
  select(cvegeo,nom_ent,nom_mun)
                      




#Datos Censo. Lectura y limpieza====
censo<-read.csv("datos/censo2020/Viviendas00.csv")%>%
  janitor::clean_names()%>%
  #Seleccionar variables
  select(ent,mun,cobertura,estrato,upm,clavivp,
         factor, tenencia)%>%
  #Omitir viviendas móviles, locales y refugios
  filter(clavivp<7 | clavivp==99)%>%

  mutate(
    cvegeo=paste0(ifelse(
      nchar(ent)==1,
      paste0("0",ent),ent),
      ifelse(nchar(mun)==1,
             paste0("00",mun),
             ifelse(nchar(mun)==2,
                    paste0("0",mun),
                    mun)
             
             
      )
      
    )
    
  )%>%
  left_join(cat)





#Graf
censo%>%
  as_survey(weights=c(factor))%>%
  filter(ent==9 & tenencia==2)%>%
  group_by(nom_mun)%>%
  summarise(viviendas=survey_prop(vartype = c("cv","ci")))%>%
  mutate(viviendas=viviendas*100,
         nom_mun=fct_reorder(nom_mun,viviendas))%>%
  ggplot(.,aes(x=nom_mun,y=viviendas))+
  geom_image(aes(image=imagen), size=0.03)+
  coord_flip()+
  #Línea para conectar la casa
  geom_segment(aes(y=0,
                   x=nom_mun,
                   yend=viviendas-.1,
                   xend=nom_mun),
               color="#b38e5d",
               size = 1.7)+
  #Etiquetas de valor
  geom_text(aes(label=format(round(viviendas,2))), 
            vjust=0.5,
            hjust=-1, color="#285c4d",
            size=8,fontface="bold")+
  
  #Modificar límites de eje y
  scale_y_continuous(limits=c(0, 20))+
  theme_minimal()+
  labs(title="Ciudad de México. Viviendas particulares habitadas rentadas por alcaldía, 2020",
       subtitle = "(Porcentaje respecto del total de viviendas particulares habitadas en renta de la entidad)",
       x="Alcaldía",
       y="Porcentaje",
       caption = "Fuente: @claudiodanielpc con datos de INEGI. Censo de Población y Vivienda 2020.")+
  theme(plot.title = element_text(hjust = 0, size=30,face="bold"),
        plot.subtitle = element_text(hjust = 0, size=25, face="italic"),
        plot.caption = element_text(hjust = 0,size=18),
        legend.position = "none",
        text=element_text("Tahoma",
                          size=20),
        axis.text.x=element_text(size=15))


##Salvar gráfico
ggsave("Documentos/Github/30DayChartChallenge/2022/day_2/day2.png",
       height=15,width=20,bg = "white")
