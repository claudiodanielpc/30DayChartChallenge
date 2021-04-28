###Script #30DayChartChallenge
#Día 28
#Future

##Borrar datos del entorno
rm(list=ls())

if(!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse, extrafont,
               lubridate, scales)

#Directorio de trabajo
setwd("C:/Users/ALIENWARE/Documents/GitHub/30DayChartChallenge")
##Descargar datos de proyecciones de población de CONAPO  
pob<-read.csv("http://www.conapo.gob.mx/work/models/CONAPO/Datos_Abiertos/Proyecciones2018/pob_mit_proyecciones.csv",
              encoding="latin",
              header=TRUE,
              check.names=FALSE)%>%
  #Nombres a minúsculas
  janitor::clean_names()%>%
  select(!renglon)%>%
  #Omitir Nacional
  filter(entidad!="República Mexicana")%>%
  group_by(ano, entidad)%>%
  summarise(poblacion=sum(poblacion))


#Gráfico

pob%>%
  #filter(entidad=="Ciudad de México")%>%
  mutate(poblacion=poblacion/1000000)%>%
  ggplot(.,aes(ano,poblacion))+
  
   geom_point(fill="#99d8c9",
             size=1.2, shape=21, alpha=0.4)+
  geom_smooth(level=0.99, color = "transparent", 
              fill = "red")+
  geom_vline(xintercept = 2020, linetype="dashed", 
             color = "#636363", size=1.5)+
 scale_y_continuous("Población (millones de personas)",
                    labels=comma)+
  ggthemes::theme_excel_new(
    base_family="Calibri")+
  labs(title="México. Población por entidad federativa, 1970-2050",
       subtitle="Millones de personas",
       caption="Nota: La línea vertical indica lo que podría pasar con la población de cada entidad en los próximos años.
Fuente: @claudiodanielpc con datos de CONAPO. Proyecciones de población",
       x="Año")+
  theme(plot.title = element_text(hjust = 0, size=30,face="bold"),
        plot.subtitle = element_text(hjust = 0, size=25, face="italic"),
        plot.caption = element_text(hjust = 0,size=20),
        legend.position = "none",
        axis.ticks = element_blank(),
        axis.title = element_text(size=15, color="black"),
        axis.text = element_text(size=12, color="black"),
        strip.text.x = element_text(size=20, color="black",
                                    face="bold"),
        strip.background = element_rect(
          color="black", fill="#e7e1ef", 
          size=1.2, linetype="dotted"
        ))+
  facet_wrap(~entidad, 
             scale="free_y", ncol=4)


##Salvar gráfico
ggsave("day28.png", width = 20, height = 15)
