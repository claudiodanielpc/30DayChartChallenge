###Script #30DayChartChallenge
#Día 16
#Tree

##Borrar datos del entorno
rm(list=ls())

#Directorio de trabajo
setwd("C:/Users/ALIENWARE/Documents/GitHub/30DayChartChallenge")

##Se cargan las librerías necesarias

if(!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse, extrafont, treemapify)

##Url de descarga

url<-"https://www.transparenciapresupuestaria.gob.mx/work/models/PTP/DatosAbiertos/Bases_de_datos_presupuesto/XLSX/PEF_2021.xlsx"
#Lectura y limpieza====
pef2021<-openxlsx::read.xlsx(url)%>%
  #Nombres a minúsculas
  janitor::clean_names()%>%
  #Monto a numérico
  group_by(desc_ramo)%>%
  summarise(monto_aprobado=sum(monto_aprobado))%>%
  #millones
  mutate(monto_aprobado=monto_aprobado/1000000000)


#Gráfico====

pef2021%>%
  ggplot(.,aes(area = monto_aprobado, fill=monto_aprobado,
               label=paste(desc_ramo,
                                            paste(format(round(monto_aprobado,1),
                                                         big.mark = ",")),
                           sep="\n")))+
  geom_treemap()+
  geom_treemap_text(fontface = "bold", 
                    colour = "black", place = "topleft",
                    grow = F)+
  ggthemes::theme_stata(base_family = "sans")+
labs(title="Presupuesto de Egresos de la Federación por ramo, 2021",
     subtitle="Miles de millones de pesos",
     caption="
Fuente: @claudiodanielpc con datos de la Secretaría de Hacienda y Crédito Público. Transparencia Presupuestaria")+
  theme(plot.title = element_text(hjust = 0, size=25,face="bold"),
        plot.subtitle = element_text(hjust = 0, size=20, face="italic"),
        plot.caption = element_text(hjust = 0,size=15),
        legend.position ="none")+
  scale_fill_distiller(type="seq",palette = "YlGn",
                       direction = 1)


ggsave("day16.png", height=10, width=20, units='in', dpi=300)
