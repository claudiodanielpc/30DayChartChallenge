###Script #30DayChartChallenge
#Día 19
#Global change

##Borrar datos del entorno
rm(list=ls())


#Directorio de trabajo
setwd("C:/Users/ALIENWARE/Documents/GitHub/30DayChartChallenge")

#Librerías
if(!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse, extrafont, ggthemes)


download.file("http://www.bcv.org.ve/sites/default/files/precios_consumidor/4_5_7_indice_y_variaciones_mensuales_serie_desde_dic_2007.xls",
              destfile="ipc.xls", mode="wb")

#Crear lista de años como caracter para poder generar variable en dataframe
lista<-as.character(c(2008:2020))


#Lectura y limpieza de datos====
ipc<-readxl::read_xls("ipc.xls")%>%
  #Eliminar filas innecesarias
  slice(-(1:11))%>%
  slice(-(182:185))%>%
  #Renombrar variables
  rename(mes=1,
         ipc=2)%>%
  #Seleccionar variables
  select(mes, ipc)%>%
  #Crear variable de año
  mutate(year=as.numeric(ifelse(mes %in% lista,
                     mes,NA)))%>%
  #Rellenar año
  fill(year)%>%
  #Quitar NAs
  drop_na()%>%
mutate(mes=case_when(
  mes=="Enero" ~ 1,
  mes=="Febrero" ~ 2,
  mes=="Marzo" ~ 3,
  mes=="Abril" ~ 4,
  mes=="Mayo" ~ 5,
  mes=="Junio" ~ 6,
  mes=="Julio" ~ 7,
  mes=="Agosto" ~8,
  mes=="Septiembre" ~9,
  mes=="Octubre" ~10,
  mes=="Noviembre" ~11,
  mes=="Diciembre" ~12))%>%
#Crear variable de fecha
  mutate(fecha=lubridate::ymd(paste(
    year,mes,"15",sep=" ")))%>%
  arrange(fecha)%>%
#Calcular variación promedio anual
mutate(var=(as.numeric(ipc)/lag(as.numeric(ipc),12)-1)*100)%>%
  drop_na()


#Gráfico====
ipc%>%
  ggplot(.,aes(fecha,var))+
  geom_line()+
  geom_line(size=2, color="#01a2d9")+
  ggrepel::geom_label_repel(aes(label=ifelse(fecha =="2019-02-15" | 
                                              fecha=="2018-11-15" |
                                              fecha=="2019-07-15" |
                                              fecha=="2020-12-15",
                             format(round(var,1),big.mark = ","),
                             "")),
            size=3.5,fontface="bold",
            color="red",
            hjust=0.5,
            arrow = arrow(length = unit(0.02, "npc")))+
  theme_economist_white(base_family = "sans")+
  scale_y_continuous("Var. % anual",labels = scales::comma)+
  scale_x_date("Mes",
    date_breaks = "6 month", 
               date_labels = "%b %Y")+ 
  labs(
    title = "Venezuela. Índice Nacional de Precios al Consumidor, 2009-2020",
    subtitle="Variación porcentual respecto al mismo mes del año anterior",
    caption = "
Fuente: @claudiodanielpc con datos del Banco Central de Venezuela."
  )+
  theme(plot.title = element_text(hjust = 0, size=30,face="bold"),
        plot.subtitle = element_text(hjust = 0, size=25, face="italic"),
        plot.caption = element_text(hjust = 0,size=20),
        legend.position = "none",
        axis.ticks = element_blank(),
        axis.title = element_text(size=15),
        axis.text.x=element_text(size=12, angle=90, 
                                 hjust=0.95,vjust=0.2),
        axis.text.y = element_text(size=12))


#Guardar gráfico
ggsave("day20.png", height=10, 
       width=20, units='in', dpi=300)




