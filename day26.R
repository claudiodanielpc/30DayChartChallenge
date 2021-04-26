###Script #30DayChartChallenge
#Día 26
#Trends


##Borrar datos del entorno
rm(list=ls())

if(!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse, extrafont,
               lubridate, scales)


#Directorio de trabajo
setwd("C:/Users/ALIENWARE/Documents/GitHub/30DayChartChallenge")



#Lectura de datos y construcción de variables====
#Fuente de descarga de datos:
#Banco de información económica: http://en.www.inegi.org.mx/app/indicadores/?tm=0

enoe<-readxl::read_xlsx("C:/Users/ALIENWARE/Documents/canadevi/enoe/enoe.xlsx")%>%
  mutate(fecha=ymd(paste(year,month,"15")))



##Subocupación====


enoe%>%
  ggplot(.,aes(fecha,tsubocup))+
  geom_line(color="#feb24c", size=1.5)+
  geom_smooth()+
  ggrepel::geom_label_repel(aes(label=ifelse(fecha =="2020-04-15" | 
                                               fecha=="2020-05-15" |
                                               fecha=="2020-06-15" |
                                               fecha=="2009-05-15" |
                                               fecha=="2021-03-15" ,
                                             paste0(format(fecha, "%b %Y"),
                                                    "\n",round(tsubocup,
                                                               1)),
                                             
                                             "")),
                            size=4,fontface="bold",
                            color="red",
                            hjust=0.5,
                            arrow = arrow(length = unit(0.02, "npc")))+
  ##Formato del eje de la fecha
  scale_x_date("Fecha",expand = c(0, 0),
               date_labels = "%b %Y", date_breaks = "365 days")+
  theme_minimal(base_family = "Century Gothic")+
  labs(title="Tasa de subocupación mensual, 2005-2021",
       subtitle="%",
       caption="Nota: La información de 2005 hasta marzo de 2020 proviene de la Encuesta Nacional de Ocupación y Empleo (ENOE), 
  de abril a junio de 2020 corresponde a la Encuesta Telefónica de Ocupación y Empleo (ETOE) y a partir de julio de 2020
  la información se genera con la Encuesta Nacional de Ocupación y Empleo (Nueva edición) (ENOE-N).
  
  Fuente: @claudiodanielpc con datos de INEGI.",
       y="%")+
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
ggsave("day26.png", height=10, 
       width=20, units='in', dpi=300)

