###Script #30DayChartChallenge
#Día 3
#Historical

##Borrar datos del entorno
rm(list=ls())

#Directorio de trabajo
setwd("D:/Documentos/GitHub/30DayChartChallenge/2022")
#creamos carpeta en donde almacenaremos los archivos
dir.create("day_3", showWarnings = F)

##Se cargan las librerías necesarias

if(!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse)

##Liga de descarga de archivo Excel
url<-"http://www.conasami.gob.mx/pdf/datosAbiertos/2019/SalariosMinimosGenerales/SalarioM%C3%ADnimoHist%C3%B3rico1877-2019_corteabril.xls"
download.file(url, mode="wb",
              destfile = "day_3/salmin.xls")


#Lectura archivo
sm<-readxl::read_excel("day_3/salmin.xls",sheet = 2)%>%
  rename(year=1,
         smreal=4
  )%>%
  select(year,smreal)%>%
  filter(year>=1934)
  

#Gráfica
sm%>%
ggplot(.,aes(x=factor(year),y=smreal))+
         geom_line(group=1, size=2.5, color="#9d2449")+
  
  ggthemes::theme_wsj()+
  labs(title="México. Salario mínimo real histórico, 1934-2019",
       subtitle = "(Pesos diarios, julio 2018=100)",
       x="Año",
       y="Pesos diarios",
       caption = "Fuente: @claudiodanielpc con datos la Comisión Nacional de los Salarios Mínimos.")+
  theme(plot.title = element_text(hjust = 0, size=30,face="bold"),
        plot.subtitle = element_text(hjust = 0, size=25, face="italic"),
        plot.caption = element_text(hjust = 0,size=18),
        legend.position = "none",
        text=element_text(
                          size=20),
        axis.text.x=element_text(size=12, angle=90))



##Salvar gráfico
ggsave("day_3/day3.png",
       height=12,width=20,bg = "white")

  
