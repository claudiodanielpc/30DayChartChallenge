###Script #30DayChartChallenge
#Día 5
#Slope

if(!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse, extrafont,srvyr)


#Directorio de trabajo
setwd("D:/datos/enigh")
#creamos carpeta en donde almacenaremos los archivos
dir.create("2020", showWarnings = F)
dir.create("2010", showWarnings = F)


##Descarga de datos----
##2020

url<-"https://www.inegi.org.mx/contenidos/programas/enigh/nc/2020/microdatos/enigh2020_ns_concentradohogar_csv.zip"


#Se crea tempfile para no almacenar los zips
temp<-tempfile()
##Descargar y extraer catálogo
download.file(url,mode = "wb",
             destfile = temp)

unzip(temp,
     exdir = "2020")
unlink(temp)


##2008


url<-"https://www.inegi.org.mx/contenidos/programas/enigh/nc/2010/microdatos/NCV_Concentrado_2010_concil_2010_csv.zip"


#Se crea tempfile para no almacenar los zips
temp<-tempfile()
##Descargar y extraer catálogo
download.file(url,mode = "wb",
              destfile = temp)

unzip(temp,
      exdir = "2010")
unlink(temp)






##Cálculo del tipo de hogar

##2010


enigh10<-read.csv("2010/concentrado.csv")%>%
  as_survey(weights=c(factor))%>%
  group_by(clase_hog)%>%
  summarise(hogares=survey_prop(vartype = c("cv")))%>%
  mutate(year=2010)



##2020


enigh20<-read.csv("2020/concentradohogar.csv")%>%
  as_survey(weights=c(factor))%>%
  group_by(clase_hog)%>%
  summarise(hogares=survey_prop(vartype = c("cv")))%>%
  mutate(year=2020)




#consolidada
consol<-rbind(enigh10,enigh20)%>%
  mutate(clase_hog=
           case_when(
             clase_hog==1 ~ "Unipersonal",
             clase_hog==2 ~ "Nuclear",
             clase_hog==3 ~ "Ampliado",
             clase_hog==4 ~ "Compuesto",
             clase_hog==5 ~ "Corresidente"
             
           ),
         hogares=hogares*100
         )


#Gráfico====
consol%>%
ggplot(.,aes(x = factor(year), y = hogares, group = clase_hog)) +
  geom_line(aes(color = clase_hog), size = 2) +
  geom_point(aes(color = clase_hog), size = 4)+
  scale_x_discrete(position = "top") +
  theme_bw()+
  
  #Porcentaje 2010
  geom_text(data = consol %>% filter(year == 2010), 
            aes(label = paste0(round(hogares,2), "%")) , 
            hjust = 1.35, 
            fontface = "bold", 
            size = 4)+
  
  #Porcentaje 2020
  geom_text(data = consol %>% filter(year == 2020), 
            aes(label = paste0(round(hogares,2), "%")) , 
            hjust = -.35, 
            fontface = "bold", 
            size = 4)+
  #Paleta de colores
  scale_color_brewer("Clase de hogar",palette = "Set1")+
  #Tema feo de STATA :)
  ggthemes::theme_stata()+
  labs(title="México. Hogares por clase, 2010-2020",
       subtitle = "(Porcentaje)",
       x="Año",
       y="Porcentaje",
       caption = "Fuente: @claudiodanielpc con datos de INEGI. Encuesta Nacional de Ingresos y Gastos de los Hogares 2010 y 2020.")+
  theme(plot.title = element_text(hjust = 0, size=30,face="bold"),
        plot.subtitle = element_text(hjust = 0, size=25, face="italic"),
        plot.caption = element_text(hjust = 0,size=18),
        legend.position = "bottom",
        text=element_text("Palatino Linotype",
                          size=20),
        axis.text.x=element_text(size=15))+
  #Ventanas divertidas
  facet_grid(~clase_hog)



#Cambiar directorio de trabajo para almacenar
setwd("D:/Documentos/GitHub/30DayChartChallenge/2022")
#creamos carpeta en donde almacenaremos los archivos
dir.create("day_5", showWarnings = F)

##Salvar gráfico
ggsave("day_5/day5.png",
       height=15,width=20,bg = "white")




