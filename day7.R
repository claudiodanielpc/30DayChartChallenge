###Script #30DayChartChallenge
#Día 7
#Physical

##Borrar datos del entorno
rm(list=ls())


#Directorio de trabajo
setwd("C:/Users/ALIENWARE/Documents/GitHub/30DayChartChallenge")

#Librerías
if(!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse, extrafont, viridis)

url<-"http://datos.conagua.gob.mx/DatosAbiertos/Temperatura_Promedio_Excel.zip"

##Creación de directorio temporal

td<- tempdir()

# Descarga del archivo temporal

#Se crea tempfile para no almacenar los zips
temp<-tempfile()
##Descargar y extraer catálogo
download.file(url,
              destfile = temp)

unzip(temp)
unlink(temp)



#Lectura y limpieza de datos====
archivos<-list.files(path = "Temperatura_Promedio_Excel/", 
                        pattern = "Tmed")


temp<-purrr::map(archivos,
                    ~ readxl::read_xlsx(glue::glue("Temperatura_Promedio_Excel/{.x}"), 
                                        range="A2:M34",
                                        col_names=T,
                                        col_types = "text")%>%
                   janitor::clean_names())%>%
                   set_names(archivos)%>%
                   bind_rows(.id = "year")%>%
  #Obtener año del nombre del archivo
                mutate(year=as.integer(str_remove(year, "Tmed.xlsx")))%>%
              gather(month, temp, -year,-entidad)%>%
  mutate(temp=as.numeric(temp))%>%
  #Se quitan los datos de 2020, los cuales están incompletos            
  filter(year!=2020)%>%
  mutate(month = factor(month, levels=rev(c("ene", "feb", 
                                                              "mar", "abr", "may", "jun", "jul", "ago",
                                                              "sep","oct","nov","dic"))))


#Gráfico



temp%>%
ggplot(., 
       aes(x = year, y = month, fill=temp)) + 
  geom_tile()+
  scale_x_continuous("Año",
    breaks = seq(from = 1985, to = 2019, by = 2)
  )+
  #Paleta de colores
  scale_fill_viridis("°C", option = "inferno")+
  theme_minimal()+
  ##Títulos, subtítulos y fuente
  labs(title = "Temperatura promedio por entidad federativa",
       subtitle = "1985-2019",
       y = "Meses",
       caption = "Notas: 
Fuente: @claudiodanielpc con datos de la Comisión Nacional del Agua (CONAGUA) datos.conagua.gob.mx")+
  ##Temita
  theme(legend.position="bottom",
        legend.direction="horizontal",
        legend.title=element_text(colour="black"),
        legend.margin=margin(grid::unit(0,"cm")),
        legend.title.align = 0.5,
        legend.text=element_text(colour="black",size=25,face="bold"),
        legend.key.height=grid::unit(0.8,"cm"),
        legend.key.width=grid::unit(2,"cm"),
        axis.text.x=element_text(colour="black", size=25,
                                 angle=90, vjust=0.5),
        axis.text.y=element_text(colour="black",size=15),
        axis.ticks=element_blank(),
        plot.background=element_blank(),
        panel.border=element_blank(),
        plot.margin=margin(0.7,0.4,0.1,0.2,"cm"),
        strip.text.x = element_text(size=23, color="black",
                                    face="bold"),
        plot.title=element_text(colour="black",hjust=0,size=35,face="bold"),
        plot.subtitle = element_text(hjust = 0, size=30, face="italic"),
        plot.caption = element_text(hjust = 0,size=20),
        text=element_text("Arial",size=40))+
  facet_wrap(~entidad, ncol=4)
  

##Salvar el gráfico
ggsave("day7.png",height = 24,width = 30, units="in",dpi=320)