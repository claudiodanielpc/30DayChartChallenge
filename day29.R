###Script #30DayChartChallenge
#Día 29
#Deviations


##Borrar datos del entorno
rm(list=ls())

if(!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse, extrafont, scales, ggthemes)

##Datos Encuesta Expectativas Banxico====
#Fuente: https://www.banxico.org.mx/publicaciones-y-prensa/encuestas-sobre-las-expectativas-de-los-especialis/encuestas-expectativas-del-se.html
#Directorio de trabajo general
setwd("C:/Users/ALIENWARE/Documents")


#Directorio para almacenar datos de Banxico
dir.create("banxico")

unzip("C:/Users/ALIENWARE/Downloads/Microdatos.zip",
      exdir = "banxico") 
archivos <- list.files(path = "C:/Users/ALIENWARE/Documents/banxico", pattern = "Microdatos")
banxico <- purrr::map_dfr(
  archivos,
  ~ data.table::fread(glue::glue("banxico/{.x}"), na.strings = "*") %>%
    tibble() %>%
    janitor::clean_names()%>%
  mutate(mes=lubridate::month(fecha_encuesta),
         year=lubridate::year(fecha_encuesta))%>%
    filter(nombre_relativo_corto=="varpibt" & 
             mes==1 & year>=2009 & year<2021))

#Datos PIB INEGI====

url<-"https://www.inegi.org.mx/contenidos/programas/pib/2013/datosabiertos/pibtr_trimestral_csv.zip"


download.file(url, mode="wb", destfile = "banxico/archivo.zip")

unzip("banxico/archivo.zip", exdir = "banxico") 


#Leer datos
pib<-read.csv("banxico/conjunto_de_datos/conjunto_de_datos_pibtr_pibt_3r2020_t4.csv")%>%
  janitor::clean_names()%>%
  filter(i_descriptores=="B.1bP---Producto interno bruto<C1,C2>")%>%
  rename(pibn=1)%>%
  pivot_longer(!pibn,
               names_to="year",
               values_to="pib")%>%
#Dejar solo los datos anuales
    filter(str_detect(year, '_anual'))%>%
  #Extraer año y transformar a numérico
  mutate(year=as.integer(str_sub(year,2,5)))%>%
  filter(year>=2009)%>%
  select(year, pib)


##Pegar datos

banxico<-banxico%>%
  left_join(pib)


#Gráfico====

banxico%>%
  #Dejar únicamente a aquellos analistas con serie completa
  group_by(id_analista)%>%
  filter(n()>11)%>%
  ungroup()%>%
  #Gráfico
  ggplot()+
  geom_line(aes(year,pib), color="#7fcdbb",
            size=2)+
  geom_point(aes(year,dato), 
             color="black",
             fill="#dd1c77", 
             shape=21,
             size=3)+
    theme_economist()+
  scale_x_continuous("Año",
                     breaks = seq(from = 2009, 
                                  to = 2020, by = 1))+
  labs(
    title = "Producto Interno Bruto de México, 2009-2020",
    subtitle="Variación porcentual anual.
Observado y estimado por analistas en enero de cada año",
    y="Variación % anual",
    caption = "Notas:
1. Los analistas son identificados por un número y no se puede saber la institución a la que pertenecen.    
Fuente: @claudiodanielpc con datos de INEGI. Producto Interno Bruto (PIB) - Trimestral. Base 2013 y
Banco de México. Encuesta sobre las expectativas de los especialistas en economía del sector privado."
  )+
  theme(plot.title = element_text(hjust = 0, size=30,face="bold"),
        plot.subtitle = element_text(hjust = 0, size=25, face="italic"),
        plot.caption = element_text(hjust = 0,size=20),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size=15),
        legend.title = element_text(size=15),
        axis.title = element_text(size=15),
        axis.text.x=element_text(size=12, angle=90, 
                                 hjust=0.5,vjust=0.5),
        axis.text.y = element_text(size=12, angle = 0,
                                   hjust=0.5,vjust=0.5),
        strip.text.x = element_text(size=20, color="white",
                                    face="bold"),
        strip.background = element_rect(
          color="black", fill="#756bb1", 
          size=1.2
        ),
        text=element_text("Consolas"))+
  facet_wrap(~id_analista)


#Directorio de trabajo para las gráficas
setwd("C:/Users/ALIENWARE/Documents/GitHub/30DayChartChallenge")


##salvar gráfico

ggsave("day29.png", height=10, 
       width=20, units='in', dpi=300)