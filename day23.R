  ###Script #30DayChartChallenge
  #Día 23
  #Tiles
  
  ##Borrar datos del entorno
  rm(list=ls())
  
  
  #Directorio de trabajo
  setwd("C:/Users/ALIENWARE/Documents/GitHub/30DayChartChallenge")
  
  
  # Librerías ====
  if(!require('pacman')) install.packages('pacman')
  pacman::p_load(tidyverse, extrafont, viridis)
  
  
  metro<-openxlsx::read.xlsx("https://datos.cdmx.gob.mx/dataset/70166aa5-4ba9-43a5-9fe1-9ced9245fbd9/resource/b23a7eb4-3a3e-4a19-bd06-83d18a002ff2/download/informacion-ingresos-cierre-2012_2021-ok.xlsx")%>%
  janitor::clean_names()%>%
    filter(tipo_ingreso=="Boletos" | 
             tipo_ingreso=="Recargas")%>%
    mutate(fecha=as.POSIXct.Date(fecha))%>%
    mutate(year=lubridate::year(fecha),
           mes=lubridate::month(fecha))%>%
    filter(year>2082 & year<2091)%>%
    mutate(year=recode(year,`2083`=2013,
                       `2084`=2014,
                       `2085`=2015,
                       `2086`=2016,
                       `2087`=2017,
                       `2089`=2019,
                       `2088`=2018,
                       `2090`=2020))%>%
    select(year,mes, linea_1:linea_12)%>%
    pivot_longer(cols = -c(year,mes),
                 names_to="linea",
                 values_to="monto")%>%
    group_by(year,linea, mes)%>%
    summarise(monto=sum(monto))%>%
  mutate(pct=monto/sum(monto)*100)%>%
    ungroup()%>%
    
  mutate(mes=case_when(
    mes==1 ~ "Enero",
    mes==2 ~ "Febrero",
    mes==3 ~ "Marzo",
    mes==4 ~ "Abril",
    mes==5 ~ "Mayo",
    mes==6 ~ "Junio",
    mes==7 ~ "Julio",
    mes==8 ~ "Agosto",
    mes==9 ~ "Septiembre",
    mes==10 ~ "Octubre",
    mes==11 ~ "Noviembre",
    mes==12 ~ "Diciembre"))%>%
    mutate(mes = factor(mes, levels=(c("Enero", "Febrero", 
                                              "Marzo", "Abril", 
                                              "Mayo", "Junio", 
                                              "Julio", "Agosto",
                                              "Septiembre","Octubre",
                                              "Noviembre","Diciembre"))))%>%
    mutate(linea=case_when(
      linea=="linea_1" ~ "Línea 1",
      linea=="linea_2" ~ "Línea 2",
      linea=="linea_3" ~ "Línea 3",
      linea=="linea_4" ~ "Línea 4",
      linea=="linea_5" ~ "Línea 5",
      linea=="linea_6" ~ "Línea 6",
      linea=="linea_7" ~ "Línea 7",
      linea=="linea_8" ~ "Línea 8",
      linea=="linea_9" ~ "Línea 9",
      linea=="linea_a" ~ "Línea A",
      linea=="linea_b" ~ "Línea B",
      linea=="linea_12" ~ "Línea 12"))%>%
  mutate(linea = factor(linea, levels=rev(c("Línea 1", "Línea 2", 
                                     "Línea 3", "Línea 4", 
                                     "Línea 5", "Línea 6", 
                                     "Línea 7", "Línea 8",
                                     "Línea 9","Línea A",
                                     "Línea B","Línea 12"))))

  
  
  metro%>%
  ggplot(., 
         aes(x = mes, y = linea, fill=monto/1000000)) + 
    geom_tile()+
    #Paleta de colores
    scale_fill_viridis("Millones de pesos",
                       option = "viridis",
                       guide=guide_colourbar(title.position = "top",
                                             title.hjust = 0.5))+
    theme_minimal(base_family = "Century Gothic")+
    ##Títulos, subtítulos y fuente
    labs(title = "Distribución de los ingresos por boletos y recargas del Metro de la Ciudad de México",
         subtitle = "%",
         y = "Línea",
         x="Meses",
         caption = " 
Fuente: @claudiodanielpc con datos de la Agencia Digital de Innovación Pública de la CDMX")+
    ##Temita
    theme(legend.position="top",
          legend.direction="horizontal",
          legend.title=element_text(colour="black", size=25),
          legend.margin=margin(grid::unit(0,"cm")),
          legend.text=element_text(colour="black",size=25,face="bold"),
          legend.key.height=grid::unit(0.8,"cm"),
          legend.key.width=grid::unit(2,"cm"),
          axis.title = element_text(size=25),
          axis.text.x=element_text(colour="black", size=25,
                                   angle=90, vjust=0.5),
          axis.text.y=element_text(colour="black",size=25),
          axis.ticks=element_blank(),
          plot.background=element_blank(),
          panel.border=element_blank(),
          plot.margin=margin(0.7,0.4,0.1,0.2,"cm"),
          strip.text.x = element_text(size=30, color="black",
                                      face="bold"),
          strip.background = element_rect(
            color="black", fill="#fdae6b", 
            size=1.5, linetype="solid"
          ),
          plot.title=element_text(colour="black",hjust=0,
                                  size=40,face="bold"),
          plot.subtitle = element_text(hjust = 0, size=35, face="italic"),
          plot.caption = element_text(hjust = 0,size=25))+
    facet_wrap(~year, ncol = 4)
  
  
  
  ##Salvar el gráfico
  ggsave("day23.png",height = 24,width = 30, units="in",dpi=300)