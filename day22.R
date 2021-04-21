###Script #30DayChartChallenge
#Día 22
#Animation

##Borrar datos del entorno
rm(list=ls())


#Directorio de trabajo
setwd("C:/Users/ALIENWARE/Documents/GitHub/30DayChartChallenge")


# Librerías ====
if(!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse, extrafont,
               ggimage, gganimate)


#Imagen para gráfico====

imagen<-"https://www.soyfutbol.com/__export/1614059822809/sites/debate/img/2021/02/22/escudo_america_crop1614059801583.jpg_943222218.jpg"
download.file(imagen, destfile = "imagen.jpg",
              mode = 'wb')

imagen<-jpeg::readJPEG("imagen.jpg")


ligamx<-readxl::read_xlsx("C:/Users/ALIENWARE/Downloads/sportsref_download.xlsx")%>%
  janitor::clean_names()%>%
  #Variables a ocupar
  select(fecha,ronda, gf,gc)%>%
  #Filtro de torneo
  filter(ronda=="Guard1anes 2021 Temporada regular")%>%
  drop_na()%>%
  ##Generar acumulados de goles a favor y en contra
  mutate(cumgf=cumsum(gf),
         cumgc=cumsum(gc),
         #Crear variable de goles a favor por cada gol en contra
         g=cumgf/cumgc,
         fecha=as.Date(fecha))


##Gráfico

#Definir color de tipografía del gráfico
colorf<-"#fee391"

ligamx%>%
  ggplot(.,aes(fecha,g))+
  annotation_custom(grid::rasterGrob(imagen, 
                               width = unit(1,"npc"), 
                               height = unit(1,"npc")), 
                    -Inf, Inf, -Inf, Inf)+
  geom_line(size=2, color= colorf,
            arrow = arrow())+
  ggrepel::geom_label_repel(aes(label=ifelse(fecha =="2021-04-17",
                                             round(g,1),
                                             "")),
                            size=7,fontface="bold",
                            color="red",
                            hjust=0.5)+
  
  scale_y_continuous("Goles a favor por cada gol en contra",
                     limits=c(0,3))+
  scale_x_date("Meses")+
  theme_classic(base_family = "Ink Free")+
  labs(title = "Club América. Evolución de la relación 
goles a favor por cada gol en contra",
       subtitle = "Guard1anes 2021",
       caption = "
Fuente: @claudiodanielpc con datos de https://fbref.com.")+
  theme(plot.title = element_text(hjust = 0, 
                                  size=30,
                                  face="bold",
                                  color="black"),
        plot.subtitle = element_text(hjust = 0,
                                     size=25, 
                                     face="bold.italic",
                                     color="black"),
        plot.caption = element_text(hjust = 0,
                                    size=18,
                                    color="black",
                                    face="bold"),
        axis.title = element_text(color = "black", 
                                  face="bold"),
        axis.text = element_text(color="black",
                                   face="bold"),
        legend.position = "none",
        text=element_text(
                          size=20))+
  transition_reveal(fecha)->p



p %>% animate(fps = 15,
                       nframes = 10,
                       duration=8,
                       end_pause = 25,
                       width = 600, height = 600)

anim_save("day22.gif")