###Script #30DayChartChallenge
#Día 4
#Magical

##Borrar datos del entorno
rm(list=ls())


#Directorio de trabajo
setwd("C:/Users/ALIENWARE/Documents/GitHub/30DayChartChallenge")

#Librerías
if(!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse, extrafont, gganimate)

#Lectura y limpieza de datos====
us<-read.csv("https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1168&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=ICNSA&scale=left&cosd=2019-01-14&coed=2021-03-27&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Weekly%2C%20Ending%20Saturday&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2021-04-04&revision_date=2021-04-04&nd=1967-01-07")%>%
    janitor::clean_names()%>%
  ###Cambiar variable de caracter a formato de fecha  
  mutate(date=as.Date.character(date))


##Gráfico plano
us%>%
ggplot(.,aes(date,icnsa))+
         geom_line(size=2, color="#01665e")+
  theme_minimal()+
  #Formato de número del eje y
  scale_y_continuous("Number",labels = scales::comma)+
  ##Formato del eje de la fecha
  scale_x_date("Date",expand = c(0, 0),
               date_labels = "%b %Y", date_breaks = "30 days")+
  labs(
    title = "United States. Initial Claims, 2019-2021",
    subtitle="Not seasonally adjusted",
    caption = "
    
    
    
Source: @claudiodanielpc using data from U.S. Employment and Training Administration, 
Initial Claims [ICNSA], retrieved from FRED, Federal Reserve Bank of St. Louis; 
https://fred.stlouisfed.org/series/ICNSA, April 4, 2021."
  )+
  theme(plot.title = element_text(hjust = 0, size=25,face="bold"),
        plot.subtitle = element_text(hjust = 0, size=20, face="italic"),
        plot.caption = element_text(hjust = 0,size=15),
        legend.position = "none",
        text=element_text("Tahoma",
                          size=20),
        axis.text.x=element_text(size=12, angle=90))+
  #Código para animación
  transition_reveal(date)+ view_follow()->p


##Especificaciones para salvar la animación
p %>% animate(fps = 20,
                       nframes = 100,
                       duration=10,
                       end_pause = 25,
                       width = 800, height = 800)

anim_save("day4.gif")

  
