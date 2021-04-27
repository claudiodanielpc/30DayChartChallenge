###Script #30DayChartChallenge
#Día 26
#Educational


##Borrar datos del entorno
rm(list=ls())

if(!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse, extrafont, scales, ggthemes)


#Directorio de trabajo
setwd("C:/Users/ALIENWARE/Documents/GitHub/30DayChartChallenge")



#Datos====

colima<-read.csv("http://www.datos.col.gob.mx/2018/SEP/DAMECE20182019-01.csv")%>%
  janitor::clean_names()%>%
  select(nivel,masculino, femenino)%>%
  pivot_longer(!nivel,
               names_to="sexo",
               values_to="alumnos")%>%
  group_by(nivel, sexo)%>%
  summarise(alumnos=sum(alumnos))%>%
  mutate(sd=sd(alumnos),
         sexo=ifelse(sexo=="femenino", 
                     "Femenino", "Masculino"),
         nivel=ifelse(nivel=="PREESCOLAR", "Preescolar",
                      ifelse(nivel=="PRIMARIA", "Primaria",
                             "Secundaria")))
  


colima%>%
  ggplot(.,aes(sexo,alumnos, fill=sexo))+
  geom_bar(stat="identity",color="black",
           position=position_dodge())+
  geom_errorbar(aes(ymin=alumnos-sd,
                    ymax=alumnos+sd),
                width=.6, color="blue",
position=position_dodge(.9))+
  scale_fill_manual(values=c('#999999',
                             '#E69F00'))+
  scale_y_continuous(labels = comma)+
  theme_wsj()+
  labs(title="Colima. Matrícula Escolar",
       subtitle="Ciclo Escolar:2018-2019",
       caption="Fuente: @claudiodanielpc con datos de www.datos.gob.mx.",
       y="Alumnos",
       x="Sexo")+
  theme(plot.title = element_text(hjust = 0, size=30,face="bold"),
        plot.subtitle = element_text(hjust = 0, size=25, face="italic"),
        plot.caption = element_text(hjust = 0,size=20),
        legend.position = "none",
        axis.ticks = element_blank(),
        axis.title = element_text(size=15, color="black"),
        axis.text = element_text(size=12, color="black"),
        strip.text.x = element_text(size=25, color="black",
                                    face="bold"),
        strip.background = element_rect(
          color="black", fill="#fdae6b", 
          size=1.5, linetype="solid"
        ))+
  facet_wrap(~nivel)


#Guardar gráfico
ggsave("day27.png", height=10, 
       width=20, units='in', dpi=300)
