###Script #30DayChartChallenge
#Día 6 y 16
#Our World in Data/Environment

##Borrar datos del entorno
rm(list=ls())

#Directorio de trabajo
setwd("D:/Documentos/GitHub/30DayChartChallenge/2022")
#creamos carpeta en donde almacenaremos los archivos
dir.create("day_6_16", showWarnings = F)

##Se cargan las librerías necesarias

if(!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse, extrafont)

#Url y datos
url<-"https://nyc3.digitaloceanspaces.com/owid-public/data/energy/owid-energy-data.csv"
energy<-read.csv(url)

#Gráfico
energy%>%
  filter(country=="Mexico" & year>=1985)%>%
  select(year,fossil_electricity,renewables_electricity,electricity_generation)%>%
  pivot_longer(!year,names_to = "tipo",values_to="energia")%>%
  mutate(tipo=
           case_when(tipo=="electricity_generation"~"Generación de electricidad",
                     tipo=="fossil_electricity"~"Electricidad fósil",
                     TRUE ~ "Electricidad renovable"
                     
                     )
           )%>%
  ggplot(.,aes(factor(year),energia, group=tipo))+
  geom_line(aes(color=tipo),size=1.5)+
  scale_color_manual("Tipo",values = c("#285c4d", "#b38e5d","#9d2449"))+
  theme_minimal() +
  labs(title="México. Generación de electricidad total,
por vía fósil y renovable, 1985-2021",
       subtitle = "(Terawatt-hora)",
       x="Año",
       y="Terawatt-hora",
       caption = "Fuente: @claudiodanielpc con datos de Our Wolrd in Data: https://github.com/owid/energy-data")+
  theme(text=element_text(family="Century Gothic"),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0, size=30,face="bold", color="#285c4d"),
        plot.subtitle = element_text(hjust = 0, size=20, face="italic", color="#285c4d"),
        plot.caption = element_text(hjust = 0,size=1, color="#285c4d", face="bold"),
        axis.title = element_text(color="#285c4d"),
        axis.text = element_text(face="bold", size=12, color="#285c4d"),
        axis.text.x=element_text(angle = 90)
        
  )



#Salvar gráfico====
ggsave("day_6_16/day_6_16.png",
       height=7,width=12,bg = "white")
