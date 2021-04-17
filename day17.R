###Script #30DayChartChallenge
#Día 17
#Pop culture

##Borrar datos del entorno
rm(list=ls())

readlin
#Directorio de trabajo
setwd("C:/Users/ALIENWARE/Documents/GitHub/30DayChartChallenge")


# Librerías ====
if(!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse, extrafont,
               rvest, tm, ggwordcloud )


#URL canción: Wannabe de Spice Girls
url<-"http://www.songlyrics.com/spice-girls/wannabe-instrumental-lyrics/"


# Extraer letra de canción
lyrics<-
  tibble(
    lyrics = 
      url %>%
      read_html() %>% 
      html_nodes("#songLyricsDiv") %>%
      html_text())%>%
  gsub(pattern = "\n", replacement = " ")



#Pasar a corpus y trabajar con la letra
corp <- Corpus(VectorSource(lyrics))%>%
  tm_map(.,content_transformer(tolower))%>%
tm_map(.,removeWords, stopwords("english"))%>%
  tm_map(., removePunctuation)%>%
  tm_map(., stripWhitespace)


##Contar palabras
dtm <- TermDocumentMatrix(corp)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
frame<- data.frame(word = names(v),freq=v)

#Wordcloud

frame%>%
ggplot(., aes(label = word, size = freq,
              color=freq)) +
geom_text_wordcloud()+
  scale_size_area(max_size = 40)+
  theme_minimal()+
  scale_color_gradient(low = "darkred", 
                       high = "red")+
  labs(title="Spice Girls. Wannabe",
       subtitle="One of the most popular pop songs of the 90s",
       caption="
Source: @claudiodanielpc using the lyrics from songlyrics.com")+
theme(plot.title = element_text(hjust = 0, size=35,face="bold"),
      plot.subtitle = element_text(hjust = 0, size=30, face="italic"),
      plot.caption = element_text(hjust = 0,size=25),
      plot.background=element_rect(fill = "#bcbddc"),
      text=element_text("Ink Free"))


#Guardar gráfico
ggsave("day17.png", height=10, width=20, units='in', dpi=300)