######## DETALLE ESTADISTICO DE SAN VALENTIN #####
############### R LADIES XALAPA ##################
#### Presentado por Ameyalli Perea 12-02-2022 ####


# Basado en el trabajo de Reuben Joseph


# 1. Borrar datos anteriores
rm(list=ls())

# 2. Cambiar directorio
getwd()
setwd("G:/Mi unidad/Ameyalli/Rladies 12-02-22")
getwd()

install.packages("stopwords")
install.packages("wordcloud")
install.packages("tm")
install.packages("SnowballC")
install.packages("wordcloud")
install.packages("RColorBrewer") 


# 3. Llamar librerias
library(rwhatsapp)
library(lubridate)
library(tidyverse)
library(tidytext)
library(kableExtra)
library(RColorBrewer)
library(knitr)
library(stopwords)
library(wordcloud)
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer") 


# 4. Se lee el archivo
miChat <- rwa_read('Chat de WhatsApp con Mago.txt')


# Opcional: Cargar lista de palabras vacias
palabras_vacias <- read.table(file = "esp.txt", header = TRUE)

palabras_vacias


# 5. Palabras vacias de acuerdo al chat
#plain_chat<-rwa_read("Chat de WhatsApp con Mago.txt") %>% mutate(count_character= nchar(text), words= nchar(gsub('[^ ]+', '',text))+1)

to_remove <- c(stopwords("es"), "que","ke", "k", "multimedia", "voy", "omitido", "si",
               "no", "aveces", "a","veces", "allá", "sé", "mañana", "tal", "ay", "ah", "oye", "puedes",
               "puede", "cómo", "como", "aún", "aun", "sé", "ve", "mira", "pues", "así", "asi", "va", "dijo", "se",
               "okey", "ok", "okas", "https", "dice", "digo", "pues", "1", "2", "3", "4", "5", "6",
               "7", "8", "9", "0", "30", "2022")

# 6. Quitar palabras vacias

chat_clean <- miChat %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

chat_clean <- chat_clean %>%
  na.omit(chat_clean)

chat_clean <- chat_clean %>%
  filter(!word %in% to_remove)


# 7. Construye el dataframe de datos "limpios"
df<- miChat %>% unnest_tokens(input = text, output = word) %>% filter(!word %in% to_remove) %>% count(word, sort = TRUE) 

# 8. Construcion de la nube de palabras
set.seed(1234) # for reproducibility 
wordcloud(words = df$word, freq = df$n, min.freq = 50,   
          max.words=200, random.order=TRUE,      
          colors=brewer.pal(8, "Dark2"))

