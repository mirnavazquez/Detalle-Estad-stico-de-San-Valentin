######## DETALLE ESTADISTICO DE SAN VALENTIN #####
############### R LADIES XALAPA ##################
#### Presentado por Ameyalli Perea 12-02-2022 ####


# Basado en el trabajo de Saul Buentello https://medium.com/swlh/chat-analysis-on-whatsapp-part-1-text-analysis-and-data-visualization-with-r-3a7e4e8362f2


# 1. Borrar datos anteriores
rm(list=ls())


# 2. Cambiar directorio
getwd()
setwd("G:/Mi unidad/Ameyalli/Rladies 12-02-22")
getwd()


# 3. Para ver la version de R 
R.version



# 4. Instalar librerias
install.packages('rwhatsapp')
install.packages('lubridate')
install.packages('tidyverse')
install.packages('tidytext')
install.packages('kableExtra')
install.packages('RColorBrewer')
install.packages('knitr')

# 4.1 Llamar librerias
library(rwhatsapp)
library(lubridate)
library(tidyverse)
library(tidytext)
library(kableExtra)
library(RColorBrewer)
library(knitr)


# 5. Se lee el archivo
miChat <- rwa_read('Chat de WhatsApp con Mago.txt')



# 6. Formato de la tabla
miChat %>% 
  head(10) %>%
  kable() %>% 
  kable_styling(font_size = 10)



# 7. An�lisis por estaciones del a�o
miChat <- miChat %>% 
  mutate(day = date(time)) %>% 
  mutate(
    # SEGMENTACI�N POR MES
    estacion = case_when(
      day >= dmy(29022020) & day <= dmy(21032020) ~ "Invierno 2020",
      day >= dmy(22032020) & day <= dmy(21062020) ~ "Primavera 2020",
      day >= dmy(22062020) & day <= dmy(22092020) ~ "Verano 2020",
      day >= dmy(23092020) & day <= dmy(21122020) ~ "Oto�o 2020",
      
      day >= dmy(22122020) & day <= dmy(21032021) ~ "Invierno 2021",
      day >= dmy(22032021) & day <= dmy(21062021) ~ "Primavera 2021",
      day >= dmy(22062021) & day <= dmy(22092021) ~ "Verano 2021",
      day >= dmy(23092021) & day <= dmy(21122021) ~ "Oto�o 2021",
      day >= dmy(22122021) ~ "Invierno 2022"
    )
  ) %>% 
  mutate( estacion = factor(estacion) ) %>% 
  filter(!is.na(author))


# 8. Definir la paleta de colores de las graficas
paleta.estaciones <- brewer.pal(9,"Set1")[c(7,5,1,3,4,2,6,8,9)]


# 9. VERIFICANDO CU�NTOS MENSAJES SE ENVIARON DURANTE EL PERIODO DE TIEMPO
miChat %>% 
  group_by(estacion) %>% 
  count(day) %>%
  ggplot(aes(x = day, y = n, fill=estacion)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values=paleta.estaciones) +
  ylab("N�mero de mensajes") + xlab("Fecha") +
  ggtitle("Mensajes por d�a", "Frecuencia de mensajes por estaci�n del a�o con Maguito") +
  theme_minimal() +
  theme( legend.title = element_blank(), 
         legend.position = "bottom")

ggplotly()








# 10. MENSAJES POR D�A DE LA SEMANA
miChat %>% 
  mutate( wday.num = wday(day),
          wday.name = weekdays(day)) %>% 
  group_by(estacion, wday.num, wday.name) %>% 
  count() %>% 
  ggplot(aes(x = reorder(wday.name, -wday.num), y = n, fill=estacion)) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(values=paleta.estaciones) +
  ylab('') + xlab('') +
  coord_flip() +
  ggtitle('N�mero de mensajes por d�a de la semana ft China', 'Frecuencia de mensajes 2020-2021-2022') +
  theme_minimal() +
  theme( legend.title = element_blank(), 
         legend.position = 'bottom')


# 11. Patrones de la semana
# no sirve si el horario es de 12hrs
# MANTENER EL ORDEN DE D�AS DE LA SEMANA Y RENOMBRARLOS
diasemana <- c('domingo','lunes','martes','mi�rcoles','jueves','viernes','s�bado')
names(diasemana) <- 1:7
# MENSAJES POR HORA DEL D�A
miChat %>% 
  mutate( hour = hour(time), 
          wday.num = wday(day),
          wday.name = weekdays(day)) %>% 
  count(estacion, wday.num, wday.name, hour) %>% 
  ggplot(aes(x = hour, y = n, fill=estacion)) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(values=paleta.estaciones) +
  ylab('N�mero de mensajes') + xlab('Horario') +
  ggtitle('N�mero de mensajes a lo largo del d�a ft China', 'Frecuencia de mensajes 2020-2021-2022') +
  facet_wrap(~wday.num, ncol=7, labeller = labeller(wday.num=diasemana))+
  theme_minimal() +
  theme( legend.title = element_blank(), 
         legend.position = 'bottom',
         panel.spacing.x=unit(0.0, 'lines'))


# 12. Frecuencia por usuarios
# CAMBIEMOS EL NOMBRE DE LOS USUARIOS POR CONFIDENCIALIDAD
levels(miChat$author)[2] <- 'Maguito'
levels(miChat$author)[1] <- 'Ame'
# MENSAJES POR USUARIO
miChat %>%
  mutate(day = date(time)) %>%
  group_by(estacion) %>% 
  count(author) %>% 
  ggplot(aes(x = reorder(author, n), y = n, fill=estacion)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values=paleta.estaciones) +
  ylab("N�mero total de mensajes") + xlab("Usuario") +
  coord_flip() +
  ggtitle("N�mero total de mensajes por usuario.", "�Qui�n es m�s comunicativo? �Qui�n escribe m�s?") +
  theme_minimal() +
  theme( legend.title = element_blank(), 
         legend.position = "bottom")






##### Puntos 13, 14, 15 NO funcionan solo referencia
# 13.
# LIBRER�A PARA FETCH DE IMAGEN PNG DE EMOJI DESDE https://abs.twimg.com
library("ggimage")
# EMOJI RANKING
plotEmojis <- miChat %>% 
  unnest(emoji, emoji_name) %>% 
  mutate( emoji = str_sub(emoji, end = 1)) %>% 
  mutate( emoji_name = str_remove(emoji_name, ':.*')) %>% 
  count(emoji, emoji_name) %>% 
  
  # PLOT TOP 30 EMOJIS
  top_n(30, n) %>% 
  arrange(desc(n)) %>% 
  # CREA UNA URL DE IMAGEN CON EL UNICODE DE EMOJI
  mutate( emoji_url = map_chr(emoji, 
                              ~paste0( 'https://abs.twimg.com/emoji/v2/72x72/', as.hexmode(utf8ToInt(.x)),'.png')) 
  )
# PLOT DEL RANKING DE EMOJIS M�S USADOS
plotEmojis %>% 
  ggplot(aes(x=reorder(emoji_name, n), y=n)) +
  geom_col(aes(fill=n), show.legend = FALSE, width = .2) +
  geom_point(aes(color=n), show.legend = FALSE, size = 3) +
  #geom_image(aes(image=emoji_url), size=.04)+
  scale_fill_gradient(low='#2b83ba',high='#d7191c') +
                      scale_color_gradient(low='#2b83ba',high='#d7191c') +
                                           ylab('N�mero de veces que el emoji fue usado') +
                                             xlab('Emoji y significado') +
                                             ggtitle('Emojis m�s utilizados de manera general', 'Emojis m�s usados por los 2') +
                                             coord_flip() +
                                             theme_minimal() +
                                             theme()



# 14.
# EMOJI RANK POR USUARIO
plotEmojis <- miChat %>%
  unnest(emoji, emoji_name) %>%
  mutate( emoji = str_sub(emoji, end = 1)) %>% # 
  count(author, emoji, emoji_name, sort = TRUE) %>%
  # PLOT DEL TOP 8 EMOJIS POR USUARIO
  group_by(author) %>%
  top_n(n = 8, n) %>%
  slice(1:8) %>% 
  # CREA UNA URL DE IMAGEN CON EL UNICODE DE EMOJI
  mutate( emoji_url = map_chr(emoji, 
                              ~paste0("https://abs.twimg.com/emoji/v2/72x72/",as.hexmode(utf8ToInt(.x)),".png")) )



# 15.
# PLOT DE LA DATA
plotEmojis %>% 
  ggplot(aes(x = reorder(emoji, -n), y = n)) +
  geom_col(aes(fill = author, group=author), show.legend = FALSE, width = .20) +
  # USAR PARA HACER FETCH DE UNA IMAGEN PNG DE EMOJI https://abs.twimg.com
  geom_image(aes(image=emoji_url), size=.13) +
  ylab("N�mero de veces que se us� el emoji") +
  xlab("Emoji") +
  facet_wrap(~author, ncol = 5, scales = "free") +
  ggtitle("Emojis m�s usados en la conversaci�n, por usuario") +
  theme_minimal() +
  theme(axis.text.x = element_blank())




