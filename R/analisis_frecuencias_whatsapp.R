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



# 7. Análisis por estaciones del año
miChat <- miChat %>% 
  mutate(day = date(time)) %>% 
  mutate(
    # SEGMENTACIÓN POR MES
    estacion = case_when(
      day >= dmy(29022020) & day <= dmy(21032020) ~ "Invierno 2020",
      day >= dmy(22032020) & day <= dmy(21062020) ~ "Primavera 2020",
      day >= dmy(22062020) & day <= dmy(22092020) ~ "Verano 2020",
      day >= dmy(23092020) & day <= dmy(21122020) ~ "Otoño 2020",
      
      day >= dmy(22122020) & day <= dmy(21032021) ~ "Invierno 2021",
      day >= dmy(22032021) & day <= dmy(21062021) ~ "Primavera 2021",
      day >= dmy(22062021) & day <= dmy(22092021) ~ "Verano 2021",
      day >= dmy(23092021) & day <= dmy(21122021) ~ "Otoño 2021",
      day >= dmy(22122021) ~ "Invierno 2022"
    )
  ) %>% 
  mutate( estacion = factor(estacion) ) %>% 
  filter(!is.na(author))


# 8. Definir la paleta de colores de las graficas
paleta.estaciones <- brewer.pal(9,"Set1")[c(7,5,1,3,4,2,6,8,9)]


# 9. VERIFICANDO CUÁNTOS MENSAJES SE ENVIARON DURANTE EL PERIODO DE TIEMPO
miChat %>% 
  group_by(estacion) %>% 
  count(day) %>%
  ggplot(aes(x = day, y = n, fill=estacion)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values=paleta.estaciones) +
  ylab("Número de mensajes") + xlab("Fecha") +
  ggtitle("Mensajes por día", "Frecuencia de mensajes por estación del año con Maguito") +
  theme_minimal() +
  theme( legend.title = element_blank(), 
         legend.position = "bottom")

ggplotly()








# 10. MENSAJES POR DÍA DE LA SEMANA
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
  ggtitle('Número de mensajes por día de la semana ft China', 'Frecuencia de mensajes 2020-2021-2022') +
  theme_minimal() +
  theme( legend.title = element_blank(), 
         legend.position = 'bottom')


# 11. Patrones de la semana
# no sirve si el horario es de 12hrs
# MANTENER EL ORDEN DE DÍAS DE LA SEMANA Y RENOMBRARLOS
diasemana <- c('domingo','lunes','martes','miércoles','jueves','viernes','sábado')
names(diasemana) <- 1:7
# MENSAJES POR HORA DEL DÍA
miChat %>% 
  mutate( hour = hour(time), 
          wday.num = wday(day),
          wday.name = weekdays(day)) %>% 
  count(estacion, wday.num, wday.name, hour) %>% 
  ggplot(aes(x = hour, y = n, fill=estacion)) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(values=paleta.estaciones) +
  ylab('Número de mensajes') + xlab('Horario') +
  ggtitle('Número de mensajes a lo largo del día ft China', 'Frecuencia de mensajes 2020-2021-2022') +
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
  ylab("Número total de mensajes") + xlab("Usuario") +
  coord_flip() +
  ggtitle("Número total de mensajes por usuario.", "¿Quién es más comunicativo? ¿Quién escribe más?") +
  theme_minimal() +
  theme( legend.title = element_blank(), 
         legend.position = "bottom")






##### Puntos 13, 14, 15 NO funcionan solo referencia
# 13.
# LIBRERÍA PARA FETCH DE IMAGEN PNG DE EMOJI DESDE https://abs.twimg.com
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
# PLOT DEL RANKING DE EMOJIS MÁS USADOS
plotEmojis %>% 
  ggplot(aes(x=reorder(emoji_name, n), y=n)) +
  geom_col(aes(fill=n), show.legend = FALSE, width = .2) +
  geom_point(aes(color=n), show.legend = FALSE, size = 3) +
  #geom_image(aes(image=emoji_url), size=.04)+
  scale_fill_gradient(low='#2b83ba',high='#d7191c') +
                      scale_color_gradient(low='#2b83ba',high='#d7191c') +
                                           ylab('Número de veces que el emoji fue usado') +
                                             xlab('Emoji y significado') +
                                             ggtitle('Emojis más utilizados de manera general', 'Emojis más usados por los 2') +
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
  ylab("Número de veces que se usó el emoji") +
  xlab("Emoji") +
  facet_wrap(~author, ncol = 5, scales = "free") +
  ggtitle("Emojis más usados en la conversación, por usuario") +
  theme_minimal() +
  theme(axis.text.x = element_blank())




