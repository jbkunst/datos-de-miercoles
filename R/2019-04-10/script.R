library(tidyverse)
library(ggridges)
library(hrbrthemes)
library(ggrepel)
library(ggforce)

theme_set(theme_ipsum_rc())

datos <- read_tsv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-04-10/partidos.txt")

datos <- datos %>% 
  mutate(
    mundial = str_c(anfitrion, " '", anio),
    goles_totales = equipo_1_final + equipo_2_final,
    partido = str_c(equipo_1, equipo_1_final, "-", equipo_2_final, equipo_2, sep = " ")
  ) %>% 
  # variable auxiliar para geom_mark_rec
  group_by(anio) %>% 
  mutate(max_goles_mundial = max(goles_totales)) %>% 
  ungroup() %>% 
  mutate(
    grupo = case_when(
      anio > 1986 & goles_totales == max_goles_mundial ~ 1,
      anio >= 2010 & (1 <= goles_totales) & (goles_totales <= 3) ~ 2,
      TRUE ~ 0
      ),
    descripcion = case_when(
      grupo == 1 ~ "En los últimos 30 años\nla máxima cantidad de goles\nha ido a la baja",
      grupo == 2 ~ "Últimamente, la mayoría\nde los partidos tienen\nentre 1 y 3 goles",
      TRUE ~ ""
      )
    ) 

datosg <- datos %>% 
  count(anio, mundial, goles_totales) 

# datos para etiquetas
datos_partidos <- datosg %>% 
  filter(n == 1, goles_totales >= 10) %>% 
  semi_join(datos, ., by = c("anio", "goles_totales"))

formato_anio_mundial <- function(x  = 1923){
  
  datosg %>% 
    distinct(mundial, .keep_all = TRUE) %>% 
    filter(anio == x) %>% 
    pull(mundial)
  
}
  
ggplot(datosg, aes(anio, goles_totales)) +
  stat_smooth(geom = "line", data = datos, alpha = 0.1, size = 6, color = "blue", span = 0.5) +
  geom_point(aes(size = n, color = n), alpha = 0.5) +
  scale_color_viridis_c(option = "B", begin = 0.2, end = 0.8, direction = -1) +
  scale_size_continuous(limits = c(1, NA), range = c(2, 5)) +
  scale_y_continuous(labels = as.integer, breaks = 0:12, minor_breaks = NULL, limits = c(0, 14)) +
  scale_x_continuous(
    breaks = pull(distinct(datosg, anio)),
    labels = formato_anio_mundial, 
    minor_breaks = NULL,
    limits = c(NA, 2040)) +
  geom_text(
    aes(xc, yc, label = lbl),
    data = tibble(
      lbl = "Debido a eventos históricos, como que Italia\n se encontraba bajo un régimen fascista\ny la II Guerra Mundial, no hubieron mundiales entre 1938 y 1950",
      xc = 1944,
      yc = 5  
    ),
    size = 3,
    family = "Roboto Condensed",
    angle = 90,
    color = "gray50"
    ) +
  geom_label_repel(
    aes(label = partido), data = datos_partidos,
    nudge_y = 1,
    nudge_x = -1,
    family = "Roboto Condensed",
    force = 15,
    size = 3,
    label.r = 0.05,
    color  = "gray50",
    seed = 1235
    ) +
  geom_mark_rect(
    aes(fill = notar, label = descripcion, group = grupo, filter = grupo != 0), 
    label.colour = "gray50",
    label.fontface = "plain",
    label.family = "Roboto Condensed",
    label.fontsize = 8,
    data = datos,
    radius = unit(1,"mm"),
    fill = "blue",
    alpha = 0.05,
    color = "transparent"
    ) +
  theme(
    axis.text.x = element_text(angle = 40, hjust = 1, size = 8),
    legend.position = "none",
    panel.grid.major = element_line(color = "gray90", size = 0.7, linetype = 2),
    axis.title.y = element_text(hjust = 0.5)
    ) +
  labs(
    title = "Goooooles por partido!!!!!",
    subtitle = "Cada punto representa la cantidad de partidos con dicha cantidad de goles",
    caption = "#DatosDeMiercoles por @jbkunst\njkunst.com",
    x = NULL,
    y = "Cantidad de goles"
    )
  

ggsave(filename = "R/2019-04-10/imagen_1.png", width = 16, height = 9)
  