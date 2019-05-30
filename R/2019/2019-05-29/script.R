# paquetes ----------------------------------------------------------------
library(tidyverse)
library(ggmap)
library(ggalt)
library(gganimate)
library(lubridate)

# datos -------------------------------------------------------------------
world <- map_data("world")

world <- world %>%
  as_tibble() %>% 
  filter(region != "Antarctica")

terremotos <- read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-05-29/terremotos.csv")

glimpse(terremotos)


p <- ggplot(data = terremotos) +
  geom_polygon(
    data = world,
    aes(long, lat, group = group),
    color = "gray20", fill = "gray10",
    size = 0.25
  ) +
  geom_point(
    aes(longitud, latitud, size = magnitud, color = profundidad),
    color = "white", alpha = 0.2
  ) +
  coord_proj("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") +
  theme_void() +
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = "black")
  )

p


p +
  transition_reveal(fecha)

