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

terremotos <- terremotos %>%
  filter(tipo == "terremoto") %>% 
  mutate(energia = 10^(11.8 + 1.5 *magnitud))

glimpse(terremotos)

terremotos <- terremotos %>%
  summarise(fecha_min = min(fecha), fecha_max = max(fecha)) %>%
  gather() %>%
  pull(value) %>%
  {seq(.[1], .[2], by = "days")} %>%
  tibble(fecha = .) %>%
  full_join(terremotos, by = "fecha") %>% 
  mutate_if(is.numeric, replace_na, 0)

terremotos2 <- terremotos %>% 
  filter(year(fecha) == max(year(fecha)))
 
p <- ggplot(data = terremotos2) +
  geom_polygon(
    data = world,
    aes(long, lat, group = group),
    color = "gray20", fill = "gray10",
    size = 0.25
  ) +
  geom_point(
    aes(longitud, latitud, size = energia, color = profundidad),
    color = "white", alpha = 0.2
  ) +
  coord_proj("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") +
  theme_void() +
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = "black")
  )

p

panim <- p +
  transition_states(fecha, wrap = F, transition_length = 0) +
  shadow_wake(0.4)

nd <- terremotos2 %>% 
  distinct(fecha) %>% 
  nrow()

animate(panim, nframes = nd)
