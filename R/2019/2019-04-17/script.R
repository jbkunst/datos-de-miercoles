library(tidyverse)
library(hrbrthemes)

theme_set(theme_void() + theme_ipsum_rc())

get_reg_poly_coords <- function(sides = 5, radius = 1, x0 = 0, y0 = 0) {
  # https://stackoverflow.com/a/7198179/829971
  x <- radius * cos(2*pi*(1:sides)/sides) + x0
  y <- radius * sin(2*pi*(1:sides)/sides) + y0
  
  return(tibble(x, y))
  
}

ggplot(data = get_reg_poly_coords(), aes(x, y)) +
  geom_path() +
  geom_point()

tiempo_pantalla <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-04-17/tiempo_pantalla.csv")
cambio_lealtades <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-04-17/cambio_lealtades.csv")
personajes_libros <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-04-17/personajes_libro.csv")


glimpse(cambio_lealtades)

lealtades_largo <- cambio_lealtades %>% 
  rename(fin_t0 = lealtad_inicial) %>%
  select(-episodios) %>% 
  gather(tiempo, lealtad, -nombre, -origen) %>% 
  arrange(tiempo, lealtad, nombre) %>% 
  select(tiempo, lealtad, nombre) %>% 
  mutate(tiempo = as.numeric(str_extract(tiempo, "\\d+"))) %>% 
  group_by(lealtad, tiempo) %>% 
  ungroup()


# para cada lealtad debemos conocer su posicion
lealtad_pos <- lealtades_largo %>% 
  count(lealtad, sort = TRUE) %>% 
  select(-n)

lealtad_pos <- lealtad_pos %>% 
  nrow() %>% 
  get_reg_poly_coords(., radius = 100) %>% 
  bind_cols(lealtad_pos, .)

lealtad_pos

# para cada lealtad/tiempo debemos conocer su tamaño
lealtad_pos_tamaño <- lealtades_largo %>% 
  count(tiempo, lealtad) %>% 
  left_join(lealtad_pos, by = "lealtad")

ggplot(lealtad_pos_tamaño) +
  geom_point(aes(x, y, size = n)) +
  facet_wrap(vars(tiempo))

# para cada personaje debemos obtener su posicion en cada tiempo
lealtades_pos <- lealtades_largo %>% 
  count(tiempo, lealtad) %>% 
  mutate(coordenadas = map2(n, n/10, get_reg_poly_coords)) %>% 
  unnest() %>% 
  select(-tiempo, -lealtad, -n) %>% 
  bind_cols(lealtades_largo, .) %>% 
  left_join(lealtad_pos_tamaño, by = c("tiempo", "lealtad"),  suffix = c(".personaje", ".lealtad")) %>% 
  mutate(
    x = x.personaje + x.lealtad,
    y = y.personaje + y.lealtad
  )

library(gganimate)

ggplot() +
  geom_point(aes(x, y, size = n/5), data = lealtad_pos_tamaño, color = "darkred") +
  geom_point(aes(x, y), data = lealtades_pos, color = "darkblue") +
  coord_fixed() +
  labs(title = 'Temporada: {frame_time}') +
  transition_time(tiempo) +
  ease_aes('sine-in-out')

