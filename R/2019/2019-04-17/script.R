# paquetes, funciones y datos ---------------------------------------------
library(tidyverse)
library(hrbrthemes)
library(gganimate)

get_reg_poly_coords <- function(sides = 5, RADIO = 1, x0 = 0, y0 = 0) {
  # https://stackoverflow.com/a/7198179/829971
  x <- RADIO * cos(2*pi*(1:sides)/sides) + x0
  y <- RADIO * sin(2*pi*(1:sides)/sides) + y0
  
  return(tibble(x, y))
  
}

ggplot(data = get_reg_poly_coords(), aes(x, y)) +
  geom_path() +
  geom_point()

tiempo_pantalla <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-04-17/tiempo_pantalla.csv")
# personajes_libros <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-04-17/personajes_libro.csv")
cambio_lealtades <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-04-17/cambio_lealtades.csv")

glimpse(cambio_lealtades)

count(cambio_lealtades, nombre, sort = TRUE)

# lealtades a formato largo (tidy) ----------------------------------------
lealtades_largo <- cambio_lealtades %>% 
  rename(fin_t0 = lealtad_inicial) %>%
  select(-episodios) %>% 
  gather(temporada, lealtad, -nombre, -origen) %>% 
  arrange(temporada, lealtad, nombre) %>% 
  select(temporada, lealtad, nombre) %>% 
  mutate(
    # Esto es lo que más me llevó temporada
    lealtad = case_when(
      lealtad =="Rey Robert Baratheon" ~ "Baratheon",
      lealtad =="Viserys Targaryen" ~ "Targaryen",
      lealtad =="Rey Joffrey Baratheon" ~ "Lannister",
      lealtad =="Daenerys Targaryen" ~ "Targaryen",
      lealtad =="Guardia de la Noche" ~ "Guardia de la Noche",
      lealtad =="Otra, Westeros" ~ "Westeros",
      lealtad =="Salvajes" ~ "Salvajes",
      lealtad =="Rey Tommen Baratheon" ~ "Lannister",
      lealtad =="Petyr Baelish, Lord Protector del Valle" ~ "El Valle",
      lealtad =="Otra, Essos" ~ "Essos",
      lealtad =="Muerta/o" ~ "Muerta/o",
      lealtad =="Roose Bolton, Lord Guardian del Norte" ~ "Bolton",
      lealtad =="Reina Cersei Lannister" ~ "Lannister",
      lealtad =="Jon Nieve, Rey del Norte" ~ "Stark",
      TRUE ~ lealtad
    ),
    lealtad = iconv(lealtad,from="UTF-8",to="ASCII//TRANSLIT"),
    temporada = as.numeric(str_extract(temporada, "\\d+"))
    ) %>% 
  group_by(lealtad, temporada) %>% 
  ungroup()

count(lealtades_largo, lealtad, sort = TRUE)

lealtades_largo <- filter(lealtades_largo, !is.na(lealtad))

# seleccionamos los nombres que no se repiten
# para eliminar personaje "genericos"
lealtades_largo <- lealtades_largo %>% 
  semi_join(count(cambio_lealtades, nombre, sort = TRUE) %>% filter(n == 1), by = "nombre") %>% 
  # IMPORTANTE PARA QUE NO CAMBIEN DE NOMBRE LOS PUNTOS
  arrange(temporada, nombre, lealtad)


# lealtad: posicion y tamaño ----------------------------------------------
# para cada lealtad debemos conocer su posicion, lo haremos 
# a traves de un alyout de igraph considerando los cambios 
# de una lealtad a otra
library(igraph)

ts <- lealtades_largo %>% 
  distinct(temporada) %>% 
  pull() %>% 
  head(-1)

cambios_temp <- map_df(ts, function(t = 0){
  
  full_join(
    lealtades_largo %>% filter(temporada == t),
    lealtades_largo %>% filter(temporada == t + 1),
    by = "nombre", 
    suffix = c("_anterior", "_actual")
  ) %>% 
    count(from = lealtad_anterior, to = lealtad_actual) %>% 
    filter(complete.cases(.)) %>% 
    mutate(temporada = t)
  
})

cambios <- cambios_temp %>% 
  group_by(from, to) %>% 
  summarise(n = sum(n)) %>% 
  ungroup()

g <- graph_from_data_frame(cambios, directed=TRUE)
plot(g)

# https://igraph.org/r/doc/strength.html
E(g)$weight <- pull(cambios, n)
V(g)$degree <- degree(g)
  
set.seed(123)
plot(g)

# igraph::plot.igraph()
set.seed(123)
layout <- layout_with_fr(g)

lealtades <- tibble(
  lealtad = V(g)$name,
  x = layout[, 2],
  y = layout[, 1],
  degree = degree(g)
  )

lealtades <- lealtades_largo %>% 
  count(lealtad) %>% 
  left_join(lealtades, ., by = "lealtad")

ggplot(lealtades, aes(x, y, color = lealtad, label = lealtad, size = degree)) +
  geom_point() +
  geom_text() +
  scale_size(range = c(3, 6)) +
  theme(legend.position = "none") +
  labs(title = "Layout con igraph")

lealtades <- lealtades %>% 
  mutate(rn = row_number()) %>% 
  arrange(y) %>% 
  mutate(y = seq(1:n())) %>% 
  arrange(x) %>% 
  mutate(x = seq(1:n())) %>% 
  mutate_at(vars(x, y), ~ (.x - mean(.x))/sd(.x))

ggplot(lealtades, aes(x, y, color = lealtad, label = lealtad, size = degree)) +
  geom_point() +
  geom_text() +
  scale_size(range = c(3, 6)) +
  theme(legend.position = "none") +
  labs(title = "Layout obteniendo en cada eje valores equidistantes")


# personajes: posicion ----------------------------------------------------
# para cada personaje debemos obtener su posicion en cada temporada
personajes <- lealtades_largo %>% 
  count(temporada, lealtad) %>%
  mutate(coordenadas = map2(n, 1/nrow(lealtades), get_reg_poly_coords)) %>% 
  unnest() %>% 
  select(-temporada, -lealtad, -n) %>% 
  bind_cols(lealtades_largo, .) %>% 
  left_join(lealtades, by = c("lealtad"),  suffix = c(".personaje", ".lealtad")) %>% 
  mutate(
    x = x.personaje +  x.lealtad,
    y = y.personaje +  y.lealtad
  ) %>% 
  mutate_at(vars(x, y), ~ .x + rnorm(length(.x))/nrow(lealtades))

p <- ggplot() +
  geom_point(aes(x, y, color = lealtad), alpha = 0.5, data = personajes) +
  geom_text(aes(x, y, size = n, label = lealtad), alpha = 0.5, data = lealtades) +
  scale_size_area() +
  scale_color_viridis_d() +
  theme(legend.position = "none")

p

p + facet_wrap(vars(temporada))

# personajes importantes --------------------------------------------------
# segun tiempo en pantalla 
tiempo_pantalla_importantes <- tiempo_pantalla %>% 
  filter(
    # minutos_pantalla >= quantile(minutos_pantalla, .975, na.rm = TRUE) |
      episodios >= quantile(episodios, .95, na.rm = TRUE)
      )

# diseño ------------------------------------------------------------------
library(extrafont)
# se debe instalar la fuente en wintendo
# font_import(paths = "R/2019/2019-04-17/")
loadfonts(device = "win")

FUENTE <- "Game of Thrones"
FUENTE2 <- "Roboto Condensed"
FONDO <- "black"
COLOR1 <- "#959394"
COLOR2 <- "white"
SEED <- 123

# este un parametro para probar animaciones y reducir las
# instancias de tiempo
T_MAX <- 7

p <- ggplot() +
  # personajes importantes etiquetas
  ggrepel::geom_text_repel(
  # geom_text(
    aes(x, y, label = nombre),
    seed = SEED, max.iter = 5000,
    color = COLOR1,
    size = 3,
    family = FUENTE2,
    vjust = "inward", hjust = "inward",
    data = semi_join(personajes, tiempo_pantalla_importantes, by = "nombre") %>% filter(temporada <= T_MAX) %>% arrange(temporada, nombre)
  ) +
  # https://stackoverflow.com/a/34398935/829971
  # personajes puntos principales
  geom_point(
    aes(x, y),
    size = 3,
    alpha = 0.50,
    color = COLOR2,
    stroke = 0,
    shape = 16,
    data = semi_join(personajes, tiempo_pantalla_importantes, by = "nombre") %>% filter(temporada <= T_MAX) %>% arrange(temporada, nombre)
  ) +
  # personajes puntos
  geom_point(
    aes(x, y),
    size = 3,
    alpha = 0.20,
    color = COLOR2,
    stroke = 0,
    shape = 16,
    # position = position_jitter(width = JITTER_FACTOR, height = JITTER_FACTOR, seed = SEED),
    data = anti_join(personajes, tiempo_pantalla_importantes, by = "nombre") %>% filter(temporada <= T_MAX)
  ) +
  # etiquetas lealtades
  geom_text(
    aes(x, y + 3 / nrow(lealtades), label = lealtad, size = degree),
    data = lealtades,
    color = COLOR1,
    alpha = 0.80,
    family = FUENTE
    ) +
  scale_size(range = c(2, 5)) +
  # scale_y_continuous(limits = c(-RADIO*2.5, RADIO*2.5)) +
  # scale_x_continuous(limits = c(-RADIO*2.5, RADIO*2.5)) +
  labs(
    title = "#",
    caption = "#DatosDeMiercoles por @jbkunst\njkunst.com",
    x = NULL,
    y = NULL
    ) +
  theme(
    legend.position = "none",
    panel.border = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    rect = element_rect(fill = FONDO, color = FONDO),
    text  = element_text(family = FUENTE, colour = COLOR1, size = 15),
    plot.title = element_text(family = FUENTE, colour = COLOR1, size = 25),
    plot.subtitle = element_text(family = FUENTE2, colour = COLOR1, size = 13),
    plot.caption = element_text(family = FUENTE2, colour = COLOR1, size = 10),
  )

p
p + facet_wrap(vars(temporada))
# ggsave(plot = p, filename = "R/2019/2019-04-17/test.pdf")

p <- p +
  labs(subtitle = "Cambios de leatades en cada temporada {trunc(frame_time)}") +
  transition_time(temporada) +
  shadow_wake(wake_length = 0.005, alpha = TRUE, exclude_layer = 1) +
  ease_aes("sine-in-out")


if(T_MAX < 7) {
  animate(p, width = 1000, height = 800, duration = 10, fps = 15)  
} else {
  animate(p, width = 1000, height = 800, duration = 8*3, fps = 30)
}
 
# gganimate::anim_save(
#   "R/2019/2019-04-17/GOT-lealtades.gif", p,
#   width = 1000, height = 800, duration = 8*3, fps = 30
# )







