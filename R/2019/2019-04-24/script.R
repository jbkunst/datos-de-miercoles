library(tidyverse)
library(smallvis)
# library(umapr)

gapminder <- read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-04-24/gapminder_es.csv")

gapminder <- gapminder %>% 
  mutate(
    pais = str_remove(pais, "\\(.*\\)"),
    pais = str_trim(pais)
    )

gapminder_wide <- gapminder %>% 
  group_by(anio) %>% 
  mutate_at(vars(esperanza_de_vida, poblacion, pib_per_capita), ~ (.x - mean(.x))/sd(.x)) %>% 
  ungroup() %>% 
  gather(variable, valor, -pais, -continente, -anio) %>% 
  unite(var, c("anio", "variable")) %>% 
  spread(var, valor)

gapminder_wide

gapminder_wide <- gapminder_wide %>% 
  select_if(is.numeric) %>% 
  smallvis::smallvis(max_iter = 10000, method = "tumap", verbose = TRUE) %>%
  as_tibble() %>% 
  bind_cols(gapminder_wide) %>% 
  mutate_at(vars(V1, V2), ~ (.x - mean(.x))/sd(.x)) %>%
  mutate(
    r = sqrt(V1^2 + V2^2),
    s = r >= quantile(r, .90)
    )

ggplot(gapminder_wide, aes(V1, V2)) +
  geom_point(aes(color = continente )) +
  ggrepel::geom_label_repel(aes(label = pais), data = gapminder_wide %>% filter(s))


ggplot(gapminder_wide, aes(V1, V2)) +
  geom_point(aes(color = continente )) +
  ggrepel::geom_label_repel(aes(label = pais), data = gapminder_wide %>% filter(s)) +
  facet_wrap(vars(continente)) 
