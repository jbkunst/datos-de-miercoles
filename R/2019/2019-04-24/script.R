library(tidyverse)

gapminder <- read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-04-24/gapminder_es.csv")



gapminder %>% 
  group_by(anio) %>% 
  mutate_at(vars(esperanza_de_vida, poblacion, pib_per_capita), ~ (.x - mean(.x))/sd(.x)) %>% 
  ungroup() %>% 
  gather(variable, valor, -pais, -continente, -anio) %>% 
  unite(var, c("anio", "variable")) %>% 
  spread(var, valor)

