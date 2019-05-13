library(rvest)
library(tidyverse)

# DATA --------------------------------------------------------------------
# Creditos y agradecimientos a @karbartolome
# Web scrapping de datos históricos
urls <- c("classif010197.htm", "classif250198.htm", "classif010199.htm", 
          "classif250100.htm", "classif300101.htm", "classif040202.htm", 
          "classif310103.htm", "classif300104.htm", "classif310105.htm", 
          "classif310106.htm", "classif310107.htm", "classif310108.htm", 
          "classif310109.htm", "classif310110.htm", "classif310111.htm", 
          "classif310112.htm", "classif010113.htm", "classif010114.htm", 
          "classif010115.htm", "classif010116.htm", "classif010117.htm", 
          "classif010118.htm", "classif010119.htm")

url_base <- "http://archive.ipu.org/wmn-e/arc/"

anios <- seq(1997, 2019, by = 1)

urls[anios == 2000]

# map2_df: itera por dos vectores a pares, y luego unoe con bind_rows/rbind
data <- map2_df(urls, anios, function(url = "classif010197.htm", anio = 2010) {
  
  url_parlamento <- str_c(url_base, url)
  
  message(url_parlamento, " ", anio)
  
  data_parlamento <- read_html(url_parlamento) %>% 
    html_table(fill = TRUE) %>% 
    nth(3) %>% # obtiene la 3ra tabla
    tbl_df()
  
  fila1 <- data_parlamento %>% 
    pull(1) %>% 
    first() %>% 
    {. == "WORLD CLASSIFICATION"}
  
  if(fila1) {
    # elimino primera fila
    data_parlamento <- filter(data_parlamento, row_number() != 1)
  }
  
  nombres <- data_parlamento %>% 
    filter(row_number() %in% c(1, 2)) %>% 
    summarise_all(~ .x %>% unique %>% str_c(collapse = " ")) %>% 
    t() %>% 
    as.vector()
  
  data_parlamento <- data_parlamento %>% 
    filter(row_number() >= 3) %>% 
    set_names(nombres) %>% 
    janitor::clean_names() %>% 
    mutate_all(str_remove_all, "---|\\*|\\?|\"|%") %>% 
    mutate_at(vars(matches("elections")), str_extract, "[0-9]{4}") %>% 
    mutate_at(vars(matches("lower|women|percent")), str_extract, "[0-9]+") %>% 
    mutate_all(parse_guess) %>% 
    mutate(anio_data = anio) %>% 
    mutate(
      rank_lower = rank(replace_na(lower_or_single_house_percent_w, 0), ties.method = "min"),
      rank_upper = rank(replace_na(upper_house_or_senate_percent_w, 0), ties.method = "min"),
    )
  
  data_parlamento
  
})


# LIMPIEZA ----------------------------------------------------------------
data <- data %>%
  filter(anio_data != 2000)


# AED ---------------------------------------------------------------------
ggplot(data) +
  geom_line(aes(anio_data, rank, group = country)) +
  scale_y_reverse()

