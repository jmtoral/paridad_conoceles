
# Biblioteca --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


pacman::p_load(tidyverse, readxl, tidytext)


# Datos -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


data <- read_excel("input/data/baseDatosCandidatos.xls")


# Limpieza ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


new_stop = c("así", "será", "obteniendo", "ser", 1:9, "asi", "quintana", "roo", "sí", "tamaulipecos",
             "si", "oaxaca", "aguascalientes", "tamaulipas", "hidalgo", "toda",
             "manera", "seguir", "mayor", "mas", "mismas", "mientras", "propone",
             "buscará", "cuentan", "deben", "ciento")



# Exploración -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

data |> 
  filter(str_detect(TIPO_CANDIDATO, "PROPI")) |> 
  count(SEXO, CARGO) |> 
  ggplot(aes(SEXO, n, fill=SEXO)) +
  geom_col() +
  guides(fill="none") +
  facet_wrap(~CARGO)+
  labs(title="Número de personas propietarias",
       subtitle = "por identidad de género y tipo de cargo",
       caption = "INE - Conóceles",
       y="Frecuencia") +
  hrbrthemes::theme_ipsum(grid="Y")
  


data |> 
  filter(str_detect(TIPO_CANDIDATO, "PROPI")) |> 
  ggplot(aes(EDAD, SEXO, fill=SEXO)) +
  geom_boxplot(alpha=0.4)+
  geom_jitter(height = 0.2, alpha = 0.75) +
  facet_wrap(~CARGO)+
  guides(fill="none") +
  labs(title="Edad de personas propietarias",
       subtitle = "por identidad de género y tipo de cargo",
       caption = "INE - Conóceles",
       y="Frecuencia") +
  hrbrthemes::theme_ipsum(grid="X")

  

data |> 
  filter(str_detect(TIPO_CANDIDATO, "PROPI")) |> 
  count(SEXO, ESCOLARIDAD) |> 
  ggplot(aes(SEXO, n, fill=SEXO)) +
  geom_col() +
  guides(fill="none") +
  facet_wrap(~ESCOLARIDAD)+
  labs(title="Número de personas propietarias",
       subtitle = "por identidad de género y escolaridad",
       caption = "INE - Conóceles",
       y="Frecuencia") +
  hrbrthemes::theme_ipsum(grid="Y")


data |> 
  filter(str_detect(TIPO_CANDIDATO, "PROPI")) |> 
  count(SEXO, ESCOLARIDAD) |> 
  ggplot(aes(SEXO, n, fill=SEXO)) +
  geom_col() +
  guides(fill="none") +
  facet_wrap(~ESCOLARIDAD)+
  labs(title="Número de personas propietarias",
       subtitle = "por identidad de género y escolaridad",
       caption = "INE - Conóceles",
       y="Frecuencia") +
  hrbrthemes::theme_ipsum(grid="Y")

data |> 
  filter(str_detect(TIPO_CANDIDATO, "PROPI")) |> 
  count(SEXO, ENTIDAD, CARGO) |> 
  ggplot(aes(SEXO, n, fill=SEXO)) +
  geom_col() +
  guides(fill="none") +
  facet_wrap(CARGO ~ ENTIDAD, scale="free_y")+
  labs(title="Número de personas propietarias",
       subtitle = "por identidad de género, entidad y cargo",
       caption = "INE - Conóceles",
       y="Frecuencia") +
  hrbrthemes::theme_ipsum(grid="Y")




data |> 
  filter(str_detect(TIPO_CANDIDATO, "PROPI")) |> 
  mutate(PARTIDO_COALICION = 
           str_replace_all(PARTIDO_COALICION, 
                           "FUERZA POR MÉXICO", "FXM")) |> 
  mutate(PARTIDO_COALICION = 
           str_replace_all(PARTIDO_COALICION, 
                           "JUNTOS HACEMOS HISTORIA EN", "JHH"))|> 
  mutate(PARTIDO_COALICION = 
           str_replace_all(PARTIDO_COALICION, 
                           "PARTIDO DEL TRABAJO", "PT")) |> 
  mutate(PARTIDO_COALICION = 
           str_replace_all(PARTIDO_COALICION, 
                           "PARTIDO DE LA REVOLUCIÓN DEMOCRÁTICA", 
                           "PRD")) |> 
  mutate(PARTIDO_COALICION = 
           str_replace_all(PARTIDO_COALICION, 
                           "PARTIDO ACCIÓN NACIONAL", 
                           "PAN")) |> 
  mutate(PARTIDO_COALICION = 
           str_replace_all(PARTIDO_COALICION, 
                           "MOVIMIENTO CIUDADANO", 
                           "MC")) |>
  mutate(PARTIDO_COALICION = 
           str_replace_all(PARTIDO_COALICION, 
                           "PARTIDO REVOLUCIONARIO INSTITUCIONAL", 
                           "PRI")) |> 
  mutate(PARTIDO_COALICION = 
           str_replace_all(PARTIDO_COALICION, 
                           "PARTIDO VERDE ECOLOGISTA DE MÉXICO|PARTIDO VERDE", 
                           "VERDE")) |>
  count(SEXO, PARTIDO_COALICION) |> 
  ggplot(aes(SEXO, n, fill=SEXO)) +
  geom_col() +
  guides(fill="none") +
  facet_wrap(~PARTIDO_COALICION, scale="free_y")+
  labs(title="Número de personas propietarias",
       subtitle = "por identidad de género y tipo de cargo",
       caption = "INE - Conóceles",
       y="Frecuencia") +
  hrbrthemes::theme_ipsum(grid="Y")


# Tokens ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


## Por identidad de género -------------------------------------------------------------------------------------------------------------------------------------------------------------------------

### Frecuencia --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


data |>
  mutate(prop_grp = str_c(PROPUESTA_1, PROPUESTA_2,  sep=" ")) |>
  unnest_tokens(pal, prop_grp) |>
  filter(!pal %in% tm::stopwords("es")) |>
  filter(!pal %in% new_stop) |>
  filter(!str_detect(pal, "[0-9]")) |> 
  filter(!is.na(pal)) |> 
  count(pal, SEXO, sort =T) |> 
  group_by(SEXO) |> 
  top_n(10) |> 
  ungroup() |> 
  ggplot(aes(n, 
             reorder_within(pal,n, SEXO), 
             fill=SEXO))+
  geom_col() +
  guides(fill="none")+
  facet_wrap(~SEXO, scale = "free_y")+
  labs(title="Palabras más frecuentes en propuestas de personas propietarias",
       subtitle = "por identidad de género, sin considerar propuesta de género",
       caption = "INE - Conóceles",
       y="palabra", x="Frecuencia") +
  hrbrthemes::theme_ipsum(grid="X") +
  scale_y_reordered()


data |>
  #mutate(prop_grp = str_c(PROPUESTA_1, PROPUESTA_2,  sep=" ")) |>
  unnest_tokens(pal, PROPUESTA_GENERO) |>
  filter(!pal %in% tm::stopwords("es")) |>
  filter(!pal %in% new_stop) |>
  filter(!str_detect(pal, "[0-9]")) |> 
  filter(!is.na(pal)) |> 
  count(pal, SEXO, sort =T) |> 
  group_by(SEXO) |> 
  top_n(10) |> 
  ungroup() |> 
  ggplot(aes(n, 
             reorder_within(pal,n, SEXO), 
             fill=SEXO))+
  geom_col() +
  guides(fill="none")+
  facet_wrap(~SEXO, scale = "free_y")+
  labs(title="Palabras más frecuentes en propuestas de género de personas propietarias",
       subtitle = "por identidad de género",
       caption = "INE - Conóceles",
       y="palabra", x="Frecuencia") +
  hrbrthemes::theme_ipsum(grid="X") +
  scale_y_reordered()


### TF_IDFs -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

data |>
  mutate(prop_grp = str_c(PROPUESTA_1, PROPUESTA_2,  sep=" ")) |>
  unnest_tokens(pal, prop_grp) |>
  filter(!pal %in% tm::stopwords("es")) |>
  filter(!pal %in% new_stop) |>
  filter(!str_detect(pal, "[0-9]")) |> 
  filter(!is.na(pal)) |> 
  count(pal, SEXO, sort =T) |> 
  bind_tf_idf(pal, SEXO, n) |> 
  arrange(-tf_idf) |> 
  select(pal, SEXO, tf_idf) |> 
  group_by(SEXO) |> 
  top_n(10) |> 
  ungroup() |> 
  ggplot(aes(tf_idf, 
             reorder_within(pal,tf_idf, SEXO), 
             fill=SEXO))+
  geom_col() +
  guides(fill="none")+
  facet_wrap(~SEXO, scale = "free")+
  labs(title="Palabras más representativas en propuestas de personas propietarias",
       subtitle = "por identidad de género, sin considerar propuesta de género",
       caption = "INE - Conóceles",
       y="palabra", x="TF-IDF") +
  hrbrthemes::theme_ipsum(grid="X") +
  scale_y_reordered()


data |>
  #mutate(prop_grp = str_c(PROPUESTA_1, PROPUESTA_2,  sep=" ")) |>
  unnest_tokens(pal, PROPUESTA_GENERO) |>
  filter(!pal %in% tm::stopwords("es")) |>
  filter(!pal %in% new_stop) |>
  filter(!str_detect(pal, "[0-9]")) |> 
  filter(!is.na(pal)) |> 
  count(pal, SEXO, sort =T) |> 
  bind_tf_idf(pal, SEXO, n) |> 
  arrange(-tf_idf) |> 
  select(pal, SEXO, tf_idf) |> 
  group_by(SEXO) |> 
  top_n(10) |> 
  ungroup() |> 
  ggplot(aes(tf_idf, 
             reorder_within(pal,tf_idf, SEXO), 
             fill=SEXO))+
  geom_col() +
  guides(fill="none")+
  facet_wrap(~SEXO, scale = "free")+
  labs(title="Palabras más representativas en propuestas de género de personas propietarias",
       subtitle = "por identidad de género",
       caption = "INE - Conóceles",
       y="palabra", x="TF-IDF") +
  hrbrthemes::theme_ipsum(grid="X") +
  scale_y_reordered()




# Bigrams ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

new_stop = c("así", "será", "obteniendo", "ser", 1:9, "asi", "quintana", "roo", "sí", "tamaulipecos",
             "si", "oaxaca", "aguascalientes", "tamaulipas", "hidalgo", "toda",
             "manera", "seguir", "mismas", "mientras", "propone", "incluyendo",
             "hacerlo","tal","motivo","aun","mas", "puedan", "tener",
             "buscará", "cuentan", "deben", "ciento", "permitiendo", "generando",
             "mediante", "permitirán", "promoviendo", "hacia", "sino", "dos","mil",
             "únicamente", "dando", "vez","impulsar", "permitan", "tomando", "promover")


## Por identidad de género -------------------------------------------------------------------------------------------------------------------------------------------------------------------------

### Frecuencia --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


data |>
  mutate(prop_grp = str_c(PROPUESTA_1, PROPUESTA_2,  sep=" ")) |>
  unnest_tokens(bigram, prop_grp, token="ngrams", n=2) |>
  separate(bigram, c("pal1", "pal2"), sep=" ") |> 
  filter(!pal1 %in% tm::stopwords("es")) |>
  filter(!pal1 %in% new_stop) |>
  filter(!str_detect(pal1, "[0-9]")) |> 
  filter(!is.na(pal1)) |> 
  filter(!pal2 %in% tm::stopwords("es")) |>
  filter(!pal2 %in% new_stop) |>
  filter(!str_detect(pal2, "[0-9]")) |> 
  filter(!is.na(pal2)) |> 
  unite(pal, pal1:pal2, sep=" ") |> 
  count(pal, SEXO, sort =T) |> 
  group_by(SEXO) |> 
  top_n(10) |> 
  ungroup() |> 
  ggplot(aes(n, 
             reorder_within(pal,n, SEXO), 
             fill=SEXO))+
  geom_col() +
  guides(fill="none")+
  facet_wrap(~SEXO, scale = "free_y")+
  labs(title="Bigramas más frecuentes en propuestas de personas propietarias",
       subtitle = "por identidad de género, sin considerar propuesta de género",
       caption = "INE - Conóceles",
       y="palabra", x="Frecuencia") +
  hrbrthemes::theme_ipsum(grid="X") +
  scale_y_reordered()




data |>
  #mutate(prop_grp = str_c(PROPUESTA_1, PROPUESTA_2,  sep=" ")) |>
  unnest_tokens(bigram, PROPUESTA_GENERO, token="ngrams", n=2) |>
  separate(bigram, c("pal1", "pal2"), sep=" ") |> 
  filter(!pal1 %in% tm::stopwords("es")) |>
  filter(!pal1 %in% new_stop) |>
  filter(!str_detect(pal1, "[0-9]")) |> 
  filter(!is.na(pal1)) |> 
  filter(!pal2 %in% tm::stopwords("es")) |>
  filter(!pal2 %in% new_stop) |>
  filter(!str_detect(pal2, "[0-9]")) |> 
  filter(!is.na(pal2)) |> 
  unite(pal, pal1:pal2, sep=" ") |> 
  count(pal, SEXO, sort =T) |> 
  group_by(SEXO) |> 
  top_n(10) |> 
  ungroup() |> 
  ggplot(aes(n, 
             reorder_within(pal,n, SEXO), 
             fill=SEXO))+
  geom_col() +
  guides(fill="none")+
  facet_wrap(~SEXO, scale = "free_y")+
  labs(title="Bigramas más frecuentes en propuestas de género de personas propietarias",
       subtitle = "por identidad de género",
       caption = "INE - Conóceles",
       y="palabra", x="Frecuencia") +
  hrbrthemes::theme_ipsum(grid="X") +
  scale_y_reordered()


### TF_IDFs -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

data |>
  mutate(prop_grp = str_c(PROPUESTA_1, PROPUESTA_2,  sep=" ")) |>
  unnest_tokens(bigram, prop_grp, token="ngrams", n=2) |>
  separate(bigram, c("pal1", "pal2"), sep=" ") |> 
  filter(!pal1 %in% tm::stopwords("es")) |>
  filter(!pal1 %in% new_stop) |>
  filter(!str_detect(pal1, "[0-9]")) |> 
  filter(!is.na(pal1)) |> 
  filter(!pal2 %in% tm::stopwords("es")) |>
  filter(!pal2 %in% new_stop) |>
  filter(!str_detect(pal2, "[0-9]")) |> 
  filter(!is.na(pal2)) |> 
  unite(pal, pal1:pal2, sep=" ") |> 
  count(pal, SEXO, sort =T) |> 
  bind_tf_idf(pal, SEXO, n) |> 
  arrange(-tf_idf) |> 
  select(pal, SEXO, tf_idf) |> 
  group_by(SEXO) |> 
  top_n(10) |> 
  ungroup() |> 
  ggplot(aes(tf_idf, 
             reorder_within(pal,tf_idf, SEXO), 
             fill=SEXO))+
  geom_col() +
  guides(fill="none")+
  facet_wrap(~SEXO, scale = "free")+
  labs(title="Bigramas más representativas en propuestas de personas propietarias",
       subtitle = "por identidad de género, sin considerar propuesta de género",
       caption = "INE - Conóceles",
       y="palabra", x="TF-IDF") +
  hrbrthemes::theme_ipsum(grid="X") +
  scale_y_reordered()


data |>
  #mutate(prop_grp = str_c(PROPUESTA_1, PROPUESTA_2,  sep=" ")) |>
  unnest_tokens(bigram, PROPUESTA_GENERO, token="ngrams", n=2) |>
  separate(bigram, c("pal1", "pal2"), sep=" ") |> 
  filter(!pal1 %in% tm::stopwords("es")) |>
  filter(!pal1 %in% new_stop) |>
  filter(!str_detect(pal1, "[0-9]")) |> 
  filter(!is.na(pal1)) |> 
  filter(!pal2 %in% tm::stopwords("es")) |>
  filter(!pal2 %in% new_stop) |>
  filter(!str_detect(pal2, "[0-9]")) |> 
  filter(!is.na(pal2)) |> 
  unite(pal, pal1:pal2, sep=" ") |> 
  count(pal, SEXO, sort =T) |> 
  bind_tf_idf(pal, SEXO, n) |> 
  arrange(-tf_idf) |> 
  select(pal, SEXO, tf_idf) |> 
  group_by(SEXO) |> 
  top_n(10) |> 
  ungroup() |> 
  ggplot(aes(tf_idf, 
             reorder_within(pal,tf_idf, SEXO), 
             fill=SEXO))+
  geom_col() +
  guides(fill="none")+
  facet_wrap(~SEXO, scale = "free")+
  labs(title="Bigramas más representativas en propuestas de género de personas propietarias",
       subtitle = "por identidad de género",
       caption = "INE - Conóceles",
       y="palabra", x="TF-IDF") +
  hrbrthemes::theme_ipsum(grid="X") +
  scale_y_reordered()






















