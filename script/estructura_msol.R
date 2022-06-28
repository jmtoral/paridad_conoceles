
# Biblioteca --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


pacman::p_load(tidyverse, readxl, tidytext, extrafont, showtext)



# Configuración -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#font_import(paths = "C:/Users/LENOVO/AppData/Local/Microsoft/Windows/Fonts")

font_add_google("Oswald", "osw")
font_add_google("Roboto", "roboto")
font_add_google("Bebas Neue", "bebas")

showtext_auto()

#extrafont::loadfonts(device = "win", quiet = TRUE)


# Theme -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

tema <-  theme_minimal() +
  theme(text = element_text(family="roboto", color = "grey35"),
        plot.title = element_text(size = 24, face = "bold", 
                                  #margin = margin(10,0,20,0), 
                                  family="bebas", color = "grey25"),
        plot.subtitle = element_text(size = 20, face = "bold", colour = "#666666", 
                                     #margin = margin(0, 0, 20, 0), 
                                     family="bebas"),
        strip.text = element_text(size=14, family="roboto", face="bold"),
        plot.caption = element_text(hjust = 1, size = 12,
                                    family = "roboto"),
        panel.grid = element_line(linetype = 1), 
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(size = 12, face = "bold", family="osw"),
        legend.text = element_text(size = 10, family="roboto"),
        legend.title.align = 0.5,
        axis.title = element_text(size = 14, hjust = 0.5, 
                                  face = "bold", margin = margin(0,0,0,0), family="roboto"),
        axis.text = element_text(size = 12, face = "bold", family="roboto"),
        plot.title.position = 'plot')

# Datos -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


data <- read_excel("input/data/baseDatosCandidatos.xls") |> 
  mutate(PARTIDO_COALICION = str_remove_all(PARTIDO_COALICION, "\\.")) |> 
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
                           "PVEM")) |> 
  janitor::clean_names()

prop <- data |> 
  filter(str_detect(tipo_candidato, "PROPI")) |> 
  distinct(num_lista_o_formula, .keep_all = T) |> # La base traía errores en la variable tipo_candidato
  mutate(cargo = factor(cargo, levels=c('GUBERNATURA', 
                                        'PRESIDENCIA MUNICIPAL', 
                                        'DIPUTACIÓN LOCAL RP', 
                                        'DIPUTACIÓN LOCAL MR')))

prop.v <- data |> 
  filter(str_detect(tipo_candidato, "PROPI")) |> 
  group_by(num_lista_o_formula) |> 
  mutate(n = n()) |> 
  filter(n>1) # Confirmado, hay errores de registro

#+write.csv(prop.v, "errores_conoceles.csv")



# Limpieza ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


new_stop = c("así", "será", "obteniendo", "ser", 1:9, "asi", "quintana", "roo", "sí", "tamaulipecos",
             "si", "oaxaca", "aguascalientes", "tamaulipas", "hidalgo", "toda",
             "manera", "seguir", "mayor", "mas", "mismas", "mientras", "propone",
             "buscará", "cuentan", "deben", "ciento")



# Secciones propuestas ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------


## 1. Redes sociales y página web ----------------------------------------------------------------------------------------------------------------------------------------------------------------

redes <- prop |> 
  mutate(pagina_web = ifelse(pagina_web == "No proporcionó", 
                       NA_character_, 
                       pagina_web)) |> 
  mutate(pagina_web_count = ifelse(is.na(pagina_web), 0,1)) |> 
  mutate(redes = ifelse(is.na(redes)| redes=="No proporcionó",
                        NA_character_, redes)) |> 
  mutate(redes_count= ifelse(is.na(redes), 0, str_count(redes, ",")+1)) |> 
  mutate(facebook = str_detect(redes, "facebook") |> as.numeric(),
         instagram = str_detect(redes, "instagram") |> as.numeric(),
         youtube = str_detect(redes, "youtube") |> as.numeric(),
         tiktok = str_detect(redes, "tiktok") |> as.numeric()) |> 
  mutate_if(is.numeric, ~replace_na(., 0)) |> 
  mutate(presencia = redes_count + pagina_web_count) |> 
  select(partido_coalicion:entidad, sexo, edad,
    pagina_web, pagina_web_count, redes_count:presencia)




redes |> 
  count(presencia, sexo)  |> 
  group_by(sexo) |> 
  mutate(total = sum(n)) |> 
  ungroup() |> 
  mutate(prop = n/total) |> 
  ggplot(aes(presencia, prop, fill=sexo,
             label= str_c(round(prop*100), " %"))) +
    geom_col(position = "dodge")+
  geom_text(position = position_dodge(width = .9),
            vjust=-0.5, family = "osw") +
  scale_y_continuous(labels = scales::percent)+
  scale_x_continuous(labels = c("Ninguna", seq(1,6,1)),
                     breaks = seq(0,6,1)) +
  tema +
  theme(panel.grid.major.x = element_blank())+
  labs(title="Porcentaje de personas propietarias por número de redes\n",
       x="\nNúmero de redes sociales", y="Porcentaje\n",
       caption = "Fuente: Conóceles - INE")+
  scale_fill_manual(name="", values = c("#1FC3AA", "#8624F5"))

ggsave("graficas/1_redes_genero.png", width = 10, height = 8, dpi = 100)

redes |> 
  mutate(total = 1) |> 
  select(sexo, pagina_web_count, facebook, 
         instagram, youtube, tiktok) |> 
  group_by(sexo) |> 
  summarise_all(sum) |> 
  rename(website=pagina_web_count) |> 
  pivot_longer(-sexo, names_to = "red", values_to = "cuentas") |> 
  group_by(sexo) |> 
  mutate(total = sum(cuentas)) |> 
  ungroup() |> 
  mutate(prop = cuentas/total)|> 
  ggplot(aes(red, prop, fill=sexo,
             label= str_c(round(prop*100), " %"))) +
  geom_col(position = "dodge")+
  geom_text(position = position_dodge(width = .9),
            vjust=-0.5, family = "osw") +
  scale_y_continuous(labels = scales::percent)+
  tema +
  theme(panel.grid.major.x = element_blank())+
  labs(title="Porcentaje cuentas por red social",
       subtitle = "y sexo de la persona propietaria\n",
       x="\nRed social", y="Porcentaje\n",
       caption = "Fuente: Conóceles - INE")+
  scale_fill_manual(name="", values = c("#1FC3AA", "#8624F5"))

ggsave("graficas/2_redes_genero.png", width = 10, height = 8, dpi = 100)



# 2. Edades ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
edad <- prop |> 
  group_by(sexo, cargo) |> 
  mutate(edad_avg = mean(edad)) |> 
  ungroup()

edad |> 
  ggplot(aes(sexo, edad, color = sexo)) +
  coord_flip() +
  scale_color_manual(name="", values = c("#1FC3AA", "#8624F5",
                                         "#B3001B", "gold")) +
  #guides(color = "none")+
  geom_jitter(size = 2, alpha = 0.25, width = 0.2) +
  stat_summary(fun = mean, geom = "point", size = 5)+
  stat_summary(aes(label=round(..y..)), fun=mean, 
               geom="text", family = "osw",
               size=5, color="black",
               vjust = -1)+
  geom_hline(yintercept = 42, color = "gray50", size = 0.6,
             linetype=2) +
   geom_segment(
     aes(x = sexo, xend = sexo,
         y = edad_avg, yend = 42),
     size = 0.8
     )+
  tema +
  theme(panel.grid.major.y = element_blank())+
  theme(panel.background = element_rect(fill = "grey99",
                                        color="transparent"))+
  facet_wrap(~cargo)+
  guides(color = "none")+
  labs(title="Distribución y edad promedio de las persona propietarias",
       subtitle = "por sexo y por cargo\n",
       x="", y="\nEdad",
       caption = "Fuente: Conóceles - INE\nLa línea gris muestra la media por edad de toda a muestra.")

ggsave("graficas/3_edades_genero.png", width = 8, height = 6, dpi = 100)


# Propuesta ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

propuestas_eval <- prop |> 
  mutate(across(matches("propuesta"), ~as.integer(is.na(.x))))  |> # 1 si está vacía
  mutate(total= 1) |> 
  select(partido_coalicion:entidad, sexo, edad,
         matches("propuesta"), total) 

prueba <-  prop |> # No hay personas que pusieran < 3 propuestas
  filter(!is.na(propuesta_1) & !is.na(propuesta_2) & !is.na(propuesta_genero))

propuestas_vacias_sexo_cargo <- propuestas_eval  |> 
  select(sexo, cargo,
         matches("propuesta"), total) |> 
  group_by(sexo, cargo) |> 
  summarise_all(sum) |> 
  mutate(tasa_no_respuesta = propuesta_genero/total) |> 
  mutate(tasa_respuesta = (total - propuesta_genero)/total) |> 
  mutate(orden = tasa_no_respuesta - tasa_respuesta) |> 
  arrange(-orden) |>  
  rowid_to_column("orden_id") |>  
  select(sexo, cargo, starts_with("tasa"), orden) |> 
  pivot_longer(-c(sexo, cargo, orden), names_to = "tipo", values_to = "tasa")

propuestas_vacias_sexo_cargo |> 
  mutate(label= ifelse(
    tipo == "tasa_respuesta",
    str_c(round(tasa*100), " %"),
    NA_character_
  )) |> 
  ggplot(aes(
    reorder(cargo, orden),
    tasa,
    fill=tipo))+
  geom_col() +
  geom_text(aes(label = label, y=.1),
            color="white", family="roboto",
            size=8, fontface = "bold") +
  facet_wrap(~sexo)+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual(values=c( "#c31f38", "grey80"), 
                    name="¿Presentó propuestas?",
                    breaks=c("tasa_respuesta", "tasa_no_respuesta"),
                    labels=c("Sí", "No")) +
  tema +
  theme(panel.grid.major.y = element_blank())+
  labs(title="Distribución de respuestas por sexo y cargo\n",
       x="", y="\nPorcentaje de respuesta",
       caption = "Fuente: Conóceles - INE\nLa línea punteada muestra la media por sexo.")+
  coord_flip()+
  geom_hline(yintercept = 0.5, linetype=2, size=1.2)

ggsave("graficas/4_respuesta_genero.png", width = 10, height = 6, dpi = 100)




# Por partido político

propuestas_vacias_partido <- propuestas_eval  |> 
  select(partido_coalicion,
         matches("propuesta"), total) |> 
  group_by(partido_coalicion) |> 
  summarise_all(sum) |> 
  mutate(tasa_no_respuesta = propuesta_genero/total) |> 
  mutate(tasa_respuesta = (total - propuesta_genero)/total) |> 
 # filter(tasa_no_respuesta >0) |> 
  arrange(-tasa_no_respuesta) |> 
  mutate(orden = seq_along(partido_coalicion)) |> 
  select(orden, partido_coalicion, starts_with("tasa")) |> 
  pivot_longer(-c(partido_coalicion, orden), names_to = "tipo", values_to = "tasa") 

propuestas_vacias_partido |> 
  mutate(label= ifelse(
    tipo == "tasa_respuesta",
    str_c(round(tasa*100), " %"),
    NA_character_
  )) |> 
  ggplot(aes(reorder(partido_coalicion, -orden), tasa, fill=tipo))+
  geom_col() +
  geom_text(aes(label = label, y=.1),
            color="white", family="roboto",
            size=8, fontface = "bold") +
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual(values=c( "#c31f38", "grey80"), 
                    name="¿Presentó propuestas?",
                    breaks=c("tasa_respuesta", "tasa_no_respuesta"),
                    labels=c("Sí", "No")) +
  tema +
  theme(panel.grid.major.y = element_blank())+
  labs(title="Porcentaje presentación de propuestas por patidos políticos o coaliciones",
       subtitle= "en las elecciones locales de 2022",
       x="", y="\nPorcentaje de respuesta\n",
       caption = "Fuente: Conóceles - INE\nLa línea punteada muestra la media por sexo.")+
  coord_flip()+
  geom_hline(yintercept = 0.5, linetype=2, size=1.2)


# por partido y sexo

propuestas_vacias_partido <- propuestas_eval  |> 
  select(partido_coalicion, sexo,
         matches("propuesta"), total) |> 
  group_by(partido_coalicion, sexo) |> 
  summarise_all(sum) |> 
  mutate(tasa_no_respuesta = propuesta_genero/total) |> 
  mutate(tasa_respuesta = (total - propuesta_genero)/total) |> 
  mutate(orden = tasa_no_respuesta - tasa_respuesta) |> 
  arrange(-orden) |>  
  rowid_to_column("orden_id") |> 
  select(orden_id, sexo, partido_coalicion, starts_with("tasa")) |> 
  pivot_longer(-c(partido_coalicion, orden_id, sexo), names_to = "tipo", values_to = "tasa") 

propuestas_vacias_partido |> 
  mutate(label= ifelse(
    tipo == "tasa_respuesta",
    str_c(round(tasa*100), " %"),
    NA_character_
  )) |> 
  ggplot(aes(
    reorder_within(partido_coalicion,-orden_id, sexo), 
             tasa, 
             fill=tipo)
         )+
  geom_col() +
  geom_text(aes(label = label, y=.1),
            color="white", family="roboto",
            size=8, fontface = "bold") +
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual(values=c( "#c31f38", "grey80"), 
                    name="¿Presentó propuestas?",
                    breaks=c("tasa_respuesta", "tasa_no_respuesta"),
                    labels=c("Sí", "No")) +
  tema +
  theme(panel.grid.major.y = element_blank())+
  labs(title="Porcentaje presentación de propuestas por patidos políticos o coaliciones y sexo",
       subtitle= "en las elecciones locales de 2022",
       x="", y="\nPorcentaje de respuesta\n",
       caption = "Fuente: Conóceles - INE\nLa línea punteada muestra la media por sexo.")+
  coord_flip()+
  geom_hline(yintercept = 0.5, linetype=2, size=1.2)+
  facet_wrap(~sexo, scales = "free") +
  scale_x_reordered()

ggsave("graficas/5_propuestas_sexo.png", width = 14, height = 10, dpi = 100)




# Duplicados --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

x <- prop |> 
  group_by(propuesta_genero) |> 
   mutate(n = n()) |> 
  filter(!is.na(propuesta_genero) & n >1) |> 
   select(partido_coalicion:sexo, propuesta_genero, n)  


duplicados <- prop |> 
  mutate(across(starts_with("propuesta"), 
                ~ifelse(.x %in% c("No proporcionó", "."), 
                        NA_character_, .x))) |> 
  group_by(propuesta_1) |> 
  mutate(dup_p1 = n()>1) |> ungroup()|> 
  group_by(propuesta_2) |> 
  mutate(dup_p2 = n()>1) |> ungroup()|> 
  group_by(propuesta_genero) |> 
  mutate(dup_pgen = n()>1) |> ungroup() |> 
  mutate(across(starts_with("dup"), as.integer)) |> 
  mutate(dup_p1 = ifelse(is.na(propuesta_1), 0, dup_p1))|> 
  mutate(dup_p2 = ifelse(is.na(propuesta_2), 0, dup_p2))|> 
  mutate(dup_pgen = ifelse(is.na(propuesta_genero), 0, dup_pgen))

# No hubo tantas repetidas


# Escolaridad -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

prop |> 
  select(partido_coalicion, sexo, cargo, escolaridad, cursos) |> 
  mutate(escolaridad = ifelse(is.na(escolaridad), "No contestó", escolaridad)) |> 
  mutate(escolaridad = factor(escolaridad, levels=c('No contestó','Primaria', 'Secundaria',
                                                    'Técnica','Preparatoria', 'Licenciatura', 
                                                    'Especialidad','Maestría', 'Doctorado'
  ))) |> 
  #filter(!is.na(escolaridad)) |> 
  group_by(sexo, escolaridad) |> 
  summarise(n = n()) |> ungroup() |> 
  group_by(sexo) |> 
  mutate(prop = n/sum(n)) |> 
  ggplot(aes(escolaridad, prop, fill= sexo))+
  geom_col()

# Calidad palabras --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

cal_prop <- prop |> 
  mutate(across(starts_with("propuesta"), 
                ~ifelse(.x %in% c("No proporcionó", "."), 
                        NA_character_, .x))) |> 
  mutate(prop_total = paste(propuesta_1, propuesta_2, propuesta_genero, sep=" ")) |> 
  filter(!str_detect(prop_total, "NA NA NA")) |> 
  mutate(prop_total = str_remove_all(prop_total, "NA ")) |> 
  select(partido_coalicion:entidad,edad,sexo,escolaridad,
         historia_profesional:motivo_cargo_publico,
         prop_total)

