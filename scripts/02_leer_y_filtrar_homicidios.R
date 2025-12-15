if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  readr,
  dplyr,janitor,
  purrr,scales,
  stringr,
  ggplot2,ggalt,
  foreign)
setwd("..")

#df20 <- read_csv("data/raw/DEFUN20.csv")
#df21 <- read_csv("data/raw/DEFUN21.csv")
#df22 <- read_csv("data/raw/DEFUN22.csv")
#df23 <- read_csv("data/raw/DEFUN23.csv")
df24 <- read_csv("data/raw/DEFUN24.csv")

#homicidios y migrantes internos (solo mexicanxs)
df <- df24 %>%
  mutate(
    homicidio = if_else(str_detect(CAUSA_DEF, "^(X8[5-9]|Y0[0-9])"), 1, 0),
    
    # Normalizar ENT_NAC a 3 dígitos (así viene en el diccionario)
    ENT_NAC3 = str_pad(ENT_NAC, width = 3, side = "left", pad = "0"),
    
    # Identificar nacidos en México
    nac_mex = if_else(as.numeric(ENT_NAC3) >= 1 & as.numeric(ENT_NAC3) <= 32, 1, 0),
    
    # Migración interna SOLO para nacidos en México
    migrante_interno = case_when(
      nac_mex == 1 & str_pad(ENT_RESID, 3, "left", "0") == ENT_NAC3 ~ 0,
      nac_mex == 1 & str_pad(ENT_RESID, 3, "left", "0") != ENT_NAC3 ~ 1,
      TRUE ~ NA_real_   # extranjeros o no especificados
    )
  )

estados <- tibble::tibble(
  ENT_RESID = c(
    "01","02","03","04","05","06","07","08","09","10",
    "11","12","13","14","15","16","17","18","19","20",
    "21","22","23","24","25","26","27","28","29","30",
    "31","32"
  ),
  estado = c(
    "Aguascalientes",
    "Baja California",
    "Baja California Sur",
    "Campeche",
    "Coahuila de Zaragoza",
    "Colima",
    "Chiapas",
    "Chihuahua",
    "Ciudad de México",
    "Durango",
    "Guanajuato",
    "Guerrero",
    "Hidalgo",
    "Jalisco",
    "México",
    "Michoacán de Ocampo",
    "Morelos",
    "Nayarit",
    "Nuevo León",
    "Oaxaca",
    "Puebla",
    "Querétaro",
    "Quintana Roo",
    "San Luis Potosí",
    "Sinaloa",
    "Sonora",
    "Tabasco",
    "Tamaulipas",
    "Tlaxcala",
    "Veracruz de Ignacio de la Llave",
    "Yucatán",
    "Zacatecas"
  )
)

df<-df%>%
  left_join(estados, by="ENT_RESID")

#Exploracion general

# Limpieza básica de nombres
dfe <- df %>% clean_names()

# 1. Tamaño general
n_total <- nrow(dfe)

# 2. Frecuencia de homicidios
dfe %>%
  count(homicidio) %>%
  mutate(porcentaje = round(100 * n / sum(n), 1))

# 3. Frecuencia de migrantes internos
dfe %>%
  count(migrante_interno) %>%
  mutate(porcentaje = round(100 * n / sum(n), 1))

# 4. Distribución por sexo
dfe %>%
  count(sexo) %>%
  mutate(sexo = recode(as.character(sexo), "1" = "Hombres", "2" = "Mujeres"),
         porcentaje = round(100 * n / sum(n), 1))

# 5. Distribución por estado
dfe %>%
  count(estado) %>%
  arrange(desc(n)) %>%
  mutate(porcentaje = round(100 * n / sum(n), 1))

# 6. Proporción de homicidios por sexo
dfe %>%
  group_by(sexo) %>%
  summarise(
    total = sum(homicidio == 1, na.rm = TRUE),
    migrantes = sum(homicidio == 1 & migrante_interno == 1, na.rm = TRUE),
    prop_migrantes = round(100 * migrantes / total, 1)
  ) %>%
  mutate(sexo = recode(as.character(sexo), "1" = "Hombres", "2" = "Mujeres"))

# 7. Proporción de homicidios de migrantes por estado
dfe %>%
  group_by(estado) %>%
  summarise(
    total = sum(homicidio == 1, na.rm = TRUE),
    migrantes = sum(homicidio == 1 & migrante_interno == 1, na.rm = TRUE),
    prop_migrantes = round(100 * migrantes / total, 1)
  ) %>%
  filter(total >= 10) %>%
  arrange(desc(prop_migrantes))


# Hallazgos preliminares

#proporción de homicidios de migrantes internos por estado (nota, quitamos las observaciones que no tengan por lo menos 10 homicidios para evitar respuestas extremas)

prop_estado <- df %>%
  group_by(estado) %>%
  summarise(
    homicidios_total = sum(homicidio == 1, na.rm = TRUE),
    homicidios_migrantes = sum(homicidio == 1 & migrante_interno == 1, na.rm = TRUE),
    prop_migrantes = homicidios_migrantes / homicidios_total
  )

### Visualización

# Agrupación y filtrado
prop_estado_sexo <- df %>%
  group_by(estado, SEXO) %>%
  summarise(
    homicidios_total = sum(homicidio == 1, na.rm = TRUE),
    homicidios_migrantes = sum(homicidio == 1 & migrante_interno == 1, na.rm = TRUE),
    prop_migrantes = homicidios_migrantes / homicidios_total,
    .groups = "drop"
  ) %>%
  filter(homicidios_total >= 10 & !is.na(estado))   # Mantener tu regla: mínimo 10 por sexo

# Proporción total por estado (sin sexo)
prop_estado_total <- df %>%
  group_by(estado) %>%
  summarise(
    homicidios_total = sum(homicidio == 1, na.rm = TRUE),
    homicidios_migrantes = sum(homicidio == 1 & migrante_interno == 1, na.rm = TRUE),
    prop_migrantes = homicidios_migrantes / homicidios_total,
    .groups = "drop"
  ) %>%
  filter(homicidios_total >= 10 & !is.na(estado)) %>%   # Mínimo 10 homicidios totales
  mutate(SEXO = "Total")

# Unir total + sexo
prop_estado_sexo <- prop_estado_sexo %>%
  mutate(SEXO = as.character(SEXO))
prop_estado_sexo <- bind_rows(prop_estado_sexo, prop_estado_total)

# Recodificar sexo
prop_estado_sexo <- prop_estado_sexo %>%
  mutate(SEXO = recode(SEXO,
                       "1" = "Hombres",
                       "2" = "Mujeres",
                       "Total" = "Total"))

#promedio
prom_total <- prop_estado_sexo %>% filter(SEXO == "Total") %>% summarise(mean(prop_migrantes)) %>% pull()

orden_estados <- prop_estado_total %>%
  arrange((prop_migrantes)) %>%
  pull(estado)
g_total <- ggplot(prop_estado_total,
                  aes(x = factor(estado, levels = orden_estados),
                      y = prop_migrantes)) +
  geom_bar(stat = "identity", fill = "#4C72B0") +
  geom_text(aes(label = scales::percent(prop_migrantes, accuracy = 1)),
            #position = position_dodge(width = 0.9),
            hjust = -0.1,
            size = 3.5)+
  geom_hline(yintercept = prom_total,
             linetype = "dashed",
             color = "orange",
             linewidth = 0.8) +
  annotate("text",
           x = prom_total,
           y = length(orden_estados) + 1,
           label = paste0("Promedio nacional: ", scales::percent(prom_total, accuracy = 1)),
           hjust = 0.5, vjust = 0,
           size = 4, color = "gray30")+
  coord_flip() +
  labs(
    title = "Porcentaje de homicidios de migrantes internos por estado",
    subtitle = "Se excluyen entidades con menos de 10 homicidios en el periodo",
    x = "",
    y = "",
    caption = paste0("Promedio nacional: ", scales::percent(prom_total, accuracy = 1))
  )+
  scale_y_discrete(expand = expansion(mult = c(0, 0.05)))+
  theme(plot.caption=element_text(color="orange",size=10))+
  theme_minimal(base_size = 13)+
  scale_y_continuous(labels = scales::percent, limits = c(0, 1))

g_total

# " " por municipio

prop_municipio <- df %>%
  group_by(ENT_RESID, MUN_RESID) %>%
  summarise(
    homicidios_total = sum(homicidio == 1, na.rm = TRUE),
    homicidios_migrantes = sum(homicidio == 1 & migrante_interno == 1, na.rm = TRUE),
    prop_migrantes = homicidios_migrantes / homicidios_total)%>%
  filter(homicidios_total>=10)

## municipio y sexo
prop_municipio_sexo <- df %>%
  group_by(ENT_RESID, MUN_RESID, SEXO) %>%
  summarise(
    homicidios_total = sum(homicidio == 1, na.rm = TRUE),
    homicidios_migrantes = sum(homicidio == 1 & migrante_interno == 1, na.rm = TRUE),
    prop_migrantes = homicidios_migrantes / homicidios_total)%>%
  filter(homicidios_total>=10)

#Hallazgo 2

prop_estado_sexo <- df %>%
  group_by(estado, SEXO) %>%
  summarise(
    homicidios_total = sum(homicidio == 1, na.rm = TRUE),
    homicidios_migrantes = sum(homicidio == 1 & migrante_interno == 1, na.rm = TRUE),
    prop_migrantes = homicidios_migrantes / homicidios_total
  )%>%
  filter(homicidios_total>=10)%>%
  filter(homicidios_total >= 10 & !is.na(estado)) %>%
  mutate(
    SEXO= recode(as.character(SEXO),
                  "1" = "Hombres",
                  "2" = "Mujeres",
                  "9" = "No especificado"),
    SEXO = factor(SEXO)
  ) %>%
  filter(SEXO!= "No especificado")  

prom_hombres <- prop_estado_sexo %>%ungroup()%>%
  filter(SEXO == "Hombres") %>%
  summarise(mean(prop_migrantes)) %>%
  pull()

prom_mujeres <- prop_estado_sexo %>%ungroup()%>%
  filter(SEXO== "Mujeres") %>%
  summarise(mean(prop_migrantes)) %>%
  pull()

# Pasar la tabla a formato ancho para dumbbell (una columna por sexo)
prop_wide <- prop_estado_sexo %>%
  select(estado, SEXO, prop_migrantes) %>%
  tidyr::pivot_wider(
    names_from = SEXO,
    values_from = prop_migrantes
  )

# Ordenar estados por, por ejemplo, el promedio entre hombres y mujeres
prop_wide <- prop_wide %>%
  mutate(
    brecha = abs(Hombres - Mujeres),
    estado = factor(estado, levels = estado[order(-brecha)])
  )%>%
  filter(!is.na(Hombres) & !is.na(Mujeres))


prop_wide <- prop_wide %>%
  arrange(desc(Hombres)) %>%
  mutate(estado = factor(estado, levels = estado))


# Gráfico tipo dumbbell
ggplot(prop_wide, aes(y = estado)) +
  
  # Línea entre hombres y mujeres
  geom_segment(aes(x = Hombres, xend = Mujeres, y = estado, yend = estado),
               color = "gray70", linewidth = 1) +
  
  # Punto hombres (verde)
  geom_point(aes(x = Hombres),
             color = "#2E7D32", size = 6) +
  
  # Punto mujeres (amarillo)
  geom_point(aes(x = Mujeres),
             color = "#FFCC00", size = 6) +
  
  # Líneas de promedio nacional
  geom_vline(xintercept = prom_hombres, linetype = "dashed", color = "#2E7D32") +
  geom_vline(xintercept = prom_mujeres, linetype = "dashed", color = "#FFCC00") +
  
  # Etiquetas de porcentaje
  geom_text(aes(x = Hombres, label = percent(Hombres, accuracy = 1)),
            nudge_x = -0.05, size = 4.5, color = "#2E7D32") +
  
  geom_text(aes(x = Mujeres, label = percent(Mujeres, accuracy = 1)),
            nudge_x = 0.05, size = 4.5, color = "#CC9900")+
  scale_x_continuous(labels = percent, limits = c(0, 1)) +
  labs(
    title = "Porcentaje de homicidios de migrantes internos por estado y sexo",
    subtitle = "Comparación Hombres–Mujeres",
    x = "Porcentaje",
    y = NULL,
    caption = paste0(
      "Promedio nacional hombres: ", percent(prom_hombres, accuracy = 1),
      " | Promedio nacional mujeres: ", percent(prom_mujeres, accuracy = 1)
    )
  ) +

  theme_minimal(base_size = 13) +
  theme(
    plot.caption = element_text(color = "gray30"),
    axis.text.y = element_text(size = 9)
  )

# Hallazgo 3: 

df <- df %>%
  mutate(
    causa_grupo = case_when(
      str_detect(CAUSA_DEF, "^(V|W|X[0-5])") ~ "Accidente",
      str_detect(CAUSA_DEF, "^X6|^X7|^X8[0-4]") ~ "Suicidio",
      str_detect(CAUSA_DEF, "^X8[5-9]|^Y0[0-9]") ~ "Homicidio",
      str_detect(CAUSA_DEF, "^Y1|^Y2|^Y3") ~ "Intención no determinada",
      str_detect(CAUSA_DEF, "^Y3[5-9]|^Y[4-8]") ~ "Otras causas externas",
      TRUE ~ "Enfermedad"
    )
  )


df_clean <- df %>%
  filter(!is.na(migrante_interno),
         !is.na(CAUSA_DEF))

tabla_causas <- table(df_clean$migrante_interno,
                      df_clean$causa_grupo)
tabla_causas

chi<-chisq.test(tabla_causas)
chi

chi$residuals

#Visual 
df_plot <- df_clean %>%
group_by(migrante_interno, causa_grupo) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(migrante_interno) %>%
  mutate(prop = n / sum(n))

ggplot(df_plot, aes(x = causa_grupo,
                    y = prop,
                    fill = factor(migrante_interno))) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_manual(values = c("#2E7D32", "#FFCC00"),
                    labels = c("No migrante interno", "Migrante interno")) +
  labs(
    title = "Distribución de causas de muerte por condición migratoria",
    x = "Categoría de causa de muerte",
    y = "Proporción dentro de cada grupo",
    fill = "Condición migratoria"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 25, hjust = 1),
    plot.title = element_text(face = "bold")
  )

ggplot(df_plot, aes(x = factor(migrante_interno),
                    y = prop,
                    fill = factor(migrante_interno))) +
  geom_col() +
  facet_wrap(~ causa_grupo, scales = "free_y") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_manual(values = c("#2E7D32", "#FFCC00"),
                    labels = c("No migrante interno", "Migrante interno")) +
  labs(
    title = "Causas de muerte (agrupada) por condición migratoria",
    x = "Condición migratoria",
    y = ""
  ) +
  theme_minimal(base_size = 13) +
  theme(
    strip.text = element_text(face = "bold"),
    legend.position = "down"
  )
