# Cargar librerías
library(dplyr)
library(tidyr)

# Paso 1: Cargar el dataset y convertirlo a un dataframe
data(mtcars)
df <- as.data.frame(mtcars)
print("Paso 1: Dataset cargado")
print(head(df))

# Paso 2: Selección de columnas y filtrado de filas
df_filtered <- df %>%
  select(mpg, cyl, hp, gear) %>%
  filter(cyl > 4)
print("Paso 2: Selección y filtrado")
print(df_filtered)

# Paso 3: Ordenación y renombrado de columnas
df_ordered <- df_filtered %>%
  arrange(desc(hp)) %>%
  rename(consumo = mpg, potencia = hp)
print("Paso 3: Ordenación y renombrado")
print(df_ordered)

# Paso 4: Creación de nuevas columnas y agregación de datos
df_with_efficiency <- df_ordered %>%
  mutate(eficiencia = consumo / potencia)

df_summary <- df_with_efficiency %>%
  group_by(cyl) %>%
  summarise(
    consumo_medio = mean(consumo, na.rm = TRUE),
    potencia_maxima = max(potencia, na.rm = TRUE)
  )
print("Paso 4: Nueva columna y agregación")
print(df_with_efficiency)
print(df_summary)

# Paso 5: Creación del segundo dataframe y unión de dataframes
df_transmission <- data.frame(
  gear = c(3, 4, 5),
  tipo_transmision = c("Manual", "Automática", "Semiautomática")
)

df_joined <- left_join(df_with_efficiency, df_transmission, by = "gear")
print("Paso 5: Unión de dataframes")
print(df_joined)

# Paso 6: Transformación de formatos
# Convertir a formato largo
df_long <- df_joined %>%
  pivot_longer(cols = c(consumo, potencia, eficiencia),
               names_to = "medida",
               values_to = "valor")

print("Paso 6: Formato largo")
print(df_long)

# Agrupar para manejar duplicados
df_long_grouped <- df_long %>%
  group_by(cyl, gear, tipo_transmision, medida) %>%
  summarise(valor = mean(valor, na.rm = TRUE), .groups = "drop")

print("Paso 6: Formato largo agrupado")
print(df_long_grouped)

# Convertir a formato ancho
df_wide <- df_long_grouped %>%
  pivot_wider(names_from = medida, values_from = valor)

print("Paso 6: Formato ancho")
print(df_wide)

# Verificación final
print("Proceso completado con éxito")
