library(dplyr)
library(lubridate)
# Cargar los datos

#Crear la columna 'date' combinando year, month y date_of_month
birth <- birth %>%
  mutate(date = make_date(year, month, date_of_month))
head(birth)

# Visualizar los nacimientos por fecha
birth <- birth %>%
  mutate(
    weekday = wday(date, label = TRUE, abbr = TRUE),  # Día de la semana
    week = week(date),  # Semana del año
    month_name = month(date, label = TRUE, abbr = TRUE)  # Nombre del mes
  )

# Crear el gráfico de calor de calendario con ajustes en las etiquetas y el espaciado
ggplot(birth, aes(x = weekday, y = -week, fill = births)) +
  geom_tile(color = "white", size = 0.2) +  # Celdas de color
  scale_fill_gradient(low = "lightyellow", high = "red") +  # Colores según nacimientos
  facet_wrap(~ month_name, ncol = 3, scales = "free_y") +  # Dividir por meses y permitir ajustar escalas verticales
  labs(title = "Mapa de Calor de Nacimientos por Día del Año",
       x = "Día de la Semana", y = "Semana del Año") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_blank(),  # Ocultar etiquetas del eje Y
    axis.ticks.y = element_blank(),  # Ocultar marcas del eje Y
    strip.background = element_rect(fill = "lightblue", color = "black"),
    strip.text = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),  # Rotar las etiquetas de días de la semana
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  )


max_births <- max(birth$births, na.rm = TRUE)

ggplot(birth, aes(x = weekday, y = -week)) +
  geom_tile(aes(fill = ifelse(births == max_births, "max", "normal")), color = "white", size = 0.2) +
  scale_fill_manual(values = c("normal" = "red", "max" = "yellow"), guide = FALSE) +  # Marcar el día máximo en amarillo
  facet_wrap(~ month_name, ncol = 3, scales = "free_y") +  # Dividir por meses
  labs(title = "Mapa de Calor de Nacimientos por Día del Año",
       x = "Día de la Semana", y = "Semana del Año") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_blank(),  # Ocultar etiquetas del eje Y
    axis.ticks.y = element_blank(),  # Ocultar marcas del eje Y
    strip.background = element_rect(fill = "lightblue", color = "black"),
    strip.text = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),  # Rotar las etiquetas de días de la semana
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  )
