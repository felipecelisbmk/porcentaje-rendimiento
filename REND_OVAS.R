# -----------------------------------------------------------------------------------
# Proyecto: SEGUIMIENTO PRODUCTIVO_OVAS HASTA PRIMERA ALIMENTACIÓN (FEBRERO, 2024)
# Autor: Marcelo E. Araneda 
# Fecha de Inicio: 09-02-2024
# ------------------------------------------------------------------------------------

##IMPORTACIÓN DE PAQUETES 

library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(corrplot)
install.packages("gridExtra")
library(gridExtra)
library(grid)
install.packages("psych")
install.packages("MASS")
library(MASS)
library(psych)
library(stats)
install.packages("multcomp")
library(multcomp)
library(psych)
install.packages("car")
library(car)
install.packages("knitr")
library(knitr)
install.packages("pander")
library(pander)

# Leer los datos desde Excel
datos <- readxl::read_excel("~/R_FILES/PORCENTAJE_RENDIMIENTO/PORCENTAJE_RENDIMIENTO/DATOS_REND.xlsx", sheet = "RENDIMIENTO")

# Convertir MONTH - YEAR a un formato de fecha
datos$MONTH_YEAR <- as.Date(paste("01-", datos$`MONTH - YEAR`, sep = ""), format = "%b %Y")

datos$`MONTH - YEAR` <- as.Date(datos$`MONTH - YEAR`, format = "%Y-%m-%d")


datosasd <- datos

# Filtra el dataframe por el RANGO DE FECHAS -
#COMENTAR AL TERMINAR ESTE ANALISIS DE FUERA DE TEMPORADA
datos <- datos[datos$`MONTH - YEAR` >= as.Date("2023-10-01") & datos$`MONTH - YEAR` <= as.Date("2024-02-29"), ]

# Crear la nueva columna con el formato deseado
datos$new_date <- toupper(format(datos$`MONTH - YEAR`, "%b %y"))

# Ajustar el modelo de regresión lineal
modelo <- lm(LOST ~ DELIVERY, data = datos)

# Obtener los valores de R-cuadrado, el coeficiente de correlación (r) y el p-value
rsq <- summary(modelo)$r.squared
r <- cor(datos$DELIVERY, datos$LOST)
pvalue <- summary(modelo)$coefficients[2, 4]

grafico <- ggplot(datos, aes(x = DELIVERY, y = LOST)) +
  geom_point(size = 3.8) +  # puntos de datos con un tamaño de 3
  geom_smooth(method = "lm", se = TRUE, color = "blue", alpha = 0.5) +  # línea de regresión
  labs(title = "Relationship between Delivery Date and Total Loss",
       x = "DELIVERY DATE",
       y = "TOTAL LOSS  % (MORTALITY + ELIMINATIONS)") +
  theme_gray() +  # Utilizar el tema predeterminado de ggplot2
  theme(panel.background = element_rect(fill = "gainsboro"),
        axis.title = element_text(size = 14, color = "black"),  # Ajustar el tamaño y color de los títulos de los ejes
        axis.text = element_text(size = 12, color = "black"),  # Ajustar el tamaño y color del texto en los ejes
        axis.text.x = element_text(size = 8)) +  # Ajustar el tamaño del texto en el eje x
  scale_x_continuous(breaks = datos$DELIVERY, labels = datos$new_date) +  # Personalizar etiquetas del eje x
  annotate("text", x = max(datos$DELIVERY), y = Inf, 
           label = sprintf("R-squared: %.2f\nCorrelation coefficient r: %.2f\np-value: %.4f", rsq, r, pvalue),
           hjust = 1, vjust = 1, color = "red", size = 4) +  # Anotar los valores de R-cuadrado, r y p-value
  coord_cartesian(xlim = c(min(datos$DELIVERY), max(datos$DELIVERY)), ylim = c(0.00, 7)) +  # Ajustar los límites del eje x y y
  scale_y_continuous(breaks = seq(0.00, 7, by = 1.5), labels = function(x) sprintf("%.2f", x))  # Ajustar el formato de los valores en el eje y



# Mostrar el gráfico
print(grafico)
