datos <- read.csv("encuesta_brexit.csv", sep = ",", header = T)
library(ggplot2)
datos$Fecha <- as.Date(datos$Fecha, "%d/%m/%y")

g <- ggplot(data = datos, mapping = aes(x = datos$Fecha)) +
  geom_smooth(mapping = aes(y = datos$Si, color = "Si")) +
  geom_smooth(mapping = aes(y = datos$No, color = "No")) +
  geom_point(mapping = aes(y = datos$Si, color = "Si")) +
  geom_point(mapping = aes(y = datos$No, color = "No")) +
  scale_color_manual(values = c( "#f85f73","#07689f"), name = "Respuesta") +
  scale_x_date(date_breaks = "6 month") +
  theme_minimal() +
  theme(legend.position = "bottom")
g



