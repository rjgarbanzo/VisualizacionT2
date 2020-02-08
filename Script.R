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



#########################################################################

library(dplyr)
library(sf)
library(scales)

cantones <- read_sf("geo_data/geo_data/cantones/cantones.shp")

IDH <- IDH_cantonal_2014
to_upper <- function(x) toupper(chartr("áéíóúÁÉÍÓÚ", "aeiouAEIOU", x))
IDH$Cantón <- to_upper(IDH$Cantón)
names(IDH)[1] <- "Posicion"
names(IDH)[2] <- "Canton"
IDH <- IDH[-1]


cantones <- left_join(x = cantones, y = IDH, by = c("cant_nom1" = "Canton"))

cantones <- subset(cantones, cantones$prov_nom1 == "SAN JOSE")


ggplot(cantones, aes(fill = cantones$IDH)) +
  geom_sf(color = "black", size = .1) +
  theme_void() +
  scale_fill_distiller(labels = comma,
                       trans = "log10",
                       name = "Cantidad de delitos reportados",
                       direction = 1) +
  labs(title = "Distribución de los delitos reportados en Costa Rica",
       caption = "Fuente: Organismos De Investigación Judicial") +
  theme(legend.key.height = unit(1,"cm"),
        plot.title = element_text(hjust = .5, size = 12))



