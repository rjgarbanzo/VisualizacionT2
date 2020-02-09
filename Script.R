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
library(ggplot2)
library(ggrepel)


#IDH <- read.csv("IDH_cantonal_2014.csv", sep = ",")

cantones <- read_sf("geo_data/geo_data/cantones/cantones.shp")
IDH <- IDH_cantonal_2014
to_upper <- function(x) toupper(chartr("áéíóúÁÉÍÓÚ", "aeiouAEIOU", x))
IDH$Cantón <- to_upper(IDH$Cantón)
names(IDH)[1] <- "Posicion"
names(IDH)[2] <- "Canton"
IDH <- IDH[-1]

cantones <- left_join(x = cantones, y = IDH, by = c("cant_nom1" = "Canton"))
cantones <- subset(cantones, cantones$prov_nom1 == "SAN JOSE")


centros <- st_centroid(cantones)
cantones <- cbind(cantones,st_coordinates(st_centroid(centros$geometry)))




ggplot(cantones, aes(fill = cantones$IDH)) +
  geom_sf(color = "green", size = .1) +
  geom_label_repel(
    mapping = aes(x = X, y = Y, label = paste(cantones$cant_nom2,"-",cantones$IDH)),
    size = 3,
    fill = "white",
    label.r = 0)
  theme_void() +
  scale_fill_distiller(labels = comma,
                       trans = "log10",
                       name = "Cantidad de delitos reportados",
                       direction = 1) +
  labs(title = "Indice de criminalidad") +
  theme(legend.key.height = unit(1,"cm"), plot.title = element_text(hjust = .5, size = 12))


  
  
  
  
  ggplot(cantones, mapping = aes(fill = cantones$IDH)) +
    geom_sf(color = "green", size = .1) +
    geom_label_repel(
      mapping = aes(x = X, y = Y, label = paste(cantones$cant_nom2,"-",cantones$IDH)),
      size = 3, fill = "white", label.r = 0) +
  theme_void() +
    scale_fill_viridis_c(labels = comma,
                       trans = "log10",
                       name = "IDH",
                       direction = 1) +
    labs(title = "Indice -  Provincia de San Jose") +
    theme(legend.position = "bottom", legend.key.width = unit(2,"cm"), 
          plot.title = element_text(hjust = .5, size = 14))


  
###################################################################################
library(forcats)
library(ggplot2)
library(dplyr)
library(sf)
library(scales)
library(ggplot2)
library(ggrepel)
library(egg)
library(grid)  
    
OIJ2010 <- read.csv("OIJ_estadisticas_2010.csv", sep = ",")
OIJ2011 <- read.csv("OIJ_estadisticas_2011.csv", sep = ",")
OIJ2012 <- read.csv("OIJ_estadisticas_2012.csv", sep = ",")
OIJ2013 <- read.csv("OIJ_estadisticas_2013.csv", sep = ",")
OIJ2014 <- read.csv("OIJ_estadisticas_2014.csv", sep = ",")
OIJ2015 <- read.csv("OIJ_estadisticas_2015.csv", sep = ",")


provincias <- read_sf("geo_data/geo_data/provincias/provincias.shp")
asaltos <- rbind(OIJ2010, OIJ2011, OIJ2012, OIJ2013, OIJ2014, OIJ2015)
asaltos <- asaltos[-c(2,3,4,6,7,8,10,11,12)]
conteo_victimas <- count(asaltos, asaltos$SubVictima, asaltos$Provincia, name = "conteo")
asaltos_provincias <- count(asaltos, asaltos$Delito, asaltos$Provincia, name = "conteo")
mapa_asaltos <- left_join(x = provincias, y = asaltos_provincias, by = c("prov_nom1" = "asaltos$Provincia"))


centros <- st_centroid(provincias)
provincias <- cbind(provincias, st_coordinates(st_centroid(centros$geometry)))



g1 <- ggplot(mapa_asaltos, mapping = aes(fill = conteo)) +
  geom_sf(color = "black", size = .1) +
  geom_label_repel(mapping = aes(x = provincias$X, y = provincias$Y, label = paste(mapa_asaltos$prov_nom2)),
    size = 3, fill = "white", label.r = 0) +
  theme_void() +
  scale_fill_viridis_c(labels = comma,
                       trans = "log10",
                       name = "Cantidad de Asaltos",
                       direction = 1) +
  labs(title = "Cantidad de asaltos por provincia - Costa Rica") +
  theme(legend.position = "bottom", legend.key.width = unit(2,"cm"), 
        plot.title = element_text(hjust = .5, size = 14))





################################################################################




g2 <- ggplot(data = conteo_victimas, 
             mapping = aes(y = conteo_victimas$conteo,
                           x = conteo_victimas$`asaltos$Provincia`)) +
  geom_col() +
  coord_flip() +
  theme_minimal() +
  labs(x = "Provincias", y = "Cantidad de Asaltos") +
  theme(text = element_text(size = 14, face = "bold"))


#################################################################################

gl <- list(g1, g2)
grid.arrange(grobs = gl,
             nrow = 1,
             widths = c(2,2),
             layout_matrix = rbind(c(1, 2),c(1, 2)),
             top = textGrob("Indice de desarrollo social de los cantones del GAM",
                            gp = gpar(fontface = 2,
                                      fontsize = 16),
                            hjust = 0.5,
                            x = 0.5)
)














###################################################################################
###################################################################################
#EJERCICIO 4

datos_violencia <- read.csv("violencia.csv", sep = ";")
names(datos_violencia)[1] <- "Provincia"
names(datos_violencia)[2] <- "Canton"

to_upper <- function(x) toupper(chartr("áéíóúÁÉÍÓÚ", "aeiouAEIOU", x))
datos_violencia$Provincia <- to_upper(datos_violencia$Provincia)
datos_violencia$Canton <- to_upper(datos_violencia$Canton)
datos_violencia$Distrito <- to_upper(datos_violencia$Distrito)



provincias <- read_sf("geo_data/geo_data/provincias/provincias.shp")
cantones <- read_sf("geo_data/geo_data/cantones/cantones.shp")
distritos <- read_sf("geo_data/geo_data/distritos/distritos.shp")



provincias_violencia <- left_join(x = provincias, y = datos_violencia, by = c("prov_nom1" = "Provincia"))  
cantones_violencia <- left_join(x = cantones, y = datos_violencia, by = c("cant_nom1" = "Canton"))
distritos_violencia <- left_join(x = distritos, y = datos_violencia, by = c("dist_nom1" = "Distrito"))  

centros <- st_centroid(provincias)
provincias <- cbind(provincias, st_coordinates(st_centroid(centros$geometry)))
