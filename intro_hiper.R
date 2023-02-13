
# paquetes ----------------------------------------------------------------

library(prismaread)
library(tidyverse)
library(raster)
library(leaflet)
library(leafem)

# convertir ---------------------------------------------------------------

?pr_convert()

# archivo de entrada (.he5)
prod <- "producto/PRS_L2D_STD_20221231140811_20221231140815_0001.he5"

# carpeta de salida
salida <- "salida"

# formato de salida (.tif)
formato <- "GTiff"

# conversión
pr_convert(in_file = prod,
           out_folder = salida,
           out_format = formato,
           VNIR = TRUE)

# visualizar --------------------------------------------------------------

# levanto el stack de bandas
stack <- raster::stack("salida/PRS_L2D_STD_20221231140811_20221231140815_0001_HCO_VNIR.tif")

# genero mapa RGB
options(viewer = NULL)

leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
  # RGB
  addRasterRGB(
    stack,
    r = 33,
    g = 21,
    b = 12,
    quantiles = c(.02, .98),
    na.color = NA,
    group = "RGB",
    layerId = "RGB")

# vector ------------------------------------------------------------------

# levanto vector (.shp)
puntos <- shapefile("vector/puntos.shp")

# extraigo
reflec <- extract(stack, puntos)

# acomodo los datos
reflec2 <- reflec |> 
  as_tibble() |> 
  # agrego la descripción de c/punto
  mutate(tipo = puntos$tipo) |> 
  # transformo a tabla larga
  pivot_longer(cols = -tipo,
               values_to = "reflec",
               names_to = "capa") |> 
  # extraigo los número de 'capa'
  mutate(capa = str_replace(capa, "layer.", "") |> as.numeric())

# leo los metadatos
centros <- read_delim("salida/PRS_L2D_STD_20221231140811_20221231140815_0001_HCO_VNIR.wvl") |> 
  rename(capa = band, long = wl)

# combino con los datos de 'reflec2'
datos <- full_join(reflec2, centros, by = "capa") |> 
  dplyr::select(tipo, capa, reflec, long, -capa)

# firma espectral ---------------------------------------------------------

ggplot(data = datos, aes(x = long, y = reflec, color = tipo)) +
  geom_point(size = 1, alpha = .6) +
  geom_line(alpha = .6) +
  # facet_wrap(~ tipo) +
  labs(x = "Longitud de onda (nm)",
       y = "Reflectancia de superficie") +
  scale_color_brewer(palette = "Dark2",
                     name = "Superficie",
                     labels = c("Paraná", "Vegetación")) +
  theme_bw() +
  theme(aspect.ratio = 1,
        legend.position = "top")
