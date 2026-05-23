# ============================================================
# MAPA POSTER LULC - DISTRITO DE SAN GABAN, MADRE DE DIOS
# MapBiomas Perú Colección 3 - Año 2024
# ============================================================

rm(list = ls())

library(terra)
library(sf)
library(geodata)
library(tidyverse)
library(ggplot2)
library(ggspatial)
library(patchwork)
library(openxlsx)
library(cowplot)
library(glue)
library(scales)

options(scipen = 999)

# ============================================================
# 1. Crear carpetas
# ============================================================

dir.create("tif", showWarnings = FALSE)
dir.create("png", showWarnings = FALSE)
dir.create("excel", showWarnings = FALSE)

# ============================================================
# 2. Descargar MapBiomas Perú 2024
# ============================================================

url <- "https://storage.googleapis.com/mapbiomas-public/initiatives/peru/collection_3/LULC/peru_collection3_integration_v1-classification_2024.tif"

archivo_tif <- "tif/mapbiomas_peru_2024.tif"

if (!file.exists(archivo_tif)) {
  download.file(url, archivo_tif, mode = "wb")
}

r <- rast(archivo_tif)
names(r) <- "lulc_2024"

# ============================================================
# 3. Descargar límites administrativos del Perú
# ============================================================

peru3 <- geodata::gadm(
  country = "PER",
  level = 3,
  path = "tmp"
) |> 
  st_as_sf() |> 
  st_make_valid()

# Revisar nombres si deseas verificar
unique(peru3$NAME_1)
unique(peru3$NAME_2)
unique(peru3$NAME_3)

# ============================================================
# 4. Seleccionar distrito de SAN GABAN
# Madre de Dios / Carabaya / SAN GABAN
# ============================================================

manu <- peru3 |> 
  filter(
    NAME_1 == "Puno",
    NAME_2 == "Carabaya",
    NAME_3 == "San Gaban"
  ) |> 
  st_make_valid()

ggplot() +
  geom_sf(data = manu, fill = "lightgreen", color = "black") 

# Si no encuentra, revisar:
# peru3 |> filter(NAME_1 == "Puno") |> select(NAME_1, NAME_2, NAME_3)

manu_v <- vect(manu)

# ============================================================
# 5. Recortar y enmascarar MapBiomas
# ============================================================

manu_v <- project(manu_v, crs(r))

r_manu <- crop(r, manu_v)
r_manu <- mask(r_manu, manu_v)
plot(r_manu)
# ============================================================
# 6. Leyenda MapBiomas Perú Colección 3
# ============================================================

lgnd_all <- tribble(
  ~value, ~clase,                                      ~color,
  1,  "Formación boscosa",                            "#1f8d49",
  3,  "Bosque",                                       "#1f8d49",
  4,  "Bosque seco",                                  "#7dc975",
  5,  "Manglar",                                      "#04381d",
  6,  "Bosque inundable",                             "#026975",
  10, "Formación natural no boscosa",                 "#d6bc74",
  11, "Zona pantanosa o pastizal inundable",          "#519799",
  12, "Pastizal / herbazal",                          "#d6bc74",
  29, "Afloramiento rocoso",                          "#ffaa5f",
  66, "Matorral",                                     "#a89358",
  70, "Loma costera",                                 "#be9e00",
  13, "Otra formación no boscosa",                    "#d89f5c",
  14, "Área agropecuaria",                            "#ffefc3",
  15, "Pasto",                                        "#edde8e",
  18, "Agricultura",                                  "#e974ed",
  35, "Palma aceitera",                               "#9065d0",
  40, "Arroz",                                        "#c71585",
  72, "Otros cultivos",                               "#910046",
  9,  "Plantación forestal",                          "#7a5900",
  21, "Purma",                                        "#ffefc3",
  22, "Área sin vegetación",                          "#d4271e",
  23, "Playa",                                        "#ffa07a",
  24, "Infraestructura urbana",                       "#d4271e",
  30, "Minería",                                      "#9c0027",
  32, "Salina costera",                               "#fc8114",
  61, "Salar",                                        "#f5d5d5",
  68, "Otra área natural sin vegetación",             "#E97A7A",
  25, "Otra área sin vegetación",                     "#db4d4f",
  26, "Cuerpo de agua",                               "#2532e4",
  33, "Río",                                          "#2532e4",
  31, "Acuicultura",                                  "#091077",
  34, "Glaciar",                                      "#93dfe6",
  27, "No observado",                                 "#ffffff"
)

# ============================================================
# 7. Calcular áreas por clase
# ============================================================

area_km2 <- cellSize(r_manu, unit = "km")

df_area <- as.data.frame(c(r_manu, area_km2), na.rm = TRUE)
names(df_area) <- c("value", "area_km2")

tabla_area <- df_area |> 
  group_by(value) |> 
  summarise(area_km2 = sum(area_km2, na.rm = TRUE), .groups = "drop") |> 
  left_join(lgnd_all, by = "value") |> 
  mutate(
    area_pct = area_km2 / sum(area_km2) * 100,
    area_km2 = round(area_km2, 2),
    area_pct = round(area_pct, 2)
  ) |> 
  arrange(desc(area_km2))

write.xlsx(
  tabla_area,
  "excel/areas_lulc_distrito_manu_2024.xlsx",
  overwrite = TRUE
)

# Colores presentes
clrs <- setNames(tabla_area$color, tabla_area$clase)

# ============================================================
# 8. Convertir raster a tabla para ggplot
# ============================================================

df_map <- as.data.frame(r_manu, xy = TRUE, na.rm = TRUE)

df_map <- df_map |> 
  rename(value = lulc_2024) |> 
  left_join(lgnd_all, by = "value") |> 
  filter(!is.na(clase))

# ============================================================
# 9. Mapas inset
# ============================================================

peru0 <- geodata::gadm(country = "PER", level = 0, path = "tmp") |> 
  st_as_sf()
# Chile
chile <- geodata::gadm(country = "CHL",level = 0,path = "tmp") |> 
  st_as_sf()
# Colombia
colombia <- geodata::gadm(country = "COL",level = 0,path = "tmp") |> 
  st_as_sf()
# Brasil
brasil <- geodata::gadm(country = "BRA",level = 0,path = "tmp") |> 
  st_as_sf()
# Bolivia
bolivia <- geodata::gadm(country = "BOL",level = 0,path = "tmp") |> 
  st_as_sf()

mdd <- peru3 |> 
  filter(NAME_1 == "Puno") |> 
  summarise()

mdd_box = st_as_sfc(st_bbox(mdd))

inset_peru <- ggplot() +
  geom_sf(data = peru0, fill = "grey92", color = "grey40", linewidth = 0.25) +
  geom_sf(data = chile, fill = "grey92", color = "grey40", linewidth = 0.25) +
  geom_sf(data = colombia, fill = "grey92", color = "grey40", linewidth = 0.25) +
  geom_sf(data = brasil, fill = "grey92", color = "grey40", linewidth = 0.25) +
  geom_sf(data = bolivia, fill = "grey92", color = "grey40", linewidth = 0.25) +
  geom_sf(data = mdd, fill = "#8BC34A", color = "black", linewidth = 0.4) +
  geom_sf(data = mdd_box , fill = NA, color = 'red', size = 0.4)+
  theme_void() +
  coord_sf(xlim = c(-81.41094,-68), ylim = c(-20,1),expand = FALSE)+
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    panel.background = element_rect(fill = "#8ecae6"), 
  )

inset_peru

manu_box = st_as_sfc(st_bbox(manu))

inset_mdd <- ggplot() +
  geom_sf(data = mdd, fill = "grey92", color = "black", linewidth = 0.35) +
  geom_sf(data = manu_box , fill = NA, color = 'red', size = 0.4)+
  geom_sf(data = manu, fill = "#00C853", color = "black", linewidth = 0.5) +
  theme_void() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    panel.background = element_rect(fill = "white"), 
  )

inset_mdd


library(elevatr)
library(ggnewscale)
library(sf)
library(ggplot2)
library(tidyverse)
library(raster)
library(ggspatial)
library(cptcity)
library(leaflet)
library(leafem)
library(leaflet.extras)
library(grid)
library(RStoolbox)
elev = get_elev_raster(manu, z=12)
Poligo_alt    <- crop(elev, manu)                           #
Poligo_alt   <- Poligo_alt <- mask(Poligo_alt, manu)
plot(Poligo_alt)

slopee    = terrain(Poligo_alt  , opt = "slope")
aspecte    = terrain(Poligo_alt, opt = "aspect")
hille     = hillShade(slopee, aspecte, angle = 40, direction = 270)
plot(hille )

hill.p        <-  rasterToPoints(hille)
hill.pa_      <-  data.frame(hill.p)
# ============================================================
# 10. Mapa principal
# ============================================================
library(sf)

# Coordenadas del bounding box
xmin <- -70.6
xmax <- -70.18
ymin <- -13.95
ymax <- -13.1

# Crear polígono
poligono <- st_polygon(list(rbind(
  c(xmin, ymin),  # esquina inferior izquierda
  c(xmax, ymin),  # esquina inferior derecha
  c(xmax, ymax),  # esquina superior derecha
  c(xmin, ymax),  # esquina superior izquierda
  c(xmin, ymin)   # cerrar polígono
)))

# Convertir a sf
poligono_sf <- st_sf(
  geometry = st_sfc(poligono),
  crs = 4326
)

elev = get_elev_raster(poligono_sf, z=12)
Poligo_    <- crop(elev, poligono_sf)                           #
Poligo_   <- Poligo_ <- mask(Poligo_, poligono_sf)
plot(Poligo_)

slope    = terrain(Poligo_  , opt = "slope")
aspect    = terrain(Poligo_, opt = "aspect")
hill     = hillShade(slope, aspect, angle = 40, direction = 270)
plot(hill)

hil        <-  rasterToPoints(hill)
hill.p      <-  data.frame(hil)


mapa_principal <- ggplot() +
  geom_raster(data = hill.pa_, aes(x,y, fill = layer), show.legend = F)+
  scale_fill_gradientn(colours=grey(1:100/100))+
  new_scale_fill()+
  geom_raster(data = hill.p, aes(x,y, fill = layer), show.legend = F)+
  scale_fill_gradientn(colours=grey(1:100/100))+
  new_scale_fill()+
  geom_raster(
    data = df_map,
    aes(x = x, y = y, fill = clase), alpha=0.7
  ) +
  scale_fill_manual(values = clrs, na.value = "transparent") +
  geom_sf(data = manu, fill = NA, color = "black", linewidth = 1) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white")
  ) +
  annotation_north_arrow(
    location = "br",
    which_north = "true",
    height = unit(1.2, "cm"),
    width = unit(1.2, "cm"),
    style = north_arrow_fancy_orienteering
  ) +
  coord_sf(expand = FALSE) +
  labs(
    x = "Longitud",
    y = "Latitud",
    fill = "Cobertura y uso del suelo"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "grey75", linewidth = 0.25),
    panel.background = element_rect(fill = "#f8f8f8", color = NA),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    axis.text = element_text(size = 8, color = "black", face = "bold"),
    
    axis.text.x = element_text(size = 7, face = "bold"),
    axis.text.y = element_text(size = 7, angle = 90)
  )+
  coord_sf(xlim = c(-70.6,-70.18), ylim = c(-13.95,-13.1),expand = FALSE)

mapa_principal

# Insertar mapas de ubicación
mapa_principal_inset <- ggdraw(mapa_principal) +
  draw_plot(inset_peru, x = 0.09, y = 0.77, width = 0.26, height = 0.22) +
  draw_plot(inset_mdd,  x = 0.08, y = 0.58, width = 0.26, height = 0.18)
mapa_principal_inset

ggsave(
  filename = "png/Mapa.png",
  plot = mapa_principal_inset,
  width = 13,
  height = 26,
  units = "cm",
  dpi = 500,
  bg = "white"
)



# ============================================================
# 11. Tabla de áreas como gráfico
# ============================================================

tabla_plot <- ggplot(tabla_area, aes(x = 1, y = reorder(clase, area_km2))) +
  geom_tile(aes(fill = clase), width = 0.12, height = 0.7) +
  geom_text(aes(x = 1.15, label = clase), hjust = 0, size = 3.2) +
  geom_text(aes(x = 2.7, label = area_km2), hjust = 1, size = 3.1) +
  geom_text(aes(x = 3.55, label = paste0(area_pct, "%")), hjust = 1, size = 3.1) +
  scale_fill_manual(values = clrs) +
  xlim(0.8, 3.7) +
  labs(title = "Tabla de áreas por clase") +
  theme_void() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 11),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8)
  )
tabla_plot
# ============================================================
# 12. Gráfico de barras
# ============================================================

bar_plot <- ggplot(
  tabla_area, aes(x = reorder(clase, area_km2), y = area_km2, fill = clase)) +
  geom_col(color = "black",linewidth = 0.25,width = 0.75) +
  geom_text(
    aes(label = clase,y = area_km2 + max(tabla_area$area_km2) * 0.02 ),
    size = 3,fontface = "bold", angle = 0, hjust = 0) +
  scale_fill_manual(values = clrs) +
  coord_flip() +
  labs(
    title = "Gráfico de barras de cobertura de área",x = NULL, y = "Área (km²)") +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 12),
    axis.text.y = element_blank(), # OCULTA nombres laterales
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 9,color = "black", face = "bold"),
    axis.title = element_text(face = "bold", size = 11),
    panel.border = element_rect( color = "black",fill = NA,linewidth = 0.8 ),
    panel.grid.minor = element_blank())

bar_plot
# ============================================================
# 13. Gráfico circular tipo dona
# ============================================================

donut_plot <- ggplot(tabla_area, aes(x = 2, y = area_km2, fill = clase)) +
  geom_col(color = "white", linewidth = 0.35) +
  coord_polar(theta = "y") +
  xlim(0.5, 2.5) +
  scale_fill_manual(values = clrs) +
  labs(title = "Gráfico circular de cobertura de área") +
  theme_void() +
  theme(
    legend.position = c(0.03, 0.5),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 11),
    legend.title = element_blank(),
    legend.text = element_text(size = 7),
    legend.key.size = unit(0.5, "cm"),
    panel.border = element_rect(color = "white", fill = NA, linewidth = 0.8)
  )

donut_plot 
# ============================================================
# 14. Leyenda cartográfica
# ============================================================

leyenda_plot <- ggplot(tabla_area, aes(x = 1, y = reorder(clase, area_km2))) +
  geom_tile(aes(fill = clase), width = 0.2, height = 0.6) +
  geom_text(aes(x = 1.25, label = clase), hjust = 0, size = 3.3) +
  scale_fill_manual(values = clrs) +
  xlim(0.8, 3.2) +
  labs(title = "Convenciones cartográficas") +
  theme_void() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 11),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8)
  )
leyenda_plot
# ============================================================
# 15. Panel de título
# ============================================================

titulo_plot <- ggplot() +
  annotate(
    "text",
    x = 0.5,
    y = 0.65,
    label = "DISTRITO DE SAN GABAN\nPUNO, PERÚ",
    size = 4.8,
    fontface = "bold"
  ) +
  annotate(
    "text",
    x = 0.5,
    y = 0.30,
    label = "COBERTURA Y USO\nDEL SUELO",
    size = 8,
    fontface = "bold"
  ) +
  xlim(0, 1) +
  ylim(0, 1) +
  theme_void() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8)
  )
titulo_plot 
# ============================================================
# 16. Nota y propiedades del mapa
# ============================================================

nota_plot <- ggplot() +
  annotate(
    "text",
    x = 0.02,
    y = 0.72,
    hjust = 0,
    label = "NOTA\nDatos de cobertura y uso del suelo \nobtenidos de MapBiomas Perú\nColección 3, año 2024. Las áreas fueron \ncalculadas en km²\na partir del raster clasificado y \nel límite distrital de Manu.",
    size = 2.5
  ) +
  annotate(
    "text",
    x = 0.58,
    y = 0.70,
    hjust = 0,
    label = "
    Propiedades del mapa\nCRS: EPSG:4326 - WGS84\nÁrea de estudio: Distrito de San Gaban\nProvincia: Carabaya\nDepartamento: Puno\nFuente LULC: MapBiomas Perú C3",
    size = 2.5
  ) +
  xlim(0, 1) +
  ylim(0, 1) +
  theme_void() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8)
  )
nota_plot

ggsave(
  filename = "png/nota.png",
  plot = nota_plot,
  width = 13.3,
  height = 3,
  units = "cm",
  dpi = 500,
  bg = "white"
)



fuente_plot <- ggplot() +
  
  # TÍTULO
  annotate(
    "text",
    x = 0.5,
    y = 0.78,
    label = "Cobertura y uso del suelo del distrito de San Gaban",
    size = 4.5,
    fontface = "bold",
    lineheight = 0.9
  ) +
  
  # FUENTES
  annotate(
    "text",
    x = 0.5,
    y = 0.45,
    label = paste(
      "Mapa base: límites GADM",
      "Datos LULC: MapBiomas Perú 2024",
      "Procesamiento y diseño cartográfico: R",
      "Gorky Florez Castillo",
      sep = "\n"
    ),
    size = 3.4,
    lineheight = 1.05   # <-- controla separación entre líneas
  ) +
  
  coord_cartesian(xlim = c(0,1), ylim = c(0,1)) +
  
  theme_void() +
  
  theme(
    panel.border = element_rect(
      color = "black",
      fill = NA,
      linewidth = 0.8
    )
  )

fuente_plot

# ============================================================
# 17. Composición final tipo póster
# ============================================================

panel_derecho <- titulo_plot /
  tabla_plot /
  bar_plot /
  donut_plot /
  leyenda_plot /
  fuente_plot +
  plot_layout(heights = c(1.1, 1.6, 1.4, 1.4, 1.5, 0.9))



# ============================================================
# 18. Exportar mapa
# ============================================================

ggsave(
  filename = "png/Leyenda.png",
  plot = panel_derecho,
  width = 5,
  height = 18,
  units = "in",
  dpi = 500,
  bg = "white"
)
