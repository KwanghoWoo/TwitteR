library(spData)
library(tmap)
library(sf)
world_moll = st_transform(world, crs = "+proj=moll")
tm_shape(world_moll) +
  tm_polygons(col = "MAP_COLORS")

tm_shape(world_moll) +
  tm_polygons(col = "MAP_COLORS", palette="Pastel1")

