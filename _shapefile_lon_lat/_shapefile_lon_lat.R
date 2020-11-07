
# add lon and lat to shp in R
# LIYJ@MSU.EDU
# 2019/05/21 21:33 PM

remove(list = ls())

## set work dir
path <- rstudioapi::getSourceEditorContext()$path
dir  <- dirname(rstudioapi::getSourceEditorContext()$path); dir
setwd(dir)


library(sf)
# library(rgeos)
library(dplyr)
# library(ggplot2)

# ~~ my data --------------------------------------------------------------
getwd()
list.dirs()

shp.dir <- "./R"
shp.lyr <- 'tpsite2_pourpoint500_point'

shp  <- sf::st_read(dsn = shp.dir, layer = shp.lyr) # %>% st_transform(32617)

### Transform or convert coordinates of simple feature
shp <- st_transform(x = shp, crs = "+init=epsg:4269")

st_crs(shp)
str(shp)

xy <- st_coordinates(x = shp) %>% as.data.frame() %>% dplyr::mutate(id = row_number())

shp_xy <- merge(x = shp, y = xy, by.x = 'pointid', by.y = 'id', all.x = T)


### write to file

getwd()
fname <- paste0('./R/tpsite2_pourpoint500_point_xy.shp'); fname
st_write(shp_xy, fname, delete_layer = TRUE) # overwrites







