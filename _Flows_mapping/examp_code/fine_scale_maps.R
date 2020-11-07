

# easy way #1 +++++++++ ---------------------------------------------------


library(echarts4r.maps)

em_bank()

df <- data.frame(
  # name = c("Beijing", "Jiangsu", "Henan"),
  region = c("北京", "江苏", "河南"),
  value = c(1,2, 3)
)





df %>% 
  e_charts(region) %>%
  em_map("China") %>% 
  e_map(value, map = "China") %>% 
  e_visual_map(value) %>% 
  e_theme("infographic") %>%

  
  e_data(flows) %>%
  e_geo("China") %>% 

  

e_lines(
  start_lon,
  start_lat, ## source_lon, source_lat
  end_lon,
  end_lat,   ## target_lon, target_lat
  from,
  to,
  num,       ## source_name, target_name, value
  
  name = "flights",  ## coord_system = "geo", name = NULL


  lineStyle = list(
    curveness = 0.5,
    color = list(
      type = "linear",
      x = 0,
      y = 0,
      x2 = 0, # 0
      y2 = 1,
      colorStops = list(
        list(
          offset = 0, color = 'red'  # color at 0% position
        ),
        list(
          offset = 1, color = 'blue' # color at 100% position
        )
      ),
      global = FALSE
    )

  )
) %>%
  
  e_add("lineStyle", width) %>% 
  e_toolbox_feature(feature = "dataZoom")






# easy way #2 +++ ---------------------------------------------------------

library(echarts4r)

df <- data.frame(
  name = c("Beijing", "Jiangsu", "Henan"),
  values = c(3, 5, 7)
)

# nation <- jsonlite::read_json("https://code.highcharts.com/mapdata/countries/cn/cn-all.geo.json")
nation <- jsonlite::read_json("https://code.highcharts.com/mapdata/countries/cn/custom/cn-all-sar-taiwan.geo.json")

### data source: https://code.highcharts.com/mapdata/


nation$crs$properties <- "urn:ogc:def:crs:OGC:1.3:CRS84"  #"urn:ogc:def:crs:EPSG:4490"


df %>% 
  e_charts(name) %>% 
  e_map_register("nation_whatever", nation) %>% 
  e_map(values, map = "nation_whatever") %>% 
  e_visual_map(values)%>% 
  e_theme("infographic")






# easy way #3 +  ---------------------------------------------------------

# ref: https://echarts4r.john-coene.com/articles/make-geo-json.html
library(dplyr)
library(echarts4r)
library(echarts4r.maps)


library(echarts4r)
library(sp)
library(raster)
library(geojsonio)

# sp: spatial data management package.
# raster: get spatial data from gadm.org.
# geojsonio: convert spatial data into json format.
# rmapshaper: simplify the spatial data.

library(rmapshaper)


india_sp <- raster::getData('GADM', country = 'China', level = 1) # 
india_sp %>% 
  head() %>% 
  knitr::kable()



india_sp@data$mark <- (nchar(india_sp@data$NL_NAME_1) + runif(1, min=1, max=100))
india_sp_data      <- india_sp@data


india_json <- geojsonio::geojson_list(india_sp)
print(object.size(india_json), units = "Mb")

india_small <- rmapshaper::ms_simplify(india_sp, keep = 0.05) 
india_json_small <- geojsonio::geojson_list(india_small)
print(object.size(india_json_small), units = "Mb") 


india_json_small$features <- india_json_small$features %>% 
  purrr::map(function(x){ 
    x$properties$name <- x$properties$NAME_2 # copy NAME_2 to name for tooltip
    return(x)
  })


india_sp_data$mark <- as.numeric(india_sp_data$mark)

india_json_small$crs$properties

# plot 
india_sp_data %>%
  e_charts() %>%
  e_map_register("India_small", india_json_small) %>%
  e_map(map = "India_small")
  e_map(map = "India_small", serie = mark)

  ################ not work ############### #
  ### add color #2
  dplyr::mutate(states = NAME_1) %>%
  e_charts(states) %>%
  e_map_register("India_small", india_json_small) %>%
  e_map(mark, map = "India_small") %>%
  # e_map(mark) %>% 
  e_visual_map(mark) 


  ################ not work ############### #
### add color #1
india_sp_data %>% 
  e_charts(NAME_1) %>% 
  e_map_register("India_small", india_json_small) %>%
  # e_map(mark) %>% 
  e_map(mark, map = "India_small") %>%
  e_visual_map(min = 1, max = 25) 






