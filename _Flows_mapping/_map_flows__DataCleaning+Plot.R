

remove(list = ls())

## Set work dir ------------------------------------------------------------
path <- rstudioapi::getSourceEditorContext()$path
dir  <- dirname(rstudioapi::getSourceEditorContext()$path); dir
# wd   <- paste0(dir, '/agri/ag_trade_data')
setwd(dir)

library(tidyverse)
library(tidyr)
library(dplyr)



Sys.setlocale(category = "LC_ALL", locale = "chs") #cht for traditional Chinese, etc.
Sys.setlocale(category = "LC_CTYPE", locale = "chs")


### Data cleaning ------------------------------------------------------------
### ~~ test code ---------------------------------------------------------------

f <- './_data/pop_flow_6th_2010/上海.txt' # 上海.txt, 甘肃.txt

### section 1 of the data
pop <- read.csv(f, 
                sep = '',
                nrows = 30, skip = 1, 
                header = F,
                # encoding="UTF-8", 
                # fileEncoding = "UTF-8",
                stringsAsFactors = F)

names(pop) <- c('to', 'from', 'num')

pop1 <- pop %>%  select(from, to, num)

head(pop1)


### the data

pop <- read.csv(f, sep = '', 
                # nrows = 30, 
                skip = 33, header = F,
                stringsAsFactors = T) %>% as.data.frame()
ncol(pop)

names(pop) <- c('to', 'from_num')

pop2 <- pop %>% 
  dplyr::mutate(num = as.numeric(gsub("\\D", "", from_num))) %>%
  dplyr::mutate(from = as.character(gsub("[[:digit:]]+", "", from_num))) %>%
  select(from, to, num)

head(pop2)

## 
pop <- rbind(pop1, pop2)




### ~~ for loop ----------------------------------------------------------------
fs <- list.files(path = './_data/pop_flow_6th_2010', pattern = '.txt', full.names = T); fs   
length(fs) # total 31 
df <- data.frame()

for (f in fs) {
  
  print(f)
  
  ### section 1 of the data
  pop <- read.csv(f, sep = '', nrows = 30, skip = 1, header = F, stringsAsFactors = F)

  names(pop) <- c('to', 'from', 'num')

  pop1 <- pop %>% select(from, to, num)

  
  ### section 2 of the data
  pop <- read.csv(f, sep = '',  skip = 33, header = F, stringsAsFactors = T) %>% as.data.frame()

  names(pop) <- c('to', 'from_num')
  
  
  
  if (ncol(pop) < 3) { ### sometimes it come out 3 cols
    pop2 <- pop %>%
      dplyr::mutate(num = as.numeric(gsub("\\D", "", from_num))) %>%
      dplyr::mutate(from = as.character(gsub("[[:digit:]]+", "", from_num))) %>%
      select(from, to, num)
  } else {
    names(pop) <- c('to', 'from', 'num')
  }

  
  
  ##
  pop12 <- rbind(pop1, pop2)

  df <- rbind(df, pop12)
}


nrow(df) # 31*60 = 1860




### ~~ Chinese chr to English -----------------------------------------------

### https://github.com/pzhaonet/pinyin

# devtools::install_github("pzhaonet/pinyin")
library(pinyin)


mypy <- pydic(method = 'toneless', 
              # method = 'quanpin',
              dic = c("pinyin2"), multi = FALSE) # load dictionary 
py("河南", dic = mypy, sep = '')


capFirst <- function(s) {
  paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "")
}



df.id <- df %>% 
  dplyr::mutate(id = row_number())

df.long <- df.id %>% 
  gather(from_to, name, from:to, factor_key=TRUE)


df.long.en <- df.long %>% mutate(name_en = capFirst(
  py(name, dic = mypy, sep = '')
  ))


### replace the wrong ones

## Xicang   --> Tibet
## Shanxi   --> Shaanxi 陕西
## Neimengu --> Inner Mongolia

df.long.en.replace <- df.long.en %>% 
  mutate(name_en2 = ifelse(grepl("陕西",name),"Shaanxi", 
                           ifelse(grepl("Neimenggu",name_en),'Inner Mongolia', 
                                  ifelse(grepl("Xicang",name_en),"Tibet", name_en)))) %>%
  dplyr::select(id, from_to, name_en2, num)

df.final <- df.long.en.replace %>%
  spread(key = from_to, value = name_en2)

# Saving on object in RData format
getwd()
save(df.final, file = "pop_flow_data.RData")

load("pop_flow_data.RData")

### Shapefiles data -----------------------------------------------------------------

library(sf) # much faster than rgdal
library(sp)
library(rgeos)
library(rgdal)
library(dplyr)
# shp <- sf::st_read(dsn = "G:/My Drive/_paper/China_SDG/_Nature#2_20181214/shp_cn", 
#                    layer = "cn_prov")
shp <- sf::st_read(dsn = "G:/My Drive/_data/shp/CHN_mainland", 
                   layer = "China_province") %>% st_transform(4269)
shp <- shp[-c(33,34), c(77,123,128,131)] # remove additional cols

str(shp)



# shp <- st_transform(shp, "+init=epsg:4490") ## 4490, 7408, 21453
# 
plot(shp, axes=T)



# 1. get the centroids of each province: rgeos ---------------------------------

# using rgeos
sp_cent <- gCentroid(spgeom = as(shp, "Spatial"), byid = TRUE)
sp_cent@proj4string
sp_cent@coords
centroids.rgeos <- sp_cent@coords  %>% as.data.frame()

class(shp)
shp.df <- as.data.frame(shp)
shp_with_cen <- cbind(shp.df, centroids.rgeos)
names(shp_with_cen)

shp_with_cen_cn <-  shp_with_cen %>% 
  dplyr::select(PINYIN_NAM, x, y)

names(shp_with_cen_cn) <- c('GEOID', 'lon', 'lat')



# Hebei -------------------------------------------------------------------

#### Hebei, Hebei: 39.53356,  116.08708 (original)
#### Shijiazhuang: 38.039782, 114.514121 (update to)

shp_with_cen_cn <- shp_with_cen_cn %>% 
  mutate(
    # lon = ifelse(State == "WI" & Name == "John_Smith", "John_Smith1", Name)
    lon = ifelse(GEOID == "Hebei", 114.514121, lon),
    lat = ifelse(GEOID == "Hebei", 38.0397820, lat)
  )


## set work dir
path <- rstudioapi::getSourceEditorContext()$path
dir  <- dirname(rstudioapi::getSourceEditorContext()$path); dir
# wd   <- paste0(dir, '/agri/ag_trade_data')
setwd(dir)
dir <- getwd(); dir
fname <- paste0(dir, '_data/results/shp_with_cen_cn', '.csv'); fname
write.csv(shp_with_cen_cn, file = fname, row.names = F)






# merge pop flow data and spatial centroids points -----------------------------

# test <- df.final[1:30,]

dt0 <- merge(df.final, shp_with_cen_cn, by.x = 'to', by.y='GEOID', all.x=T)
# Rename 
names(dt0)[names(dt0)=="lat"] <- "end_lat"
names(dt0)[names(dt0)=="lon"] <- "end_lon"


dt1 <- merge(dt0, shp_with_cen_cn, by.x = 'from', by.y='GEOID', all.x=T)
# Rename 
names(dt1)[names(dt1)=="lat"] <- "start_lat"
names(dt1)[names(dt1)=="lon"] <- "start_lon"






# net flow ----------------------------------------------------------------
dt01 <- dt1 %>% mutate(id_out = gsub(pattern = ' ', '', paste0(from, '_', to)), ## 31*30=930
                       id_in  = gsub(pattern = ' ', '', paste0(to, '_', from))
                       )
### remove duplicated data -- because of repeated data appeared in the txt of both provinces
dt02 <- dt01[!duplicated(dt01$id_out),]

# dir <- getwd(); dir
# fname <- paste0(dir, '_data/results/pop_flow.csv'); fname
# write.csv(dt02, file = fname, row.names = F)



# net flow ----------------------------------------------------------------

prov.list <- unique(dt02$from)
prov.list
length(prov.list)


dt_NetFlow <- data.frame()

for (p in prov.list){
  
  print(p)
  ### select one province, from which flow out to others
  dt03 <- dt02 %>% filter(from == p) %>% dplyr::select(-id_in)     # test 'Anhui' ## keep id_out
  # Rename 
  names(dt03)[names(dt03)=="num"] <- "num_out"
  
  
  ### select one province, to which flow in from others
  dt04 <- dt02 %>% filter(to   == p) %>% dplyr::select(num, id_in) #%>% # test 'Anhui'
    # mutate(id_in_ = id_in)
  # Rename 
  names(dt04)[names(dt04)=="num"] <- "num_in"
  
  
  ### match the pairs, and cal net flow
  dt05_match <- merge(dt03, dt04, by.x='id_out', by.y='id_in') %>% 
    mutate(num_net_out = num_out - num_in)
  
  
  ### bind all together
  dt_NetFlow <- rbind(dt_NetFlow, dt05_match)
  
}

dt_NetFlow_over0 <- dt_NetFlow %>% filter(num_net_out >= 0) %>%
  mutate(mark = 
           ifelse(grepl("Beijing",from),100, 
                  ifelse(grepl("Henan",from), 1, NA)
           )
  )
### http://www.jisuanqinet.com/shuxue/pailiezuhe.html
### C(31,2) = 465






##########################
data <- dt02 %>% dplyr::select(from, to, num)
data1 <- data %>% left_join(data, by = c("from" = "to", "to" = "from")) %>%
  mutate(num_net_out = num.x - num.y,
         id_out     = gsub(pattern = ' ', '', paste0(from, '_', to))) %>%
  dplyr::select(from, to, id_out, num_net_out) %>%
  filter(num_net_out >= 0)




# test flow between provinces ---------------------------------------------

### highlight Beijing and Henan by assigning values

dt2 <- dt1 %>% mutate(mark = 
                        ifelse(grepl("Beijing",from),100, 
                               ifelse(grepl("Henan",from), 1, NA)
                               )
                      )



### .1 Henan, Beijing related --------------------------------------------------

dt.oi_1 <- dt_NetFlow_over0 %>% dplyr::filter(from %in% c('Henan', 'Beijing'))
dt.oi_2 <- dt_NetFlow_over0 %>% dplyr::filter(to   %in% c('Henan', 'Beijing'))
dt.oi   <- rbind(dt.oi_1, dt.oi_2)

### .2 Beijing related pop flow --------------------------------------------------

dt.oi_1 <- dt_NetFlow_over0 %>% dplyr::filter(from %in% c('Beijing'))
dt.oi_2 <- dt_NetFlow_over0 %>% dplyr::filter(to   %in% c('Beijing'))
dt.oi   <- rbind(dt.oi_1, dt.oi_2)

# ### .3 Henan and Beijing
# dt.oi   <- dt2 %>% dplyr::filter(from %in% c('Henan', 
#                                              'Tianjin', 'Hebei',
#                                              'Beijing') &
#                                    to %in% c('Henan', 
#                                              'Tianjin', 'Hebei',
#                                              'Beijing'))



# ### .4 Yunnan and Guangdong
# dt.oi   <- dt_NetFlow_over0 %>% dplyr::filter(from %in% c('Yunnan', 'Guangdong') &
#                                                 to %in% c('Yunnan', 'Guangdong'))



# ### .5
# dt.oi   <- dt_NetFlow_over0 %>% 
#   dplyr::filter(from %in% c('Henan', 
#                             'Tianjin', 'Hebei',
#                             'Beijing') &
#                   to %in% c('Henan',
#                             'Tianjin', 'Hebei',
#                             'Beijing')) %>%
#     mutate(mark = 
#            ifelse(grepl("Beijing",from),100, 
#                   ifelse(grepl("Henan",from), 1, NA)
#            )
#   )



# map ---------------------------------------------------------------------

# install.packages("remotes")
# remotes::install_github('JohnCoene/echarts4r.maps')

library(echarts4r.maps)

library(echarts4r)


flows <- 
  dt.oi %>%
  # dt_NetFlow_over0 %>%
  dplyr::mutate(width = scales::rescale(num_net_out, to = c(0, 20))) %>%
  dplyr::mutate(region = 
                  ifelse(grepl("Beijing",from),'北京', 
                         ifelse(grepl("Henan",from), '河南', NA)
                         )
                ) %>%
    dplyr::select(start_lat, start_lon, 
                end_lat, end_lon,
                id_out, from, to, num_out,  num_in, num_net_out, 
                region,
                width, mark)

# Rename 
names(flows)[names(flows)=="num_net_out"] <- "num"



# More icons available with 
# echarts.assets
# https://echarts4r-assets.john-coene.com/
icon <- 'path://M1705.06,1318.313v-89.254l-319.9-221.799l0.073-208.063c0.521-84.662-26.629-121.796-63.961-121.491c-37.332-0.305-64.482,36.829-63.961,121.491l0.073,208.063l-319.9,221.799v89.254l330.343-157.288l12.238,241.308l-134.449,92.931l0.531,42.034l175.125-42.917l175.125,42.917l0.531-42.034l-134.449-92.931l12.238-241.308L1705.06,1318.313z'


# region = c("Beijing", "Shanghai", "Jilin")
# region = c("北京", "黑龙江", "广州")
# value  = c(1,2, 3)





bg <- flows %>% dplyr::select(region, mark) %>% filter(!is.na(region))

# map flow ----------------------------------------------------------------

flows %>%
# bg %>%  

  
  e_charts() %>%
  ### remove "China", will get the world map
  e_geo("China") %>%
  em_map("China") %>% # sub-national maps
  
    
  ### subset extent based on lng and lat
  # e_geo(
  #   roam = TRUE,
  #   zoom = 1.5,
  #   # nameMap = 'China',
  #   boundingCoords = list(  # China lat: lon: 73.66~135.05; 3.86~53.55
  #     c(75,  57),      # [lng, lat] of left-top corner
  #     c(135, 15)       # [lng, lat] of right-bottom corner
  #   )
  # ) %>% 
  
  e_lines(
    start_lon,
    start_lat, ## source_lon, source_lat
    end_lon,
    end_lat,   ## target_lon, target_lat
    from,
    to,
    num,       ## source_name, target_name, value
    
    name = "flights",  ## coord_system = "geo", name = NULL
    
    # effect = list(
    #   show = TRUE,
    #   symbol = icon,
    #   trailLength = 0.5
    # ),

    
   
    lineStyle = list(
      curveness = 0.5,
      
      ### Linear gradient. First four parameters are x0, y0, x2, and y2, each ranged from 0 to 1,
      ###  standing for percentage in the bounding box.
      ###  If global is `true`, then the first four parameters are in absolute pixel positions.
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
  e_toolbox_feature(feature = "dataZoom") #%>%
  
  
  # ## add colors
  # e_data(bg) %>%
  # e_charts(region) %>%
  # em_map("China") %>%
  # e_map(mark, map = "China") %>% 
  # e_visual_map(mark) %>% 
  # e_theme("infographic") # %>% 
  # # e_geo("China")

# 



# ################################# #
# sessions %>% 
#   e_country_names(Country, Country, type = "country.name") %>% # helper
#   e_charts(Country) %>% 
#   e_map(Sessions) %>% 
#   e_visual_map(Sessions) %>% 
#   e_data(flights) %>% 
#   e_geo() %>% 
#   e_lines(
#     start_lon, 
#     start_lat, 
#     end_lon, 
#     end_lat,
#     name = "flights",
#     lineStyle = list(normal = list(curveness = 0.3))
#   )








