## set work dir
path <- rstudioapi::getSourceEditorContext()$path
dir  <- dirname(rstudioapi::getSourceEditorContext()$path); dir
# wd   <- paste0(dir, '/agri/ag_trade_data')
setwd(dir)

## This gist shows how to create Flow Maps in R using ggplot2. 
## source: This is based on different bits of code from other with amazing R skills: 

#' @ceng_l          : http://web.stanford.edu/~cengel/cgi-bin/anthrospace/great-circles-on-a-recentered-worldmap-in-ggplot
#' @3wen            : http://egallic.fr/maps-with-r/
#'   @spatialanalysis : http://spatialanalysis.co.uk/2012/06/mapping-worlds-biggest-airlines/
#'   @freakonometrics : http://freakonometrics.hypotheses.org/48184


# Libraries
library(maps)
library(geosphere)
library(dplyr)
library(ggplot2)
library(rworldmap)
library(plyr)
library(data.table)
library(ggthemes)


# Get World map
worldMap <- getMap()
mapworld_df <- fortify( worldMap )


# Read data on airports and flights
airports <- read.csv("http://www.stanford.edu/~cengel/cgi-bin/anthrospace/wp-content/uploads/2012/03/airports.csv", as.is=TRUE, header=TRUE)
flights <- read.csv("http://www.stanford.edu/~cengel/cgi-bin/anthrospace/wp-content/uploads/2012/03/PEK-openflights-export-2012-03-19.csv", as.is=TRUE, header=TRUE)

# get airport locations
airport_locations <- airports[, c("IATA","longitude", "latitude")]

# aggregate number of flights (frequency of flights per pair)
flights.ag <- ddply(flights, c("From","To"), function(x) count(x$To))


# Link airport lat  long to origin and destination
OD <- left_join(flights.ag, airport_locations, by=c("From"="IATA") )
OD <- left_join(OD, airport_locations, by=c("To"="IATA") )
OD$id <-as.character(c(1:nrow(OD))) #create and id for each pair



##### Two Simple Maps ##### 

# 1. Using straight lines
ggplot() + 
  geom_polygon(data= mapworld_df, aes(long,lat, group=group), fill="gray30") +
  geom_segment(data = OD, aes(x = longitude.x, y = latitude.x, xend = longitude.y, yend = latitude.y, color=freq),
               arrow = arrow(length = unit(0.01, "npc"))) +
  scale_colour_distiller(palette="Reds", name="Frequency", guide = "colorbar") +
  coord_equal()


# 2. Using Curved Lines
ggplot() + 
  geom_polygon(data= mapworld_df, aes(long,lat, group=group), fill="gray30") +
  geom_curve(data = OD, aes(x = longitude.x, y = latitude.x, xend = longitude.y, yend = latitude.y, color=freq),
             curvature = -0.2, arrow = arrow(length = unit(0.01, "npc"))) +
  scale_colour_distiller(palette="Reds", name="Frequency", guide = "colorbar") +
  coord_equal() +
  theme_bw()



##### A more professional map ####  
# Using shortest route between airports considering the spherical curvature of the planet

# get location of Origin and destinations airports
setDT(OD) # set OD as a data.table for faster data manipulation
beijing.loc <- OD[ From== "PEK", .(longitude.x, latitude.x)][1] # Origin
dest.loc <- OD[ , .(longitude.y, latitude.y)] # Destinations

# calculate routes between Beijing (origin) and other airports (destinations)
routes <- gcIntermediate(beijing.loc, dest.loc, 100, breakAtDateLine=FALSE, addStartEnd=TRUE, sp=TRUE)
class(routes) # SpatialLines object


# Convert a SpatialLines object into SpatialLinesDataFrame, so we can fortify and use it in ggplot
# create empty data frate  
ids <- data.frame()
# fill data frame with IDs for each line
for (i in (1:length(routes))) {         
  id <- data.frame(routes@lines[[i]]@ID)
  ids <- rbind(ids, id)  }

colnames(ids)[1] <- "ID" # rename ID column

# convert SpatialLines into SpatialLinesDataFrame using IDs as the data frame
routes <- SpatialLinesDataFrame(routes, data = ids, match.ID = T)

# Fortify routes (convert to data frame)  +++  join attributes
routes_df <- fortify(routes, region= "ID") # convert into something ggplot can plot
gcircles <- left_join(routes_df, OD, by= ("id"))
head(gcircles)

### Recenter ####

center <- 115 # positive values only - US centered view is 260

# shift coordinates to recenter great circles
gcircles$long.recenter <-  ifelse(gcircles$long  < center - 180 , gcircles$long + 360, gcircles$long) 

# shift coordinates to recenter worldmap
worldmap <- map_data ("world")
worldmap$long.recenter <-  ifelse(worldmap$long  < center - 180 , worldmap$long + 360, worldmap$long)

### Function to regroup split lines and polygons
# takes dataframe, column with long and unique group variable, returns df with added column named group.regroup
RegroupElements <- function(df, longcol, idcol){  
  g <- rep(1, length(df[,longcol]))
  if (diff(range(df[,longcol])) > 300) {          # check if longitude within group differs more than 300 deg, ie if element was split
    d <- df[,longcol] > mean(range(df[,longcol])) # we use the mean to help us separate the extreme values
    g[!d] <- 1     # some marker for parts that stay in place (we cheat here a little, as we do not take into account concave polygons)
    g[d] <- 2      # parts that are moved
  }
  g <-  paste(df[, idcol], g, sep=".") # attach to id to create unique group variable for the dataset
  df$group.regroup <- g
  df
}

### Function to close regrouped polygons
# takes dataframe, checks if 1st and last longitude value are the same, if not, inserts first as last and reassigns order variable
ClosePolygons <- function(df, longcol, ordercol){
  if (df[1,longcol] != df[nrow(df),longcol]) {
    tmp <- df[1,]
    df <- rbind(df,tmp)
  }
  o <- c(1: nrow(df))  # rassign the order variable
  df[,ordercol] <- o
  df
}

# now regroup
gcircles.rg <- ddply(gcircles, .(id), RegroupElements, "long.recenter", "id")
worldmap.rg <- ddply(worldmap, .(group), RegroupElements, "long.recenter", "group")

# close polys
worldmap.cp <- ddply(worldmap.rg, .(group.regroup), ClosePolygons, "long.recenter", "order")  # use the new grouping var



# Flat map
ggplot() +
  geom_polygon(data=worldmap.cp, aes(long.recenter,lat,group=group.regroup), size = 0.2, fill="#f9f9f9", color = "grey65") +
  geom_line(data= gcircles.rg, aes(long.recenter,lat,group=group.regroup, color=freq), size=0.4, alpha= 0.5) +
  scale_colour_distiller(palette="Reds", name="Frequency", guide = "colorbar") +
  theme_map()+
  ylim(-60, 90) +
  coord_equal() +
  theme_bw()


# Spherical Map
ggplot() +
  geom_polygon(data=worldmap.cp, aes(long.recenter,lat,group=group.regroup), size = 0.2, fill="#f9f9f9", color = "grey65") +
  geom_line(data= gcircles.rg, aes(long.recenter,lat,group=group.regroup, color=freq), size=0.4, alpha= 0.5) +
  scale_colour_distiller(palette="Reds", name="Frequency", guide = "colorbar") +
  # Spherical element
  scale_y_continuous(breaks = (-2:2) * 30) +
  scale_x_continuous(breaks = (-4:4) * 45) +
  coord_map("ortho", orientation=c(61, 90, 0)) 


# Any ideas on how to color the oceans ? :)











############ 2 ######################
#load packages and map
require(tidyverse)
require(ggmap)
api = 'AIzaSyDg9jAuIDFil8HdO8Och7crqSbn3lOGrU0'
# save api key
register_google(key = api)
# check if key is saved
has_google_key()
basemap <- get_map("Singapore",
                   source = "stamen",
                   maptype = "toner",
                   zoom = 11)

#oversample data (because of too few rides) and summarize for station pairs
a %>%
  sample_n(size=5000,replace=T) %>%
  mutate(Entry_Station_Lat = as.numeric(as.character(Entry_Station_Lat)),
         Exit_Station_Lat = as.numeric(as.character(Exit_Station_Lat)),
         Entry_Station_Long = as.numeric(as.character(Entry_Station_Long)),
         Exit_Station_Long = as.numeric(as.character(Exit_Station_Long))) %>%
  group_by(Entry_Station_Lat,Entry_Station_Long,Exit_Station_Lat,Exit_Station_Long) %>%
  summarize(count=n()) -> plotData

#extract entry Stations
a %>%
  mutate(Entry_Station_Lat = as.numeric(as.character(Entry_Station_Lat)),
         Exit_Station_Lat = as.numeric(as.character(Exit_Station_Lat)),
         Entry_Station_Long = as.numeric(as.character(Entry_Station_Long)),
         Exit_Station_Long = as.numeric(as.character(Exit_Station_Long))) %>%
  select(Entry_Station_Lat,Entry_Station_Long) %>%
  group_by(Entry_Station_Lat,Entry_Station_Long) %>%
  summarize(freq=n()) -> entryStations

#extract exit stations    
a %>%
  mutate(Entry_Station_Lat = as.numeric(as.character(Entry_Station_Lat)),
         Exit_Station_Lat = as.numeric(as.character(Exit_Station_Lat)),
         Entry_Station_Long = as.numeric(as.character(Entry_Station_Long)),
         Exit_Station_Long = as.numeric(as.character(Exit_Station_Long))) %>%
  dplyr::select(Exit_Station_Lat,Exit_Station_Long) %>%
  group_by(Exit_Station_Lat,Exit_Station_Long) %>%
  summarize(freq=n()) -> exitStations

#plot map, curves with size proportional to frequency, points for entry and exit stations
g = ggplot(plotData)
map = ggmap(basemap, base_layer = g)
map = map + coord_cartesian() +
  geom_curve(color="red",alpha=0.5,curvature=0.2,
             aes(x=Entry_Station_Long,size = count,
                 y=Entry_Station_Lat,
                 xend=Exit_Station_Long,
                 yend=Exit_Station_Lat)) +
  geom_point(data=exitStations,alpha=0.5,size=4,
             aes(x=Exit_Station_Long,
                 y=Exit_Station_Lat)) +
  geom_point(data=entryStations,alpha=0.5,size=4,
             aes(x=Entry_Station_Long,
                 y=Entry_Station_Lat))
map









############################################################################
a <- 
  structure(list(token_id = c(
    1.12374e+19, 1.12374e+19, 1.81313e+19, 
    1.85075e+19, 1.30752e+19, 1.30752e+19, 1.32828e+19, 1.70088e+19, 
    1.70088e+19, 1.70088e+19, 1.05536e+19, 1.44818e+19, 1.44736e+19, 
    1.44736e+19, 1.44736e+19, 1.44736e+19, 1.89909e+19, 1.15795e+19, 
    1.15795e+19, 1.15795e+19, 1.70234e+19, 1.70234e+19, 1.44062e+19, 
    1.21512e+19, 1.21512e+19, 1.95909e+19, 1.95909e+19, 1.50179e+19, 
    1.50179e+19, 1.24174e+19, 1.36445e+19, 1.98549e+19, 1.92068e+19, 
    1.18468e+19, 1.18468e+19, 1.92409e+19, 1.92409e+19, 1.21387e+19, 
    1.9162e+19, 1.9162e+19, 1.40385e+19, 1.40385e+19, 1.32996e+19, 
    1.32996e+19, 1.69103e+19, 1.69103e+19, 1.57387e+19, 1.40552e+19, 
    1.40552e+19, 1.00302e+19),
    Entry_Station_Lat = c(
      1.31509, 1.33261, 
      1.28425, 1.31812, 1.33858, 1.29287, 1.39692, 1.37773, 1.33858, 
      1.33322, 1.28179, 1.30036, 1.43697, 1.39752, 1.27637, 1.39752, 
      1.41747, 1.35733, 1.28405, 1.37773, 1.35898, 1.42948, 1.32774, 
      1.42948, 1.349, 1.36017, 1.34971, 1.38451, 1.31509, 1.31509, 
      1.37002, 1.34971, 1.31231, 1.39169, 1.31812, 1.44909, 1.29341, 
      1.41747, 1.33759, 1.44062, 1.31509, 1.38451, 1.29461, 1.32388, 
      1.41747, 1.27614, 1.39752, 1.39449, 1.33261, 1.31231), 
    Entry_Station_Long = c(
      103.76525,  103.84718, 103.84329, 103.89308, 103.70611, 103.8526, 103.90902,
      103.76339, 103.70611, 103.74217, 103.859, 103.85563, 103.7865, 
      103.74745, 103.84596, 103.74745, 103.83298, 103.9884, 103.85152, 
      103.76339, 103.75191, 103.83505, 103.67828, 103.83505, 103.74956, 
      103.88504, 103.87326, 103.74437, 103.76525, 103.76525, 103.84955, 
      103.87326, 103.83793, 103.89548, 103.89308, 103.82004, 103.78479, 
      103.83298, 103.69742, 103.80098, 103.76525, 103.74437, 103.80605, 
      103.93002, 103.83298, 103.79156, 103.74745, 103.90051, 103.84718, 
      103.83793), 
    Exit_Station_Lat = structure(c(
      48L, 34L, 118L, 60L,
      14L, 54L, 10L, 49L, 49L, 74L, 71L, 65L, 102L, 5L, 102L, 119L, 
      116L, 10L, 13L, 88L, 117L, 66L, 40L, 62L, 117L, 37L, 67L, 34L, 
      85L, 44L, 102L, 44L, 115L, 29L, 92L, 17L, 121L, 70L, 120L, 52L, 
      85L, 34L, 42L, 11L, 4L, 115L, 62L, 48L, 92L, 14L),
      .Label = c(
        "1.27082", "1.27091", "1.27236", "1.27614", "1.27637", "1.27646", "1.27935",
        "1.28221", "1.28247", "1.28405", "1.28621", "1.28819", "1.28932", 
        "1.29287", "1.29309", "1.29338", "1.29341", "1.29461", "1.29694", 
        "1.29959", "1.29974", "1.30034", "1.30252", "1.30287", "1.30392", 
        "1.30394", "1.30619", "1.30736", "1.30842", "1.31139", "1.3115", 
        "1.31167", "1.31188", "1.31509", "1.31654", "1.31756", "1.31913", 
        "1.31977", "1.32008", "1.3205", "1.32104", "1.32388", "1.32573",
        
        "1.32725", "1.32774", "1.33119", "1.33155", "1.33261", "1.33322", 
        "1.33474", "1.33554", "1.33759", "1.33764", "1.33858", "1.33921", 
        "1.34037", "1.34225", "1.34293", "1.3432", "1.34426", "1.34857", 
        "1.349", "1.34905", "1.35158", "1.35733", "1.35898", "1.36017", 
        "1.3625", "1.36849", "1.37002", "1.37121", "1.37304", "1.37666", 
        "1.37775", "1.3786", "1.37862", "1.38001", "1.38029", "1.3803", 
        "1.38178", "1.38269", "1.38295", "1.38399", "1.38423", "1.38451", 
        "1.38671", "1.38672", "1.38777", "1.38814", "1.3894", "1.39147", 
        "1.39169", "1.39189", "1.39208", "1.39389", "1.39449", "1.39452", 
        "1.39628", "1.39692", "1.39717", "1.39732", "1.39752", "1.39821", 
        "1.39928", "1.39962", "1.4023", "1.40455", "1.40511", "1.40524", 
        "1.40843", "1.40961", "1.41184", "1.41588", "1.41685", "1.41747", 
        "1.42526", "1.42948", "1.43256", "1.43697", "1.44062", "1.44909"), 
      class = "factor"),
    Exit_Station_Long = structure(c(
      59L, 19L, 
      27L, 4L, 65L, 3L, 63L, 6L, 6L, 21L, 93L, 121L, 9L, 56L, 9L, 32L, 
      16L, 63L, 44L, 23L, 50L, 12L, 54L, 11L, 50L, 71L, 87L, 19L, 7L, 
      118L, 9L, 118L, 49L, 90L, 96L, 31L, 45L, 61L, 38L, 2L, 7L, 19L, 
      117L, 47L, 34L, 49L, 11L, 59L, 96L, 65L), 
      .Label = c(
        "103.67828", 
        "103.69742", "103.70611", "103.72092", "103.73274", "103.74217", 
        "103.74437", "103.74529", "103.74745", "103.74905", "103.74956", 
        "103.75191", "103.7537", "103.75803", "103.76011", "103.76215", 
        "103.76237", "103.76449", "103.76525", "103.76648", "103.76667", 
        "103.76893", "103.7696", "103.77082", "103.77145", "103.77266", 
        "103.774", "103.77866", "103.78185", "103.78425", "103.78479", 
        "103.7865", "103.78744", "103.79156", "103.79631", "103.79654", 
        "103.79836", "103.80098", "103.803", "103.80605", "103.80745", 
        "103.80781", "103.80978", "103.81703", "103.82004", "103.82592", 
        "103.82695", "103.83216", "103.83298", "103.83505", "103.83918", 
        "103.83953", "103.83974", "103.84387", "103.84496", "103.84596", 
        "103.84673", "103.84674", "103.84718", "103.84823", "103.84955", 
        "103.85092", "103.85152", "103.85226", "103.8526", "103.85267", 
        "103.85436", "103.85446", "103.85452", "103.86088", "103.86149", 
        "103.86275", "103.86291", "103.86395", "103.86405", "103.86896", 
        "103.87087", "103.87135", "103.87534", "103.87563", "103.8763", 
        "103.87971", "103.88003", "103.88126", "103.88243", "103.88296", 
        "103.88504", "103.8858", "103.88816", "103.8886", "103.88934", 
        "103.89054", "103.89237", "103.89313", "103.8938", "103.89548", 
        "103.89719", "103.89723", "103.89854", "103.9003", "103.90051", 
        "103.90208", "103.90214", "103.9031", "103.90484", "103.90537", 
        "103.90597", "103.90599", "103.90663", "103.9086", "103.90902", 
        "103.9126", "103.9127", "103.91296", "103.91616", "103.9165", 
        "103.93002", "103.94638", "103.94929", "103.95337", "103.9884"
      ), class = "factor")), .Names = c("token_id", "Entry_Station_Lat", 
                                        "Entry_Station_Long", "Exit_Station_Lat", "Exit_Station_Long"
      ), row.names = c(10807L, 10808L, 10810L, 10815L, 10817L, 10818L, 
                       10819L, 10820L, 10823L, 10824L, 10826L, 10827L, 10829L, 10831L, 
                       10832L, 10833L, 10834L, 10835L, 10836L, 10838L, 10840L, 10841L, 
                       10843L, 10847L, 10850L, 10852L, 10854L, 10855L, 10859L, 10861L, 
                       10869L, 10872L, 10883L, 10886L, 10891L, 10895L, 10896L, 10897L, 
                       10900L, 10902L, 10903L, 10906L, 10910L, 10911L, 10912L, 10913L, 
                       10915L, 10920L, 10921L, 10924L), class = "data.frame")

                                    

str(a)
require(ggplot2)
require(ggmap)
basemap <- get_map("Singapore",
                   source = "stamen", maptype = 'terrain',
                   # maptype = "toner",
                   zoom = 11)

g <- ggplot(a)
map <- ggmap(basemap, base_layer = g)
map


str(a)
head(a)

map + 
  coord_cartesian() +
  geom_curve(data = a, 
             #size = 1.3,     
             size = as.numeric(token_id)/10^19,
             
             aes(x=as.numeric(Entry_Station_Long),
                 y=as.numeric(Entry_Station_Lat),
                 
                 color=as.factor(token_id), alpha=0.5, 
                 # curvature = 0.15, 
                 
                 xend=as.numeric(as.character(Exit_Station_Long)),
                 yend=as.numeric(as.character(Exit_Station_Lat))
                 ))

#


head(df_vertices)
ggmap_with_curve<- ggmap(map)+
  geom_point(aes(x = Longitude, y = Latitude, size=scaledkWh), colour="red", data = df_vertices, alpha =0.5)+
  scale_size_continuous(range=c(1,30))+
  geom_curve(data=df, aes(x=Longitude_from, y=Latitude_from, xend=Longitude_to, yend=Latitude_to),
             arrow=arrow(angle=15,ends="first",length=unit(0.7,"cm"),type="closed"), 
             size= df$scaledAmount,alpha=0.5, curvature = 0.15)


#







# - change #1 from the author ---------------------------------------------


################################################################ #
# how to change the line width based on variables in the data
## set work dir
path <- rstudioapi::getSourceEditorContext()$path
dir  <- dirname(rstudioapi::getSourceEditorContext()$path); dir
# wd   <- paste0(dir, '/agri/ag_trade_data')
setwd(dir)

library(echarts4r)

flights <- read.csv(
  paste0("https://raw.githubusercontent.com/plotly/datasets/",
         "master/2011_february_aa_flight_paths.csv")
)

flights <- read.csv("./2011_february_aa_flight_paths.csv")[1:20,]

flights <- flights %>% 
  dplyr::mutate(width = scales::rescale(cnt, to = c(0, 15)))

flights %>%
  e_charts() %>%
  e_geo() %>%
  e_lines(
    start_lon,
    start_lat,
    end_lon,
    end_lat,
    airport1,
    airport2,
    cnt,
    lineStyle = list(normal = list(curveness = 0.5)),
    # effect = list(show = TRUE),
    name = "flights"
  ) %>% 
  e_add("lineStyle", width)
#




# - change #2 -------------------------------------------------------------

library(echarts4r)

flights <- read.csv(
  paste0("https://raw.githubusercontent.com/plotly/datasets/",
         "master/2011_february_aa_flight_paths.csv")
)

flights <- flights %>% 
  dplyr::mutate(width = scales::rescale(cnt, to = c(0, 15)))

# More icons available with 
# echarts.assets
# https://echarts4r-assets.john-coene.com/
icon <- 'path://M1705.06,1318.313v-89.254l-319.9-221.799l0.073-208.063c0.521-84.662-26.629-121.796-63.961-121.491c-37.332-0.305-64.482,36.829-63.961,121.491l0.073,208.063l-319.9,221.799v89.254l330.343-157.288l12.238,241.308l-134.449,92.931l0.531,42.034l175.125-42.917l175.125,42.917l0.531-42.034l-134.449-92.931l12.238-241.308L1705.06,1318.313z'

flights %>%
  e_charts() %>%
  e_geo() %>%
  e_lines(
    start_lon,
    start_lat,
    end_lon,
    end_lat,
    airport1,
    airport2,
    cnt,
    # lineStyle = list(normal = list(curveness = 0.5)),
    name = "flights",
    effect = list(
      show = TRUE,
      symbol = icon,
      trailLength = .1
    ),
    lineStyle = list(
      curveness = .5,
      color = list(
        type = "linear",
        x = 0,
        y = 0,
        x2 = 0,
        y2 = 1,
        colorStops = list(
          list(
            offset = 0, 
            color = 'red'
          ),
          list(
            offset = 1, 
            color = 'blue'
          )
        ),
        global = FALSE
      )
    )
  ) %>% 
  e_add("lineStyle", width) %>% 
  e_toolbox_feature(feature = "dataZoom")

# 




























a <- 
  structure(list(token_id = c(1.12374e+19, 1.12374e+19, 1.81313e+19, 
                              1.85075e+19, 1.30752e+19, 1.30752e+19, 1.32828e+19, 1.70088e+19, 
                              1.70088e+19, 1.70088e+19, 1.05536e+19, 1.44818e+19, 1.44736e+19, 
                              1.44736e+19, 1.44736e+19, 1.44736e+19, 1.89909e+19, 1.15795e+19, 
                              1.15795e+19, 1.15795e+19, 1.70234e+19, 1.70234e+19, 1.44062e+19, 
                              1.21512e+19, 1.21512e+19, 1.95909e+19, 1.95909e+19, 1.50179e+19, 
                              1.50179e+19, 1.24174e+19, 1.36445e+19, 1.98549e+19, 1.92068e+19, 
                              1.18468e+19, 1.18468e+19, 1.92409e+19, 1.92409e+19, 1.21387e+19, 
                              1.9162e+19, 1.9162e+19, 1.40385e+19, 1.40385e+19, 1.32996e+19, 
                              1.32996e+19, 1.69103e+19, 1.69103e+19, 1.57387e+19, 1.40552e+19, 
                              1.40552e+19, 1.00302e+19), Entry_Station_Lat = c(1.31509, 1.33261, 
                                                                               1.28425, 1.31812, 1.33858, 1.29287, 1.39692, 1.37773, 1.33858, 
                                                                               1.33322, 1.28179, 1.30036, 1.43697, 1.39752, 1.27637, 1.39752, 
                                                                               1.41747, 1.35733, 1.28405, 1.37773, 1.35898, 1.42948, 1.32774, 
                                                                               1.42948, 1.349, 1.36017, 1.34971, 1.38451, 1.31509, 1.31509, 
                                                                               1.37002, 1.34971, 1.31231, 1.39169, 1.31812, 1.44909, 1.29341, 
                                                                               1.41747, 1.33759, 1.44062, 1.31509, 1.38451, 1.29461, 1.32388, 
                                                                               1.41747, 1.27614, 1.39752, 1.39449, 1.33261, 1.31231), Entry_Station_Long = c(103.76525, 
                                                                                                                                                             103.84718, 103.84329, 103.89308, 103.70611, 103.8526, 103.90902, 
                                                                                                                                                             103.76339, 103.70611, 103.74217, 103.859, 103.85563, 103.7865, 
                                                                                                                                                             103.74745, 103.84596, 103.74745, 103.83298, 103.9884, 103.85152, 
                                                                                                                                                             103.76339, 103.75191, 103.83505, 103.67828, 103.83505, 103.74956, 
                                                                                                                                                             103.88504, 103.87326, 103.74437, 103.76525, 103.76525, 103.84955, 
                                                                                                                                                             103.87326, 103.83793, 103.89548, 103.89308, 103.82004, 103.78479, 
                                                                                                                                                             103.83298, 103.69742, 103.80098, 103.76525, 103.74437, 103.80605, 
                                                                                                                                                             103.93002, 103.83298, 103.79156, 103.74745, 103.90051, 103.84718, 
                                                                                                                                                             103.83793), Exit_Station_Lat = structure(c(48L, 34L, 118L, 60L, 
                                                                                                                                                                                                        14L, 54L, 10L, 49L, 49L, 74L, 71L, 65L, 102L, 5L, 102L, 119L, 
                                                                                                                                                                                                        116L, 10L, 13L, 88L, 117L, 66L, 40L, 62L, 117L, 37L, 67L, 34L, 
                                                                                                                                                                                                        85L, 44L, 102L, 44L, 115L, 29L, 92L, 17L, 121L, 70L, 120L, 52L, 
                                                                                                                                                                                                        85L, 34L, 42L, 11L, 4L, 115L, 62L, 48L, 92L, 14L), .Label = c("1.27082", 
                                                                                                                                                                                                                                                                      "1.27091", "1.27236", "1.27614", "1.27637", "1.27646", "1.27935", 
                                                                                                                                                                                                                                                                      "1.28221", "1.28247", "1.28405", "1.28621", "1.28819", "1.28932", 
                                                                                                                                                                                                                                                                      "1.29287", "1.29309", "1.29338", "1.29341", "1.29461", "1.29694", 
                                                                                                                                                                                                                                                                      "1.29959", "1.29974", "1.30034", "1.30252", "1.30287", "1.30392", 
                                                                                                                                                                                                                                                                      "1.30394", "1.30619", "1.30736", "1.30842", "1.31139", "1.3115", 
                                                                                                                                                                                                                                                                      "1.31167", "1.31188", "1.31509", "1.31654", "1.31756", "1.31913", 
                                                                                                                                                                                                                                                                      "1.31977", "1.32008", "1.3205", "1.32104", "1.32388", "1.32573", 
                                                                                                                                                                                                                                                                      "1.32725", "1.32774", "1.33119", "1.33155", "1.33261", "1.33322", 
                                                                                                                                                                                                                                                                      "1.33474", "1.33554", "1.33759", "1.33764", "1.33858", "1.33921", 
                                                                                                                                                                                                                                                                      "1.34037", "1.34225", "1.34293", "1.3432", "1.34426", "1.34857", 
                                                                                                                                                                                                                                                                      "1.349", "1.34905", "1.35158", "1.35733", "1.35898", "1.36017", 
                                                                                                                                                                                                                                                                      "1.3625", "1.36849", "1.37002", "1.37121", "1.37304", "1.37666", 
                                                                                                                                                                                                                                                                      "1.37775", "1.3786", "1.37862", "1.38001", "1.38029", "1.3803", 
                                                                                                                                                                                                                                                                      "1.38178", "1.38269", "1.38295", "1.38399", "1.38423", "1.38451", 
                                                                                                                                                                                                                                                                      "1.38671", "1.38672", "1.38777", "1.38814", "1.3894", "1.39147", 
                                                                                                                                                                                                                                                                      "1.39169", "1.39189", "1.39208", "1.39389", "1.39449", "1.39452", 
                                                                                                                                                                                                                                                                      "1.39628", "1.39692", "1.39717", "1.39732", "1.39752", "1.39821", 
                                                                                                                                                                                                                                                                      "1.39928", "1.39962", "1.4023", "1.40455", "1.40511", "1.40524", 
                                                                                                                                                                                                                                                                      "1.40843", "1.40961", "1.41184", "1.41588", "1.41685", "1.41747", 
                                                                                                                                                                                                                                                                      "1.42526", "1.42948", "1.43256", "1.43697", "1.44062", "1.44909"
                                                                                                                                                                                                        ), class = "factor"), Exit_Station_Long = structure(c(59L, 19L, 
                                                                                                                                                                                                                                                              27L, 4L, 65L, 3L, 63L, 6L, 6L, 21L, 93L, 121L, 9L, 56L, 9L, 32L, 
                                                                                                                                                                                                                                                              16L, 63L, 44L, 23L, 50L, 12L, 54L, 11L, 50L, 71L, 87L, 19L, 7L, 
                                                                                                                                                                                                                                                              118L, 9L, 118L, 49L, 90L, 96L, 31L, 45L, 61L, 38L, 2L, 7L, 19L, 
                                                                                                                                                                                                                                                              117L, 47L, 34L, 49L, 11L, 59L, 96L, 65L), .Label = c("103.67828", 
                                                                                                                                                                                                                                                                                                                   "103.69742", "103.70611", "103.72092", "103.73274", "103.74217", 
                                                                                                                                                                                                                                                                                                                   "103.74437", "103.74529", "103.74745", "103.74905", "103.74956", 
                                                                                                                                                                                                                                                                                                                   "103.75191", "103.7537", "103.75803", "103.76011", "103.76215", 
                                                                                                                                                                                                                                                                                                                   "103.76237", "103.76449", "103.76525", "103.76648", "103.76667", 
                                                                                                                                                                                                                                                                                                                   "103.76893", "103.7696", "103.77082", "103.77145", "103.77266", 
                                                                                                                                                                                                                                                                                                                   "103.774", "103.77866", "103.78185", "103.78425", "103.78479", 
                                                                                                                                                                                                                                                                                                                   "103.7865", "103.78744", "103.79156", "103.79631", "103.79654", 
                                                                                                                                                                                                                                                                                                                   "103.79836", "103.80098", "103.803", "103.80605", "103.80745", 
                                                                                                                                                                                                                                                                                                                   "103.80781", "103.80978", "103.81703", "103.82004", "103.82592", 
                                                                                                                                                                                                                                                                                                                   "103.82695", "103.83216", "103.83298", "103.83505", "103.83918", 
                                                                                                                                                                                                                                                                                                                   "103.83953", "103.83974", "103.84387", "103.84496", "103.84596", 
                                                                                                                                                                                                                                                                                                                   "103.84673", "103.84674", "103.84718", "103.84823", "103.84955", 
                                                                                                                                                                                                                                                                                                                   "103.85092", "103.85152", "103.85226", "103.8526", "103.85267", 
                                                                                                                                                                                                                                                                                                                   "103.85436", "103.85446", "103.85452", "103.86088", "103.86149", 
                                                                                                                                                                                                                                                                                                                   "103.86275", "103.86291", "103.86395", "103.86405", "103.86896", 
                                                                                                                                                                                                                                                                                                                   "103.87087", "103.87135", "103.87534", "103.87563", "103.8763", 
                                                                                                                                                                                                                                                                                                                   "103.87971", "103.88003", "103.88126", "103.88243", "103.88296", 
                                                                                                                                                                                                                                                                                                                   "103.88504", "103.8858", "103.88816", "103.8886", "103.88934", 
                                                                                                                                                                                                                                                                                                                   "103.89054", "103.89237", "103.89313", "103.8938", "103.89548", 
                                                                                                                                                                                                                                                                                                                   "103.89719", "103.89723", "103.89854", "103.9003", "103.90051", 
                                                                                                                                                                                                                                                                                                                   "103.90208", "103.90214", "103.9031", "103.90484", "103.90537", 
                                                                                                                                                                                                                                                                                                                   "103.90597", "103.90599", "103.90663", "103.9086", "103.90902", 
                                                                                                                                                                                                                                                                                                                   "103.9126", "103.9127", "103.91296", "103.91616", "103.9165", 
                                                                                                                                                                                                                                                                                                                   "103.93002", "103.94638", "103.94929", "103.95337", "103.9884"
                                                                                                                                                                                                                                                              ), class = "factor")), .Names = c("token_id", "Entry_Station_Lat", 
                                                                                                                                                                                                                                                                                                "Entry_Station_Long", "Exit_Station_Lat", "Exit_Station_Long"
                                                                                                                                                                                                                                                              ), row.names = c(10807L, 10808L, 10810L, 10815L, 10817L, 10818L, 
                                                                                                                                                                                                                                                                               10819L, 10820L, 10823L, 10824L, 10826L, 10827L, 10829L, 10831L, 
                                                                                                                                                                                                                                                                               10832L, 10833L, 10834L, 10835L, 10836L, 10838L, 10840L, 10841L, 
                                                                                                                                                                                                                                                                               10843L, 10847L, 10850L, 10852L, 10854L, 10855L, 10859L, 10861L, 
                                                                                                                                                                                                                                                                               10869L, 10872L, 10883L, 10886L, 10891L, 10895L, 10896L, 10897L, 
                                                                                                                                                                                                                                                                               10900L, 10902L, 10903L, 10906L, 10910L, 10911L, 10912L, 10913L, 
                                                                                                                                                                                                                                                                               10915L, 10920L, 10921L, 10924L), class = "data.frame")
#get Packages
require(leaflet)
require(geosphere)

#format data
a$Entry_Station_Long = as.numeric(as.character(a$Entry_Station_Long))
a$Entry_Station_Lat = as.numeric(as.character(a$Entry_Station_Lat))
a$Exit_Station_Long = as.numeric(as.character(a$Exit_Station_Long))
a$Exit_Station_Lat = as.numeric(as.character(a$Exit_Station_Lat))
a$id = as.factor(as.numeric(as.factor(a$token_id)))

#create some colors
factpal <- colorFactor(heat.colors(30), pathList$id)

#create a list of interpolated paths
pathList = NULL
for(i in 1:nrow(a)){
  tmp = gcIntermediate(c(a$Entry_Station_Long[i],
                         a$Entry_Station_Lat[i]),
                       c(a$Exit_Station_Long[i],
                         a$Exit_Station_Lat[i]),n = 25,
                       addStartEnd=TRUE)
  tmp = data.frame(tmp)
  tmp$id = a[i,]$id
  tmp$color = factpal(a[i,]$id)
  pathList = c(pathList,list(tmp))
}

#create empty base leaflet object
leaflet() %>% addTiles() -> lf

#add each entry of pathlist to the leaflet object
for (path in pathList){
  lf %>% addPolylines(data = path,
                      lng = ~lon, 
                      lat = ~lat,
                      color = ~color) -> lf
  
}
#show output
lf





################## NICE !!!!!!!!!!!  ########
class(a)
#load packages and map
require(tidyverse)
require(ggmap)
basemap <- get_map("Singapore",
                   source = "stamen",
                   maptype = "toner",
                   zoom = 11)

#oversample data (because of too few rides) and summarize for station pairs
a %>%
  sample_n(size=5000,replace=T) %>%
  mutate(Entry_Station_Lat = as.numeric(as.character(Entry_Station_Lat)),
         Exit_Station_Lat = as.numeric(as.character(Exit_Station_Lat)),
         Entry_Station_Long = as.numeric(as.character(Entry_Station_Long)),
         Exit_Station_Long = as.numeric(as.character(Exit_Station_Long))) %>%
  group_by(Entry_Station_Lat,Entry_Station_Long,Exit_Station_Lat,Exit_Station_Long) %>%
  dplyr::summarize(count=n()) -> plotData

#extract entry Stations
a %>%
  mutate(Entry_Station_Lat = as.numeric(as.character(Entry_Station_Lat)),
         Exit_Station_Lat = as.numeric(as.character(Exit_Station_Lat)),
         Entry_Station_Long = as.numeric(as.character(Entry_Station_Long)),
         Exit_Station_Long = as.numeric(as.character(Exit_Station_Long))) %>%
  select(Entry_Station_Lat,Entry_Station_Long) %>%
  group_by(Entry_Station_Lat,Entry_Station_Long) %>%
  dplyr::summarize(freq=n()) -> entryStations

#extract exit stations    
a %>%
  mutate(Entry_Station_Lat = as.numeric(as.character(Entry_Station_Lat)),
         Exit_Station_Lat = as.numeric(as.character(Exit_Station_Lat)),
         Entry_Station_Long = as.numeric(as.character(Entry_Station_Long)),
         Exit_Station_Long = as.numeric(as.character(Exit_Station_Long))) %>%
  select(Exit_Station_Lat,Exit_Station_Long) %>%
  group_by(Exit_Station_Lat,Exit_Station_Long) %>%
  dplyr::summarize(freq=n()) -> exitStations

#plot map, curves with size proportional to frequency, points for entry and exit stations
g = ggplot(plotData)
map <- ggmap(basemap, base_layer = g)

map

map + coord_cartesian() +
  geom_curve(color="red",alpha=0.5,curvature=0.2,
             # data = plotData,
             aes(x=Entry_Station_Long,
                 size = count/10,
                 color = as.factor(Entry_Station_Long),
                 y=Entry_Station_Lat,
                 xend=Exit_Station_Long,
                 yend=Exit_Station_Lat))+
  geom_point(data=exitStations,
             alpha=0.5,
             size=4,
             aes(x=Exit_Station_Long,
                 y=Exit_Station_Lat)) +
  geom_point(data=entryStations,
             alpha=0.5,
             size=4,
             aes(x=Entry_Station_Long,
                 y=Entry_Station_Lat))

