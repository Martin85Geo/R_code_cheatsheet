
# Create Flow Map in R using ggplot2
# Source: https://gist.github.com/rafapereirabr/9a36c2e5ff04aa285fa3

## This gist shows how to create Flow Maps in R using ggplot2. 
## source: This is based on different bits of code from other with amazing R skills: 

#' @ceng_l          : http://web.stanford.edu/~cengel/cgi-bin/anthrospace/great-circles-on-a-recentered-worldmap-in-ggplot
#' @3wen            : http://egallic.fr/maps-with-r/
#' @spatialanalysis : http://spatialanalysis.co.uk/2012/06/mapping-worlds-biggest-airlines/
#' @freakonometrics : http://freakonometrics.hypotheses.org/48184


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
  coord_equal()



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
  coord_equal()


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