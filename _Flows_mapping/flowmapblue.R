


# devtools::install_github("FlowmapBlue/flowmapblue.R")
library(flowmapblue)
getwd()
source('Token_mapboxAccessToken.R')

locations <- read.csv('https://gist.githubusercontent.com/ilyabo/a7b9701424257146b571149d92a14926/raw/2e9e1e9bcf64cf0090781b451037229ccb78e1b1/locations.csv')
flows <- read.csv('https://gist.githubusercontent.com/ilyabo/a7b9701424257146b571149d92a14926/raw/2e9e1e9bcf64cf0090781b451037229ccb78e1b1/flows.csv')
mapboxAccessToken <- YOUR_MAPBOX_ACCESS_TOKEN
flowmapblue(locations, flows, mapboxAccessToken, clustering=TRUE, darkMode=TRUE, animation=FALSE)
