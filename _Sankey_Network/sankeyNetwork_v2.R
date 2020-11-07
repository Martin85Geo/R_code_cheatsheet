
# Yingjie Li
# LIYJ@MSU.EDU
# LAST UPDATE ON: APR 7, 2019


remove(list = ls())

## set work dir
path <- rstudioapi::getSourceEditorContext()$path
dir  <- dirname(rstudioapi::getSourceEditorContext()$path); dir
setwd(dir)

# Load package
library(networkD3)
library(tidyverse)

data <- read.csv('./data3.csv') %>% as.data.frame()
row.names(data) <-data[,1]


data <- data[,-1]

# Transform it to connection data frame with tidyr from the tidyverse:
links <- data %>% 
  as.data.frame() %>% 
  rownames_to_column(var="source") %>% 
  gather(key="target", value="value", -1) %>%
  filter(value != 0)

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(name=c(as.character(links$source), as.character(links$target)) %>% unique())


names(data)


# write.csv(links, 'links.csv')

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. 
#  So we need to reformat it.
links$IDsource=match(links$source, nodes$name)-1 
links$IDtarget=match(links$target, nodes$name)-1


####################################################
# Add a 'group' column to each connection:


links$group=as.factor(c("f","w","b", # f1, w1, b1 --> f2
                        "f","w","b", # f1, w1, b1 --> w2 
                        "f","w","b",  # f1, w1, b1 --> b2
                        
                        ## stage 2 --> stage 3
                        "f","w","b",  # f2, w2, b2 --> f3
                        "f","w","b",  # f2, w2, b2 --> w2
                        "f","w","b")) # f2, w2, b2 --> b3



# Add a 'group' column to each node. Here I decide to put all of them in the same group to make them grey
nodes$group=as.factor(nodes$name)



# prepare color scale: I give one specific color for each node.
# Give a color for each group:

my_color <- 'd3.scaleOrdinal().domain(["Forest_1999", "Water_1999", "BareGround_1999",
                                       "Forest_2013", "Water_2013", "BareGround_2013",
                                       "Forest_2018", "Water_2018", "BareGround_2018", 
                                       "f","w","b"]).range(["green", "blue", "orange", 
                                                            "green", "blue", "orange", 
                                                            "green", "blue", "orange", 
                                                            "lightgreen", "#6baed6", "yellow"])'


# Make the Network
sankeyNetwork(Links = links, Nodes = nodes, 
              Source = "IDsource", Target = "IDtarget", 
              Value = "value", NodeID = "name", 
              colourScale=my_color, 
              fontSize=12,
              LinkGroup="group", 
              NodeGroup="group")







##################################################### #
## you can coustmize the color of each link
## find the order by viewing the "links" dataframe

links$group=as.factor(c("f1","w","b", # f1, w1, b1 --> f2
                        "f","w","b1", # f1, w1, b1 --> w2 
                        "f","w","b",  # f1, w1, b1 --> b2
                        
                        ## stage 2 --> stage 3
                        "f","w","b",  # f2, w2, b2 --> f3
                        "f","w","b",  # f2, w2, b2 --> w2
                        "f","w","b")) # f2, w2, b2 --> b3

# Add a 'group' column to each node. Here I decide to put all of them in the same group to make them grey
nodes$group=as.factor(nodes$name)



my_color <- 'd3.scaleOrdinal().domain(["Forest_1999", "Water_1999", "BareGround_1999",
"Forest_2013", "Water_2013", "BareGround_2013",
"Forest_2018", "Water_2018", "BareGround_2018", 
"f","w","b", "f1", "b1"]).range(["green", "blue", "orange", 
"green", "blue", "orange", 
"green", "blue", "orange", 
"lightgreen", "#6baed6", "yellow", "black", "red"])'


# Make the Network
# sankeyNetwork(Links = links, Nodes = nodes, 
#               Source = "IDsource", Target = "IDtarget", 
#               Value = "value", NodeID = "name", 
#               colourScale=my_color, 
#               fontSize=12,
#               LinkGroup="group", 
#               NodeGroup="group")



# # Make the Network
# sn <-
# sankeyNetwork(Links = links, Nodes = nodes,
#               Source = "IDsource", Target = "IDtarget",
#               Value = "value", NodeID = "name", 
#               NodeGroup="group",
#               LinkGroup="group", 
#               fontSize=12,
#               colourScale=my_color,
#               sinksRight=FALSE)
# 
# sn


###############
# library(webshot)
# webshot("sn.html", "simpleNetwork.png")


