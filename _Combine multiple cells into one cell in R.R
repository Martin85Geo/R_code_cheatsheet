
## Combine multiple cells into one cell in R

## Group 1
library(tidyverse)

N <- 10
set.seed(06510)
grp1 <- t(replicate(N, sample(seq(1:4), 4, replace = FALSE)) )
as.data.frame( I(grp1) )

sapply( as.data.frame( I(grp1) ) , mode )
I(grp1) 

b <- apply(grp1,1,paste,collapse=",") %>% as.data.frame()

matrix(apply(grp1,1,paste,collapse=","),ncol=1)

do.call(paste, c(data.frame(grp1), sep = ","))

d <- matrix(apply(grp1[,2:4],1,paste,collapse=","),ncol=1) %>% as.data.frame()
