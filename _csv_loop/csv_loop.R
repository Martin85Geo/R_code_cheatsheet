

# START WITH A CLEAN ENVIRONMENT
rm(list=ls())

path <- rstudioapi::getSourceEditorContext()$path
dir  <- dirname(rstudioapi::getSourceEditorContext()$path)
dir
setwd(dir)

getwd()
filenames <- list.files(path=getwd())  
filenames
numfiles <- length(filenames)  
numfiles


dirnames <- list.dirs(path=getwd(), recursive=FALSE)
numdir   <- length(dirnames)  
dirnames
numdir



# "AUDJPY-2009-05.csv"

# for (i in filenames){  
#   #name <- gsub("-","_", i)
#   name <- gsub(".csv", "", i)  
#   i <- paste(".\\", i, sep="")
#   assign(name, read.csv(i, header=T))}



for (i in dirnames){
  name <- gsub('', '', i)
  print(name)
  i <- paste(name, "/result.csv", sep="")
  print(i)
  name <- substr(name, nchar(name)-1, nchar(name)) 
    # extract the last two chr
    # substr(x, start, stop)
  print(name)
  assign(name, read.csv(i, header=T))
}




