

dir.old <- "C:/Users/YINGJIE/Zotero/storage"
dir.new <- "G:/My Drive/lib/MEGAsync/storage"

fs.list <- list.files(path = dir.old, pattern = ".pdf$", recursive = T, full.names = T, include.dirs = T) 
head(fs.list, 20)
length(fs.list)

file.copy(from = file.path(dir.old, fs.list), to = dir.new, overwrite = T, copy.date = T)

### This is for deleting your old files
# file.remove(fs.list)

# how many succeed? ------------------------------------------------------
fs.new <- list.files(path = dir.new, pattern = ".pdf$", recursive = T, full.names = T, include.dirs = T) 
length(fs.new)
length(fs.new) - length(fs.list) ## how many succeed?


# rename the files --------------------------------------------------------
files <- fs.new

sapply(files,FUN = function(eachfile){
  file.rename(from=eachfile, 
              to=gsub(pattern=' - |\\. - ', replacement = '_', eachfile))
})
