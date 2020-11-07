

# The Pirate Plot – An R Pirate’s favorite plot
# https://www.r-bloggers.com/the-pirate-plot-an-r-pirates-favorite-plot/
# https://cran.r-project.org/web/packages/yarrr/vignettes/pirateplot.html
# https://bookdown.org/ndphillips/YaRrr/pirateplot.html

# To use the install_github function, you also need to have the devtools library installed and loaded!
# install.packages("devtools")
# library(devtools)
# install_github("ndphillips/yarrr")



library(yarrr) # load yarrr
# yarrr.guide() # run main package guide

BeardLengths <- BeardLengths


length.means <- aggregate(Beard ~ Ship,
                          FUN = mean,
                          data = BeardLengths)

barplot(length.means$Beard,
        names.arg = length.means$Ship,
        main = "Beard Length by Ship")

boxplot(Beard ~ Ship,
        data = BeardLengths,
        main = "Beard Length by Ship")


pirateplot(formula = Beard ~ Ship, 
           data = BeardLengths, 
           pal = "google",         ### pal = "southpark", 'black', 'pony', 
           theme = 3,
           # trans.vec = c(.8, .2, .2, .2, .2), 
           avg.line.lwd = 0.5,
           
           
           ### points
           point.o = .3,           ### opacity
           point.pch = 21,         ### point shape
           point.bg = "white",     ### point backgrounds/fill
           point.col = "black",    ### point color
           point.cex = .7,         ### point size 0~1
           
           ### Inference Display
           inf.disp = "bean",      # Wrap inference around bean ## "rect" or "bean" or "line"
           inf.method = 'hdi',     # OR inf.method = ‘ci’
           inf.f.o = .5,           # transparentcy of the inf filling
           
           
           ### beans
           bean.b.o = .4,          # transparentcy of the bean borders
           # bean.f.o = .6,          # Bean fill
           
           ### Quantiles
           quant = c(.1, .9),      # 10th and 90th quantiles
           quant.col = "gray50",   # Black quantile lines

           ### Gridlines
           gl.col = "gray70", # Gray gridlines
           gl.lwd = c(.5, 0),      ### turn off minor grid lines
           
           
           ### Background
           # back.col = transparent("blue", .95), # Add light blue background
           
           main = "Beard Length by Ship")


