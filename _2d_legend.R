

dfxx <- read.table(textConnection(
  "toxin  dose    x   y
A   1   0.851   0.312
A   10  0.268   0.443
A   100 0.272   0.648
B   1   0.981   0.015
B   10  0.304   0.658
B   100 0.704   0.821
C   1   0.330   0.265
C   10  0.803   0.167
C   100 0.433   0.003
D   1   0.154   0.611
D   10  0.769   0.616
D   100 0.643   0.541
"), header = TRUE)

dfxx$dose <- factor(dfxx$dose)


mainPlot <-
  ggplot(dfxx, aes(x = x, y = y, col = toxin, alpha = dose)) +
  geom_point(size = 4)
mainPlot


allLevels <-
  expand.grid(toxin = levels(dfxx$toxin), dose = levels(dfxx$dose))

legendPlot <-
  ggplot(allLevels, aes(x = toxin, y = dose, col = toxin, alpha = dose)) +
  geom_point(size = 4)
legendPlot

library(gridExtra)

grid.arrange(
  mainPlot   + theme(legend.position = "none"), 
  legendPlot + theme(legend.position = "none") + 
    ggtitle("Legend"), 
  layout_matrix = matrix(c(1,1,1,NA,2,NA), ncol = 2), 
  widths=c(2,1), heights = c(1,2,1))
