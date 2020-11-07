


# 1) add function/equation to lm (linear model) ---------------------------

library(ggpmisc)
library(ggplot2)
set.seed(100)
x <- seq(from = 1, to = 20, by = 1)
y <- sample(seq(from = 20, to = 50, by = 5), size = 20, replace = TRUE)
df <- data.frame(x,y)


ggplot(df, aes(x=x, y=y)) +
  
  # change x, y labels
  labs(x=expression(paste("Observed(",x100,Km^2,")",sep='')),
       y=expression(paste("Simulated(",x100,km^2,")",sep=''))) +
  
  geom_point(shape=20, color="red", size = 5) +
  geom_smooth(method = 'lm', formula = y~x)+
  # add line
  geom_abline(linetype="dashed",size=2) +
  
  # add equation
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE,
               size=10,
               # vjust= ,
               hjust=.01) +
  theme_bw()




# 2) someone else ---------------------------------------------------------

lm_lb <- function(.df, .formula, .f = 'lm', digits = 2) {
  f <- match.fun(.f)
  reg <- f(as.formula(.formula), data = .df)
  xx <- round(unname(coefficients(reg)), digits )
  yy <- names(reg$model)[1]
  nm <- names(reg$model)[-1]
  slope <- xx[-1]
  itpt <- xx[1]
  right <- paste(itpt, 
                 paste(slope, nm, sep = ' * ', collapse = ' + '), 
                 sep = ' + ')
  right <- stringr::str_replace(right, stringr::fixed('+ -'), '- ')
  return(as.expression(
    paste(yy, right , sep = ' = ')
  ))
}

## example
reg_lab <- lm_lb(mtcars, 'hp ~ wt + disp + drat', 'lm')
plot(mtcars$hp, mtcars$wt, main = reg_lab)




