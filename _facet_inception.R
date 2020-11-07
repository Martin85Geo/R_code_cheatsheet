


library(ggplot2)
library(dplyr)
create_plot <- function(.data, title = "Diamonds", theme_size = 12) {
  ggplot(.data) + 
    geom_point(aes(x = carat, y = price, color = cut), alpha = 0.3, size = 0.3) + 
    theme_bw() + 
    labs(title = title) + 
    theme_bw(theme_size) +
    # theme(axis.text = element_blank(), legend.position = 'none') + 
    scale_color_manual(values = c('Fair'="#1B9E77", 'Good'="#D95F02", 'Very Good'="#7570B3", 
                                  'Premium'="#E7298A", 'Ideal'="#66A61E"))
}


dd <- diamonds
create_plot(diamonds)


# ref: https://coolbutuseless.github.io/2018/10/31/facet_inception/
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
facet_inception <- function(.data, cols, title = NULL, theme_size) {
  if (length(cols) == 1) {
    create_plot(.data, title, theme_size) +
      facet_wrap(cols[1], labeller = label_both) 
  } else {
    this_col <- cols[1]
    cols     <- cols[-1]
    sub_data <- split(.data, .data[[this_col]])
    sub_plots <- sub_data %>% 
      purrr::imap(~facet_inception(.x, cols, paste(title, paste0(this_col, ': ', .y), collapse = ", "), theme_size = theme_size))
    patchwork::wrap_plots(sub_plots)
  }
}

facet_inception(diamonds, c('cut'), theme_size = 12)


# install.packages("devtools")
# devtools::install_github("thomasp85/patchwork")
library(patchwork)


facet_inception(diamonds, c('cut', 'color'), theme_size = 12)


facet_inception(diamonds, c('cut', 'color', 'clarity'), theme_size = 4)


