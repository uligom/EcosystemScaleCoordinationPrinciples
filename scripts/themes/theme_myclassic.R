#### Theme for plots with large fonts and white background
# define font sizes
require(ggplot2)

theme_myclassic <- theme_light() +  # remove gray background
  theme(title        = element_text(size = 32),
        axis.title.x = element_text(size = 28),
        axis.text.x  = element_text(size = 24),
        axis.title.y = element_text(size = 28),
        axis.text.y  = element_text(size = 24),
        legend.title = element_text(size = 28),
        legend.text  = element_text(size = 24))