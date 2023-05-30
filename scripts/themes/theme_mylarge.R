#### Theme for plots with extra large fonts and white background

theme_mylarge <- theme_light() +  # remove gray background
  theme(title        = element_text(size = 40),
        axis.title.x = element_text(size = 36),
        axis.text.x  = element_text(size = 32),
        axis.title.y = element_text(size = 36),
        axis.text.y  = element_text(size = 32),
        legend.title = element_text(size = 36),
        legend.text  = element_text(size = 32)) # define font sizes