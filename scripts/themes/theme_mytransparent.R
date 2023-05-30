#### Theme for plots with extra large fonts and transparent background

theme(
  title        = element_text(size = 40),
  axis.title.x = element_text(size = 36),
  axis.text.x  = element_text(size = 32),
  axis.title.y = element_text(size = 36),
  axis.text.y  = element_text(size = 32),
  legend.title = element_text(size = 36),
  legend.text  = element_text(size = 32)) # define font sizes
  panel.background = element_rect(fill = "transparent"), # bg of the panel
  plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
  panel.grid.major = element_blank(), # get rid of major grid
  panel.grid.minor = element_blank(), # get rid of minor grid
  legend.background = element_rect(fill = "transparent"), # get rid of legend bg
  legend.box.background = element_rect(fill = "transparent"), # get rid of legend panel bg
  legend.key = element_rect(fill = "transparent", colour = NA), # get rid of key legend fill, and of the surrounding
  # axis.line = element_line(colour = "gray25") # adding a black line for x and y axis
) # make background transparent