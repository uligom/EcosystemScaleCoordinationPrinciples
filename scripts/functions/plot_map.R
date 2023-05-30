#### Function to plot site availability (grouped)
# Where no data is NA (intersection of all variables)

# TO DO:  - add option for intersection of only subset of given variables


### Arguments:
# data        Tibble of data.
# .group      Grouping variable.
# filter_na   Logical. Option to filter out NA entries to only count intersection
#             of available data. If FALSE (default), plots every datapoint with
#             available coordinates, if TRUE, filters only to cases with full information.
# theme       Character, either 'classic' (default), or 'large' (best for saving to .jpg).
# savepath    Folder path. If given, saves figure(s) to the given folder path.
#             If omitted, defaults to NULL and skips the saving process.

plot_map <- function(data, .group, filter_na = FALSE, palette = NULL, theme = "classic",
                     savepath = NULL, vers_out = vers_out, transparent = TRUE,
                     title = glue::glue("Sites with available information (N = {n_data})")) {
  ### Utilities ----------------------------------------------------------------
  require(dplyr)
  require(ggplot2)
  require(rlang)
  require(tidyr)
  
  source("scripts/themes/theme_myclassic.R", local = T)   # theme for plots
  source("scripts/themes/theme_mylarge.R", local = T)     # theme for plots
  
  
  
  ### Quotation ----------------------------------------------------------------
  # Group
  group_quoted <- rlang::enquo(.group)
  # Theme
  theme_symbol  <- rlang::sym(paste0("theme_my", theme))
  # Path for saving
  if (!is.null(savepath)) {path_symbol <- rlang::sym(savepath)}
  
  
  ## Transparency
  if (transparent == T) {
    ext <- "png"
    dev <- ext
  } else {
    ext <- "jpg"
    dev <- "jpeg"
  }
  
  
  
  ### Process ------------------------------------------------------------------
  ## Filter out NA
  if (filter_na == T){
    data <- data %>% tidyr::drop_na(GPPsat) %>% unique()
  }
  
  ## Number of datapoints
  n_data <- data %>% nrow()
  
  
  
  ### Plot ---------------------------------------------------------------------
  palette = palette
  p0 <- data %>%
    dplyr::group_by(!!group_quoted) %>% 
    ggplot2::ggplot(aes(longitude, latitude, fill = !!group_quoted)) +
    ggplot2::borders("world", colour = "gray50", fill = "gray75") +  # world borders
    ggplot2::geom_point(colour = "gray25", shape = 21, size = 5, stroke = 1) + # points
    ggplot2::scale_fill_manual(values = palette) + # color palette
    ggplot2::labs(title = title) + # title
    rlang::eval_tidy(theme_symbol) + # theme
    ggplot2::theme( # legend text spacing
      legend.text = element_text(color = "gray25", size = 24),
      legend.key.height = unit(15, "mm"),
      legend.key.width = unit(20, "mm")
    ) +
    NULL
  
  ## Add transparency
  if (transparent == T) {
    p0 <- p0 +
      theme(
        panel.background = element_rect(fill = 'transparent'), # panel bg
        plot.background = element_rect(fill = 'transparent', color = NA), # plot bg
        panel.grid.major = element_blank(), # remove major gridlines
        panel.grid.minor = element_blank(), # remove minor gridlines
        legend.background = element_rect(fill = 'transparent'), # legend bg
        legend.box.background = element_rect(color = "transparent",
                                             fill = 'transparent'), # legend panel
        legend.margin = margin(0, 0, 0, 0), # remove legend box lines
        legend.key = element_rect(fill = "transparent") # background underneath legend keys
      )
  }
  
  
  
  ### Saving -------------------------------------------------------------------
  if (!is.null(savepath)) {
    ggplot2::ggsave(glue::glue("sites_map_{vers_out}.{ext}"),
           plot = p0,
           device = dev,
           path = path_symbol,
           bg = "transparent",
           width = 508, height = 285.75, units = "mm", dpi = 600) # 1920 x 1080 px resolution (16:9)
  }
  
  
  
  ### Output -------------------------------------------------------------------
  return(p0)
}
# debugonce(plot_map)