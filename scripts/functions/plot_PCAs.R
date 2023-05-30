plot_PCAs = function(figs = c(1:3), plot_main = T) {
  #### PRINCIPAL COMPONENT ANALYSIS
  
  ### Author: Ulisse Gomarasca, Mirco Migliavacca, Talie Musavi
  ### Script options -----------------------------------------------------------
  # Measure script run time
  library(tictoc)
  tic()
  
  savedata <- T # save output
  
  
  
  ### Utilities ----------------------------------------------------------------
  ## Packages:
  packages <- c("ggplot2", "tidyr", "dplyr", "readr", "ggrepel", "cowplot", "factoextra")
  
  # Install packages not yet installed
  installed_packages <- packages %in% rownames(installed.packages())
  if (any(installed_packages == FALSE)) {
    install.packages(packages[!installed_packages])
  }
  
  invisible(lapply(packages, library, character.only = TRUE))
  
  
  
  ### Load data and plot -------------------------------------------------------
  ## Plotting settings
  # Colors:
  CatCol <- c(
    CSH = "#586158", DBF = "#C46B39", EBF = "#4DD8C0", ENF = "#3885AB", GRA = "#9C4DC4",
    MF = "#C4AA4D", OSH = "#443396", SAV = "#CC99CC", WET = "#88C44D", WSA = "#AB3232"
  )
  Three_colorblind <- c("#A8AD6F", "#AD6FA8", "#6FA8AD") #c("#809844", "#4f85b0", "#b07495")
  graph_elements_dark <- "black"
  plot_elements_light <- "gray75"
  plot_elements_dark <- "gray25"
  
  # Transparency:
  boot_alpha_main <- 0.9
  boot_alpha_small <- 0.05
  
  # Text:
  # if (n_pcs > 3) {x_angle <- 270; x_adjust <- 0.25} else {x_angle <- 0; x_adjust <- 0} # option to change orientation of x axis text
  x_angle <- 0; x_adjust <- 0
  title_text <- 7 # Nature Communications: max 7 pt; cowplot multiplier: 1/1.618; 7 pt : 1/1.618 = x pt : 1; x = 7 / 1/1.618; x = 11.326 (round up to integer)
  subtitle_text <- 6
  normal_text <- 6 # Nature Communications: min 5 pt; cowplot multiplier: 1/1.618; 5 pt : 1/1.618 = x pt : 1; x = 5 / 1/1.618; x = 8.09 (round up to integer)
  
  # Element dimensions:
  plot_linewidth <- 0.33
  point_shape <- 18
  point_size <- 1.5
  
  
  
  # Initialize figure lists:
  p_biplot <- list(); p_r2 <- list(); p_load <- list(); p_contr <- list(); col_ii <- list()
  
  # Labels:
  veg_sub_labels <- c("All Sites", "All Forests", "Evergreen Needle-Forests") # labels of subgroups
  
  
  
  ## Loop over all requested figures
  for (qq in figs) {
    ### Set Scenario for analysis ----------------------------------------------
    n_pcs <- NA # initialize number of PCs to retain
    ## 1) LEAF ECONOMICS SPECTRUM ####
    if (qq == 1){
      figure_name <- "Figure_1" # loading/saving name
      ii <- 1 # index for single/compound figures
    }
    
    
    ## 11) LEAF ECONOMICS SPECTRUM - all forests ####
    if (qq == 11){
      figure_name <- "Supplementary_Figure_1_forests" # loading name
      ii <- 2 # index for single/compound figures
    }
    
    
    ## 12) LEAF ECONOMICS SPECTRUM - evergreen needleleaf forests ####
    if (qq == 12){
      figure_name <- "Supplementary_Figure_1_enf" # loading name
      ii <- 3 # index for single/compound figures
    }
    
    
    ## 13) LEAF ECONOMICS SPECTRUM - wNarea ####
    if (qq == 13){
      figure_name <- "Supplementary_Figure_3" # loading/saving name
      ii <- 1 # index for single/compound figures
    }
    
    
    ## 101) LEAF ECONOMICS SPECTRUM - GPP/LAI ####
    if (qq == 101){
      figure_name <- "LES_GPP_LAI" # loading/saving name
      ii <- 1 # index for single/compound figures
    }
    
    
    ## 2) GLOBAL SPECTRUM ####
    if (qq == 2){
      figure_name <- "Figure_2" # loading/saving name
      ii <- 1 # index for single/compound figures
    }
    
    
    ## 21) GLOBAL SPECTRUM - all forests ####
    if (qq == 21){
      figure_name <- "Supplementary_Figure_4_forests" # loading name
      ii <- 2 # index for single/compound figures
    }
    
    
    ## 22) GLOBAL SPECTRUM - evergreen needleleaf forests ####
    if (qq == 22){
      figure_name <- "Supplementary_Figure_4_enf" # loading name
      ii <- 3 # index for single/compound figures
    }
    
    
    
    ## 3) LEAST COST HYPOTHESIS ####
    if (qq == 3){
      figure_name <- "Figure_3" # loading/saving name
      ii <- 1 # index for single/compound figures
    }
    
    
    ## 31) LEAST COST HYPOTHESIS - all forests ####
    if (qq == 31){
      figure_name <- "Supplementary_Figure_9_forests" # loading name
      ii <- 2 # index for single/compound figures
    }
    
    
    ## 32) LEAST COST HYPOTHESIS - evergreen needleleaf forests ####
    if (qq == 32){
      figure_name <- "Supplementary_Figure_9_enf" # loading name
      ii <- 3 # index for single/compound figures
    }
    
    
    ## 35) LEAST COST HYPOTHESIS - PNUEarea variant ####
    if (qq == 35){
      figure_name <- "Supplementary_Figure_5" # loading/saving name
      ii <- 1 # index for single/compound figures
    }
    
    
    ## 36) LEAST COST HYPOTHESIS - APAR_N variant ####
    if (qq == 36){
      figure_name <- "Supplementary_Figure_6" # loading/saving name
      ii <- 1 # index for single/compound figures
    }
    
    
    ## 37) LEAST COST HYPOTHESIS - WUEet variant ####
    if (qq == 37){
      figure_name <- "Supplementary_Figure_7" # loading/saving name
      ii <- 1 # index for single/compound figures
    }
    
    
    ## 38) LEAST COST HYPOTHESIS - uWUE variant ####
    if (qq == 38){
      figure_name <- "Supplementary_Figure_8" # loading/saving name
      ii <- 1 # index for single/compound figures
    }
    
    
    
    ############################################################################
    ### Import data ------------------------------------------------------------
    ## Sites
    df_subset <- read.table(glue::glue("results/PCA_input_{figure_name}.csv"), header = T, sep = ";")
    
    ## PCA results
    load(glue::glue("results/PCA_results_{figure_name}.RData"))
    
    ## Retained PCs
    load(glue::glue("results/PCA_PCs_retained_{figure_name}.RData"))
    
    ## PCA statistics
    pca_stats <- read.table(glue::glue("results/PCA_stats_bootstrapped_{figure_name}.csv"), header = T, sep = ";")
    
    
    
    ### Re-define figure output names ------------------------------------------
    if (qq == 12) { # redefine figure names for compound figures
      figure_name <- "Supplementary_Figure_1"
    } else if (qq == 22) {
      figure_name <- "Supplementary_Figure_4"
    } else if (qq == 32) {
      figure_name <- "Supplementary_Figure_9"
    }
    
    ## Announce step
    if (plot_main == F & qq %in% c(1:3)) {
      # do nothing
    } else if (!(qq %in% c(11, 21, 31))) {
    print(glue::glue("==========================================================
                      ==========================================================
                      Plotting PCA output for '{figure_name}'.....")
          )
    }
    
    
    
    ### Plot -------------------------------------------------------------------
    ## A) biplot color coded by IGBP biome type ----
    p_biplot[[ii]] <- fviz_pca_biplot(pca_result,
                                axes = c(1, 2),
                                col.ind = df_subset$IGBP, #"grey50",
                                # col.ind = NA, #plot_elements_light, #"white",
                                geom.ind = "point",
                                palette = CatCol,#'futurama',
                                label = "var",
                                col.var = plot_elements_dark,
                                labelsize = 2,
                                repel = TRUE,
                                pointshape = 16,
                                pointsize = 2,
                                alpha.ind = 0.67,
                                arrowsize = 0.5) +
      labs(title = "",
           x = "PC1",
           y = "PC2",
           fill = "IGBP") +
      guides(fill = guide_legend(title = "")) +
      theme(title = element_blank(),
            text = element_text(size = normal_text),
            axis.line = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_text(size = title_text, face = "bold"),
            axis.text = element_text(size = normal_text),
            # plot.margin = unit(c(0, 0, 0, 0), "cm"),
            # legend.position = "none"
            legend.text = element_text(size = subtitle_text),
            legend.key.height = unit(5, "mm"),
            legend.key.width = unit(2, "mm")
      ) +
      NULL
    
    
    
    ## B) explained variance ----
    dat_boot <- pca_stats %>%
      dplyr::select(PC_name, PC, R2_boot) %>% unique()%>% # remove repetitions
      dplyr::mutate(PC = as.character(PC))
    
    dat_true <- pca_stats %>%
      dplyr::select(PC_name, PC, R2, R2_median, R2_std) %>% unique() %>% # remove repetitions
      dplyr::mutate(PC = as.character(PC))
    
    p_r2[[ii]] <- ggplot(data = dat_true, aes(x = PC_name, y = R2, group = 1)) + # x = PC -> only numbers on axis, x = PC_name -> can give problems with PC10 being ordered before PC2;
      # group 1 to avoid warning/error for some reason
      geom_errorbar(aes(ymin = R2 - R2_std, ymax = R2 + R2_std),
                    color = Three_colorblind[1], linewidth = plot_linewidth, width = 0.4) + # standard deviation from bootstrapping
      # geom_bar(stat = "identity", position = position_dodge(), fill = Three_colorblind[1], width = 0.61) + #b07a4f, #9c6a5e, #643c3c
      geom_line(color = Three_colorblind[1]) +
      geom_point(color = Three_colorblind[1], size = point_size) + # actual value
      geom_jitter(data = dat_boot, aes(x = PC_name, y = R2_boot, group = 1), alpha = boot_alpha_small,
                  color = plot_elements_dark, shape = point_shape, size = 0.2, width = 0.1) + # bootstrapped values
      geom_point(aes(x = PC_name, y = R2_median), color = plot_elements_dark,
                 alpha = boot_alpha_main, shape = point_shape, size = point_size) + # median of bootstrap
      geom_text(aes(x = PC_name, y = R2 + R2_std + 2, label = paste0(R2 %>% round(digits = 1), "%")),
                nudge_x = 0.33, size = 2) + # values
      labs(title = "", x = "", y = "Explained variance") +
      theme_classic() +
      theme(title = element_blank(),
            text = element_text(size = normal_text),
            axis.line = element_line(color = graph_elements_dark),
            axis.ticks.x = element_line(color = graph_elements_dark),
            axis.ticks.y = element_blank(),
            axis.title = element_text(size = title_text, face = "bold"),
            # axis.title.x = element_blank(), # already specified in 'labs'
            axis.text = element_text(size = normal_text),
            axis.text.y = element_blank(),
            plot.margin = unit(c(0, 1, 0, 1), "cm"),
            legend.position = "none"
      ) +
      NULL
    
    
    
    ## C) loadings ----
    dat_boot <- pca_stats %>%
      dplyr::filter(PC <= n_pcs[1]) %>% # remove additional PCs
      dplyr::select(PC_name, var, loading_boot) %>% unique() # remove repetitions
    
    dat_true <- pca_stats %>%
      dplyr::filter(PC <= n_pcs[1]) %>% # remove additional PCs
      dplyr::select(PC_name, var, loading, loading_median, loading_std) %>% unique() # remove repetitions
    
    p_load[[ii]] <- ggplot(data = dat_true, aes(x = var, y = loading)) +
      facet_grid(. ~ PC_name, scales = "free_y") +
      geom_errorbar(aes(ymin = loading - loading_std, ymax = loading + loading_std), # loading_q25, ymax = loading_q75
                    color = Three_colorblind[2], linewidth = plot_linewidth, width = 0.9) + # standard error = std from bootstrapping
      geom_bar(stat = "identity", position = position_dodge(), fill = Three_colorblind[2]) + #b07a4f, #9c6a5e, #643c3c
      geom_hline(yintercept = 0, color = graph_elements_dark) +
      geom_jitter(data = dat_boot, aes(x = var, y = loading_boot), alpha = boot_alpha_small, color = plot_elements_dark,
                  shape = point_shape, size = 0.2, width = 0.1) + # bootstrapped values
      geom_point(aes(x = var, y = loading_median), alpha = boot_alpha_main, shape = point_shape,
                 size = point_size, color = plot_elements_dark) + # median of bootstrapped results
      coord_flip() +  # flip axis for better visualization
      scale_y_continuous(breaks = waiver(), n.breaks = 4) + # change x axis (y since it is flipped)
      labs(y = "Loadings", x = "", title = "") +
      theme_classic() +
      theme(title = element_text(size = normal_text, face = "bold"),
            text = element_text(size = normal_text),
            axis.line.x = element_line(color = graph_elements_dark),
            axis.line.y = element_blank(),
            axis.ticks.x = element_line(color = graph_elements_dark),
            axis.ticks.y = element_blank(),
            axis.title = element_text(size = title_text),
            axis.text = element_text(size = normal_text),
            axis.text.x = element_text(angle = x_angle, vjust = x_adjust),
            legend.position = "none",
            legend.title = element_text(size = title_text),
            legend.text = element_text(size = subtitle_text),
            legend.key.height = unit(1.0, "mm"),
            legend.key.width = unit(1.0, "mm"),
            plot.margin = unit(c(0, 0, 0, 0), "cm"),
            strip.text = element_text(face = "bold", size = title_text),
            strip.background = element_blank()
      ) +
      NULL
    
    
    
    ## D) contributions ----
    dat_boot <- pca_stats %>%
      dplyr::filter(PC <= n_pcs[1]) %>% # remove additional PCs
      dplyr::select(PC_name, var, contrib_boot) %>% unique() # remove repetitions
    
    dat_true <- pca_stats %>%
      dplyr::filter(PC <= n_pcs[1]) %>% # remove additional PCs
      dplyr::select(PC_name, var, contrib, contrib_median, contrib_std) %>% unique() # remove repetitions
    
    p_contr[[ii]] <- ggplot(data = dat_true, aes(x = var, y = contrib)) +
      facet_grid(. ~ PC_name, scales = "free_y") +
      geom_errorbar(aes(ymin = contrib_median - contrib_std, ymax = contrib_median + contrib_std), # ymin = contrib_q25, ymax = contrib_q75
                    color = Three_colorblind[3], linewidth = plot_linewidth, width = 0.9) + # standard error = standard deviation from bootstrapping
      geom_bar(stat = "identity", position = position_dodge(), fill = Three_colorblind[3]) + #4f85b0, #59918e, #3c6464
      geom_hline(yintercept = 0, color = graph_elements_dark) +
      geom_jitter(data = dat_boot, aes(x = var, y = contrib_boot), alpha = boot_alpha_small, color = plot_elements_dark,
                  shape = point_shape, size = 0.2, width = 0.1) + # bootstrapped values
      geom_point(aes(x = var, y = contrib_median), alpha = boot_alpha_main, shape = point_shape,
                 size = point_size, color = plot_elements_dark) + # median of bootstrapped results
      coord_flip() + # flip axis for better visualization
      scale_y_continuous(breaks = waiver(), n.breaks = 4) + # change x axis (y since it is flipped)
      labs(y = "Contribution [%]", x = "", title = "") +
      theme_classic() +
      theme(title = element_text(size = normal_text, face = "bold"),
            text = element_text(size = normal_text),
            axis.line.x = element_line(color = graph_elements_dark),
            axis.line.y = element_blank(),
            axis.ticks.x = element_line(color = graph_elements_dark),
            axis.ticks.y = element_blank(),
            axis.title = element_text(size = title_text),
            axis.text = element_text(size = normal_text),
            axis.text.x = element_text(angle = x_angle, vjust = x_adjust),
            legend.position = "none",
            legend.title = element_text(size = title_text),
            legend.text = element_text(size = subtitle_text),
            legend.key.height = unit(1.0, "mm"),
            legend.key.width = unit(1.0, "mm"),
            # plot.margin = unit(c(0, 0, 0, 0), "cm"),
            strip.text = element_text(face = "bold", size = title_text),
            strip.background = element_blank()
      ) +
      NULL
    
  
    
    ### Figures for the article Figure 1-3, Supplementary Figure 5-8 -----------
    # NB: cowplot retains absolute text sizes (rescales text in subplots)
    if ((qq %in% c(1:3) & plot_main == T) | qq %in% c(13, 35:38, 101)) { # "single" figures (Figure 1-3, Supplementary Figure 5-8)
      fig_top <- cowplot::plot_grid( # top row of final figure
        p_biplot[[ii]], p_r2[[ii]],
        align = "h",
        nrow = 1,
        ncol = 2,
        rel_widths = c(1.618, 1), # golden ratio
        labels = c("a", "b"),
        label_size = title_text
      )
      fig_bottom <- cowplot::plot_grid( # bottom row of final figure
        p_load[[ii]], p_contr[[ii]],
        align = "h",
        axis = "l",
        nrow = 1,
        ncol = 2,
        labels = c("c", "d"),
        label_size = title_text
      )
      
      
      fig_paper <- cowplot::plot_grid( # final figure
        fig_top, fig_bottom,
        align = "v",
        nrow = 2,
        ncol = 1,
        rel_heights = c(1.618, 1) # golden ratio (1.618 : 0.618047 = 1 : 0.381953)
      )
      
      
      
      ### Save Figure 1-3, Supplementary Figure 5-8 ----------------------------
      if (savedata) {
        ## Final figure
        ggplot2::ggsave(filename = glue::glue("results/{figure_name}.pdf"), plot = fig_paper, device = "pdf",
                        width = 180, height = 101.26, units = "mm", dpi = 600) # 180 mm width for Nature Communications
        ggplot2::ggsave(filename = glue::glue("results/{figure_name}.jpg"), plot = fig_paper, device = "jpeg",
                        width = 180, height = 101.26, units = "mm", dpi = 600) # 180 mm width for Nature Communications
        
      }
      
      
      
    } else if ((plot_main == F & qq %in% c(1:3)) | qq %in% c(11, 12, 21, 22, 31, 32)) { # "compound" figures: Supplementary Figures 1, 3, 8 (all sites + forests + enf)
      col_ii[[ii]] <- cowplot::plot_grid( # first column of final figure
        p_biplot[[ii]], p_r2[[ii]], p_load[[ii]], p_contr[[ii]],
        align = "h",
        axis = "l",
        nrow = 4,
        ncol = 1,
        rel_heights = c(60, 30, 40, 40),
        labels = letters[c(ii, ii + 3, ii + 6, ii + 9)],
        label_size = title_text
      )
    } # end of conditions to combine subplots
    
    
    ### Save compound figures and reset lists of plots -------------------------
    if (qq %in% c(12, 22, 32)) {
      ### Supplementary Figures 1, 4, 9 ----------------------------------------
      fig_paper <- cowplot::plot_grid( # final figure
        plotlist = col_ii,
        align = "h",
        axis = "l",
        nrow = 1,
        ncol = ii,
        labels = veg_sub_labels,
        label_size = title_text
      )
      
      
      ### Save Supplementary Figures 1, 4, 9 -----------------------------------
      if (savedata) {
        ## Final figure
        ggplot2::ggsave(filename = glue::glue("results/{figure_name}.pdf"), plot = fig_paper, device = "pdf",
                        width = 180, height = 160, units = "mm", dpi = 600) # 180 mm width (2 columns) and 185 max height (by long legend) for Nature Communications
        ggplot2::ggsave(filename = glue::glue("results/{figure_name}.jpg"), plot = fig_paper, device = "jpeg",
                        width = 180, height = 160, units = "mm", dpi = 600) # 180 mm width (2 columns) and 185 max height (by long legend) for Nature Communications
        
      }
      
      
      ### Reset plot lists -----------------------------------------------------
      p_biplot <- list(); p_r2 <- list(); p_load <- list(); p_contr <- list(); col_ii <- list()
      gc() # clean memory
      
      
    } # end of condition to save and reset compound figures
    
    
    
  } # end of loop
  
  
  
  
} # end of function
### Debug ----------------------------------------------------------------------
# debugonce(plot_PCAs)