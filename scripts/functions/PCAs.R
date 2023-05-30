PCAs = function(figs = c(1:3), boots = 499) {
  #### PRINCIPAL COMPONENT ANALYSIS
  
  ### Author: Ulisse Gomarasca, Mirco Migliavacca, Talie Musavi
  ### Script options -------------------------------------------------------------
  # Measure script run time
  library(tictoc)
  tic()
  
  savedata <- T # save output
  
  
  
  ### Utilities ----------------------------------------------------------------
  ## Packages:
  packages <- c("reshape2", "FactoMineR", "ade4",  "modelr",
                "tidyr", "dplyr", "readr", "stringr")#, "factoextra", "gridExtra", "ggplot2", "ggrepel", "ggridges", "cowplot")
  
  # Install packages not yet installed
  installed_packages <- packages %in% rownames(installed.packages())
  if (any(installed_packages == FALSE)) {
    install.packages(packages[!installed_packages])
  }
  
  invisible(lapply(packages, library, character.only = TRUE))
  

  
  ### Import data --------------------------------------------------------------
  df <- read.table("Input data.csv", header = T, sep = ",")
  
  
  
  ### Analysis options -----------------------------------------------------------
  ## Set number of runs (bootstrapping)
  tt <- boots # number of repetitions for Dray test and error bars. WARNING: time consuming
  
  
  
  ### Analysis -----------------------------------------------------------------
  ## Initialize site list
  site_list <- tibble() # initialize cumulative list of sites used in all analyses
  
  
  
  for (qq in figs) {
    ### Set Scenario for analysis ----------------------------------------------
    n_pcs <- NA # initialize number of PCs to retain
    ## 1) LEAF ECONOMICS SPECTRUM ####
    if (qq == 1){
      ### Saving options ----
      figure_name <- "Figure_1"
      
      
      ### Select variables and subset sites ------------------------------------
      ## Selecting EFPs
      EFPs_codes_4_PCA <- c("SITE_ID", "IGBP", "GPPsat", "wLL", "wNmass", "wLMA", "RECOmax")
      
      ## Filter
      df_subset <- df %>%
        dplyr::select(all_of(EFPs_codes_4_PCA)) %>%
        # dplyr::filter(IGBP != "ENF") %>% # OPTION: test bias of ENF by removing them
        tidyr::drop_na() # exclude NA (TO DO, only for most limiting variables? would need multiple imputation)
    }
    
    
    ## 11) LEAF ECONOMICS SPECTRUM - all forests ####
    if (qq == 11){
      ### Saving options ----
      figure_name <- "Supplementary_Figure_1_forests"
      
      
      ### Select number of PCs for plotting ----
      n_pcs <- 2 # NB: manual selection of x components for comparison with all sites
      
      
      ### Select variables and subset sites ------------------------------------
      ## Selecting EFPs
      EFPs_codes_4_PCA <- c("SITE_ID", "IGBP", "GPPsat", "wLL", "wNmass", "wLMA", "RECOmax")
      
      ## Filter
      df_subset <- df %>%
        dplyr::select(all_of(EFPs_codes_4_PCA)) %>%
        dplyr::filter(IGBP %in% c("DNF", "DBF", "EBF", "ENF", "MF")) %>% # filter for analysis on forests
        tidyr::drop_na() # exclude NA (TO DO, only for most limiting variables? would need multiple imputation)
    }
    
    
    ## 12) LEAF ECONOMICS SPECTRUM - evergreen needleleaf forests ####
    if (qq == 12){
      ### Saving options ----
      figure_name <- "Supplementary_Figure_1_enf"
      
      
      ### Select number of PCs for plotting ----
      n_pcs <- 2 # NB: manual selection of x components for comparison with all sites
      
      
      ### Select variables and subset sites ------------------------------------
      ## Selecting EFPs
      EFPs_codes_4_PCA <- c("SITE_ID", "IGBP", "GPPsat", "wLL", "wNmass", "wLMA", "RECOmax")
      
      ## Filter
      df_subset <- df %>%
        dplyr::select(all_of(EFPs_codes_4_PCA)) %>%
        dplyr::filter(IGBP == "ENF") %>% # filter for analysis on enf
        tidyr::drop_na() # exclude NA (TO DO, only for most limiting variables? would need multiple imputation)
    }
    
    
    ## 13) LEAF ECONOMICS SPECTRUM - wNarea ####
    if (qq == 13){
      ### Saving options ----
      figure_name <- "Supplementary_Figure_3"
      
      
      ### Select variables and subset sites ------------------------------------
      ## Selecting EFPs
      EFPs_codes_4_PCA <- c("SITE_ID", "IGBP", "GPPsat", "wLL", "wNarea", "RECOmax")
      
      ## Filter
      df_subset <- df %>%
        dplyr::select(all_of(EFPs_codes_4_PCA)) %>%
        # dplyr::filter(IGBP != "ENF") %>% # OPTION: test bias of ENF by removing them
        tidyr::drop_na() # exclude NA (TO DO, only for most limiting variables? would need multiple imputation)
    }
    
    
    ## 101) LEAF ECONOMICS SPECTRUM - GPP/LAI ####
    if (qq == 101){
      ### Saving options ----
      figure_name <- "LES_GPP_LAI"
      
      
      ### Select variables and subset sites --------------------------------------
      ## Selecting EFPs
      EFPs_codes_4_PCA <- c("SITE_ID", "IGBP", "GPP_LAI", "wLL", "wLMA", "wNmass", "RECOmax")
      
      ## Filter
      df_subset <- df %>%
        dplyr::select(all_of(EFPs_codes_4_PCA)) %>%
        # dplyr::filter(IGBP != "ENF") %>% # OPTION: test bias of ENF by removing them
        tidyr::drop_na() # exclude NA (TO DO, only for most limiting variables? would need multiple imputation)
    }
    
    
    ## 2) GLOBAL SPECTRUM ####
    if (qq == 2){
      ### Saving options ----
      figure_name <- "Figure_2"
      
      
      ### Select number of PCs for plotting ----
      n_pcs <- 3 # NB: manual selection of x components
      
      
      ### Select variables and subset sites ----
      ## Selecting EFPs
      EFPs_codes_4_PCA <- c("SITE_ID", "IGBP",
                            "GPPsat",
                            "wLMA", "wNmass", "wSSD",
                            "Hc", "LAImax")
      
      ## Filter
      df_subset <- df %>%
        dplyr::select(all_of(EFPs_codes_4_PCA)) %>%
        # dplyr::filter(IGBP != "ENF") %>% # OPTION: test bias of ENF by removing them
        tidyr::drop_na() # exclude NA (TO DO, only for most limiting variables? would need multiple imputation)
    }
    
    
    ## 21) GLOBAL SPECTRUM - all forests ####
    if (qq == 21){
      ### Saving options ----
      figure_name <- "Supplementary_Figure_4_forests"
      
      
      ### Select number of PCs for plotting ----
      n_pcs <- 3 # NB: manual selection of x components for comparison with all sites
      
      
      ### Select variables and subset sites ----
      ## Selecting EFPs
      EFPs_codes_4_PCA <- c("SITE_ID", "IGBP",
                            "GPPsat",
                            "wLMA", "wNmass", "wSSD",
                            "Hc", "LAImax")
      
      ## Filter
      df_subset <- df %>%
        dplyr::select(all_of(EFPs_codes_4_PCA)) %>%
        dplyr::filter(IGBP %in% c("DNF", "DBF", "EBF", "ENF", "MF")) %>% # filter for analysis on forests
        tidyr::drop_na() # exclude NA (TO DO, only for most limiting variables? would need multiple imputation)
    }
    
    
    ## 22) GLOBAL SPECTRUM - evergreen needleleaf forests ####
    if (qq == 22){
      ### Saving options ----
      figure_name <- "Supplementary_Figure_4_enf"
      
      
      ### Select number of PCs for plotting ----
      n_pcs <- 3 # NB: manual selection of x components for comparison with all sites
      
      
      ### Select variables and subset sites ----
      ## Selecting EFPs
      EFPs_codes_4_PCA <- c("SITE_ID", "IGBP",
                            "GPPsat",
                            "wLMA", "wNmass", "wSSD",
                            "Hc", "LAImax")
      
      ## Filter
      df_subset <- df %>%
        dplyr::select(all_of(EFPs_codes_4_PCA)) %>%
        dplyr::filter(IGBP == "ENF") %>% # filter for analysis on enf
        tidyr::drop_na() # exclude NA (TO DO, only for most limiting variables? would need multiple imputation)
    }
    
    
    ## 3) LEAST COST HYPOTHESIS ####
    if (qq == 3){
      ### Saving options ----
      figure_name <- "Figure_3"
      
      
      ### Select variables and subset sites --------------------------------------
      ## Selecting EFPs
      EFPs_codes_4_PCA <- c("SITE_ID", "IGBP", "PNUE", "Gsmax", "WUEt", "Hc", "EF", "Ta")
      
      ## Filter
      df_subset <- df %>%
        dplyr::select(all_of(EFPs_codes_4_PCA)) %>%
        # dplyr::filter(IGBP != "ENF") %>% # OPTION: test bias of ENF by removing them
        tidyr::drop_na() # exclude NA (TO DO, only for most limiting variables? would need multiple imputation)
    }
    
    
    ## 31) LEAST COST HYPOTHESIS - all forests ####
    if (qq == 31){
      ### Saving options ----
      figure_name <- "Supplementary_Figure_9_forests"
      
      
      ### Select number of PCs for plotting ----
      n_pcs <- 2 # NB: manual selection of x components for comparison with all sites
      
      
      ### Select variables and subset sites ----
      ## Selecting EFPs
      EFPs_codes_4_PCA <- c("SITE_ID", "IGBP", "PNUE", "Gsmax", "WUEt", "Hc", "EF", "Ta")
      
      ## Filter
      df_subset <- df %>%
        dplyr::select(all_of(EFPs_codes_4_PCA)) %>%
        dplyr::filter(IGBP %in% c("DNF", "DBF", "EBF", "ENF", "MF")) %>% # filter for analysis on forests
        tidyr::drop_na() # exclude NA (TO DO, only for most limiting variables? would need multiple imputation)
    }
    
    
    ## 32) LEAST COST HYPOTHESIS - evergreen needleleaf forests ####
    if (qq == 32){
      ### Saving options ----
      figure_name <- "Supplementary_Figure_9_enf"
      
      
      ### Select number of PCs for plotting ----
      n_pcs <- 2 # NB: manual selection of x components for comparison with all sites
      
      
      ### Select variables and subset sites ----
      ## Selecting EFPs
      EFPs_codes_4_PCA <- c("SITE_ID", "IGBP", "PNUE", "Gsmax", "WUEt", "Hc", "EF", "Ta")
      
      ## Filter
      df_subset <- df %>%
        dplyr::select(all_of(EFPs_codes_4_PCA)) %>%
        dplyr::filter(IGBP == "ENF") %>% # filter for analysis on enf
        tidyr::drop_na() # exclude NA (TO DO, only for most limiting variables? would need multiple imputation)
    }
    
    
    ## 35) LEAST COST HYPOTHESIS - PNUEarea variant ####
    if (qq == 35){
      ### Variant name (defines other options) ----
      variant <- "PNUEarea"
      
      
      ### Saving options ----
      figure_name <- "Supplementary_Figure_5"
      
      
      ### Select variables and subset sites --------------------------------------
      ## Selecting EFPs
      EFPs_codes_4_PCA <- c("SITE_ID", "IGBP", variant, "LAImax", "Gsmax", "WUEt", "Hc", "EF", "Ta")
      
      ## Filter
      df_subset <- df %>%
        dplyr::select(all_of(EFPs_codes_4_PCA)) %>%
        # dplyr::filter(IGBP != "ENF") %>% # OPTION: test bias of ENF by removing them
        tidyr::drop_na() # exclude NA (TO DO, only for most limiting variables? would need multiple imputation)
    }
    
    
    ## 36) LEAST COST HYPOTHESIS - APAR_N variant ####
    if (qq == 36){
      ### Variant name (defines other options) ----
      variant <- "APAR_N"
      
      
      ### Saving options ----
      figure_name <- "Supplementary_Figure_6"
      
      
      ### Select variables and subset sites --------------------------------------
      ## Selecting EFPs
      EFPs_codes_4_PCA <- c("SITE_ID", "IGBP", variant, "LAImax", "Gsmax", "WUEt", "Hc", "EF", "Ta")
      
      ## Filter
      df_subset <- df %>%
        dplyr::select(all_of(EFPs_codes_4_PCA)) %>%
        # dplyr::filter(IGBP != "ENF") %>% # OPTION: test bias of ENF by removing them
        tidyr::drop_na() # exclude NA (TO DO, only for most limiting variables? would need multiple imputation)
    }
    
    
    ## 37) LEAST COST HYPOTHESIS - WUEet variant ####
    if (qq == 37){
      ### Variant name (defines other options) ----
      variant <- "WUEet"
      
      
      ### Saving options ----
      figure_name <- "Supplementary_Figure_7"
      
      n_pcs <- 2
      
      ### Select variables and subset sites --------------------------------------
      ## Selecting EFPs
      EFPs_codes_4_PCA <- c("SITE_ID", "IGBP", "PNUE", "Gsmax", variant, "Hc", "EF", "Ta")
      
      ## Filter
      df_subset <- df %>%
        dplyr::select(all_of(EFPs_codes_4_PCA)) %>%
        # dplyr::filter(IGBP != "ENF") %>% # OPTION: test bias of ENF by removing them
        tidyr::drop_na() # exclude NA (TO DO, only for most limiting variables? would need multiple imputation)
    }
    
    
    ## 38) LEAST COST HYPOTHESIS - uWUE variant ####
    if (qq == 38){
      ### Variant name (defines other options) ----
      variant <- "uWUE"
      
      
      ### Saving options ----
      figure_name <- "Supplementary_Figure_8"
      
      n_pcs <- 2
      
      ### Select variables and subset sites --------------------------------------
      ## Selecting EFPs
      EFPs_codes_4_PCA <- c("SITE_ID", "IGBP", "PNUE", "Gsmax", variant, "Hc", "EF", "Ta")
      
      ## Filter
      df_subset <- df %>%
        dplyr::select(all_of(EFPs_codes_4_PCA)) %>%
        # dplyr::filter(IGBP != "ENF") %>% # OPTION: test bias of ENF by removing them
        tidyr::drop_na() # exclude NA (TO DO, only for most limiting variables? would need multiple imputation)
    }
    
    
    
    ##############################################################################
    print(glue::glue("==========================================================
                  ==========================================================
                   Running analysis for '{figure_name}'....."))
    
    
    ### Note down used sites -----------------------------------------------------
    site_list <- bind_rows(site_list, tibble(df_subset %>% select(SITE_ID), analysis = figure_name)) %>% 
      unique() %>% 
      arrange(SITE_ID)
    
    
    
    ### Bootstrap ----------------------------------------------------------------
    set.seed(8675309) # set seed for reproducibility
    
    df_boot <- df_subset %>% modelr::bootstrap(n = tt) # bootstrap
    
    
    
    ### PCA ----------------------------------------------------------------------
    ## Extract bootstrapped data
    # Initialize
    N_PCS <- tibble() # number of PCs as retained PCs by Dray method if not otherwise specified
    pca_stats <- tibble() # variable contributions and loadings
    R2 <- c() # explained variance
    
    # Loop
    for (j in 1:tt) {
      ## Extract bootstrapped data for jth bootstrap
      dat <- df_boot %>% 
        slice(j) %>% # select jth row (with bootstrapped dataframe)
        pull(strap) %>% # extract list
        as.data.frame() # extract data
      
      ## Run PCA without multiple imputation
      pca_result <- FactoMineR::PCA(dat %>% dplyr::select(-SITE_ID, -IGBP), scale.unit = T, ncp = 10, graph = F)
      
      
      
      ### Check uncertainties and significance ----------------------------------
      ## Uncertainty PCA following Dray et al., 2008 ----
      # and the methodology described in Diaz et al., 2015
      tab <- na.omit(dat %>% dplyr::select(-SITE_ID, -IGBP))
      pca1 <- ade4::dudi.pca(tab, center = TRUE, scale = TRUE, scannf = FALSE, nf = 10)
      
      ## Dray test
      pc_tested <- ade4::testdim(pca1, nrepet = 999)
      
      
      
      ### Export bootstrapped PCA outputs ----------------------------------------
      N_PCS <- N_PCS %>% 
        bind_rows(tibble(strap = j, n_pcs = pc_tested$nb.cor)) # number of PCs for jth run
      
      pca_stats <- bind_rows(pca_stats,
                             pca_result$var$contrib %>% # add contributions
                               as_tibble(rownames = "var") %>%
                               pivot_longer(cols = !var, names_to = "PC", values_to = "contrib") %>% 
                               left_join(pca_result$var$coord %>% # add loadings
                                           as_tibble(rownames = "var") %>%
                                           pivot_longer(cols = !var, names_to = "PC", values_to = "loading"),
                                         by = c("var", "PC")
                               ) %>% 
                               mutate(PC = str_sub(PC, start = 5), # PC name to digit
                                      strap = j) # bootstrap run number
      ) # variable contributions and loadings
      
      R2 <- bind_rows(R2,
                      tibble(PC = pca_result[["eig"]] %>% rownames(),
                             exp_var = pca_result[["eig"]][,2],
                             strap = j) %>% 
                        mutate(PC = str_sub(PC, start = 6)) # PC name to digit
      )
      
    }
    
    
    
    ### Statistics of bootstrapped extracted PCA outputs -------------------------
    ## Output of Dray method ----
    # Number of PCs to retain
    N_PCS <- N_PCS %>%
      group_by(n_pcs) %>% 
      summarise(n_rep = n()) %>% # count of repeated values
      mutate(retained = n_rep / tt * 100) # percentage of runs
    # print(pc_tested)
    pc_ret <- N_PCS %>% filter(retained == max(retained))
    
    # print(N_PCS)
    print(paste0("Number of statistical significant components according to Dray method (Dray et al., 2008) was ",
                 pc_ret[1,1], " in ", round(pc_ret[1,3], digits = 1), "% of runs."))
    
    if (is.na(n_pcs)) {
      n_pcs <- N_PCS %>% 
        filter(retained == max(retained)) %>% 
        select(n_pcs) %>% 
        unlist() %>% unname()
    } # set number of PCs as retained PCs by Dray method if not otherwise specified
    
    
    ## Contributions and loadings ----
    pca_stats <- pca_stats %>% 
      group_by(PC, var) %>% 
      mutate(
        contrib_mean = mean(contrib),
        contrib_median = median(contrib),
        contrib_std = sd(contrib),
        # contrib_q25 = quantile(contrib, 0.25), contrib_q75 = quantile(contrib, 0.75),
        loading_mean = mean(loading),
        loading_median = median(loading),
        loading_std = sd(loading),
        # loading_q25 = quantile(loading, 0.25), loading_q75 = quantile(loading, 0.75)
      ) %>% 
      ungroup() %>% 
      dplyr::rename(contrib_boot = contrib, loading_boot = loading) # rename to avoid messy join later
    
    ## Modify PC names
    pca_stats <- pca_stats %>%
      mutate(PC_name = paste0("PC", PC))
    
    
    
    ## Explained variance ----
    R2 <- R2 %>% 
      group_by(PC) %>% 
      mutate(
        R2_mean = mean(exp_var),
        R2_median = median(exp_var),
        R2_std = sd(exp_var),
        # R2_q25 = quantile(exp_var, 0.25), R2_q75 = quantile(exp_var, 0.75)
      ) %>% 
      ungroup() %>% 
      dplyr::rename(R2_boot = exp_var) # rename to avoid messy join later
    
    ## Add to stats
    pca_stats <- pca_stats %>% left_join(R2, by = c("PC", "strap"))
    
    
    
    ### Repeat PCA on main subset ------------------------------------------------
    pca_result <- FactoMineR::PCA(df_subset %>% dplyr::select(-SITE_ID, -IGBP), scale.unit = T, ncp = 9, graph = F) # repeat PCA on actual subset (not bootstrapped)
    
    ## Include actual values for non-bootstrapped dataset
    pca_stats <- pca_stats %>% 
      dplyr::left_join( # add R2 of analysis on actual data (not the mean of bootstrapping)
        tibble(PC = pca_result[["eig"]] %>% rownames(),
               R2 = pca_result[["eig"]][,2]
        ) %>% 
          mutate(PC = str_sub(PC, start = 6)), # extract PC numbers
        by = "PC"
      ) %>% 
      dplyr::left_join( # add contribution values of analysis on actual data (not the mean of bootstrapping)
        pca_result$var$contrib %>% # add contributions
          as_tibble(rownames = "var") %>%
          pivot_longer(cols = !var, names_to = "PC", values_to = "contrib") %>% 
          mutate(PC = str_sub(PC, start = 5)), # extract PC numbers
        by = c("PC", "var")
      ) %>%
      dplyr::left_join( # add loading values of analysis on actual data (not the mean of bootstrapping)
        pca_result$var$coord %>% # add contributions
          as_tibble(rownames = "var") %>%
          pivot_longer(cols = !var, names_to = "PC", values_to = "loading") %>% 
          mutate(PC = str_sub(PC, start = 5)), # extract PC numbers
        by = c("PC", "var")
      )
    
    ## Include percentage of retention for PCs (how often PC is retained over bootstraps)
    pca_stats <- pca_stats %>% 
      dplyr::left_join(N_PCS %>% dplyr::mutate(PC = n_pcs %>% as.character) %>% dplyr::select(PC, retained),
                       by = "PC"
      ) 
    
    ## Clean output
    pca_stats <- pca_stats %>% 
      dplyr::mutate(PC = as.integer(PC)) %>% 
      dplyr::relocate(PC_name, .after = PC) %>% 
      dplyr::relocate(R2, .before = R2_boot) %>% 
      dplyr::relocate(contrib, .before = contrib_boot) %>% 
      dplyr::relocate(loading, .before = loading_boot) %>% 
      dplyr::arrange(PC)
    
    
    
    ### Outcome ----------------------------------------------------------------
    R2_N_label <- paste0("Retained axis (", n_pcs[1], ") explained ", round(sum(pca_result$eig[1:n_pcs[1],2]), 1), "% of variance (N = ", nrow(df_subset), ").")
    print(R2_N_label)
    
    
    
    ### Save -------------------------------------------------------------------
    if (savedata == T){
      ## PCA results
      save(pca_result, file = glue::glue("results/PCA_results_{figure_name}.RData"))
      
      ## Retained PCs
      save(n_pcs, file = glue::glue("results/PCA_PCs_retained_{figure_name}.RData"))
      
      ## PCA statistics
      # With bootstrapping:
      write.table(pca_stats, file = glue::glue("results/PCA_stats_bootstrapped_{figure_name}.csv"), row.names = F, sep = ";")
      # Without bootstrapping:
      write.table(pca_stats %>% dplyr::select(-contrib_boot, -loading_boot, -R2_boot, -strap) %>% unique(),
                  file = glue::glue("results/PCA_stats_{figure_name}.csv"), row.names = F, sep = ";")
      
      ## Final data input for each analysis
      write.table(df_subset, file = glue::glue("results/PCA_input_{figure_name}.csv"), row.names = F, sep = ";")
    }
  } # end loop for different scenarios (analysis sections)
  
  
  
  ### Save site list -------------------------------------------------------------
  if (savedata == T ) {
    # Save list of used sites
    write.table(site_list %>% dplyr::select(SITE_ID) %>% unique(), file = glue::glue("results/site_list.csv"), row.names = F, sep = ";")
    
    if (figs %in% c(1:3)) {
      # Save IGBP stats
      site_igbp <- site_list %>%
        dplyr::filter(analysis %in% c("Figure_1", "Figure_2", "Figure_3")) %>%
        dplyr::left_join(df %>% select(SITE_ID, IGBP)) %>%
        dplyr::group_by(analysis) %>%
        dplyr::mutate(n_sites = n()) %>%
        dplyr::group_by(IGBP, analysis) %>%
        dplyr::summarise(site_fraction = n() * 100 / n_sites) %>%
        unique() %>%
        dplyr::mutate(analysis = case_when(
          analysis == "Figure_1" ~ "Leaf economics spectrum",
          analysis == "Figure_2" ~ "Global spectrum of plant form and function",
          analysis == "Figure_3" ~ "Least cost hypothesis"
          # analysis == "Supplementary_Figure_3" ~ "Leaf economics spectrum - Supplementary Figure 3",
          # analysis == "Supplementary_Figure_5" ~ "Least cost hypothesis - Supplementary Figure 5",
          # analysis == "Supplementary_Figure_6" ~ "Least cost hypothesis - Supplementary Figure 6",
          # analysis == "Supplementary_Figure_7" ~ "Least cost hypothesis - Supplementary Figure 7",
          # analysis == "Supplementary_Figure_8" ~ "Least cost hypothesis - Supplementary Figure 8"
        )
        ) %>%
        dplyr::arrange(IGBP, analysis) %>%
        tidyr::pivot_wider(names_from = analysis, values_from = site_fraction)
      
      write_csv(site_igbp, file = "results/Table_S7.csv")
    }
  }
  ### End ------------------------------------------------------------------------
  toc()
  
  
}
# debugonce(PCAs)