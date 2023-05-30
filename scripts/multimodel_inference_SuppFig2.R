#### MULTIMODEL INFERENCE
# Selection and averaging of best models based on AICc
# cf. M. Fernández‐Martínez et al., ‘The role of climate, foliar stoichiometry and plant diversity on ecosystem carbon balance’, Global Change Biology, vol. 26, no. 12, Art. no. 12, 2020, doi: https://doi.org/10.1111/gcb.15385.

### Authors: Ulisse Gomarasca, Mirco Migliavacca


### Script options -------------------------------------------------------------
# Measure script run time
library(tictoc)
tic()


## Saving
savedata <- T # save output

if (savedata == TRUE) {
  save_path <- "results"
} else if (savedata == FALSE) {
  save_path <- NULL
}



### Utilities ------------------------------------------------------------------
## Packages
packages <- c("broom", "car", "cowplot", "dplyr", "ggplot2", "ggpubr", "hrbrthemes", "MuMIn",
              "purrr", "readr", "relaimpo", "reshape2", "rstatix", "strex", "stringr", "tidyr", "viridis")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

invisible(lapply(packages, library, character.only = TRUE))


## Functions
source("scripts/functions/dredged_relaimpo.R")
source("scripts/functions/plot_map.R")

## Other
source("scripts/themes/theme_myclassic.R")  # theme for plots
source("scripts/themes/theme_mylarge.R")    # theme for plots



### Import data ----------------------------------------------------------------
# Processed dataset
df <- read.table("Source data.csv", header = T, sep = ",")



### Initialize -----------------------------------------------------------------
relimp <- list()
p <- list()



### Analysis -------------------------------------------------------------------
for (qq in 1:2) {
  ### Subset variables ---------------------------------------------------------
  # Extract variables for the model (select e.g. GPPSAT and predictors)
  # Example from Burnham and Anderson (2002), page 100:
  # prevent fitting sub-models to different datasets
  ## Panel a: LAImax + wNmass + wLMA
  if (qq == 1) {
    df <- subset(dat,
                 select = c(
                   "SITE_ID", "GPPsat", "wLL", "wNmass", "wLMA", "LAImax"
                 )
    ) %>%
      tidyr::drop_na()
  ## Panel b: LAImax + wNarea
  } else if (qq == 2) {
    df <- subset(dat,
                 select = c(
                   "SITE_ID", "GPPsat", "wLL", "wNarea", "LAImax"
                 )
    ) %>%
      tidyr::drop_na()
  }
  
  
  
  ## Sites used ----
  sites <- df %>% dplyr::select(SITE_ID) %>%
    dplyr::left_join(dat %>% dplyr::select(SITE_ID, IGBP, latitude, longitude),
                     by = "SITE_ID")
  
  # Plot site locations
  CatCol <- c("#586158", "#C46B39", "#4DD8C0", "#3885AB", "#9C4DC4",
              "#C4AA4D", "#443396", "#CC99CC", "#88C44D", "#AB3232")
  
  # plot_map(data = sites, .group = IGBP, theme = "large", savepath = save_path) +
  #   scale_fill_manual(values = CatCol)
  
  # Subset only terms for analysis
  df <- df %>% dplyr::select(-SITE_ID)
  
  
  
  ### 1) Test collinearity (VIF) -----------------------------------------------
  # In questo caso ho usato un fitting di un generalized linear model, puoi usare anche un modello lineare ma nel tuo caso credo sia meglio un glm
  # Qua l’algoritmo fa un fitting di tutti i modelli possibili e li classifica in base a AICc
  
  options(na.action = "na.fail") # global options for na.action (needed)
  
  ## Generalized Linear Model with all predictors
  # (cf. normality assumption)
  fm1 <- glm(GPPsat ~ ., data = df) # generalized linear model
  
  ## Variance Inflation Factor:
  vif_excl <- bind_rows(vif(fm1))   # exclude variables with values over 10
  
  
  
  ### 2) Check assumptions for GLM ---------------------------------------------
  ## Normal distribution of predictors:
  # perform Shapiro-Wilk's test
  shapiro_stats <- df %>%
    rstatix::shapiro_test(vars = names(df)) %>%
    mutate(normality = if_else(p > 0.1, TRUE, FALSE))
  shapiro_stats
  
  
  ## Heteroscedasticity (homogeneity of residuals)
  # # perform Levene's test
  # levene_stats <- df %>% levene_test(formula = GPPsat ~ .) # ERROR!
  # Residual plot
  # plot(fm1)
  
  
  ### 3) Model selection -------------------------------------------------------
  ## Remove correlated predictors after VIF
  fm0 <- glm(GPPsat ~ ., data = df)
  
  
  # 3.1) feed output(s) to dredge separately and confront AIC
  # NB: output size of 'dredge' function increases exponentially with the number of predictors
  dd0 <- dredge(fm0, beta = "partial.sd")  # automated model selection
  
  
  # 3.2) Extract ensemble of best models
  max_delta <- 4
  dlt40 <- subset(dd0, delta < max_delta) # Extract models with difference in AIC from best model's AIC < 2 or 4. Generally < 4 is suggested unless more restrictive
  
  
  # 3.3) Summary
  summar <- summary(model.avg(object = dlt40, revised.var = FALSE)) # summarize results
  
  formulas0 <- get.models(dlt40, subset = TRUE)
  
  
  
  ### 4) Calculate importance of predictors ------------------------------------
  ## Relative importance over all models (lmg) and weighted mean of lmg
  figpanel <- ifelse(qq == 1, "a", "b")
  relimp[[qq]] <- dredged_relaimpo(df, "GPPsat", dlt40) %>%  # variable values are weighted lmg; R2 is weighted over all models
    tidyr::pivot_longer(cols = !c(R2, AICc), names_to = "variable", values_to = "lmg") %>%  # transform in long format with column for lmg
    dplyr::mutate(panel = figpanel,
                  n = nrow(df)) %>% 
    dplyr::relocate(panel, variable, .before = R2)
  
  
  
  ### Plot variable importance -------------------------------------------------
  p[[qq]] <- relimp[[qq]] %>% 
    ggplot(aes(x = variable, y = lmg)) +
    geom_segment(aes(y = 0, xend = variable, yend = lmg), color = "gray70", size = 8) +
    geom_point(color = "gray50", size = 24, shape = 19, show.legend = F) +
    geom_text(aes(label = paste0(format(lmg %>% round(digits = 3) * 100, nsmall = 1), "%")),
              nudge_x = 0.24, nudge_y = 0.08, size = 16) + # values
    coord_flip() +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 1.05)) + # avoid blank space after axis
    labs(x = "", y = "") +
    theme_classic() +
    theme(axis.text = element_text(size = 32),
          axis.text.x = element_blank(), # remove values on axis (when actual values are printed)
          axis.ticks.x = element_blank(),# remove ticks from empty axis
          # axis.title = element_text(size = 40, face = "bold"),
          plot.margin = unit(c(0, 1, 0, 0), "cm"), # avoid blank space around plot
          title = element_blank()
    ) +
    NULL
  
  
} # end for loop



### Combine panels a and b -----------------------------------------------------
p0 <- cowplot::plot_grid(p[[1]], p[[2]], align = "v", labels = c("a", "b"), label_size = 32)




### Save -----------------------------------------------------------------------
if (savedata == T) {
  # plot
  ggplot2::ggsave(filename = "results/Supplementary_Figure_2.jpg", plot = p0, device = "jpeg",
                  width = 508, height = 285.75, units = "mm", dpi = 600)
  
  # variable importance (lmg) values
  write_csv(relimp[[1]] %>% as_tibble() %>% bind_rows(relimp[[2]] %>% as_tibble()), # join two outputs in one file
            "results/relaimpo_parameters_all.csv")
}
##### End ----------------------------------------------------------------------
toc()