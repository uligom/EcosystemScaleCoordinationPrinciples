#### LINEAR MIXED MODEL

### Author: Ulisse Gomarasca
### Script options -------------------------------------------------------------
# Saving settings
savedata <- T # save output

if (savedata) {
  eval_file <- glue::glue("results/evaluation_linear_mixed_models.txt")
  txt <- "Linear mixed model analysis:"; print(txt); if (savedata) {cat(paste0(txt, "\n"), file = eval_file, append = F)}
}



### Utilities ------------------------------------------------------------------
## Packages
packages <- c("car", "dplyr", "ggplot2", "lme4", "MuMIn", "readr", "rlang", "tidyr")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

invisible(lapply(packages, library, character.only = TRUE))



### Data -----------------------------------------------------------------------
dat <- read.table("Source data.csv", header = T, sep = ",")



### Data distribution ----------------------------------------------------------
qqPlot(dat$GPPsat, "norm")



### Generalized linear Mixed Model ---------------------------------------------
options(na.action = "na.fail") # global options for na.action (needed)

dat %>% names()

## Convert LAI to categorical
dat <- dat %>% 
  mutate(LAImax_cat = as.character(round(LAImax, digits = 0)))


for (i in 1:2) {
  ## GPPsat ----
  if (i == 1) {y_varname <- "GPPsat"}
  
  ## RECOmax ----
  if (i == 2) {y_varname <- "RECOmax"}
  
  ## Formula
  y_sym <- rlang::sym(y_varname)
  formula <- glue::glue("{y_varname} ~ wNmass + wLMA + wLL + (LAImax | IGBP)")
  
  ## Subset data
  df <- dat %>% dplyr::select(!!y_sym, wNmass, wLMA, wLL, LAImax, IGBP) %>% drop_na()
  
  ## Linear mixed model ----
  lmm <- lme4::lmer(formula, df, REML = T)
  
  print(summary(lmm))
  
  ## Model performance
  extractAIC(lmm)
  
  ## Test of significance
  anov <- Anova(lmm)
  print(anov)
  
  
  ## Output
  ll <- format(round(unique(coef(lmm)[[1]]$wLL), digits = 2), nsmall = 2); if (anov[rownames(anov) == "wLL", ]$`Pr(>Chisq)` < 0.05) {ll <- paste0(ll, "*")}
  lma <- format(round(unique(coef(lmm)[[1]]$wLMA), digits = 2), nsmall = 2); if (anov[rownames(anov) == "wLMA", ]$`Pr(>Chisq)` < 0.05) {lma <- paste0(lma, "*")}
  nmass <- format(round(unique(coef(lmm)[[1]]$wNmass), digits = 2), nsmall = 2); if (anov[rownames(anov) == "wNmass", ]$`Pr(>Chisq)` < 0.05) {nmass <- paste0(nmass, "*")}
  
  lai <- format(round(attr(VarCorr(lmm)$IGBP, "stddev")[2], digits = 2), nsmall = 2)
  # igbp <- format(round(as.vector(VarCorr(lmm)$IGBP), digits = 2), nsmall = 2)
  
  txt <- glue::glue(
    "When predicting {y_varname}, the fixed effects were {ll} for wLL, {lma} for wLMA, and {nmass} for wNmass (asterisks denote significant effects based on anova results).
    The within group (IGBP) standard deviation of LAImax was {lai}.") # {lai} for LAImax groups
  print(txt); if (savedata) {cat(paste0(txt, "\n"), file = eval_file, append = T)}
  
  
  ## Plot
  CatCol <- c("#586158", "#C46B39", "#4DD8C0", "#3885AB", "#9C4DC4",
                       "#C4AA4D", "#443396", "#CC99CC", "#88C44D", "#AB3232")
  
  plot(fitted(lmm), residuals(lmm), xlab = "Fitted Values", ylab = "Residuals")
  abline(h = 0, lty = 2)
  lines(smooth.spline(fitted(lmm), residuals(lmm)))
  
  print(
    ggplot() +
    geom_point(aes(x = !!y_sym, y = predict(lmm)), data = df, alpha = 0.67, size = 2.5) +
    # scale_color_manual(values = CatCol) +
    geom_abline(slope = 1, color = "red3", linetype = "dashed", linewidth = 1) +
    theme_classic()
    )
}