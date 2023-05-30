#### FUNCTION TO ITERATIVELY PERFORM RELATIVE IMPORTANCE ON A SET OF MODELS
# Typical input is a model inference output (function MuMIn::dredge)
# or a subset thereof.

### Version history ------------------------------------------------------------
# changed number extraction from strings to function strex::str_extract_number

### Arguments ------------------------------------------------------------------
# data      Input tibble with all needed predictor and predicted variables.
# y         Predicted variable.
# models    Tibble with information on coefficients and model weights in the
#           same form as the 'dredge' function output.


### Function -------------------------------------------------------------------
dredged_relaimpo <- function(data, y = "GPPsat", models) {
  ## Utilities ----
  require(dplyr)
  require(relaimpo)
  require(stringr)
  require(tidyr)
  
  ## Summary of dredge object ----
  summar <- summary(model.avg(object = models, revised.var = FALSE))
  
  ## Extract variable names from models ----
  # Extract term coding
  term_translation <- dplyr::tibble(
      term = names(attributes(summar$msTable)$term.codes),
      term.code = unname(attributes(summar$msTable)$term.codes)
  )
  
  # Term codes in models
  model_terms <- attributes(summar$msTable)$row.names
  # model_terms <- model_terms %>% tibble() # convert to tibbles
  
  for (i in 1:length(model_terms)) {
    ## Formulas for glm ----
    term.code <- strex::str_extract_numbers(model_terms[i]) %>% unlist() # extract codes for variables used
    if (length(term.code) == 1) {term.code <- str_extract_all(model_terms[i], "[:digit:]{1}") %>% unlist() %>% as.integer()} # extract codes for variables used again with different function since 'model.avg' gives different outputs

    single_terms <- dplyr::tibble(term.code = term.code) %>% # first column with codes
      dplyr::left_join(term_translation, by = "term.code") # second column with symbols
    
    model_strings <- paste0(y, " ~ ", paste(single_terms$term, collapse = " + ")) %>% 
      as.formula() # formulas for glm
    
    
    ## GLM ----
    fm <- glm(model_strings, data = data)
    
    
    ## Relative importance ----
    relimp <- relaimpo::calc.relimp(fm, rela = T)
    
    # extract lmg, R2 and AICc as tibble
    lmg_i <- dplyr::tibble(term = names(relimp@lmg),           # extract variable names
                           lmg = unname(relimp@lmg)) %>%       # extract lmg values for variables
      dplyr::mutate(weight = models$weight[i] %>% as.double()) # append model weight
    r2_i <- dplyr::tibble(R2 = relimp@R2) %>%                  # extract R2
      dplyr::mutate(weight = models$weight[i] %>% as.double()) # append model weight
    aic_i <- dplyr::tibble(AICc = models$AICc[i]) %>%            # extract AICc
      dplyr::mutate(weight = models$weight[i] %>% as.double()) # append model weight
    
    # bind ith lmg, R2 and AICc data to tibble with all models in long format
    if (i == 1) {
      lmg_all <- lmg_i
      r2_all <- r2_i
      aic_all <- aic_i
    } else {
      lmg_all <- lmg_all %>% rbind(lmg_i)
      r2_all <- r2_all %>% rbind(r2_i)
      aic_all <- aic_all %>% rbind(aic_i)
    }
  }
  ## Weighted mean of relative importance (lmg) ----
  # based on weights from dredge output
  lmg_all <- lmg_all %>% 
    dplyr::mutate(wlmg = lmg * weight) %>%  # calculate products
    dplyr::group_by(term) %>% 
    dplyr::summarise(w_lmg = sum(wlmg)) %>% # calculate weighted mean (i.e. summation of products over all models)
    dplyr::ungroup() %>% 
    dplyr::arrange(dplyr::desc(w_lmg)) %>% 
    tidyr::pivot_wider(names_from = term, values_from = w_lmg)
  
  ## Weighted mean of R2 from relative importance ----
  r2_all <- r2_all %>% 
    dplyr::mutate(wR2 = R2 * weight) %>% # calculate products
    dplyr::summarise(R2 = sum(unique(wR2))) %>% # calculate weighted mean for R2
    dplyr::ungroup()
  
  ## Weighted mean of AICc ----
  aic_all <- aic_all %>% 
    dplyr::mutate(wAICc = AICc * weight) %>% # calculate products
    dplyr::summarise(AICc = sum(unique(wAICc))) %>% # calculate weighted mean for AICc
    dplyr::ungroup()
    
  ## Output single dataframe with weighted lmg and R2 ----
  weighted_relimp <- dplyr:: bind_cols(lmg_all, r2_all, aic_all)
  
  return(weighted_relimp)
}
### Debug ----------------------------------------------------------------------
# debugonce(dredged_relaimpo)