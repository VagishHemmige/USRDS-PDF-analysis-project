# Function that safely normalizes to logical
convert_to_logical <- function(x) {
  if (is.logical(x)) return(x)
  
  x_clean <- trimws(tolower(as.character(x)))
  
  case_when(
    x_clean %in% c("true", "yes", "1")  ~ TRUE,
    x_clean %in% c("false", "no", "0") ~ FALSE,
    TRUE ~ NA
  )
}


# Compute kappa helper
compute_kappa <- function(hvar, mvar) {
  if (!(mvar %in% names(df_filt))) return(NA_real_)
  
  df_k <- df_filt %>%
    select(h = all_of(hvar), m = all_of(mvar)) %>%
    drop_na()
  
  if (nrow(df_k) == 0) return(NA_real_)
  
  kappa2(df_k, weight = "unweighted")$value
}



# Function to compute numerators, denominators, percentages

get_breakdown <- function(hvar) {
  
  base <- sub("_human$", "", hvar)
  chat <- paste0(base, "_ChatGPT")
  cla  <- paste0(base, "_claude")
  gem  <- paste0(base, "_gemini")
  
  model_vars <- c(chat, cla, gem)
  model_vars <- model_vars[model_vars %in% names(df_filt)]
  if (length(model_vars) == 0) return(NULL)
  
  df_filt %>%
    select(human = all_of(hvar), all_of(model_vars)) %>%
    mutate(human = as.logical(human)) %>%
    filter(!is.na(human)) %>%
    group_by(human) %>%
    summarise(
      chat_num = if (chat %in% names(.)) sum(!!sym(chat), na.rm = TRUE) else NA_real_,
      cla_num  = if (cla  %in% names(.)) sum(!!sym(cla),  na.rm = TRUE) else NA_real_,
      gem_num  = if (gem  %in% names(.)) sum(!!sym(gem),  na.rm = TRUE) else NA_real_,
      
      den      = n(),
      
      chat_pct = chat_num / den,
      cla_pct  = cla_num / den,
      gem_pct  = gem_num / den,
      
      .groups = "drop"
    ) %>%
    mutate(
      variable = base,
      chat_nd  = paste0(chat_num, "/", den),
      cla_nd   = paste0(cla_num, "/", den),
      gem_nd   = paste0(gem_num, "/", den)
    ) %>%
    select(variable, human,
           chat_nd, chat_pct,
           cla_nd, cla_pct,
           gem_nd, gem_pct)
}
