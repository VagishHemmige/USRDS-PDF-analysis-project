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
compute_kappa <- function(hvar, mvar, df_filt) {
  if (!(mvar %in% names(df_filt))) return(NA_real_)
  
  df_k <- df_filt %>%
    select(h = all_of(hvar), m = all_of(mvar)) %>%
    drop_na()
  
  if (nrow(df_k) == 0) return(NA_real_)
  
  kappa2(df_k, weight = "unweighted")$value
}



# Function to compute numerators, denominators, percentages

get_breakdown <- function(hvar, df_filt) {
  
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



#Function to label variables in a df being piped into a table-creating object

add_label_for_gt <- function(df, labels = label_list, var_col = "variable") {
  if (!var_col %in% names(df)) {
    return(df)
  }
  
  labels_chr <- unlist(labels)
  df$label <- labels_chr[df[[var_col]]]
  df$label[is.na(df$label)] <- df[[var_col]][is.na(df$label)]
  
  df
}



#Function to label variables in a df being piped into a table-creating object

kappa_fill <- function(x) {
  case_when(
    is.na(x)          ~ "#FFFFFF",  # missing
    x < 0             ~ "#e6b8b7",  # negative
    x == 0            ~ "#f4cccc",  # exact zero
    x < 0.21          ~ "#fce5cd",  # slight
    x < 0.41          ~ "#fff2cc",  # fair
    x < 0.61          ~ "#d9ead3",  # moderate
    x < 0.81          ~ "#b6d7a8",  # substantial
    x <= 1            ~ "#93c47d",  # almost perfect
    TRUE              ~ "#FFFFFF"
  )
}



create_kappa_legend_gt <- function(x) {

#Creates DF to explain kappa tables
legend_kappa_df <- tibble::tribble(
  ~Swatch, ~Interpretation,   ~Range,
  -0.10,   "Negative",        "< 0",
  0.00,   "Exact zero",      "0.000",
  0.10,   "Slight",          "0.001–0.20",
  0.30,   "Fair",            "0.21–0.40",
  0.50,   "Moderate",        "0.41–0.60",
  0.70,   "Substantial",     "0.61–0.80",
  0.90,   "Almost perfect",  "0.81–1.00",
  NA_real_, "Not estimable", "-"
)

#Creates table with legend for kappa figures
kappa_legend_gt <- legend_kappa_df %>%
  gt() %>%
  cols_label(
    Swatch = "",
    Interpretation = "Agreement",
    Range = "Range"
  ) %>%
  fmt(
    columns = Swatch,
    fns = function(x) rep("", length(x))
  ) %>%
  sub_missing(
    columns = Swatch,
    missing_text = ""
  ) %>%
  data_color(
    columns = Swatch,
    fn = kappa_fill
  ) %>%
  cols_width(
    Swatch ~ px(32),
    Interpretation ~ px(150),
    Range ~ px(95)
  ) %>%
  tab_header(
    title = md("**Cohen’s κ legend**")
  ) %>%
  tab_style(
    style = cell_fill(color = "#f3f3f3"),
    locations = cells_body(
      columns = Swatch,
      rows = is.na(Swatch)
    )
  ) %>%
  tab_options(
    table.font.size = px(11),
    data_row.padding = px(4),
    heading.align = "left"
  )
kappa_legend_gt
}