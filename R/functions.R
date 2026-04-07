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
make_llm_agreement_heatmap <- function(df,
                                       vars,
                                       paper_id_col = "filename",
                                       label_list = NULL,
                                       title = "LLM agreement heatmap",
                                       subtitle = "Each cell shows how many of 3 models matched the human label",
                                       low_to_high = TRUE,
                                       reorder = TRUE) {
  
  stopifnot(is.data.frame(df))
  stopifnot(is.character(vars))
  stopifnot(length(paper_id_col) == 1)
  
  if (!paper_id_col %in% names(df)) {
    stop("`paper_id_col` not found in `df`.")
  }
  
  needed_cols <- c(
    unlist(purrr::map(vars, ~ c(
      paste0(.x, "_human"),
      paste0(.x, "_ChatGPT"),
      paste0(.x, "_claude"),
      paste0(.x, "_gemini")
    ))),
    paper_id_col
  )
  
  missing_cols <- setdiff(needed_cols, names(df))
  if (length(missing_cols) > 0) {
    stop(
      "These required columns are missing from `df`: ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  heatmap_df <- purrr::map_dfr(vars, function(v) {
    human_col <- paste0(v, "_human")
    chat_col  <- paste0(v, "_ChatGPT")
    cla_col   <- paste0(v, "_claude")
    gem_col   <- paste0(v, "_gemini")
    
    pretty_label <- if (!is.null(label_list) && v %in% names(label_list)) {
      unname(label_list[[v]])
    } else {
      v
    }
    
    tibble::tibble(
      paper_id = stringr::word(as.character(df[[paper_id_col]]), 1) |>
        stringr::str_remove("\\.$"),
      variable = v,
      label = pretty_label,
      human = df[[human_col]],
      chat  = df[[chat_col]],
      cla   = df[[cla_col]],
      gem   = df[[gem_col]]
    ) |>
      dplyr::mutate(
        chat_match = dplyr::if_else(is.na(human) | is.na(chat), NA_integer_, as.integer(human == chat)),
        cla_match  = dplyr::if_else(is.na(human) | is.na(cla),  NA_integer_, as.integer(human == cla)),
        gem_match  = dplyr::if_else(is.na(human) | is.na(gem),  NA_integer_, as.integer(human == gem)),
        n_models_scorable = rowSums(!is.na(dplyr::pick(chat_match, cla_match, gem_match))),
        n_models_correct  = rowSums(dplyr::pick(chat_match, cla_match, gem_match), na.rm = TRUE),
        n_models_correct  = dplyr::if_else(n_models_scorable == 0, NA_integer_, n_models_correct)
      )
  })
  
  if (reorder) {
    row_summary <- heatmap_df |>
      dplyr::group_by(paper_id) |>
      dplyr::summarise(mean_models_correct = mean(n_models_correct, na.rm = TRUE), .groups = "drop")
    
    col_summary <- heatmap_df |>
      dplyr::group_by(label) |>
      dplyr::summarise(mean_models_correct = mean(n_models_correct, na.rm = TRUE), .groups = "drop")
    
    if (low_to_high) {
      row_order <- row_summary |>
        dplyr::arrange(mean_models_correct) |>
        dplyr::pull(paper_id)
      
      col_order <- col_summary |>
        dplyr::arrange(mean_models_correct) |>
        dplyr::pull(label)
    } else {
      row_order <- row_summary |>
        dplyr::arrange(dplyr::desc(mean_models_correct)) |>
        dplyr::pull(paper_id)
      
      col_order <- col_summary |>
        dplyr::arrange(dplyr::desc(mean_models_correct)) |>
        dplyr::pull(label)
    }
  } else {
    row_order <- df[[paper_id_col]] |>
      as.character() |>
      stringr::word(1) |>
      stringr::str_remove("\\.$") |>
      unique()
    
    col_order <- purrr::map_chr(vars, function(v) {
      if (!is.null(label_list) && v %in% names(label_list)) {
        unname(label_list[[v]])
      } else {
        v
      }
    })
  }
  
  heatmap_df <- heatmap_df |>
    dplyr::mutate(
      paper_id = factor(paper_id, levels = rev(row_order)),
      label = factor(label, levels = col_order),
      n_models_correct_f = factor(as.character(n_models_correct), levels = c("0", "1", "2", "3"))
    )
  
  # invisible layer to force all 4 legend boxes to appear, using geom_tile
  legend_df <- tibble::tibble(
    label = factor(rep(col_order[1], 4), levels = col_order),
    paper_id = factor(rep(rev(row_order)[1], 4), levels = levels(heatmap_df$paper_id)),
    n_models_correct_f = factor(c("0", "1", "2", "3"), levels = c("0", "1", "2", "3"))
  )
  
  ggplot2::ggplot(
    heatmap_df,
    ggplot2::aes(x = label, y = paper_id, fill = n_models_correct_f)
  ) +
    ggplot2::geom_tile(color = "white", linewidth = 0.25) +
    ggplot2::geom_tile(
      data = legend_df,
      ggplot2::aes(x = label, y = paper_id, fill = n_models_correct_f),
      inherit.aes = FALSE,
      alpha = 0,
      show.legend = TRUE
    ) +
    ggplot2::scale_fill_manual(
      values = c(
        "0" = "#b2182b",
        "1" = "#ef8a62",
        "2" = "#fddbc7",
        "3" = "#d1e5f0"
      ),
      breaks = c("0", "1", "2", "3"),
      drop = FALSE,
      name = "Models matching\nhuman"
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_legend(
        override.aes = list(alpha = 1)
      )
    ) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = NULL,
      y = NULL
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      axis.text.y = ggplot2::element_text(size = 7)
    )
}