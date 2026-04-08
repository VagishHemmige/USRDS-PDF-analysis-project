#Data analysis script for ATC




#---- Filter to human-positive dataset and transplant only----

df_filt_atc <- df_wide %>%
  filter(
    is_this_paper_a_new_data_analysis_human == TRUE,
    is_this_a_transplant_related_study_human == TRUE
  )


#---- Table of times to run analysis----



timing_gt<-timing_table %>%
  gt() %>%
  fmt_number(
    columns = `Time (minutes)`,
    decimals = 0
  )%>%
  tab_header(
    title = "Runtime of LLMs Used in the Analysis",
    subtitle = "Wall-clock time in minutes to process the full dataset"
  )

gt::gtsave(
  data = timing_gt,
  filename = "ATC 2026 abstract/timing_table_atc.png"
)
saveRDS(timing_gt, "ATC 2026 abstract/timing_table_atc.rds")

#---- Compute full kappa table----

kappa_results_atc <- purrr::map_dfr(human_vars, function(h) {
  base <- sub("_human$", "", h)
  
  tibble::tibble(
    variable = base,
    ChatGPT = compute_kappa(h, paste0(base, "_ChatGPT"), df_filt_atc),
    Claude  = compute_kappa(h, paste0(base, "_claude"),  df_filt_atc),
    Gemini  = compute_kappa(h, paste0(base, "_gemini"),  df_filt_atc)
  )%>%
    mutate(
    across(c(ChatGPT, Claude, Gemini), ~ ifelse(is.nan(.x), NA_real_, .x))
  )
})%>%
  filter(!variable %in% c(
    "which_pieces_of_data_were_abstracted_from_the_files",
    "which_tasks_would_not_be_covered",
    "which_type_of_transplant_study_is_this",
    "who_funded_the_study",
    "year",
    "journal",
    "title",
    "authors",
    "languages_used_SQL",
    "languages_used_Matlab",
    "languages_used_Julia"
  ))


#---- Make a clean gt table----

kappa_table_atc <- kappa_results_atc %>%
  add_label_for_gt() %>%
  gt(rowname_col = "label") %>%
  gt::cols_hide(columns = variable) %>%
  fmt_number(
    columns = c(ChatGPT, Claude, Gemini),
    decimals = 3
  ) %>%
  cols_align_decimal(
    columns = c(ChatGPT, Claude, Gemini)
  ) %>%
  tab_header(
    title = "Cohen’s κ: LLM vs Human Across All Variables",
    subtitle = "Dataset of 44 transplant-related papers"
  ) %>%
  gt::tab_stubhead(label = "Variable") %>%
  sub_missing(
    columns = c(ChatGPT, Claude, Gemini),
    missing_text = "-"
  )%>%
  gt::tab_row_group(
    label = "Programming languages",
    rows = grepl("^languages_", variable)
  ) %>%
  gt::tab_row_group(
    label = "Files used",
    rows = grepl("^files_", variable)
  )%>%
  gt::tab_row_group(
    label = "Components of USRDS used",
    rows = grepl("^component_", variable)
  ) %>%
  gt::tab_stub_indent(
    rows = grepl("^(languages_|files_|component_)", variable),
    indent = 4
  )%>%
  tab_footnote(
    footnote = "κ not estimable, usually due to insufficient variation or no usable paired observations.",
    locations = cells_body(columns = ChatGPT, rows = is.na(ChatGPT)),
    placement="right"
  ) %>%
  tab_footnote(
    footnote = "κ not estimable, usually due to insufficient variation or no usable paired observations.",
    locations = cells_body(columns = Claude, rows = is.na(Claude)),
    placement="right"
  ) %>%
  tab_footnote(
    footnote = "κ not estimable, usually due to insufficient variation or no usable paired observations.",
    locations = cells_body(columns = Gemini, rows = is.na(Gemini)),
    placement="right"
  )%>%
  data_color(
    columns = c(ChatGPT, Claude, Gemini),
    fn = kappa_fill
  )

kappa_table_atc
gtsave(
  data = kappa_table_atc,
  filename = "ATC 2026 abstract/kappa_table_atc.png"
)
saveRDS(kappa_table_atc, "ATC 2026 abstract/kappa_table_atc.rds")

#Create legend to supplement kappa table
kappa_legend_gt<-create_kappa_legend_gt()
gtsave(
  data = kappa_legend_gt,
  filename = "ATC 2026 abstract/kappa_table_legend_atc.png"
)
saveRDS(kappa_legend_gt, "ATC 2026 abstract/kappa_legend_gt.rds")




#Create a table with raw proportions


#---- Build combined dataset----
breakdown_table_atc <- map_dfr(human_vars, get_breakdown, df_filt = df_filt_atc)


#---- Produce formatted gt table with spanners and combined N/D

breakdown_gt_atc <- breakdown_table_atc %>%
  add_label_for_gt() %>%
  dplyr::select(-variable) %>%
  gt(
    groupname_col = "label",
    rowname_col = "human"
  ) %>%
  fmt_percent(
    columns = c(chat_pct, cla_pct, gem_pct),
    decimals = 1
  ) %>%
  cols_label(
    chat_nd = "Num/Den",
    chat_pct = "%",
    cla_nd = "Num/Den",
    cla_pct = "%",
    gem_nd = "Num/Den",
    gem_pct = "%"
  ) %>%
  tab_stubhead(label = md("**Human**")) %>%
  tab_spanner(
    label = md("**ChatGPT**"),
    columns = c(chat_nd, chat_pct)
  ) %>%
  tab_spanner(
    label = md("**Claude**"),
    columns = c(cla_nd, cla_pct)
  ) %>%
  tab_spanner(
    label = md("**Gemini**"),
    columns = c(gem_nd, gem_pct)
  ) %>%
  tab_options(
    row_group.as_column = TRUE,
    row_group.font.weight = "bold",
    table.font.size = px(13),
    heading.align = "left"
  )

breakdown_gt_atc
gtsave(
  data = breakdown_gt_atc,
  filename = "ATC 2026 abstract/breakdown_gt_atc.png"
)
saveRDS(breakdown_gt_atc, "ATC 2026 abstract/breakdown_gt_atc.rds")

#---- Heatmaps ----

file_vars_atc <- names(label_list)[str_detect(names(label_list), "^files_")]

files_res_atc <- make_llm_agreement_heatmap(
  df = df_filt_atc,
  vars = file_vars_atc,
  paper_id_col = "filename",
  label_list = label_list,
  title = "Files used heatmap",
  reorder = FALSE
)

files_res_atc
ggsave(
  plot = files_res_atc,
  filename = "ATC 2026 abstract/files_res_atc.png"
)




languages_vars_atc <- names(label_list)[str_detect(names(label_list), "^languages_")]

languages_res_atc <- make_llm_agreement_heatmap(
  df = df_filt_atc,
  vars = languages_vars_atc,
  paper_id_col = "filename",
  label_list = label_list,
  title = "Languages used heatmap",
  reorder = FALSE
)

languages_res_atc
ggsave(
  plot = languages_res_atc,
  filename = "ATC 2026 abstract/languages_res_atc.png"
)




component_vars_atc <- names(label_list)[str_detect(names(label_list), "^component_")]

component_res_atc <- make_llm_agreement_heatmap(
  df = df_filt_atc,
  vars = component_vars_atc,
  paper_id_col = "filename",
  label_list = label_list,
  title = "Components used heatmap",
  reorder = FALSE
)

component_res_atc
ggsave(
  plot = component_res_atc,
  filename = "ATC 2026 abstract/component_res_atc.png"
)



other_vars_atc <- setdiff(
  setdiff(
    names(label_list),
    c(file_vars_atc, languages_vars_atc, component_vars_atc)
  ),
  "did_the_authors_explicitly_state_what_files_they_used"
)

other_res_atc <- make_llm_agreement_heatmap(
  df = df_filt_atc,
  vars = other_vars_atc,
  paper_id_col = "filename",
  label_list = label_list,
  title = "Other study features heatmap",
  reorder = FALSE
)

other_res_atc

ggsave(
  filename = "ATC 2026 abstract/other_res_atc.png",
  plot = other_res_atc,
)