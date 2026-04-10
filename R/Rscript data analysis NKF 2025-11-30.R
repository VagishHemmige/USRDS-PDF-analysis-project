#Data analysis script for NKF.  This will run the full 170+ PDFs


#How well can LLMs differentiate new data analyses from papers that are not new data analyses?
df_wide %>% count(is_this_paper_a_new_data_analysis_human,
             is_this_paper_a_new_data_analysis_ChatGPT)

df_wide %>% count(is_this_paper_a_new_data_analysis_human,
                  is_this_paper_a_new_data_analysis_claude)

df_wide %>% count(is_this_paper_a_new_data_analysis_human,
                  is_this_paper_a_new_data_analysis_gemini)

#When limiting to true new analyses, how well can LLMs spot transplant-related papers?
df_wide%>%filter(is_this_paper_a_new_data_analysis_human==TRUE)%>%
  count(is_this_a_transplant_related_study_human,
        is_this_a_transplant_related_study_ChatGPT)
  
df_wide%>%filter(is_this_paper_a_new_data_analysis_human==TRUE)%>%
  count(is_this_a_transplant_related_study_human,
        is_this_a_transplant_related_study_claude)

df_wide%>%filter(is_this_paper_a_new_data_analysis_human==TRUE)%>%
  count(is_this_a_transplant_related_study_human,
        is_this_a_transplant_related_study_gemini)


#Tables from the above

new_analysis_table <- bind_rows(
  df_wide %>%
    count(
      human = is_this_paper_a_new_data_analysis_human,
      llm = is_this_paper_a_new_data_analysis_ChatGPT
    ) %>%
    mutate(model = "ChatGPT"),
  
  df_wide %>%
    count(
      human = is_this_paper_a_new_data_analysis_human,
      llm = is_this_paper_a_new_data_analysis_claude
    ) %>%
    mutate(model = "Claude"),
  
  df_wide %>%
    count(
      human = is_this_paper_a_new_data_analysis_human,
      llm = is_this_paper_a_new_data_analysis_gemini
    ) %>%
    mutate(model = "Gemini")
) %>%
  mutate(
    cell = case_when(
      human == FALSE & llm == FALSE ~ "Human FALSE / LLM FALSE",
      human == FALSE & llm == TRUE  ~ "Human FALSE / LLM TRUE",
      human == TRUE  & llm == FALSE ~ "Human TRUE / LLM FALSE",
      human == TRUE  & llm == TRUE  ~ "Human TRUE / LLM TRUE"
    )
  ) %>%
  select(model, cell, n) %>%
  pivot_wider(
    names_from = cell,
    values_from = n,
    values_fill = 0
  )

new_analysis_gt<-new_analysis_table %>%
  gt(rowname_col = "model") %>%
  tab_stubhead(label = "Model") %>%
  tab_header(title = "New data analysis classification")



gtsave(
  filename = "Kidney conference abstract/new_analysis_gt.png",
  data = new_analysis_gt,
)
saveRDS(new_analysis_gt, "Kidney conference abstract/new_analysis_gt.rds")



transplant_table <- bind_rows(
  df_wide %>%
    filter(is_this_paper_a_new_data_analysis_human == TRUE) %>%
    count(
      human = is_this_a_transplant_related_study_human,
      llm = is_this_a_transplant_related_study_ChatGPT
    ) %>%
    mutate(model = "ChatGPT"),
  
  df_wide %>%
    filter(is_this_paper_a_new_data_analysis_human == TRUE) %>%
    count(
      human = is_this_a_transplant_related_study_human,
      llm = is_this_a_transplant_related_study_claude
    ) %>%
    mutate(model = "Claude"),
  
  df_wide %>%
    filter(is_this_paper_a_new_data_analysis_human == TRUE) %>%
    count(
      human = is_this_a_transplant_related_study_human,
      llm = is_this_a_transplant_related_study_gemini
    ) %>%
    mutate(model = "Gemini")
) %>%
  mutate(
    cell = case_when(
      human == FALSE & llm == FALSE ~ "Human FALSE / LLM FALSE",
      human == FALSE & llm == TRUE  ~ "Human FALSE / LLM TRUE",
      human == TRUE  & llm == FALSE ~ "Human TRUE / LLM FALSE",
      human == TRUE  & llm == TRUE  ~ "Human TRUE / LLM TRUE"
    )
  ) %>%
  select(model, cell, n) %>%
  pivot_wider(
    names_from = cell,
    values_from = n,
    values_fill = 0
  )

transplant_gt<-transplant_table %>%
  gt(rowname_col = "model") %>%
  tab_stubhead(label = "Model") %>%
  tab_header(title = "Transplant-related study classification")


gtsave(
  filename = "Kidney conference abstract/transplant_gt.png",
  data = transplant_gt,
)
saveRDS(transplant_gt, "Kidney conference abstract/transplant_gt.rds")



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

gtsave(
  filename = "Kidney conference abstract/timing_table_full.png",
  data = timing_gt,
)
saveRDS(timing_gt, "Kidney conference abstract/timing_gt.rds")

#---- Filter to human-positive dataset----

df_filt_full <- df_wide%>%
  filter(
    is_this_paper_a_new_data_analysis_human == TRUE,
  )





#---- Compute full kappa table----

kappa_results_full <- purrr::map_dfr(human_vars, function(h) {
  base <- sub("_human$", "", h)
  
  tibble::tibble(
    variable = base,
    ChatGPT = compute_kappa(h, paste0(base, "_ChatGPT"), df_filt_full),
    Claude  = compute_kappa(h, paste0(base, "_claude"),  df_filt_full),
    Gemini  = compute_kappa(h, paste0(base, "_gemini"),  df_filt_full)
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

kappa_table_full <- kappa_results_full %>%
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
    title = "Cohen’s κ: LLM vs Human Across All Variables") %>%
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

kappa_table_full
gtsave(
  data = kappa_table_full,
  filename = "Kidney conference abstract/kappa_table_full.html"
)
saveRDS(kappa_table_full, "Kidney conference abstract/kappa_table_full.rds")

#Create legend to supplement kappa table
kappa_legend_gt<-create_kappa_legend_gt()
gtsave(
  data = kappa_legend_gt,
  filename = "Kidney conference abstract/kappa_table_legend_full.html"
)
saveRDS(kappa_legend_gt, "Kidney conference abstract/kappa_legend_gt.rds")

#Create a table with raw proportions


#---- Build combined dataset----
breakdown_table_full <- map_dfr(human_vars, get_breakdown, df_filt = df_filt_full)


#---- Produce formatted gt table with spanners and combined N/D

breakdown_gt_full <- breakdown_table_full %>%
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

breakdown_gt_full
gtsave(
  data = breakdown_gt_full,
  filename = "Kidney conference abstract/breakdown_gt_full.html"
)
saveRDS(breakdown_gt_full, "Kidney conference abstract/breakdown_gt_full.rds")

#---- Heatmaps ----

file_vars_full <- names(label_list)[str_detect(names(label_list), "^files_")]

files_res_full <- make_llm_agreement_heatmap(
  df = df_filt_full,
  vars = file_vars_full,
  paper_id_col = "filename",
  label_list = label_list,
  title = "Files used heatmap",
  reorder = FALSE,
  show_paper_ids = FALSE
)

files_res_full
ggsave(
  plot = files_res_full,
  filename = "Kidney conference abstract/files_res_full.png"
)




languages_vars_full <- names(label_list)[str_detect(names(label_list), "^languages_")]

languages_res_full <- make_llm_agreement_heatmap(
  df = df_filt_full,
  vars = languages_vars_full,
  paper_id_col = "filename",
  label_list = label_list,
  title = "Languages used heatmap",
  reorder = FALSE,
  show_paper_ids = FALSE
)

languages_res_full
ggsave(
  plot = languages_res_full,
  filename = "Kidney conference abstract/languages_res_full.png"
)


component_vars_full <- names(label_list)[str_detect(names(label_list), "^component_")]

component_res_full <- make_llm_agreement_heatmap(
  df = df_filt_full,
  vars = component_vars_full,
  paper_id_col = "filename",
  label_list = label_list,
  title = "Components used heatmap",
  reorder = FALSE,
  show_paper_ids = FALSE
)

component_res_full
ggsave(
  plot = component_res_full,
  filename = "Kidney conference abstract/component_res_full.png"
)



other_vars_full <- setdiff(
  setdiff(
    names(label_list),
    c(file_vars_full, languages_vars_full, component_vars_full)
  ),
  "did_the_authors_explicitly_state_what_files_they_used"
)

other_res_full <- make_llm_agreement_heatmap(
  df = df_filt_full,
  vars = other_vars_full,
  paper_id_col = "filename",
  label_list = label_list,
  title = "Other study features heatmap",
  reorder = FALSE,
  show_paper_ids = FALSE
)

other_res_full

ggsave(
  filename = "Kidney conference abstract/other_res_full.png",
  plot = other_res_full,
)